-- |
-- Module      : Fusion.Plugin
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : pranaysashank@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Stream fusion depends on the GHC case-of-case transformations eliminating
-- intermediate constructors.  Case-of-case transformation in turn depends on
-- inlining. During core-to-core transformations GHC may create several
-- internal bindings (e.g. join points) which may not get inlined because their
-- size is bigger than GHC's inlining threshold. Even though we know that after
-- fusion the resulting code would be smaller and more efficient. The
-- programmer cannot force inlining of these bindings as there is no way for
-- the programmer to address these bindings at the source level because they
-- are internal, generated during core-to-core transformations. As a result
-- stream fusion fails unpredictably depending on whether GHC was able to
-- inline the internal bindings or not.
--
-- [See GHC ticket #17075](https://gitlab.haskell.org/ghc/ghc/issues/17075) for
-- more details.
--
-- This plugin provides the programmer with a way to annotate certain types
-- using a custom 'ForceFusion' annotation. The programmer would annotate the
-- types that are to be eliminated by fusion via case-of-case transformations.
-- During the simplifier phase the plugin goes through the relevant bindings
-- and if one of these types are found inside a binding then that binding is
-- marked to be inlined irrespective of the size.
--
-- At the right places, fusion can provide dramatic performance improvements
-- (e.g. 10x) to the code.

{-# LANGUAGE CPP #-}

module Fusion.Plugin
    (
    -- * Using the Plugin
    -- $using

    -- * Implementation Details
    -- $impl

    -- * Results
    -- $results
      plugin
    )
where

-- Explicit/qualified imports
import Control.Monad (when)
import Data.Generics.Schemes (everywhere)
import Data.Generics.Aliases (mkT)

import qualified Data.List as DL

-- Implicit imports
import GhcPlugins

-- Imports from this package
import Fusion.Plugin.Types (ForceFusion)

-- $using
--
-- This plugin was primarily motivated by fusion issues discovered in
-- [streamly](https://github.com/composewell/streamly) but it can be used in
-- general.
--
-- To use this plugin, add this package to your @build-depends@
-- and pass the following to your ghc-options:
--
-- @
-- ghc-options: -O2 -fplugin=Fusion.Plugin
-- @

-- $impl
--
-- The plugin runs after the simplifier phase 0. It finds all non recursive
-- join point bindings whose definition begins with a case match on a type that
-- is annotated with 'ForceFusion'. It then sets AlwaysInlinePragma on those
-- bindings. This is followed by two runs of a gentle simplify pass that does
-- both inlining and case-of-case. This is followed by the rest of CoreToDos.

-- TODO:
--
-- This inlining could further create a recursive join point that does an
-- explicit case match on a type that would benefit again from inlining, so in
-- the second run we should create a loop breaker and transform the recursive
-- join point to a non-recursive join point and inline. This is not currently
-- done, the machinery is already available, just create a loop breaker for Let
-- Rec in `setInlineOnBndrs`.

-- $results
--
-- This plugin has been used extensively in the streaming library
-- [streamly](https://github.com/composewell/streamly).  Several file IO
-- benchmarks have shown 2x-6x improvements. With the use of this plugin stream
-- fusion in streamly has become much more predictable which has been verified
-- by inspecting the core generated by GHC and by inspection testing for the
-- presence of the stream state constructors.

-------------------------------------------------------------------------------
-- Set always INLINE on a binder
-------------------------------------------------------------------------------

unfoldCompulsory :: Arity -> Unfolding -> Unfolding
unfoldCompulsory arity cuf@CoreUnfolding{} =
    cuf {uf_src=InlineStable, uf_guidance = UnfWhen arity True True}
unfoldCompulsory _ x = x -- NoUnfolding

-- Sets the inline pragma on a bndr, and forgets the unfolding.
setAlwaysInlineOnBndr :: CoreBndr -> CoreBndr
setAlwaysInlineOnBndr n =
    let info =
            case zapUsageInfo $ idInfo n of
                Just i -> i
                Nothing ->
                    error "The impossible happened!! Or GHC changed their api."
        unf = unfoldingInfo info
        info' =
            setUnfoldingInfo
                (setInlinePragInfo info alwaysInlinePragma)
                (unfoldCompulsory (arityInfo info) unf)
     in lazySetIdInfo n info'

--TODO: Replace self-recursive definitions with a loop breaker.
setInlineOnBndrs :: [CoreBndr] -> CoreExpr -> CoreExpr
setInlineOnBndrs bndrs = everywhere $ mkT go
  where
    go :: CoreExpr -> CoreExpr
    go (Let (NonRec nn e) expr1)
        | any (nn ==) bndrs = Let (NonRec (setAlwaysInlineOnBndr nn) e) expr1
    go x = x

-------------------------------------------------------------------------------
-- Inspect case alternatives for interesting constructor matches
-------------------------------------------------------------------------------

-- | Checks whether a case alternative contains a type with the
-- annotation.  Only checks the first typed element in the list, so
-- only pass alternatives from one case expression.

altsContainsAnn :: UniqFM [ForceFusion] -> [Alt CoreBndr] -> Bool
altsContainsAnn _ [] = False
altsContainsAnn anns ((DataAlt dcon, _, _):_) =
    case lookupUFM anns (getUnique $ dataConTyCon dcon) of
        Nothing -> False
        Just _ -> True
altsContainsAnn anns ((DEFAULT, _, _):alts) = altsContainsAnn anns alts
altsContainsAnn _ _ = False

-------------------------------------------------------------------------------
-- Determine if a let binder contains a case match on an annotated type
-------------------------------------------------------------------------------

-- returns the Bndrs, that are either of the form
-- joinrec { $w$g0 x y z = case y of predicateAlt -> ... } -> returns [$w$go]
-- join { $j1_sGH1 x y z = case y of predicateAlt -> ... } -> returns [$j1_sGH1]
--
-- Can check the call site and return only those that would enable
-- case-of-known constructor to kick in. Or is that not relevant?
-- This only concentrates on explicit Let's, doesn't care about top level
-- Case or Lam or App.
letBndrsThatAreCases :: ([Alt CoreBndr] -> Bool) -> CoreExpr -> [CoreBndr]
letBndrsThatAreCases f expr = go undefined False expr
  where
    go :: CoreBndr -> Bool -> CoreExpr -> [CoreBndr]
    go b _ (App expr1 expr2) = go b False expr1 ++ go b False expr2
    go b x (Lam _ expr1) = go b x expr1
    go b _ (Let bndr expr1) = goLet bndr ++ go b False expr1
    go b True (Case _ _ _ alts) =
        if f alts
            then b : (alts >>= (\(_, _, expr1) -> go undefined False expr1))
            else alts >>= (\(_, _, expr1) -> go undefined False expr1)
    go b _ (Case _ _ _ alts) = alts >>= (\(_, _, expr1) -> go b False expr1)
    go b _ (Cast expr1 _) = go b False expr1
    go _ _ _ = []

    goLet :: CoreBind -> [CoreBndr]
    goLet (NonRec b expr1) = go b True expr1
    goLet (Rec bs) = bs >>= (\(b, expr1) -> goLet $ NonRec b expr1)

-------------------------------------------------------------------------------
-- Core-to-core pass to mark interesting binders to be always inlined
-------------------------------------------------------------------------------

data ReportMode = ReportSilent | ReportWarn | ReportVerbose

markInline :: ReportMode -> Bool -> Bool -> ModGuts -> CoreM ModGuts
markInline reportMode failIt transform guts = do
    dflags <- getDynFlags
    anns <- getAnnotations deserializeWithData guts
    bindsOnlyPass (mapM (transformBind dflags anns)) guts
  where
    transformBind ::
           DynFlags -> UniqFM [ForceFusion] -> CoreBind -> CoreM CoreBind
    transformBind dflags anns (NonRec b expr) = do
        let annotated = letBndrsThatAreCases (altsContainsAnn anns) expr
        let uniqueAnns = DL.nub annotated

        when (uniqueAnns /= []) $ do
            -- XXX can we print the whole path leading up to this binder?
            let msg = "In ["
                      ++ showSDoc dflags (ppr b)
                      ++ "] binders "
                      ++ showSDoc dflags (ppr uniqueAnns)
                      ++ " match on ForceFusion annotated types"
            case reportMode of
                ReportSilent -> return ()
                ReportWarn -> putMsgS msg
                ReportVerbose -> putMsgS msg
            when failIt $ error "failing"

        let expr' =
                if transform
                then setInlineOnBndrs uniqueAnns expr
                else expr
        return (NonRec b expr')

    transformBind _ _ bndr =
        -- This is probably wrong, but we don't need it for now.
        --mapM_ (\(b, expr) -> transformBind dflags anns (NonRec b expr)) bs
        return bndr

-------------------------------------------------------------------------------
-- Install our plugin core pass
-------------------------------------------------------------------------------

-- Inserts the given list of 'CoreToDo' after the simplifier phase @n@.
insertAfterSimplPhase :: Int -> [CoreToDo] -> [CoreToDo] -> [CoreToDo]
insertAfterSimplPhase i ctds todos' = go ctds
  where
    go [] = []
    go (todo@(CoreDoSimplify _ SimplMode {sm_phase = Phase o}):todos)
        | o == i = todo : (todos' ++ todos)
        | otherwise = todo : go todos
    go (todo:todos) = todo : go todos

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
#if MIN_VERSION_ghc(8,6,0)
install _ todos = do
    dflags <- getDynFlags
    let doMarkInline opt failIt transform =
            CoreDoPluginPass "Inline Join Points"
                             (markInline opt failIt transform)
        simplsimplify =
            CoreDoSimplify
                (maxSimplIterations dflags)
                SimplMode
                    { sm_phase = InitialPhase
                    , sm_names = ["Fusion Plugin Inlining"]
                    , sm_dflags = dflags
                    , sm_rules = gopt Opt_EnableRewriteRules dflags
                    , sm_eta_expand = gopt Opt_DoLambdaEtaExpansion dflags
                    , sm_inline = True
                    , sm_case_case = True
                    }
    -- We run our plugin once the simplifier finishes phase 0,
    -- followed by a gentle simplifier which inlines and case-cases
    -- twice. TODO: The gentle simplifier runs on the whole program,
    -- however it might be better to call `simplifyExpr` on the
    -- expression directly.
    return $
        insertAfterSimplPhase
            0
            todos
            [ doMarkInline ReportSilent False True
            , simplsimplify
            , doMarkInline ReportSilent False True
            , simplsimplify
            , doMarkInline ReportWarn False False
            ]
#else
install _ todos = do
    putMsgS "Warning! fusion-plugin does nothing on ghc versions prior to 8.6"
    return todos
#endif

plugin :: Plugin
plugin = defaultPlugin {installCoreToDos = install}
