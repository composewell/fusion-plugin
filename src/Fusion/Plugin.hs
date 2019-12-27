-- |
-- Module      : Fusion.Plugin
-- Copyright   : (c) 2019 Pranay Sashank
--
-- License     : BSD-3-Clause
-- Maintainer  : pranaysashank@gmail.com
-- Stability   : experimental
-- Portability : GHC

-- Run plugin with option `-fplugin=Fusion.Plugin` together with `-O2`.
--
-- Plugin is currently very experimental and fragile, use it with care.
--
-- This plugin inlines non recursive join points whose definition
-- begins with a case match on a type that is annotated with
-- ForceCaseCase. When everything goes well, this can fuse and
-- completely eliminate intermediate constructors resulting in drastic
-- performance gains. Here are some major improvements on a 143MB file:
--
{-
| Benchmark Name                                    | Old    | New     |
|---------------------------------------------------+--------+---------|
| readStream/wordcount                              | 1.969s | 308.1ms |
| decode-encode/utf8-arrays                         | 4.159s | 2.952s  |
| decode-encode/utf8                                | 5.708s | 2.117s  |
| splitting/predicate/wordsBy isSpace (word count)  | 2.050s | 311.5ms |
| splitting/long-pattern/splitOnSuffixSeq abc...xyz | 2.558s | 423.3ms |
-}
--
-- The performance improvements suggest that this is something that
-- would be nice to further pursue with a more principled approach
-- than present here.
--
-- This plugin currently runs after the simplifier runs phase 0
-- followed by a gentle simplify that does both inlining, and
-- case-case twice and then runs the rest of the CoreToDos. This
-- inlining could further create a recursive join point that does an
-- explicit case match on a type that would benefit again from
-- inlining, so in the second run we should create a loop breaker and
-- transform the recursive join point to a non-recursive join point
-- and inline. This is not currently done, the machinery is already
-- available, just create a loop breaker for Let Rec in
-- `setInlineOnBndrs`.

module Fusion.Plugin
    ( plugin
    )
where

import BasicTypes
import GhcPlugins hiding ((<>))

import Data.Generics.Schemes
import Data.Generics.Aliases

import qualified Data.List as DL

import Fusion.Plugin.Types

plugin :: Plugin
plugin =
    defaultPlugin {installCoreToDos = install}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = do
    dflags <- getDynFlags
    let myplugin = CoreDoPluginPass "Inline Join Points" pass
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
            [myplugin, simplsimplify, myplugin, simplsimplify]

-- Inserts the given list of 'CoreToDo' after the simplifier phase @n@.
insertAfterSimplPhase :: Int -> [CoreToDo] -> [CoreToDo] -> [CoreToDo]
insertAfterSimplPhase i ctds todos' = go ctds
  where
    go [] = []
    go (todo@(CoreDoSimplify _ SimplMode {sm_phase = Phase o}):todos)
        | o == i = todo : (todos' ++ todos)
        | otherwise = todo : go todos
    go (todo:todos) = todo : go todos

pass :: ModGuts -> CoreM ModGuts
pass guts = do
    dflags <- getDynFlags
    anns <- getAnnotations deserializeWithData guts
    bindsOnlyPass (mapM (transformBind dflags anns)) guts
  where
    transformBind ::
           DynFlags -> UniqFM [ForceCaseCase] -> CoreBind -> CoreM CoreBind
    transformBind dflags anns bndr@(NonRec b expr) = do
        --putMsgS $ "binding named " ++ showSDoc dflags (ppr b)
        let lets = DL.nub $ letBndrsThatAreCases (altsContainsAnn anns) expr
        -- when (not $ null lets) $ putMsgS $ "Bndrs of required Case Alts\n" ++ showSDoc dflags (ppr $ map (\l -> (l, (uf_tmpl . unfoldingInfo . idInfo) l)) lets)
        let expr' = setInlineOnBndrs lets expr
        return (NonRec b expr')

    -- This is probably wrong, but we don't need it for now.
    transformBind _ _ bndr =
        --putMsgS "Pickle Rick:\n"
        --mapM_ (\(b, expr) -> transformBind dflags anns (NonRec b expr)) bs
        --putMsgS "Pickle Rick ends\n"
        return bndr

-- Checks whether a case alternative contains a type with the
-- annotation.  Only checks the first typed element in the list, so
-- only pass alternatives from one case expression.
altsContainsAnn :: UniqFM [ForceCaseCase] -> [Alt CoreBndr] -> Bool
altsContainsAnn _ [] = False
altsContainsAnn anns ((DataAlt dcon, _, _):_) =
    case lookupUFM anns (getUnique $ dataConTyCon dcon) of
        Nothing -> False
        Just _ -> True
altsContainsAnn anns ((DEFAULT, _, _):alts) = altsContainsAnn anns alts
altsContainsAnn _ _ = False

-- returns the Bndrs, that are either of the form
-- joinrec { $w$g0 x y z = case y of predicateAlt -> ... } -> returns [$w$go]
-- join { $j1_sGH1 x y z = case y of predicateAlt -> ... } -> returns [$j1_sGH1]
--
-- Can check the call site and return only those that would enable
-- case-of-known constructor to kick in. Or is that not relevant?
-- This only concentrates on explicit Let's, doesn't care about top level
-- Case or Lam or App.
letBndrsThatAreCases :: ([Alt CoreBndr] -> Bool) -> CoreExpr -> [CoreBndr]
letBndrsThatAreCases pred expr = letBndrsThatAreCases' undefined False expr
  where
    letBndrsThatAreCases' :: CoreBndr -> Bool -> CoreExpr -> [CoreBndr]
    letBndrsThatAreCases' b _ (App expr1 expr2) =
        letBndrsThatAreCases' b False expr1 <>
        letBndrsThatAreCases' b False expr2
    letBndrsThatAreCases' b x (Lam _ expr) =
        letBndrsThatAreCases' b x expr
    letBndrsThatAreCases' b _ (Let bndr expr) =
        letBndrsFromBndr bndr <> letBndrsThatAreCases' b False expr
    letBndrsThatAreCases' b True (Case _ _ _ alts) =
        if pred alts
            then b :
                 (alts >>=
                  (\(_, _, expr) -> letBndrsThatAreCases' undefined False expr))
            else alts >>=
                 (\(_, _, expr) -> letBndrsThatAreCases' undefined False expr)
    letBndrsThatAreCases' b _ (Case _ _ _ alts) =
        alts >>= (\(_, _, expr) -> letBndrsThatAreCases' b False expr)
    letBndrsThatAreCases' b _ (Cast expr _) = letBndrsThatAreCases' b False expr
    letBndrsThatAreCases' _ _ _ = []
    letBndrsFromBndr :: CoreBind -> [CoreBndr]
    letBndrsFromBndr (NonRec b expr) = letBndrsThatAreCases' b True expr
    letBndrsFromBndr (Rec bs) =
        bs >>= (\(b, expr) -> letBndrsFromBndr $ NonRec b expr)

--TODO: Replace self-recursive definitions with a loop breaker.
setInlineOnBndrs :: [CoreBndr] -> CoreExpr -> CoreExpr
setInlineOnBndrs bndrs = everywhere $ mkT go
  where
    go :: CoreExpr -> CoreExpr
    go (Let (NonRec nn e) expr)
        | any (nn ==) bndrs = Let (NonRec (setAlwaysInlineOnBndr nn) e) expr
    go x = x

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

unfoldCompulsory :: Arity -> Unfolding -> Unfolding
unfoldCompulsory arity cuf@CoreUnfolding{} =
    cuf {uf_src=InlineStable, uf_guidance = UnfWhen arity True True}
unfoldCompulsory _ x = x -- NoUnfolding
