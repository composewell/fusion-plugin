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
-- using a custom 'Fuse' annotation. The programmer would annotate the
-- types that are to be eliminated by fusion via case-of-case transformations.
-- During the simplifier phase the plugin goes through the relevant bindings
-- and if one of these types are found inside a binding then that binding is
-- marked to be inlined irrespective of the size.
--
-- At the right places, fusion can provide dramatic performance improvements
-- (e.g. 10x) to the code.

{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import Control.Monad (mzero, when, unless)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Char (isSpace)
import Data.Generics.Schemes (everywhere)
import Data.Generics.Aliases (mkT)
import Data.IORef (readIORef, writeIORef)
import Data.Time (getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.IO (Handle, IOMode(..), withFile, hSetEncoding, utf8)

import ErrUtils (mkDumpDoc, Severity(..))
import Outputable (Outputable(..), text)
import PprCore (pprCoreBindingsWithSize, pprRules)

import qualified Data.List as DL
import qualified Data.Set as Set

-- Implicit imports
import GhcPlugins

-- Imports from this package
import Fusion.Plugin.Types (Fuse(..))

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
-- is annotated with 'Fuse'. It then sets AlwaysInlinePragma on those
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

#if MIN_VERSION_ghc(8,6,0)

-------------------------------------------------------------------------------
-- Commandline parsing lifted from streamly/benchmark/Chart.hs
-------------------------------------------------------------------------------

data Options = Options
    { optionsDumpCore :: Bool
    } deriving Show

defaultOptions :: Options
defaultOptions = Options False

setDumpCore :: Monad m => Bool -> StateT ([CommandLineOption], Options) m ()
setDumpCore val = do
    (args, opts) <- get
    put (args, opts { optionsDumpCore = val })

-- Like the shell "shift" to shift the command line arguments
shift :: StateT ([String], Options) (MaybeT IO) (Maybe String)
shift = do
    s <- get
    case s of
        ([], _) -> return Nothing
        (x : xs, opts) -> put (xs, opts) >> return (Just x)

-- totally imperative style option parsing
parseOptions :: [CommandLineOption] -> IO Options
parseOptions args = do
    maybeOptions <- runMaybeT
                        $ flip evalStateT (args, defaultOptions)
                        $ do parseLoop
                             fmap snd get
    return $ maybe defaultOptions id maybeOptions

    where

    parseOpt opt =
        case opt of
            "dump-core" -> setDumpCore True
            str -> do
                liftIO $ putStrLn $ "Unrecognized option - \"" ++ str ++ "\""
                mzero

    parseLoop = do
        next <- shift
        case next of
            Just opt -> parseOpt opt >> parseLoop
            Nothing -> return ()

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
setInlineOnBndrs :: [CoreBndr] -> CoreBind -> CoreBind
setInlineOnBndrs bndrs = everywhere $ mkT go
  where
    go :: CoreBind -> CoreBind
    go (NonRec b expr) | any (b ==) bndrs =
        NonRec (setAlwaysInlineOnBndr b) expr
    go x = x

-------------------------------------------------------------------------------
-- Inspect case alternatives for interesting constructor matches
-------------------------------------------------------------------------------

-- Checks whether a case alternative contains a type with the
-- annotation.  Only checks the first typed element in the list, so
-- only pass alternatives from one case expression.
altsContainsAnn :: UniqFM [Fuse] -> [Alt CoreBndr] -> Maybe (Alt CoreBndr)
altsContainsAnn _ [] = Nothing
altsContainsAnn anns (bndr@(DataAlt dcon, _, _):_) =
    case lookupUFM anns (getUnique $ dataConTyCon dcon) of
        Nothing -> Nothing
        Just _ -> Just bndr
altsContainsAnn anns ((DEFAULT, _, _):alts) = altsContainsAnn anns alts
altsContainsAnn _ _ = Nothing

-------------------------------------------------------------------------------
-- Determine if a let binder contains a case match on an annotated type
-------------------------------------------------------------------------------

-- XXX Can check the call site and return only those that would enable
-- case-of-known constructor to kick in. Or is that not relevant?
--
-- | Returns the Bndrs, that are either of the form:
--
-- joinrec { $w$g0 x y z = case y of predicateAlt -> ... } -> returns [$w$go]
-- join { $j1_sGH1 x y z = case y of predicateAlt -> ... } -> returns [$j1_sGH1]
--
-- Returns all the binds in the hierarchy from the parent to the bind
-- containing the case alternative as well as the case alternative scrutinizing
-- the annotated type.
letBndrsThatAreCases
    :: ([Alt CoreBndr] -> Maybe (Alt CoreBndr))
    -> CoreBind
    -> [([CoreBind], Alt CoreBndr)]
letBndrsThatAreCases f bind = goLet [] bind
  where
    -- The first argument is current binder and its parent chain. We add a new
    -- element to this path when we enter a let statement.
    --
    -- When second argument is "False" it means we do not examine the case
    -- alternatives for annotated constructors when we encounter a case
    -- statement. We pass the second arg as "True" in recursive calls to "go"
    -- after we encounter a let binder. We reset it to "False" when we do not
    -- want to consider inlining the current binder.
    --
    go :: [CoreBind] -> Bool -> CoreExpr -> [([CoreBind], Alt CoreBndr)]

    -- Match and record the case alternative if it contains a constructor
    -- annotated with "Fuse" and traverse the Alt expressions to discover more
    -- let bindings.
    go parents True (Case _ _ _ alts) =
        let binders = alts >>= (\(_, _, expr1) -> go parents False expr1)
        in case f alts of
            Just x -> (parents, x) : binders
            Nothing -> binders

    -- Only traverse the Alt expressions of the case to discover new let
    -- bindings. Do not match for annotated constructors in the Alts.
    go parents False (Case _ _ _ alts) =
        alts >>= (\(_, _, expr1) -> go parents False expr1)

    -- Enter a new let binding inside the current expression and traverse the
    -- let expression as well.
    go parents _ (Let bndr expr1) =    goLet parents bndr
                                    ++ go parents False expr1

    -- Traverse these to discover new let bindings
    go parents _ (App expr1 expr2) =    go parents False expr1
                                     ++ go parents False expr2
    go parents x (Lam _ expr1) = go parents x expr1
    go parents _ (Cast expr1 _) = go parents False expr1

    -- There are no let bindings in these.
    go _ _ (Var _) = []
    go _ _ (Lit _) = []
    go _ _ (Tick _ _) = []
    go _ _ (Type _) = []
    go _ _ (Coercion _) = []

    goLet :: [CoreBind] -> CoreBind -> [([CoreBind], Alt CoreBndr)]
    -- Here we pass the second argument to "go" as "True" i.e. we are no
    -- looking to match the case alternatives for annotated constructors.
    goLet parents bndr@(NonRec _ expr1) = go (bndr : parents) True expr1
    goLet parents (Rec bs) =
        bs >>= (\(b, expr1) -> goLet parents $ NonRec b expr1)

containsAnns
    :: ([Alt CoreBndr] -> Maybe (Alt CoreBndr))
    -> CoreBind
    -> [([CoreBind], Alt CoreBndr)]
containsAnns f bind =
    -- The first argument is current binder and its parent chain. We add a new
    -- element to this path when we enter a let statement.
    goLet [] bind
  where
    go :: [CoreBind] -> CoreExpr -> [([CoreBind], Alt CoreBndr)]

    -- Match and record the case alternative if it contains a constructor
    -- annotated with "Fuse" and traverse the Alt expressions to discover more
    -- let bindings.
    go parents (Case _ _ _ alts) =
        let binders = alts >>= (\(_, _, expr1) -> go parents expr1)
        in case f alts of
            Just x -> (parents, x) : binders
            Nothing -> binders

    -- Enter a new let binding inside the current expression and traverse the
    -- let expression as well.
    go parents (Let bndr expr1) = goLet parents bndr ++ go parents expr1

    -- Traverse these to discover new let bindings
    go parents (App expr1 expr2) = go parents expr1 ++ go parents expr2
    go parents (Lam _ expr1) = go parents expr1
    go parents (Cast expr1 _) = go parents expr1

    -- There are no let bindings in these.
    go _ (Var _) = []
    go _ (Lit _) = []
    go _ (Tick _ _) = []
    go _ (Type _) = []
    go _ (Coercion _) = []

    goLet :: [CoreBind] -> CoreBind -> [([CoreBind], Alt CoreBndr)]
    goLet parents bndr@(NonRec _ expr1) = go (bndr : parents) expr1
    goLet parents (Rec bs) =
        bs >>= (\(b, expr1) -> goLet parents $ NonRec b expr1)

-------------------------------------------------------------------------------
-- Core-to-core pass to mark interesting binders to be always inlined
-------------------------------------------------------------------------------

data ReportMode = ReportSilent | ReportWarn | ReportVerbose | ReportVerbose2

getNonRecBinder :: CoreBind -> CoreBndr
getNonRecBinder (NonRec b _) = b
getNonRecBinder (Rec _) = error "markInline: expecting only nonrec binders"

-- XXX we can possibly have a FUSE_DEBUG annotation to print verbose
-- messages only for a given type.
--
-- XXX we mark certain functions (e.g. toStreamK) with a NOFUSION
-- annotation so that we do not report them.

addMissingUnique :: (Outputable a, Uniquable a) => DynFlags -> a -> String
addMissingUnique dflags bndr =
    let suffix = showSDoc dflags $ ppr (getUnique bndr)
        bndrName = showSDoc dflags $ ppr bndr
    in if DL.isSuffixOf suffix bndrName
       then bndrName
       else bndrName ++ "_" ++ suffix

showInfo
    :: CoreBndr
    -> DynFlags
    -> ReportMode
    -> Bool
    -> [CoreBndr]
    -> [([CoreBind], Alt CoreBndr)]
    -> CoreM ()
showInfo parent dflags reportMode failIt uniqBinders annotated =
    when (uniqBinders /= []) $ do
        let showDetails (binds, c@(con,_,_)) =
                let path = DL.intercalate "/"
                        $ reverse
                        $ map (addMissingUnique dflags)
                        $ map getNonRecBinder binds
                in path ++ ": " ++
                    case reportMode of
                        ReportWarn -> showSDoc dflags (ppr con)
                        ReportVerbose -> showSDoc dflags (ppr c)
                        ReportVerbose2 ->
                            showSDoc dflags (ppr $ head binds)
                        _ -> error "transformBind: unreachable"
        let msg = "In "
                  ++ addMissingUnique dflags parent
                  ++ " binders "
                  ++ show (map (addMissingUnique dflags) (uniqBinders))
                  ++ " scrutinize data types annotated with "
                  ++ showSDoc dflags (ppr Fuse)
        case reportMode of
            ReportSilent -> return ()
            _ -> do
                putMsgS msg
                putMsgS $ DL.unlines $ map showDetails annotated
        when failIt $ error "failing"

markInline :: ReportMode -> Bool -> Bool -> ModGuts -> CoreM ModGuts
markInline reportMode failIt transform guts = do
    putMsgS $ "fusion-plugin: Checking bindings to inline..."
    dflags <- getDynFlags
    anns <- getAnnotations deserializeWithData guts
    if (anyUFM (any (== Fuse)) anns)
    then bindsOnlyPass (mapM (transformBind dflags anns)) guts
    else return guts
  where
    transformBind :: DynFlags -> UniqFM [Fuse] -> CoreBind -> CoreM CoreBind
    transformBind dflags anns bind@(NonRec b _) = do
        let annotated = letBndrsThatAreCases (altsContainsAnn anns) bind
        let uniqBinders = DL.nub (map (getNonRecBinder. head . fst) annotated)

        when (uniqBinders /= []) $
            showInfo b dflags reportMode failIt uniqBinders annotated

        let bind' =
                if transform
                then setInlineOnBndrs uniqBinders bind
                else bind
        return bind'

    transformBind _ _ bndr =
        -- This is probably wrong, but we don't need it for now.
        --mapM_ (\(b, expr) -> transformBind dflags anns (NonRec b expr)) bs
        return bndr

reportAnns :: ReportMode -> ModGuts -> CoreM ModGuts
reportAnns reportMode guts = do
    putMsgS $ "fusion-plugin: Checking presence of annotated types..."
    dflags <- getDynFlags
    anns <- getAnnotations deserializeWithData guts
    if (anyUFM (any (== Fuse)) anns)
    then bindsOnlyPass (mapM (transformBind dflags anns)) guts
    else return guts
  where
    transformBind :: DynFlags -> UniqFM [Fuse] -> CoreBind -> CoreM CoreBind
    transformBind dflags anns bind@(NonRec b _) = do
        let annotated = containsAnns (altsContainsAnn anns) bind
            uniqBinders = DL.nub (map (getNonRecBinder . head . fst) annotated)
        when (uniqBinders /= []) $
            showInfo b dflags reportMode False uniqBinders annotated
        return bind

    transformBind _ _ bndr = return bndr

-------------------------------------------------------------------------------
-- Dump core passes
-------------------------------------------------------------------------------

chooseDumpFile :: DynFlags -> FilePath -> Maybe FilePath
chooseDumpFile dflags suffix
        | Just prefix <- getPrefix

        = Just $ setDir (prefix ++ suffix)

        | otherwise

        = Nothing

        where getPrefix
                 -- dump file location is being forced
                 --      by the --ddump-file-prefix flag.
               | Just prefix <- dumpPrefixForce dflags
                  = Just prefix
                 -- dump file location chosen by DriverPipeline.runPipeline
               | Just prefix <- dumpPrefix dflags
                  = Just prefix
                 -- we haven't got a place to put a dump file.
               | otherwise
                  = Nothing
              setDir f = case dumpDir dflags of
                         Just d  -> d </> f
                         Nothing ->       f

withDumpFileHandle :: DynFlags -> FilePath -> (Maybe Handle -> IO ()) -> IO ()
withDumpFileHandle dflags suffix action = do
    let mFile = chooseDumpFile dflags suffix
    case mFile of
      Just fileName -> do
        let gdref = generatedDumps dflags
        gd <- readIORef gdref
        let append = Set.member fileName gd
            mode = if append then AppendMode else WriteMode
        unless append $
            writeIORef gdref (Set.insert fileName gd)
        createDirectoryIfMissing True (takeDirectory fileName)
        withFile fileName mode $ \handle -> do
            -- We do not want the dump file to be affected by
            -- environment variables, but instead to always use
            -- UTF8. See:
            -- https://gitlab.haskell.org/ghc/ghc/issues/10762
            hSetEncoding handle utf8
            action (Just handle)
      Nothing -> action Nothing

dumpSDocWithStyle :: PprStyle -> DynFlags -> FilePath -> String -> SDoc -> IO ()
dumpSDocWithStyle sty dflags suffix hdr doc =
    withDumpFileHandle dflags suffix writeDump
  where
    -- write dump to file
    writeDump (Just handle) = do
        doc' <- if null hdr
                then return doc
                else do t <- getCurrentTime
                        let timeStamp = if (gopt Opt_SuppressTimestamps dflags)
                                          then empty
                                          else text (show t)
                        let d = timeStamp
                                $$ blankLine
                                $$ doc
                        return $ mkDumpDoc hdr d
        defaultLogActionHPrintDoc dflags handle doc' sty

    -- write the dump to stdout
    writeDump Nothing = do
        let (doc', severity)
              | null hdr  = (doc, SevOutput)
              | otherwise = (mkDumpDoc hdr doc, SevDump)
        putLogMsg dflags NoReason severity noSrcSpan sty doc'

dumpSDoc :: DynFlags -> PrintUnqualified -> FilePath -> String -> SDoc -> IO ()
dumpSDoc dflags print_unqual
    = dumpSDocWithStyle dump_style dflags
  where dump_style = mkDumpStyle dflags print_unqual

dumpPassResult :: DynFlags
               -> PrintUnqualified
               -> FilePath
               -> SDoc                  -- Header
               -> SDoc                  -- Extra info to appear after header
               -> CoreProgram -> [CoreRule]
               -> IO ()
dumpPassResult dflags unqual suffix hdr extra_info binds rules = do
   dumpSDoc dflags unqual suffix (showSDoc dflags hdr) dump_doc

  where

    dump_doc  = vcat [ nest 2 extra_info
                     , blankLine
                     , pprCoreBindingsWithSize binds
                     , ppUnless (null rules) pp_rules ]
    pp_rules = vcat [ blankLine
                    , text "------ Local rules for imported ids --------"
                    , pprRules rules ]

filterOutLast :: (a -> Bool) -> [a] -> [a]
filterOutLast _ [] = []
filterOutLast p [x]
    | p x = []
    | otherwise = [x]
filterOutLast p (x:xs) = x : filterOutLast p xs

dumpResult
    :: DynFlags
    -> PrintUnqualified
    -> Int
    -> SDoc
    -> CoreProgram
    -> [CoreRule]
    -> IO ()
dumpResult dflags print_unqual counter todo binds rules =
    dumpPassResult dflags print_unqual suffix hdr (text "") binds rules

    where

    hdr = text "["
        GhcPlugins.<> int counter
        GhcPlugins.<> text "] "
        GhcPlugins.<> todo

    suffix = show counter ++ "-"
        ++ (map (\x -> if isSpace x then '-' else x)
               $ filterOutLast isSpace
               $ takeWhile (/= '(')
               $ showSDoc dflags todo)

dumpCore :: Int -> SDoc -> ModGuts -> CoreM ModGuts
dumpCore counter todo
    guts@(ModGuts
        { mg_rdr_env = rdr_env
        , mg_binds = binds
        , mg_rules = rules
        }) = do
    dflags <- getDynFlags
    putMsgS $ "fusion-plugin: dumping core "
        ++ show counter ++ " " ++ showSDoc dflags todo

    let print_unqual = mkPrintUnqualified dflags rdr_env
    liftIO $ dumpResult dflags print_unqual counter todo binds rules
    return guts

dumpCorePass :: Int -> SDoc -> CoreToDo
dumpCorePass counter todo =
    CoreDoPluginPass "Fusion plugin dump core" (dumpCore counter todo)

_insertDumpCore :: [CoreToDo] -> [CoreToDo]
_insertDumpCore todos = dumpCorePass 0 (text "Initial ") : go 1 todos
  where
    go _ [] = []
    go counter (todo:rest) =
        todo : dumpCorePass counter (text "After " GhcPlugins.<> ppr todo)
             : go (counter + 1) rest

-------------------------------------------------------------------------------
-- Install our plugin core pass
-------------------------------------------------------------------------------

-- Inserts the given list of 'CoreToDo' after the simplifier phase 0.
insertAfterSimplPhase0
    :: [CoreToDo] -> [CoreToDo] -> CoreToDo -> [CoreToDo]
insertAfterSimplPhase0 origTodos ourTodos report =
    go False origTodos ++ [report]
  where
    go False [] = error "Simplifier phase 0/\"main\" not found"
    go True [] = []
    go _ (todo@(CoreDoSimplify _ SimplMode
            { sm_phase = Phase 0
            , sm_names = ["main"]
            }):todos)
        = todo : ourTodos ++ go True todos
    go found (todo:todos) = todo : go found todos

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install args todos = do
    options <- liftIO $ parseOptions args
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
    -- twice.
    --
    -- TODO: The gentle simplifier runs on the whole program,
    -- however it might be better to call `simplifyExpr` on the
    -- expression directly.
    --
    -- TODO do not run simplify if we did not do anything in markInline phase.
    return $
        (if optionsDumpCore options then _insertDumpCore else id) $
        insertAfterSimplPhase0
            todos
            [ doMarkInline ReportSilent False True
            , simplsimplify
            , doMarkInline ReportSilent False True
            , simplsimplify
            ]
            (CoreDoPluginPass "Check fusion" (reportAnns ReportWarn))
#else
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = do
    putMsgS "Warning! fusion-plugin does nothing on ghc versions prior to 8.6"
    return todos
#endif

plugin :: Plugin
plugin = defaultPlugin {installCoreToDos = install}

-- Orphan instance for 'Fuse'
instance Outputable Fuse where
    ppr _ = text "Fuse"
