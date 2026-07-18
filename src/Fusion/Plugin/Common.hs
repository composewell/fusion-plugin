{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE CPP #-}

module Fusion.Plugin.Common
    (
    -- * Options and reporting mode
      ReportMode(..)
    , Options(..)

    -- * Debug helpers
    , dbgLevel
    , debug
    , showWithUnique
    , listPath

    -- * Annotation lookup
    , getAnnotationsByStableName
    , resolveTHNames
    , binderAnnKeys
    , binderDisplayName
    , lookupBinderAnn
    , subsumedBySameName
    , binderClosure

    -- * Context / annotation traversal
    , altsContainsAnn
    , getNonRecBinder
    , constrTyCon

    -- * Reporting primitives
    , qualifiedName
    , qualifiedTyConName
    , showDetailsCaseMatch
    , showDetailsConstr
    , showInfo

    -- * Package/module names and dump paths
    , modulePackageName
    , pluginDumpDir
    , pluginDumpStem

    -- * Core dumping
    , dumpBindCore
    , dumpCore
    , writeBindCore
    , dumpCoreSuffix
    )
where

#include "Fusion/Plugin/Common.h"


#if MIN_VERSION_ghc(8,6,0)


-------------------------------------------------------------------------------
-- Debug stuff
-------------------------------------------------------------------------------

-- XXX Can use the debugLevel from dflags
-- Increase this level to see debug output
dbgLevel :: Int
dbgLevel = 0

debug :: Int -> String -> a -> a
debug level str x =
    if dbgLevel >= level
    then trace str x
    else x

showBndr :: Outputable a => DynFlags -> a -> String
showBndr dflags bndr = showSDoc dflags $ ppr bndr

showWithUnique :: (Outputable a, Uniquable a) => DynFlags -> a -> String
showWithUnique dflags bndr =
    let suffix = showSDoc dflags $ ppr (getUnique bndr)
        bndrName = showBndr dflags bndr
    in if DL.isSuffixOf suffix bndrName
       then bndrName
       else bndrName ++ "_" ++ suffix

listPath :: DynFlags -> [CoreBind] -> [Char]
listPath dflags binds =
      DL.intercalate "/"
    $ reverse
    $ map (showWithUnique dflags)
    $ map getNonRecBinder binds

-------------------------------------------------------------------------------
-- Commandline parsing lifted from streamly/benchmark/Chart.hs
-------------------------------------------------------------------------------

data ReportMode =
      ReportSilent
    | ReportWarn
    | ReportVerbose
    | ReportVerbose1
    | ReportVerbose2
    deriving (Eq, Show)

data Options = Options
    { optionsDumpCore :: Bool
    , optionsDumpCoreSizes :: Bool
    , optionsDumpCoreIfAnnotated :: Bool
    , optionsDumpCoreIfViolated :: Bool
    , optionsVerbosityLevel :: ReportMode
    , optionsWError :: Bool
    , optionsCsvAppend :: Bool
    } deriving Show

-- Checks whether a case alternative contains a type for which the given
-- predicate is true. Only checks the first typed element in the list, so
-- only pass alternatives from one case expression.
altsContainsAnn ::
    DynFlags -> (Name -> Bool) -> [Alt CoreBndr] -> Maybe (Alt CoreBndr)
altsContainsAnn _ _ [] = Nothing
altsContainsAnn _ _ ((ALT_CONSTR(DEFAULT,_,_)):[]) =
    debug 2 "Case trivial default" Nothing
altsContainsAnn dflags isInteresting (bndr@(ALT_CONSTR(DataAlt dcon,_,_)):_) =
    let name = GET_NAME $ dataConTyCon dcon
        mesg = "Case DataAlt type " ++ showWithUnique dflags name
    in if isInteresting name
       then debug 2 (mesg ++ " annotated") (Just bndr)
       else debug 2 (mesg ++ " not annotated") Nothing
altsContainsAnn dflags isInteresting ((ALT_CONSTR(DEFAULT,_,_)):alts) =
    altsContainsAnn dflags isInteresting alts
altsContainsAnn _ _ ((ALT_CONSTR(LitAlt _,_,_)):_) =
    debug 2 "Case LitAlt" Nothing

getNonRecBinder :: CoreBind -> CoreBndr
getNonRecBinder (NonRec b _) = b
getNonRecBinder (Rec _) = error "markInline: expecting only nonrec binders"



-- | Resolve the 'TyCon' a 'Constr' hit represents. When the 'Id' is itself a
-- data constructor (regardless of arity), resolve via its 'DataCon' rather
-- than the 'Id's own type: a non-nullary constructor's own type is a
-- function over its fields (e.g. @(:) :: a -> [a] -> [a]@), and even a
-- nullary constructor's is forall-quantified (e.g. @[] :: forall a. [a]@),
-- so neither is itself the constructed type that 'tyConAppTyConPicky_maybe'
-- can see. Falls back to the 'Id's own type for a bare boxed-use hit that is
-- not itself a constructor (e.g. a local binder of the annotated type).
constrTyCon :: Id -> Maybe TyCon
constrTyCon con =
    case isDataConId_maybe con of
        Just dcon -> Just (dataConTyCon dcon)
        Nothing -> tyConAppTyConPicky_maybe (varType con)









-- | Like GHC 'getAnnotations' but keyed by 'OccName' string instead of by
-- 'Name' (i.e. by 'Unique'). 'getAnnotations' folds annotations into a
-- 'NameEnv', and it stores only an 'Int' key internally. We read 'mg_anns'
-- directly: it is populated once from the module's '{-# ANN #-}' pragmas
-- (before any Core-to-core pass runs) and it is never rewritten by any
-- 'CoreToDo', so the 'Name's inside it are exactly as resolved by the renamer,
-- unaffected by any later binder cloning/renaming.
--
-- At most one annotation of this type is permitted per binding: a binding
-- carrying more than one is a compile error (the @annName@ argument names the
-- annotation type in that message).
getAnnotationsByStableName
    :: Data a => String -> ([Word8] -> a) -> ModGuts -> CoreM (Map.Map String a)
getAnnotationsByStableName annName deserialize guts =
    Map.traverseWithKey single
        (Map.fromListWith (++) (mapMaybe annPair (mg_anns guts)))

    where

    annPair (Annotation (NamedTarget name) payload) =
        (\v -> (getOccString name, [v])) <$> fromSerialized deserialize payload
    annPair _ = Nothing

    single _ [v] = return v
    single name _ =
        error $ "fusion-plugin: the binding '" ++ name
              ++ "' has more than one " ++ annName
              ++ " annotation; at most one " ++ annName
              ++ " annotation is allowed per binding."

-- | Resolve a list of Template Haskell 'TH.Name's to GHC 'Name's.
resolveTHNames :: [TH.Name] -> CoreM [Name]
resolveTHNames = fmap catMaybes . mapM thNameToGhcName

-------------------------------------------------------------------------------
-- Core-to-core pass to mark interesting binders to be always inlined
-------------------------------------------------------------------------------

-- XXX we can possibly have a FUSE_DEBUG annotation to print verbose
-- messages only for a given type.
--
-- XXX we mark certain functions (e.g. toStreamK) with a NOFUSION
-- annotation so that we do not report them.

showDetailsCaseMatch
    :: DynFlags
    -> ReportMode
    -> ([CoreBind], Alt CoreBndr)
    -> String
showDetailsCaseMatch dflags reportMode (binds, c@(ALT_CONSTR(con,_,_))) =
    let vstr =
            case reportMode of
                ReportVerbose -> showSDoc dflags (ppr con)
                ReportVerbose1 -> showSDoc dflags (ppr c)
                ReportVerbose2 -> showSDoc dflags (ppr $ head binds)
                _ -> error "transformBind: unreachable"
        tstr =
            case con of
                DataAlt dcon ->
                    " :: " ++ qualifiedTyConName (dataConTyCon dcon)
                _ -> ""
    in listPath dflags binds ++ ": " ++ vstr ++ tstr


-- | Show a 'Name' fully qualified as @Module.Name@ so that entities with the
-- same unqualified name defined in different modules (e.g. several @Step@
-- types) can be told apart in a report. Wired-in names with no defining
-- module are shown by their bare name.
qualifiedName :: Name -> String
qualifiedName name =
    case nameModule_maybe name of
        Just m ->
            moduleNameString (moduleName m) ++ "." ++ getOccString name
        Nothing -> getOccString name

-- | Show a 'TyCon' fully qualified as @Module.Name@ (see 'qualifiedName').
qualifiedTyConName :: TyCon -> String
qualifiedTyConName = qualifiedName . getName

showDetailsConstr
    :: DynFlags
    -> ReportMode
    -> ([CoreBind], Id)
    -> String
showDetailsConstr dflags reportMode (binds, con) =
    let t = constrTyCon con
        vstr =
            case reportMode of
                ReportVerbose -> showSDoc dflags (ppr con)
                ReportVerbose1 -> showSDoc dflags (ppr con)
                ReportVerbose2 -> showSDoc dflags (ppr $ head binds)
                _ -> error "transformBind: unreachable"
        tstr =
            case t of
                Nothing -> " :: Not a Type Constructor"
                Just x -> " :: " ++ qualifiedTyConName x
    in listPath dflags binds ++ ": " ++ vstr ++ tstr

-- Orphan instance for 'Fuse'
instance Outputable Fuse where
    ppr _ = text "Fuse"

-- Orphan instance for 'InspectTypeClasses', used only to print a banner naming
-- the directive that triggered a focused report.
instance Outputable InspectTypeClasses where
    ppr (ForbidTypeClasses names) =
        text "ForbidTypeClasses" <+> text (show names)
    ppr (PermitTypeClasses names) =
        text "PermitTypeClasses" <+> text (show names)

showInfo
    :: CoreBndr
    -> DynFlags
    -> ReportMode
    -> String
    -> [CoreBndr]
    -> [([CoreBind], a)]
    -> (DynFlags -> ReportMode -> ([CoreBind], a) -> String)
    -> CoreM ()
showInfo parent dflags reportMode
        tag uniqBinders annotated showDetails =
    when (uniqBinders /= []) $ do
        let mesg = "In "
                  ++ showWithUnique dflags parent
                  ++ " binders "
                  ++ show (map (showWithUnique dflags) (uniqBinders))
                  ++ " " ++ tag
                  ++ " data types annotated with "
                  ++ showSDoc dflags (ppr Fuse)
        case reportMode of
            ReportSilent -> return ()
            ReportWarn -> return ()
            _ -> do
                putMsgS mesg
                putMsgS $ DL.unlines
                        $ DL.nub
                        $ map (showDetails dflags reportMode) annotated

binderAnnKeys :: CoreBndr -> [String]
binderAnnKeys b = nm : [base | Just base <- [DL.stripPrefix "$w" nm]]

    where

    nm = getOccString (GET_NAME b)

-- | The source-level display name of a binder: its occurrence name with any
-- @$w@ worker prefix stripped. The worker/wrapper transformation renames an
-- annotated binding @f@ to a worker @$wf@; reporting it as @f@ keeps dump file
-- names and the core-sizes CSV keyed by the original binding name.
binderDisplayName :: CoreBndr -> String
binderDisplayName b = fromMaybe nm (DL.stripPrefix "$w" nm)

    where

    nm = getOccString (GET_NAME b)

-- | True if another top-level binder that shares this binder's display name
-- (see 'binderDisplayName') already reaches it through its closure
-- (see 'binderClosure'), which makes a separate report for it redundant.
--
-- A single source binding can leave several top-level binders that collapse to
-- the same display name. Worker/wrapper splitting turns @f@ into a wrapper @f@
-- and a worker @$wf@; exporting adds a cast wrapper on top, e.g. for an exported
-- @drainN@ the final Core holds @drainN = drainN' \`cast\` co@, the ordinary
-- wrapper @drainN'@, and the worker @$wdrainN@ -- three binders all displaying
-- as @drainN@. Each outer one's closure contains the inner ones, so we report
-- only the outermost binder (the one no same-named sibling reaches) and skip the
-- rest, collapsing them to a single entry. When a binding leaves just one
-- survivor (e.g. a non-exported @f@ reduced to only @$wf@) nothing reaches it
-- and it is reported (this returns 'False').
subsumedBySameName :: [(CoreBndr, CoreExpr)] -> CoreBndr -> Bool
subsumedBySameName binds b = any covers binds

    where

    name = binderDisplayName b

    covers (other, _) =
           other /= b
        && binderDisplayName other == name
        && any ((== b) . fst) (binderClosure binds other)

-- | Look up a per-binder annotation, accounting for worker/wrapper splitting.
--
-- The annotations are keyed by the source-level binder name (e.g. @f@). The
-- strictness analyser, however, may split an annotated function @f@ into a
-- worker @$wf@ and a wrapper @f@; when @f@ is not exported the wrapper is
-- frequently inlined away at its (single) call site, leaving only @$wf@ in the
-- final Core. In that case the binder's occurrence name (@$wf@) would not be
-- found in the annotation map. So if the direct lookup fails and the binder is
-- a @$w@ worker, retry with the prefix stripped.
lookupBinderAnn :: CoreBndr -> Map.Map String a -> Maybe a
lookupBinderAnn b m = listToMaybe (mapMaybe (`Map.lookup` m) (binderAnnKeys b))

-------------------------------------------------------------------------------
-- Transitive closure of a binding
-------------------------------------------------------------------------------

-- | All 'Var' occurrences appearing anywhere in an expression. Like
-- 'classTyConsInBind' but collects term-level variable references instead of
-- type constructors.
exprVarOccs :: CoreExpr -> VarSet
exprVarOccs = goE

    where

    goE (Var v) = unitVarSet v
    goE (Lit _) = emptyUniqSet
    goE (App e1 e2) = goE e1 `unionUniqSets` goE e2
    goE (Lam _ e) = goE e
    goE (Let b e) = goB b `unionUniqSets` goE e
    goE (Case e _ _ alts) =
        goE e `unionUniqSets` unionManyUniqSets (map goAlt alts)
    goE (Cast e _) = goE e
    goE (Tick _ e) = goE e
    goE (Type _) = emptyUniqSet
    goE (Coercion _) = emptyUniqSet

    goAlt (ALT_CONSTR(_,_,e)) = goE e

    goB (NonRec _ e) = goE e
    goB (Rec bs) = unionManyUniqSets (map (goE . snd) bs)

-- | Return the binder itself plus the transitive closure of all /other top
-- level/ binders reachable from its RHS (following term-level references).
--
-- This reconstructs a whole function that the optimiser has scattered across
-- several top level bindings (a wrapper referencing a @$w@ worker, a worker
-- referencing a @$s$w@ specialisation, and so on). References to imported ids
-- or local binders are ignored (only names in this module's @binds@ are
-- followed).
binderClosure :: [(CoreBndr, CoreExpr)] -> CoreBndr -> [(CoreBndr, CoreExpr)]
binderClosure binds root = go [root] emptyVarSet []

    where

    topSet = mkVarSet (map fst binds)

    go [] _ acc = acc
    go (v : vs) seen acc
        | v `elemVarSet` seen = go vs seen acc
        | otherwise =
            case lookup v binds of
                Nothing -> go vs (extendVarSet seen v) acc
                Just e ->
                    let refs = nonDetEltsUniqSet
                                   (intersectVarSet (exprVarOccs e) topSet)
                    in go (refs ++ vs) (extendVarSet seen v) ((v, e) : acc)

fallbackDumpDir :: String -> FilePath
fallbackDumpDir pkgName = "fusion-plugin-output" </> pkgName

pluginDumpDir :: DynFlags -> String -> FilePath
pluginDumpDir dflags pkgName =
    fromMaybe (fallbackDumpDir pkgName) (dumpDir dflags)

pluginDumpStem :: DynFlags -> String -> String -> FilePath
pluginDumpStem dflags pkgName modName =
    pluginDumpDir dflags pkgName </> (modName ++ ".")

pluginDumpPrefix :: DynFlags -> String -> String -> FilePath
pluginDumpPrefix dflags pkgName modName =
    case dumpDir dflags of
        Just _  -> modName ++ "."
        Nothing -> fallbackDumpDir pkgName </> (modName ++ ".")




-- | Split a string on @\'-\'@ characters.
splitOnDash :: String -> [String]
splitOnDash s =
    case break (== '-') s of
        (a, []) -> [a]
        (a, _ : rest) -> a : splitOnDash rest

-- | Recover the bare package name from a unit-id string. During a build the
-- unit id of the package being compiled looks like @my-pkg-0.2.8-inplace@ (or
-- with a hash instead of @inplace@); we strip the trailing version and any
-- component suffix, leaving just @my-pkg@. If no version-like component is
-- found the string is returned unchanged.
packageNameFromUnitId :: String -> String
packageNameFromUnitId uid =
    case break isVersion (splitOnDash uid) of
        (before, _ : _) | not (null before) -> DL.intercalate "-" before
        _ -> uid
  where
    isVersion x = not (null x) && all (\c -> isDigit c || c == '.') x

-- | The bare package name of the module currently being compiled.
modulePackageName :: Module -> String
modulePackageName =
    packageNameFromUnitId
#if MIN_VERSION_ghc(9,0,0)
        . unitString . moduleUnit
#else
        . unitIdString . moduleUnitId
#endif

writeBindCore
    :: DynFlags -> String -> String -> String -> [(CoreBndr, CoreExpr)]
    -> CoreBndr -> CoreM FilePath
writeBindCore dflags pkgName modName suffix allBinds b = do
    let dir = pluginDumpDir dflags pkgName
        path = pluginDumpStem dflags pkgName modName
                    ++ binderDisplayName b ++ suffix
        closure = binderClosure allBinds b
        doc = vcat (map (ppr . uncurry NonRec) closure)
    liftIO $ do
        createDirectoryIfMissing True dir
        writeFile path (showSDoc dflags doc ++ "\n")
    return path

dumpBindCore
    :: DynFlags -> String -> String -> [(CoreBndr, CoreExpr)] -> CoreBndr
    -> CoreM ()
dumpBindCore dflags pkgName modName allBinds b = do
    path <- writeBindCore dflags pkgName modName ".dump-simpl" allBinds b
    putMsgS $ "fusion-plugin: "
            ++ showWithUnique dflags b ++ ": dumped core to " ++ path

-------------------------------------------------------------------------------
-- Report unfused constructors
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Dump core passes
-------------------------------------------------------------------------------

-- Only for GHC versions before 9.0.0
#if !MIN_VERSION_ghc(9,0,0)
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

-- Copied from GHC.Utils.Logger
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
#endif

filterOutLast :: (a -> Bool) -> [a] -> [a]
filterOutLast _ [] = []
filterOutLast p [x]
    | p x = []
    | otherwise = [x]
filterOutLast p (x:xs) = x : filterOutLast p xs

dumpCoreSuffix :: DynFlags -> Int -> SDoc -> String
dumpCoreSuffix dflags counter title = printf "%02d" counter ++ "-"
    ++ (map (\x -> if isSpace x then '-' else x)
           $ filterOutLast isSpace
           $ filter (/= ':')
           $ takeWhile (/= '(')
           $ showSDoc dflags title)
    ++ "."

-- dump core not supported on 9.0.0, 9.0.0 does not export Logger
#if __GLASGOW_HASKELL__!=900
-- Only for GHC versions >= 9.2.0
#if MIN_VERSION_ghc(9,2,0)
dumpPassResult ::
      Logger
   -> DynFlags
   -> PRINT_UNQUAL
   -> SDoc                  -- Header
   -> SDoc                  -- Extra info to appear after header
   -> CoreProgram -> [CoreRule]
   -> IO ()
dumpPassResult logger dflags unqual hdr extra_info binds rules = do
#if MIN_VERSION_ghc(9,3,0)
    let flags = logFlags logger
    let getDumpAction = putDumpFile
#else
    let flags = dflags
    let getDumpAction = putDumpMsg
#endif
    (getDumpAction logger)
        flags dump_style Opt_D_dump_simpl title undefined dump_doc

    where

    title = showSDoc dflags hdr

    dump_style = mkDumpStyle unqual

#else

dumpPassResult :: DynFlags
               -> PRINT_UNQUAL
               -> FilePath
               -> SDoc                  -- Header
               -> SDoc                  -- Extra info to appear after header
               -> CoreProgram -> [CoreRule]
               -> IO ()
dumpPassResult dflags unqual suffix hdr extra_info binds rules = do
   dumpSDoc dflags unqual suffix (showSDoc dflags hdr) dump_doc

  where

#endif
    dump_doc  = vcat [ nest 2 extra_info
                     , blankLine
                     , pprCoreBindingsWithSize binds
                     , ppUnless (null rules) pp_rules ]
    pp_rules = vcat [ blankLine
                    , text "------ Local rules for imported ids --------"
                    , pprRules rules ]

dumpCoreFileName :: DynFlags -> String -> String -> Int -> SDoc -> Maybe FilePath
dumpCoreFileName dflags pkgName modName counter title
    | not (gopt Opt_DumpToFile dflags) = Nothing
    | otherwise =
        Just $ pluginDumpStem dflags pkgName modName
            ++ dumpCoreSuffix dflags counter title ++ "dump-simpl"

dumpResult
#if MIN_VERSION_ghc(9,2,0)
    :: Logger
    -> DynFlags
#else
    :: DynFlags
#endif
    -> PRINT_UNQUAL
    -> FilePath
    -> Int
    -> SDoc
    -> CoreProgram
    -> [CoreRule]
    -> IO ()
#if MIN_VERSION_ghc(9,2,0)
dumpResult logger dflags print_unqual prefixOverride counter todo binds rules =
    dumpPassResult logger1 dflags1 print_unqual hdr (text "") binds rules
#else
dumpResult dflags print_unqual prefixOverride counter todo binds rules =
    dumpPassResult
        dflags1 print_unqual (_suffix ++ "dump-simpl") hdr (text "") binds rules
#endif

    where

    hdr = text "["
        GhcPlugins.<> int counter
        GhcPlugins.<> text "] "
        GhcPlugins.<> todo

    _suffix = dumpCoreSuffix dflags counter todo

#if MIN_VERSION_ghc(9,4,0)
    dflags1 = dflags
    logger1 =
        logger
            { logFlags =
                (logFlags logger) {log_dump_prefix = prefixOverride ++ _suffix} }
#elif MIN_VERSION_ghc(9,2,0)
    dflags1 = dflags {dumpPrefixForce = Just prefixOverride}
    logger1 = logger
#else
    dflags1 = dflags {dumpPrefixForce = Just prefixOverride}
#endif
#endif

dumpCore :: Bool -> Int -> SDoc -> ModGuts -> CoreM ModGuts
dumpCore verbose counter title guts = do
    dflags <- getDynFlags
    let passMsg = show counter ++ " " ++ showSDoc dflags title
#if __GLASGOW_HASKELL__!=900
    let pkgName = modulePackageName (mg_module guts)
        modName = moduleNameString (moduleName (mg_module guts))
        prefixOverride = pluginDumpPrefix dflags pkgName modName
    let fileMsg = case dumpCoreFileName dflags pkgName modName counter title of
            Just f  -> takeFileName f
            Nothing -> "stdout (" ++ passMsg ++ ")"
#else
    let fileMsg = "stdout (" ++ passMsg ++ ")"
#endif
    when verbose $
        putMsgS $ "fusion-plugin: dumping core pass result to " ++ fileMsg

#if MIN_VERSION_ghc(9,6,0)
    hscEnv <- getHscEnv
    let logger = hsc_logger hscEnv
    let ptc = PromTickCtx
            { ptcListTuplePuns = False
            , ptcPrintRedundantPromTicks = False
            }
    let print_unqual =
            mkNamePprCtx ptc (hsc_unit_env hscEnv) (mg_rdr_env guts)
    liftIO $ dumpResult logger dflags print_unqual prefixOverride counter
                title (mg_binds guts) (mg_rules guts)
#elif MIN_VERSION_ghc(9,2,0)
    hscEnv <- getHscEnv
    let logger = hsc_logger hscEnv
    let print_unqual =
            mkPrintUnqualified (hsc_unit_env hscEnv) (mg_rdr_env guts)
    liftIO $ dumpResult logger dflags print_unqual prefixOverride counter
                title (mg_binds guts) (mg_rules guts)
#elif MIN_VERSION_ghc(9,0,0)
    putMsgS $ "fusion-plugin: dump-core not supported on GHC 9.0 "
#else
    let print_unqual = mkPrintUnqualified dflags (mg_rdr_env guts)
    liftIO $ dumpResult dflags print_unqual prefixOverride counter
                title (mg_binds guts) (mg_rules guts)
#endif
    return guts

#endif
