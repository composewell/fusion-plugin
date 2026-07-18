{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE CPP #-}

module Fusion.Plugin.Inspect
    ( fusionReport
    )
where

#include "Fusion/Plugin/Common.h"

import Fusion.Plugin.Common
import Fusion.Plugin.Fuse

#if MIN_VERSION_ghc(8,6,0)


data InspectAnns = InspectAnns
    { iaPatternMatches :: INSPECT_PM_FM
    , iaAllocations    :: INSPECT_ALLOC_FM
    , iaClasses        :: INSPECT_CLASSES_FM
    , iaSizes          :: MAX_CORE_SIZE_FM
    , iaDumps          :: DUMP_CORE_FM
    }


-- | The position-normalized form of an inspection directive. Both
-- 'InspectPatternMatches' and 'InspectAllocations' are reduced to this common
-- shape so that the reporting machinery need not know which one it came from;
-- the two differ only in the position filter, the report labels, and the
-- banner text captured here.
data NormInspect = NormInspect
    { niInteresting :: Name -> Bool
    -- ^ The "isInteresting" predicate: which type 'Name's the directive cares
    -- about.
    , niExclusion   :: [Name]
    -- ^ Names to exclude from the reported hits (the allow list).
    , niPosition    :: Context -> Bool
    -- ^ Position filter: keep only scrutinizing or only constructing hits.
    , niPermit      :: Maybe (String, [TH.Name])
    -- ^ For "permit" (allowlist) directives, the display name and allow list
    -- used to warn about stale entries; 'Nothing' for "forbid" directives.
    , niForbidLabel :: String
    -- ^ Label used in the terse "found ..." report line.
    , niBanner      :: String
    -- ^ The directive rendered for the detailed report banner.
    , niHeapFilter  :: Bool
    -- ^ Whether to drop non-heap-allocated hits in allocation matches.
    }

-- | Normalize an 'InspectPatternMatches' directive. All of its constructors
-- act on the scrutinizing (pattern-match) position.
normPatternMatches :: UNIQ_FM -> InspectPatternMatches -> CoreM NormInspect
normPatternMatches anns d = do
    (interesting, excl, permit, banner) <- case d of
        ForbidPatternMatches thNames -> do
            names <- resolveTHNames thNames
            return
                ( \n -> n `elem` names
                , []
                , Nothing
                , "ForbidPatternMatches " ++ show thNames
                )
        ForbidFusedPatternMatches thForbid thAllow -> do
            forbidden <- resolveTHNames thForbid
            allowed <- resolveTHNames thAllow
            return
                ( \n -> isJust (lookupUFM anns n) || n `elem` forbidden
                , allowed
                , Nothing
                , "ForbidFusedPatternMatches "
                    ++ show thForbid ++ " " ++ show thAllow
                )
        PermitPatternMatches thAllow -> do
            allowed <- resolveTHNames thAllow
            return
                ( const True
                , allowed
                , Just ("PermitPatternMatches", thAllow)
                , "PermitPatternMatches " ++ show thAllow
                )
    return NormInspect
        { niInteresting = interesting
        , niExclusion = excl
        , niPosition = isPatternMatch
        , niPermit = permit
        , niForbidLabel = "forbidden pattern matches"
        , niBanner = banner
        , niHeapFilter = False
        }

-- | Normalize an 'InspectAllocations' directive. All of its constructors act
-- on the constructing (allocating) position.
normAllocations :: UNIQ_FM -> InspectAllocations -> CoreM NormInspect
normAllocations anns d = do
    (interesting, excl, permit, banner) <- case d of
        ForbidAllocations thNames -> do
            names <- resolveTHNames thNames
            return
                ( \n -> n `elem` names
                , []
                , Nothing
                , "ForbidAllocations " ++ show thNames
                )
        ForbidFusedAllocations thForbid thAllow -> do
            forbidden <- resolveTHNames thForbid
            allowed <- resolveTHNames thAllow
            return
                ( \n -> isJust (lookupUFM anns n) || n `elem` forbidden
                , allowed
                , Nothing
                , "ForbidFusedAllocations "
                    ++ show thForbid ++ " " ++ show thAllow
                )
        PermitAllocations thAllow -> do
            allowed <- resolveTHNames thAllow
            return
                ( const True
                , allowed
                , Just ("PermitAllocations", thAllow)
                , "PermitAllocations " ++ show thAllow
                )
    return NormInspect
        { niInteresting = interesting
        , niExclusion = excl
        , niPosition = isConstruction
        , niPermit = permit
        , niForbidLabel = "forbidden allocations"
        , niBanner = banner
        , niHeapFilter = True
        }

-- | If the given top level bind's own binder carries an 'InspectPatternMatches'
-- and/or an 'InspectAllocations' annotation, print a report of interesting
-- types case-matched or constructed anywhere in its RHS, per the annotation's
-- rules. A binding may carry one of each; both are processed. No-op if the
-- binder is not annotated, or if the binding has no offending types.
--
-- The output honours the module-wide verbosity: at the default (or
-- @verbose=1@) a single terse @found forbidden types@ line is printed; at
-- @verbose=2@ and above the full "Inspecting ..." banner plus per-hit
-- @SCRUTINIZE@/@CONSTRUCT@ breakdown is printed.
-- Returns the number of directives (0, 1 or 2) that reported a violation.
reportInspected
    :: DynFlags -> ReportMode -> UNIQ_FM -> INSPECT_PM_FM -> INSPECT_ALLOC_FM
    -> [(CoreBndr, CoreExpr)] -> CoreBind -> CoreM Int
reportInspected dflags reportMode anns pmAnns allocAnns allBinds (NonRec b _)
    | subsumedBySameName allBinds b = return 0
    | otherwise = do
        n1 <- maybe (return 0)
                (\d -> normPatternMatches anns d >>= go)
                (lookupBinderAnn b pmAnns)
        n2 <- maybe (return 0)
                (\d -> normAllocations anns d >>= go)
                (lookupBinderAnn b allocAnns)
        return (n1 + n2)

    where

    go ni = do
        let isInteresting = niInteresting ni
            exclusion = niExclusion ni
            inPosition = niPosition ni
            heapFilter = if niHeapFilter ni
                         then keepHeapAllocatedOnly
                         else id
        let allHits = filter (inPosition . snd)
                    $ heapFilter
                    $ concatMap
                        (\(v, e) ->
                            containsAnns dflags isInteresting (NonRec v e))
                        (binderClosure allBinds b)
            results = filterExcluded exclusion allHits
        warnStalePermitted ni allHits
        if null results
        then return 0
        else do
            case reportMode of
                ReportSilent -> terse ni results
                ReportWarn -> terse ni results
                _ -> detailed ni results
            return 1

    -- Warn about "permit" (allowlist) entries that never occur in the
    -- binding, so that stale types can be pruned from the permit list. The
    -- predicate for these directives is @const True@, so 'allHits' holds
    -- every (allocating) type occurrence in the relevant position(s).
    warnStalePermitted ni allHits =
        case niPermit ni of
            Nothing -> return ()
            Just (label, thAllow) -> do
                allowed <- resolveTHNames thAllow
                let present = mapMaybe (contextTyConName . snd) allHits
                    stale = filter (`notElem` present) allowed
                unless (null stale) $
                    putMsgS $ "fusion-plugin: "
                            ++ getOccString (GET_NAME b)
                            ++ ": redundant " ++ label
                            ++ " entries (safe to remove): ["
                            ++ DL.intercalate ", " (map qualifiedName stale)
                            ++ "]"

    terse ni results =
        let names = DL.nub (mapMaybe (contextQualifiedName . snd) results)
        in putMsgS $ "fusion-plugin: "
                   ++ getOccString (GET_NAME b)
                   ++ ": found " ++ niForbidLabel ni ++ " ["
                   ++ DL.intercalate ", " names ++ "]"

    detailed ni results = do
        putMsgS $ "fusion-plugin: "
                ++ showWithUnique dflags b
                ++ ": inspecting (" ++ niBanner ni ++ ")..."
        let getAlts x =
                case x of
                    (bs, CaseAlt alt) -> Just (bs, Left alt)
                    (bs, CaseScrut bndr) -> Just (bs, Right bndr)
                    _ -> Nothing
            patternMatches = mapMaybe getAlts results
            uniqBinders =
                DL.nub (map (getNonRecBinder . head . fst) patternMatches)

            getConstrs x =
                case x of
                    (bs, Constr con) -> Just (bs, con)
                    _ -> Nothing
            constrs = mapMaybe getConstrs results
            uniqConstr = DL.nub (map (getNonRecBinder . head . fst) constrs)

        showInfo b dflags reportMode "SCRUTINIZE"
            uniqBinders patternMatches showDetailsScrutinize
        showInfo b dflags reportMode "CONSTRUCT"
            uniqConstr constrs showDetailsConstr
reportInspected _ _ _ _ _ _ (Rec _) =
    error "reportInspected: expecting only NonRec binders"

-------------------------------------------------------------------------------
-- Inspect the presence/absence of type classes in a binding's Core
-------------------------------------------------------------------------------

-- | Collect the class 'TyCon's appearing anywhere in the Core of a binding. A
-- type class manifests in Core as a dictionary; its type has the class 'TyCon'
-- at the head (see 'isClassTyCon'). We gather the 'TyCon's mentioned by the
-- type of every 'Var', binder, case scrutinee and 'Type' node in the RHS and
-- keep those that are classes.
classTyConsInBind :: CoreBind -> [TyCon]
classTyConsInBind bind =
    nonDetEltsUniqSet (goBind bind)

    where

    fromType t = filterUniqSet isClassTyCon (tyConsOfType t)

    go (Var i) = fromType (varType i)
    go (Lit _) = emptyUniqSet
    go (App e1 e2) = go e1 `unionUniqSets` go e2
    go (Lam b e) = fromType (varType b) `unionUniqSets` go e
    go (Let b e) = goBind b `unionUniqSets` go e
    go (Case e b t alts) =
        go e
            `unionUniqSets` fromType (varType b)
            `unionUniqSets` fromType t
            `unionUniqSets` unionManyUniqSets (map goAlt alts)
    go (Cast e _) = go e
    go (Tick _ e) = go e
    go (Type t) = fromType t
    go (Coercion _) = emptyUniqSet

    goAlt (ALT_CONSTR(_,bs,e)) =
        unionManyUniqSets (map (fromType . varType) bs) `unionUniqSets` go e

    goBind (NonRec b e) = fromType (varType b) `unionUniqSets` go e
    goBind (Rec bs) =
        unionManyUniqSets
            (map (\(b, e) -> fromType (varType b) `unionUniqSets` go e) bs)

-- | Build the "isInteresting" predicate over class 'Name's for a given
-- 'InspectTypeClasses' directive.
inspectClassPredicate :: InspectTypeClasses -> CoreM (Name -> Bool)
inspectClassPredicate (ForbidTypeClasses thNames) = do
    names <- resolveTHNames thNames
    return (\n -> n `elem` names)
inspectClassPredicate (PermitTypeClasses thAllow) = do
    allowed <- resolveTHNames thAllow
    return (\n -> n `notElem` allowed)

-- | If the given top level bind's own binder carries an 'InspectTypeClasses'
-- annotation, print a report of the type classes present in its Core that the
-- directive flags as forbidden. No-op if the binder is not annotated, or if no
-- offending class is present.
-- Returns 0 on no violations and 1 otherwise.
reportInspectedClasses
    :: DynFlags
    -> ReportMode
    -> INSPECT_CLASSES_FM
    -> [(CoreBndr, CoreExpr)]
    -> CoreBind
    -> CoreM Int
reportInspectedClasses dflags reportMode classAnns allBinds (NonRec b _) =
    case lookupBinderAnn b classAnns of
        Just ispec | not (subsumedBySameName allBinds b) -> go ispec
        _ -> return 0

    where

    go ispec = do
        isInteresting <- inspectClassPredicate ispec
        let allClasses = concatMap
                         (\(v, e) -> classTyConsInBind (NonRec v e))
                         (binderClosure allBinds b)
            hits = filter (isInteresting . getName) allClasses
        warnStalePermitted ispec allClasses
        if null hits
        then return 0
        else do
            case reportMode of
                ReportSilent -> report ispec hits
                ReportWarn -> report ispec hits
                _ -> do
                    putMsgS $ "fusion-plugin: "
                            ++ showWithUnique dflags b
                            ++ ": inspecting (" ++ showSDoc dflags (ppr ispec) ++ ")..."
                    report ispec hits
            return 1

    report _ hits =
        let names = DL.nub (map qualifiedTyConName hits)
        in putMsgS $ "fusion-plugin: "
                   ++ getOccString (GET_NAME b)
                   ++ ": found forbidden type classes ["
                   ++ DL.intercalate ", " names ++ "]"

    -- Warn about 'PermitTypeClasses' entries that never occur in the binding,
    -- so that stale classes can be pruned from the permit list.
    warnStalePermitted (PermitTypeClasses thAllow) allClasses = do
        allowed <- resolveTHNames thAllow
        let present = map getName allClasses
            stale = filter (`notElem` present) allowed
        unless (null stale) $
            putMsgS $ "fusion-plugin: "
                    ++ getOccString (GET_NAME b)
                    ++ ": redundant PermitTypeClasses entries (safe to remove): ["
                    ++ DL.intercalate ", " (map qualifiedName stale) ++ "]"
    warnStalePermitted _ _ = return ()
reportInspectedClasses _ _ _ _ (Rec _) =
    error "reportInspectedClasses: expecting only NonRec binders"

-- Returns 0 on no violations and 1 otherwise.
reportCoreSize
    :: DynFlags -> ReportMode -> MAX_CORE_SIZE_FM
    -> [(CoreBndr, CoreExpr)] -> CoreBind -> CoreM Int
reportCoreSize dflags reportMode sizeAnns allBinds (NonRec b _) =
    case lookupBinderAnn b sizeAnns of
        Just ann | not (subsumedBySameName allBinds b) -> go ann
        _ -> return 0

    where

    clSet = binderClosure allBinds b
    terms = sum (map (cs_tm . exprStats . snd) clSet)

    go (MaxCoreSize maxSize) = do
        case reportMode of
            ReportSilent -> return ()
            ReportWarn -> return ()
            _ ->
                putMsgS $ "fusion-plugin: "
                        ++ showWithUnique dflags b
                        ++ ": core size "
                        ++ show terms
                        ++ " terms (set of " ++ show (length clSet)
                        ++ " bindings)"
        if terms > maxSize
        then do
            putMsgS $ "fusion-plugin: "
                    ++ showWithUnique dflags b
                    ++ ": core size (" ++ show terms
                    ++ " terms) exceeds the specified size ("
                    ++ show maxSize ++ " terms)."
            return 1
        else return 0
reportCoreSize _ _ _ _ (Rec _) =
    error "reportCoreSize: expecting only NonRec binders"

-- binderAnnKeys is shared with lookupBinderAnn and this function so that both
-- are consistent with each other.
failOnUnmatchedAnns
    :: DynFlags -> String -> String
    -> [CoreBind] -> [(CoreBndr, CoreExpr)]
    -> InspectAnns
    -> CoreM ()
failOnUnmatchedAnns dflags pkgName modName allTopBinds allBinds anns = do
    let candidateKeys = DL.nub (concatMap (binderAnnKeys . fst) allBinds)
        unmatched =
            filter (`notElem` candidateKeys) $ DL.nub
                ( Map.keys (iaPatternMatches anns)
               ++ Map.keys (iaAllocations anns) ++ Map.keys (iaClasses anns)
               ++ Map.keys (iaSizes anns) ++ Map.keys (iaDumps anns) )
    mapM_
        (\k ->
            putMsgS $ "fusion-plugin: " ++ k
                ++ ": annotated but no such binding in " ++ modName
                ++ " (inlined away -- try NOINLINE or NoFuseTypes on it).")
        unmatched
    when (not (null unmatched)) $ do
        -- Dump whatever Core we do have before failing: the annotated binding
        -- is gone, so there is no single 'CoreBind' left to hand to
        -- 'dumpBindCore', therefore a 'DumpCore' annotation will also not
        -- work, do a whole-module dump so we can still diagnose the problem.
        dumpAllBindsCore dflags pkgName modName allTopBinds
        liftIO $ throwGhcExceptionIO
            (ProgramError
                ("fusion-plugin: " ++ show (length unmatched)
                  ++ " inspection annotation(s) matched no binding in "
                  ++ modName ++ "; failing the build."))

-- | @runInspect@ controls whether the per-binding inspection annotations are
-- also processed. When @werror@ is set, the build is failed (after all
-- annotation violations in the module have been reported) if any
-- annotation-check violation ('InspectPatternMatches', 'InspectAllocations',
-- 'InspectTypeClasses' or 'MaxCoreSize') was found.
fusionReport
    :: String -> ReportMode -> Bool -> Options
    -> ModGuts -> CoreM ModGuts
fusionReport mesg reportMode runInspect opts guts = do
    let getAnns name =
            if runInspect
            then getAnnotationsByStableName name deserializeWithData guts
            else return Map.empty
    pmAnns    <- getAnns "InspectPatternMatches"
    allocAnns <- getAnns "InspectAllocations"
    sizeAnns  <- getAnns "MaxCoreSize"
    dumpAnns  <- getAnns "DumpCore"
    classAnns <- getAnns "InspectTypeClasses"
    let inspectAnns = InspectAnns
            { iaPatternMatches = pmAnns
            , iaAllocations    = allocAnns
            , iaClasses        = classAnns
            , iaSizes          = sizeAnns
            , iaDumps          = dumpAnns
            }
    let anyInspect =
            runInspect && (not (Map.null pmAnns) || not (Map.null allocAnns))
        anyInspectClasses = runInspect && not (Map.null classAnns)
        anyMaxCoreSize = runInspect && not (Map.null sizeAnns)
        anyDumpCore = runInspect && not (Map.null dumpAnns)
        anyViolationAnn = anyInspect || anyInspectClasses || anyMaxCoreSize
        anyReport = anyViolationAnn || anyDumpCore
    case reportMode of
        ReportSilent | not anyReport -> return guts
        _ -> do
            when (reportMode /= ReportSilent) $
                putMsgS $ "fusion-plugin: " ++ mesg ++ "..."
            dflags <- getDynFlags
            anns <- FMAP_SND getAnnotations deserializeWithData guts
            let liveBndrs = liveTopLevelBinders guts
            let pkgName = modulePackageName (mg_module guts)
            let modName = moduleNameString (moduleName (mg_module guts))
            let allBinds = flattenBinds (mg_binds guts)
            when (optionsDumpCoreSizes opts && anyViolationAnn) $ liftIO $ do
                let path = coreSizesFile dflags pkgName modName
                    writeHeader =
                        if optionsCsvAppend opts then appendFile else writeFile
                createDirectoryIfMissing True (takeDirectory path)
                writeHeader path "name,core-size\n"
            violations <-
                if anyUFM (any (== Fuse)) anns || anyReport
                then fmap sum
                        $ mapM
                            (transformBind
                                dflags anns inspectAnns
                                pkgName modName liveBndrs allBinds)
                        $ mg_binds guts
                else return 0
            -- Fail on any inspection annotation whose target has no
            -- corresponding binding in the final core.
            failOnUnmatchedAnns
                dflags pkgName modName (mg_binds guts) allBinds inspectAnns
            when (optionsWError opts && violations > 0) $ do
                putMsgS $ "fusion-plugin: " ++ show violations
                        ++ " annotation violation(s) reported in " ++ modName
                        ++ "; failing the build (werror)."
                liftIO $ throwGhcExceptionIO
                    (ProgramError
                        "fusion-plugin: annotation violations found (werror)")
            return guts

    where

    -- Returns the number of annotation-check violations reported for this
    -- binding (the module-wide unfused report below is not counted).
    transformBind
        :: DynFlags -> UNIQ_FM -> InspectAnns
        -> String -> String -> VarSet -> [(CoreBndr, CoreExpr)]
        -> CoreBind -> CoreM Int
    transformBind dflags anns iAnns
            pkgName modName liveBndrs allBinds bind@(NonRec b _) = do
        let pmAnns    = iaPatternMatches iAnns
            allocAnns = iaAllocations iAnns
            classAnns = iaClasses iAnns
            sizeAnns  = iaSizes iAnns
            dumpAnns  = iaDumps iAnns
        n1 <- if runInspect
              then reportInspected
                       dflags reportMode anns pmAnns allocAnns allBinds bind
              else return 0
        n2 <- if runInspect
              then reportInspectedClasses
                       dflags reportMode classAnns allBinds bind
              else return 0
        n3 <- if runInspect
              then reportCoreSize dflags reportMode sizeAnns allBinds bind
              else return 0
        let hasViolationAnn =
                   isJust (lookupBinderAnn b pmAnns)
                || isJust (lookupBinderAnn b allocAnns)
                || isJust (lookupBinderAnn b classAnns)
                || isJust (lookupBinderAnn b sizeAnns)
            hasDumpAnn = isJust (lookupBinderAnn b dumpAnns)
            notSubsumed = not (subsumedBySameName allBinds b)
        when (runInspect && optionsDumpCoreSizes opts
                  && hasViolationAnn && notSubsumed) $
            dumpCoreSize dflags pkgName modName allBinds b
        let shouldDump
                | hasDumpAnn = True
                | otherwise =
                       (optionsDumpCoreIfAnnotated opts && hasViolationAnn)
                    || (optionsDumpCoreIfViolated opts && n1 + n2 + n3 > 0)
        when (shouldDump && notSubsumed) $
            dumpBindCore dflags pkgName modName allBinds b
        when (b `elemVarSet` liveBndrs) $ do
            let results = keepHeapAllocatedOnly
                        $ containsAnns dflags (isJust . lookupUFM anns) bind

            let getAlts x =
                    case x of
                        (bs, CaseAlt alt) -> Just (bs, alt)
                        _ -> Nothing
            let patternMatches = mapMaybe getAlts results
            let uniqBinders = DL.nub (map (getNonRecBinder . head . fst)
                                          patternMatches)

            -- let constrs = constructingBinders anns bind
            let getConstrs x =
                    case x of
                        (bs, Constr con) -> Just (bs, con)
                        _ -> Nothing
            let constrs = mapMaybe getConstrs results
            let uniqConstr = DL.nub (map (getNonRecBinder. head . fst) constrs)

            -- TBD: For ReportWarn level prepare a single consolidated list of
            -- paths with one entry for each binder and giving one example of
            -- what it scrutinizes and/or constructs, for example:
            --
            -- \$sconcat_s8wu/step5_s8M4: Scrutinizes ConcatOuter, Constructs Yield
            --
            case reportMode of
                ReportSilent -> return ()
                ReportWarn -> do
                    let allBnds = map fst patternMatches ++ map fst constrs
                    when (not $ null allBnds) $ do
                        putMsgS "Unfused bindings:"
                        putMsgS
                            $ DL.unlines
                            $ DL.nub
                            $ map (listPath dflags) allBnds
                _ -> do
                    showInfo b dflags reportMode "SCRUTINIZE"
                        uniqBinders patternMatches showDetailsCaseMatch
                    showInfo b dflags reportMode "CONSTRUCT"
                        uniqConstr constrs showDetailsConstr
        return (n1 + n2 + n3)

    transformBind dflags anns iAnns
            pkgName modName liveBndrs allBinds (Rec bs) =
        fmap sum
            $ mapM
                (\(b, expr) ->
                    transformBind
                        dflags anns iAnns
                        pkgName modName liveBndrs allBinds (NonRec b expr))
                bs

#endif
