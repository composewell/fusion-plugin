{-# LANGUAGE CPP #-}

module Fusion.Plugin.Inspect
    ( fusionReport
    )
where

#include "Fusion/Plugin/Common.h"

#if MIN_VERSION_ghc(8,6,0)
import Control.Monad (when, unless)
import Data.Maybe (isJust, mapMaybe)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import qualified Data.List as DL
import qualified Data.Map.Strict as Map
import qualified Language.Haskell.TH.Syntax as TH
#endif

-- Implicit imports
#if MIN_VERSION_ghc(9,0,0)
import GHC.Plugins
#else
import GhcPlugins
#endif

#if MIN_VERSION_ghc(9,0,0) && !MIN_VERSION_ghc(9,2,0)
import GHC.Utils.Panic (throwGhcExceptionIO, GhcException(ProgramError))
#elif !MIN_VERSION_ghc(9,0,0)
import Panic (throwGhcExceptionIO, GhcException(ProgramError))
#endif

-- Core size reporting
#if MIN_VERSION_ghc(9,0,0)
import GHC.Core.Stats (exprStats, CoreStats(cs_tm))
#else
import CoreStats (exprStats, CoreStats(cs_tm))
#endif

import Fusion.Plugin.Types

import Fusion.Plugin.Common
    ( Options(..)
    , ReportMode(..)
    , altsContainsAnn
    , binderAnnKeys
    , binderClosure
    , binderDisplayName
    , constrTyCon
    , dumpBindCore
    , getAnnotationsByStableName
    , getNonRecBinder
    , listPath
    , lookupBinderAnn
    , modulePackageName
    , pluginDumpDir
    , pluginDumpStem
    , qualifiedName
    , qualifiedTyConName
    , resolveTHNames
    , showDetailsCaseMatch
    , showDetailsConstr
    , showInfo
    , showWithUnique
    , subsumedBySameName
    )

-- Keyed by 'OccName' string rather than by 'Name'/'Unique'. A top-level Id's
-- Unique -- and even its 'NameSort' -- is not guaranteed to survive the
-- Core-to-core passes while OccName stays the same.
#define INSPECT_PM_FM Map.Map String InspectPatternMatches
#define INSPECT_ALLOC_FM Map.Map String InspectAllocations
#define INSPECT_CLASSES_FM Map.Map String InspectTypeClasses
#define MAX_CORE_SIZE_FM Map.Map String MaxCoreSize
#define DUMP_CORE_FM Map.Map String DumpCore

#if MIN_VERSION_ghc(8,6,0)

-------------------------------------------------------------------------------
-- Core hits: locating and classifying interesting types in a binding
-------------------------------------------------------------------------------

-- | A hit found in the Core of a binding.
-- 1. 'CaseAlt' is a scrutiny that matched a specific data constructor;
-- 2. 'CaseScrut' is a scrutiny with no matched constructor -- a default-only
-- (or literal) @case@ that merely forces a value, e.g. @case s of _ -> ...@
-- where @s :: SPEC@ -- whose type comes from the case binder;
-- 3. 'Constr' is a construction or bare boxed use.
data Context = CaseAlt (Alt CoreBndr) | CaseScrut CoreBndr | Constr Id

-- letBndrsThatAreCases restricts itself to only case matches right on
-- entry to a let. This one looks for case matches anywhere.
--
-- | Report whether data constructors of interest are case matched or returned
-- anywhere in the binders, not just case match on entry or construction on
-- return.
--
containsAnns
    :: DynFlags -> (Name -> Bool) -> CoreBind -> [([CoreBind], Context)]
containsAnns dflags isInteresting bind =
    -- The first argument is current binder and its parent chain. We add a new
    -- element to this path when we enter a let statement.
    goLet [] bind

    where

    -- A data-constructor 'Id' of an interesting type is a construction hit,
    -- whether applied to its fields (head of an 'App' spine, e.g. `Yield x y`)
    -- or nullary (a bare 'Var', e.g. `Nothing`, `[]`). An ordinary variable is
    -- not: even at a boxed type like `Int` it only references a value boxed
    -- elsewhere, so it allocates nothing.
    dataConHit :: [CoreBind] -> Id -> [([CoreBind], Context)]
    dataConHit parents i
        | Just dcon <- isDataConId_maybe i
        , isInteresting (getName (dataConTyCon dcon)) = [(parents, Constr i)]
        | otherwise = []

    -- Like 'dataConHit' but keyed on a binder's /type/ rather than a data
    -- constructor: a scrutiny hit when the case binder's type is an
    -- interesting TyCon. Used for a default-only (or literal) case, which
    -- matches no data constructor, so the type must come from the scrutinee.
    scrutHit :: [CoreBind] -> CoreBndr -> [([CoreBind], Context)]
    scrutHit parents bndr =
        case tyConAppTyConPicky_maybe (varType bndr) of
            Just tycon | isInteresting (getName tycon) ->
                [(parents, CaseScrut bndr)]
            _ -> []

    go :: [CoreBind] -> CoreExpr -> [([CoreBind], Context)]

    -- Match and record the case alternative if it contains a constructor
    -- annotated with "Fuse" and traverse the Alt expressions to discover more
    -- let bindings.
    go parents (Case _ caseBndr _ alts) =
        let binders = alts >>= (\(ALT_CONSTR(_,_,expr1)) -> go parents expr1)
            hit =
                case altsContainsAnn dflags isInteresting alts of
                    Just x -> [(parents, CaseAlt x)]
                    -- 'altsContainsAnn' recognizes an interesting type only
                    -- through a matched data constructor. A default-only (or
                    -- literal) case has none, so fall back to the scrutinee's
                    -- type -- otherwise e.g. `case s of _ -> ...` with `s ::
                    -- SPEC` would go unreported.
                    Nothing -> scrutHit parents caseBndr
        in hit ++ binders

    go parents e@(App _ _) =
        let (fun, args) = collectArgs e
            hit = case fun of
                Var i -> dataConHit parents i
                _ -> []
        in hit ++ go parents fun ++ concatMap (go parents) args

    go parents (Var i) = dataConHit parents i

    -- Recursive traversal
    go parents (Let bndr expr1) = goLet parents bndr ++ go parents expr1
    go parents (Lam _ expr1) = go parents expr1
    go parents (Cast expr1 _) = go parents expr1

    -- There are no let bindings in these.
    go _ (Lit _) = []
    go _ (Tick _ _) = []
    go _ (Type _) = []
    go _ (Coercion _) = []

    goLet :: [CoreBind] -> CoreBind -> [([CoreBind], Context)]
    goLet parents bndr@(NonRec _ expr1) = go (bndr : parents) expr1
    goLet parents (Rec bs) =
        bs >>= (\(b, expr1) -> goLet parents $ NonRec b expr1)

contextTyConName :: Context -> Maybe Name
contextTyConName (CaseAlt (ALT_CONSTR(DataAlt dcon,_,_))) =
    Just (getName $ dataConTyCon dcon)
contextTyConName (CaseAlt _) = Nothing
contextTyConName (CaseScrut bndr) =
    getName <$> tyConAppTyConPicky_maybe (varType bndr)
contextTyConName (Constr con) = getName <$> constrTyCon con

-- | Like 'contextTyConName' but yields the fully-qualified @Module.Type@ name
-- (via 'qualifiedTyConName') used in reports rather than the raw 'Name'.
contextQualifiedName :: Context -> Maybe String
contextQualifiedName (CaseAlt (ALT_CONSTR(DataAlt dcon,_,_))) =
    Just (qualifiedTyConName (dataConTyCon dcon))
contextQualifiedName (CaseAlt _) = Nothing
contextQualifiedName (CaseScrut bndr) =
    qualifiedTyConName <$> tyConAppTyConPicky_maybe (varType bndr)
contextQualifiedName (Constr con) = qualifiedTyConName <$> constrTyCon con

-- | Drop any hit whose TyCon 'Name' is in the given exclusion list.
filterExcluded
    :: [Name] -> [([CoreBind], Context)] -> [([CoreBind], Context)]
filterExcluded excl =
    filter (\(_, ctx) -> maybe True (`notElem` excl) (contextTyConName ctx))

-- | True when the type occurs in a scrutinizing (pattern-match, i.e. @case@)
-- position -- a value being deconstructed. Note this is a /use/ of the value,
-- not an allocation: the box was allocated elsewhere (see 'isHeapAllocated').
isPatternMatch :: Context -> Bool
isPatternMatch (CaseAlt _)   = True
isPatternMatch (CaseScrut _) = True
isPatternMatch (Constr _)    = False

-- | True when the type occurs in a constructing (allocating) position -- a
-- value being built here.
isConstruction :: Context -> Bool
isConstruction (Constr _)    = True
isConstruction (CaseAlt _)   = False
isConstruction (CaseScrut _) = False

-- | True for TyCons whose values GHC never heap-allocates: unboxed
-- primitives (e.g. 'Int#', 'State#'), unboxed tuples/sums, and true
-- enumeration types (every data constructor nullary, e.g. '()', 'Bool')
-- which are compiled as statically shared singletons.
isNotHeapAllocatedTyCon :: TyCon -> Bool
isNotHeapAllocatedTyCon tycon =
    isPrimTyCon tycon
    || isUnboxedTupleTyCon tycon
    || isUnboxedSumTyCon tycon
    || isEnumerationTyCon tycon

-- | False for the cases that can never represent boxing: a case match or
-- construction of a non-allocating type (see 'isNotHeapAllocatedTyCon'), or a
-- bare reference to something of function type (e.g. a primop like \"+#\", or
-- a specialized worker) which is not a data construction at all.
--
-- This covers all usage including case scrutiny as well as construction.
isHeapAllocated :: Context -> Bool
isHeapAllocated (CaseAlt (ALT_CONSTR(DataAlt dcon,_,_))) =
    not (isNotHeapAllocatedTyCon (dataConTyCon dcon))
isHeapAllocated (CaseAlt _) = True
isHeapAllocated (CaseScrut bndr) =
    maybe True (not . isNotHeapAllocatedTyCon)
        (tyConAppTyConPicky_maybe (varType bndr))
isHeapAllocated (Constr con) =
    case isDataConId_maybe con of
        -- A genuine data-constructor 'Id' is never itself the "bare
        -- reference to something of function type" this guards against
        -- (that's a primop/worker, never a 'DataCon'), so the 'isFunTy'
        -- exclusion below does not apply here -- it would wrongly drop
        -- every non-nullary constructor (e.g. @(:)@), whose own type is a
        -- function over its fields.
        Just dcon -> not (isNotHeapAllocatedTyCon (dataConTyCon dcon))
        Nothing ->
            not (isFunTy (varType con))
            && maybe True (not . isNotHeapAllocatedTyCon)
                     (tyConAppTyConPicky_maybe (varType con))

-- | Drop the cases that can never represent boxing (see 'isHeapAllocated').
keepHeapAllocatedOnly
    :: [([CoreBind], Context)] -> [([CoreBind], Context)]
keepHeapAllocatedOnly = filter (isHeapAllocated . snd)

-- | Show a scrutinizing hit for the detailed report. A 'Left' is a scrutiny
-- that matched a data constructor (delegated to 'showDetailsCaseMatch'); a
-- 'Right' is a constructor-less scrutiny (a default-only\/literal @case@) whose
-- type is taken from the case binder.
showDetailsScrutinize
    :: DynFlags
    -> ReportMode
    -> ([CoreBind], Either (Alt CoreBndr) CoreBndr)
    -> String
showDetailsScrutinize dflags reportMode (binds, Left alt) =
    showDetailsCaseMatch dflags reportMode (binds, alt)
showDetailsScrutinize dflags reportMode (binds, Right bndr) =
    let vstr =
            case reportMode of
                ReportVerbose -> showSDoc dflags (ppr bndr)
                ReportVerbose1 -> showSDoc dflags (ppr bndr)
                ReportVerbose2 -> showSDoc dflags (ppr $ head binds)
                _ -> error "showDetailsScrutinize: unreachable"
        tstr =
            case tyConAppTyConPicky_maybe (varType bndr) of
                Just tc -> " :: " ++ qualifiedTyConName tc
                Nothing -> ""
    in listPath dflags binds ++ ": " ++ vstr ++ tstr

-------------------------------------------------------------------------------
-- Inspect pattern matches and allocations
-------------------------------------------------------------------------------

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
        let allHits = filter (inPosition . snd)
                    $ keepHeapAllocatedOnly
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
                            ++ getOccString (getName b)
                            ++ ": redundant " ++ label
                            ++ " entries (safe to remove): ["
                            ++ DL.intercalate ", " (map qualifiedName stale)
                            ++ "]"

    terse ni results =
        let names = DL.nub (mapMaybe (contextQualifiedName . snd) results)
        in putMsgS $ "fusion-plugin: "
                   ++ getOccString (getName b)
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
                   ++ getOccString (getName b)
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
                    ++ getOccString (getName b)
                    ++ ": redundant PermitTypeClasses entries (safe to remove): ["
                    ++ DL.intercalate ", " (map qualifiedName stale) ++ "]"
    warnStalePermitted _ _ = return ()
reportInspectedClasses _ _ _ _ (Rec _) =
    error "reportInspectedClasses: expecting only NonRec binders"

-------------------------------------------------------------------------------
-- Inspect core size
-------------------------------------------------------------------------------

-- | Path of the CSV file that 'dumpCoreSize' appends core sizes to when the
-- @dump-core-sizes@ option is set: @\<module-name\>.core-sizes.csv@ under
-- 'pluginDumpDir'.
coreSizesFile :: DynFlags -> String -> String -> FilePath
coreSizesFile dflags pkgName modName =
    pluginDumpStem dflags pkgName modName ++ "core-sizes.csv"

-- | The optimized Core size, in terms, of a binding: the sum of the term
-- counts of every binding in its closure (see 'binderClosure').
binderCoreSize :: [(CoreBndr, CoreExpr)] -> CoreBndr -> Int
binderCoreSize allBinds b =
    sum (map (cs_tm . exprStats . snd) (binderClosure allBinds b))

-- | Append a @name,core-size@ row for the given binding to the per-module CSV
-- file (see 'coreSizesFile').
dumpCoreSize
    :: DynFlags -> String -> String -> [(CoreBndr, CoreExpr)] -> CoreBndr
    -> CoreM ()
dumpCoreSize dflags pkgName modName allBinds b = liftIO $ do
    let path = coreSizesFile dflags pkgName modName
    createDirectoryIfMissing True (takeDirectory path)
    appendFile path
        (binderDisplayName b ++ "," ++ show (binderCoreSize allBinds b) ++ "\n")

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

-------------------------------------------------------------------------------
-- Reporting entry point
-------------------------------------------------------------------------------

dumpAllBindsCore :: DynFlags -> String -> String -> [CoreBind] -> CoreM ()
dumpAllBindsCore dflags pkgName modName binds = do
    let dir = pluginDumpDir dflags pkgName
        fileName = modName ++ ".dump-simpl"
        path = dir </> fileName
    liftIO $ do
        createDirectoryIfMissing True dir
        writeFile path (showSDoc dflags (vcat (map ppr binds)) ++ "\n")
    putMsgS $ "fusion-plugin: " ++ modName ++ ": dumped core to " ++ path

-- | The set of top level binders that are reachable from an exported
-- binder, and therefore guaranteed to survive into the final program.
--
-- This plugin's final check runs as the very last 'CoreToDo', but GHC's
-- "CoreTidy" runs even after that, it changes the Core and there is no way to
-- run a hook after that in the plugin. We approximate CoreTidy's reachability
-- analysis here so we can skip unreachable bindings, seeding it from the same
-- 'isExportedId' flag CoreTidy itself relies on.
--
liveTopLevelBinders :: ModGuts -> VarSet
liveTopLevelBinders guts = go initial initial

    where

    flattenBind (NonRec b e) = [(b, e)]
    flattenBind (Rec bs) = bs

    topBinds = mg_binds guts >>= flattenBind
    topBndrSet = mkVarSet (map fst topBinds)

    adjacency :: VarEnv VarSet
    adjacency = mkVarEnv
        [ (b, intersectVarSet topBndrSet (exprFreeVars rhs))
        | (b, rhs) <- topBinds
        ]

    initial = mkVarSet (filter isExportedId (map fst topBinds))

    go frontier visited
        | isEmptyVarSet frontier = visited
        | otherwise =
            let next = unionVarSets
                     $ mapMaybe (lookupVarEnv adjacency)
                     $ nonDetEltsUniqSet frontier
                newVisited = unionVarSet visited next
                newFrontier = next `minusVarSet` visited
            in go newFrontier newVisited

data InspectAnns = InspectAnns
    { iaPatternMatches :: INSPECT_PM_FM
    , iaAllocations    :: INSPECT_ALLOC_FM
    , iaClasses        :: INSPECT_CLASSES_FM
    , iaSizes          :: MAX_CORE_SIZE_FM
    , iaDumps          :: DUMP_CORE_FM
    }

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
