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
    , exprVarOccs
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
    , subsumedBySameName
    )

-- Keyed by 'OccName' string rather than by 'Name'/'Unique'. A top-level Id's
-- Unique -- and even its 'NameSort' -- is not guaranteed to survive the
-- Core-to-core passes while OccName stays the same.
#define INSPECT_PM_FM Map.Map String InspectPatternMatches
#define INSPECT_CONSTR_FM Map.Map String InspectConstructions
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
--
-- The 'Bool' each constructor carries is the /boundary/ flag -- 'True' for an
-- unavoidable crossing of the binding's boundary, not a fusion failure. The two
-- sides are dual:
--
-- * Pattern match ('CaseAlt', 'CaseScrut') -- the /input/ boundary: 'True' when
--   the scrutinized value is one of an eligible binding's own top-level
--   parameters (the leading lambda binders of its RHS), input handed in from
--   outside, so an unavoidable unpack.
--
-- * Construction ('Constr') -- the /output/ boundary: 'True' when the
--   construction sits in the binding's return\/tail position, output handed back
--   to the caller, so an unavoidable repack.
--
-- Excluded on the input side (flag 'False', so reported): lambdas nested below
-- the top-level parameters (stepper functions, join points, continuations),
-- and the parameters of any binding that is not the annotated function itself
-- or that takes part in a recursion cycle (see 'boundaryEligibleBinder'); the
-- annotated function's direct @$w@ worker /is/ included. Excluded on the
-- output side: any construction not in the direct tail spine -- one fed into a
-- call\/jump, let-bound, or built inside a nested lambda -- since it is
-- consumed within the binding. Anything excluded is a real fusion hit.
data Context
    = CaseAlt Bool (Alt CoreBndr)
    | CaseScrut Bool CoreBndr
    | Constr Bool Id

-- Inspection detects everything detectable: it looks for interesting types
-- everywhere in a binding, in every subexpression including case scrutinees.
-- (Force-inlining is deliberately narrower; those choices are documented in
-- Fusion.Plugin.Fuse)
--
-- | Whether the leading parameters of a binding in the annotated function's
-- closure are a genuine argument /boundary/ (see 'Context'). Two conditions
-- must both hold:
--
-- 1. The binding /is/ the annotated function: its display name (which strips a
--    @$w@ worker prefix, see 'binderDisplayName') equals the annotated binder's.
--    This admits the user function and its direct @$w@ worker -- whose leading
--    lambdas carry the user's actual arguments (e.g. @$wfinallyIO size start@)
--    -- while rejecting internal helpers and specializations that the optimizer
--    named differently (e.g. a SpecConstr @$s$wgo@), whose parameters are loop
--    state, not arguments.
--
-- 2. The binding takes part in no recursion cycle -- direct /or/ mutual (see
--    'participatesInCycle'). A binding in a cycle is a loop, and a parameter it
--    threads back around the cycle is loop state, not a once-per-entry argument
--    unpack.
--
-- Any binding failing either test contributes no boundaries, so all its matches
-- are reported.
boundaryEligibleBinder
    :: [(CoreBndr, CoreExpr)] -> CoreBndr -> CoreBndr -> CoreExpr -> Bool
boundaryEligibleBinder allBinds root v e =
       binderDisplayName v == binderDisplayName root
    -- The commented check is cheaper, but a direct self-reference check would
    -- miss a loop routed through a same-named helper (@$wf -> $s$wgo -> $wf@);
    -- the reachability check does not, so a real fusion hit can never be
    -- hidden in a recursive worker.
    -- && not (v `elemVarSet` exprVarOccs e)
    && not (participatesInCycle allBinds v e)

-- | True if 'v' can be reached from its own RHS 'e' by following term-level
-- references transitively within 'allBinds' -- i.e. 'v' takes part in a
-- recursion cycle, whether direct (@v@'s RHS mentions @v@) or mutual (through
-- one or more other bindings that lead back to @v@).
--
-- TODO(perf): called per closure member with an O(n) list 'lookup' and a
-- rebuilt 'topSet'. For huge modules, precompute the cyclic members once from
-- 'binderClosure' and make eligibility an O(1) set lookup.
participatesInCycle :: [(CoreBndr, CoreExpr)] -> CoreBndr -> CoreExpr -> Bool
participatesInCycle allBinds v e0 = reach (refsOf e0) emptyVarSet

    where

    topSet = mkVarSet (map fst allBinds)

    -- The top-level binders directly referenced by an expression.
    refsOf e = nonDetEltsUniqSet (intersectVarSet (exprVarOccs e) topSet)

    reach [] _ = False
    reach (w : ws) seen
        | w == v               = True
        | w `elemVarSet` seen  = reach ws seen
        | otherwise            =
            case lookup w allBinds of
                Just e  -> reach (refsOf e ++ ws) (extendVarSet seen w)
                Nothing -> reach ws (extendVarSet seen w)

-- | Report whether data constructors of interest are case matched or returned
-- anywhere in the binders, not just case match on entry or construction on
-- return.
--
containsAnns
    :: DynFlags -> (Name -> Bool) -> Bool -> CoreBind
    -> [([CoreBind], Context)]
containsAnns dflags isInteresting boundaryEligible bind =
    -- 'parents' is the current binder's let-parent chain (extended on entry to
    -- each let). 'lams' holds the binding's own top-level parameters: a @case@
    -- on one of them is a boundary unpack (see 'Context'). It is seeded once,
    -- from the leading lambdas of the RHS, and never extended -- lambdas nested
    -- deeper (steppers, join points) bind values produced within the binding.
    goBind bind

    where

    -- Split a binding's RHS into its leading lambda binders (the function's own
    -- parameters) and the body under them.
    collectLams :: CoreExpr -> ([CoreBndr], CoreExpr)
    collectLams (Lam b e) = let (bs, body) = collectLams e in (b : bs, body)
    collectLams e         = ([], e)

    goBind :: CoreBind -> [([CoreBind], Context)]
    goBind b@(NonRec _ e) =
        let (bndrs, body) = collectLams e
            -- A boundary exists only at a user function's signature. In a
            -- compiler-generated binding (a @$w@ worker, a @$s@ specialization,
            -- etc.) the leading lambdas are internal plumbing -- loop state or
            -- specialized arguments the optimizer invented -- not external
            -- input, so we seed no boundary here and report all its matches.
            lams | boundaryEligible = mkVarSet (filter isId bndrs)
                 | otherwise        = emptyVarSet
            -- The body is in return position only for an eligible binding;
            -- there its tail constructions are the unavoidable output repack.
        in go boundaryEligible lams [b] body
    goBind (Rec bs) = bs >>= (\(v, e) -> goBind (NonRec v e))

    -- A @case@ scrutinee is a boundary unpack when, after peeling any 'Cast' or
    -- 'Tick' wrappers, it is a bare reference to a top-level parameter. An
    -- application head (e.g. @step s@) is deliberately not peeled: that is a
    -- computed value, not a parameter, so it is not a boundary.
    scrutVar :: CoreExpr -> Maybe Id
    scrutVar (Var v)    = Just v
    scrutVar (Cast e _) = scrutVar e
    scrutVar (Tick _ e) = scrutVar e
    scrutVar _          = Nothing

    -- We exclude by boundary rather than positively selecting let-bound
    -- scrutinees: a leftover `case step s of Yield/..` has an /application/
    -- scrutinee (not a let binder), and that is the main failure to catch.
    isBoundaryScrut :: VarSet -> CoreExpr -> Bool
    isBoundaryScrut lams scrut =
        maybe False (`elemVarSet` lams) (scrutVar scrut)

    -- A data-constructor 'Id' of an interesting type is a construction hit,
    -- whether applied to its fields (head of an 'App' spine, e.g. `Yield x y`)
    -- or nullary (a bare 'Var', e.g. `Nothing`, `[]`). An ordinary variable is
    -- not: even at a boxed type like `Int` it only references a value boxed
    -- elsewhere, so it allocates nothing.
    dataConHit :: Bool -> [CoreBind] -> Id -> [([CoreBind], Context)]
    dataConHit inRet parents i
        | Just dcon <- isDataConId_maybe i
        , isInteresting (getName (dataConTyCon dcon)) = [(parents, Constr inRet i)]
        | otherwise = []

    -- A saturated data-constructor application: its fields are themselves part
    -- of the returned value when the construction is in return position.
    isConApp :: CoreExpr -> Bool
    isConApp (Var i) = isJust (isDataConId_maybe i)
    isConApp _       = False

    -- Like 'dataConHit' but keyed on a binder's /type/ rather than a data
    -- constructor: a scrutiny hit when the case binder's type is an
    -- interesting TyCon. Used for a default-only (or literal) case, which
    -- matches no data constructor, so the type must come from the scrutinee.
    scrutHit :: [CoreBind] -> Bool -> CoreBndr -> [([CoreBind], Context)]
    scrutHit parents boundary bndr =
        case tyConAppTyConPicky_maybe (varType bndr) of
            Just tycon | isInteresting (getName tycon) ->
                [(parents, CaseScrut boundary bndr)]
            _ -> []

    -- The 'Bool' is 'inRet': whether the current position is the binding's
    -- return/tail position, used to flag a construction as an output boundary.
    go :: Bool -> VarSet -> [CoreBind] -> CoreExpr -> [([CoreBind], Context)]

    -- Match and record the case alternative if it contains a constructor
    -- annotated with "Fuse", and traverse both the scrutinee and the Alt
    -- expressions to discover more hits and let bindings. Traversing the
    -- scrutinee ('scrut') matters for matches buried there -- e.g. a stream
    -- stepper lambda passed to an un-inlined imported combinator (such as
    -- 'foldBreak'), which lands in the scrutinee of the outer result-unpacking
    -- @case (# _, _ #)@. The scrutinee is not in return position; each alt
    -- inherits the case's return position.
    go inRet lams parents (Case scrut caseBndr _ alts) =
        let binders =
                   go False lams parents scrut
                ++ (alts >>= (\(ALT_CONSTR(_,_,expr1)) ->
                                  go inRet lams parents expr1))
            boundary = isBoundaryScrut lams scrut
            hit =
                case altsContainsAnn dflags isInteresting alts of
                    Just x -> [(parents, CaseAlt boundary x)]
                    -- 'altsContainsAnn' recognizes an interesting type only
                    -- through a matched data constructor. A default-only (or
                    -- literal) case has none, so fall back to the scrutinee's
                    -- type -- otherwise e.g. `case s of _ -> ...` with `s ::
                    -- SPEC` would go unreported.
                    Nothing -> scrutHit parents boundary caseBndr
        in hit ++ binders

    -- A construction's fields stay in return position; the arguments of an
    -- ordinary call/jump are consumed inputs, so they leave return position.
    go inRet lams parents e@(App _ _) =
        let (fun, args) = collectArgs e
            hit = case fun of
                Var i -> dataConHit inRet parents i
                _ -> []
            argRet = inRet && isConApp fun
        in hit
            ++ go inRet lams parents fun
            ++ concatMap (go argRet lams parents) args

    go inRet _ parents (Var i) = dataConHit inRet parents i

    -- Recursive traversal
    go inRet lams parents (Let bndr expr1) =
        goLet lams parents bndr ++ go inRet lams parents expr1
    -- A lambda nested below the top-level parameters (a stepper, join point or
    -- continuation) binds a value produced within the binding, not one handed in
    -- from outside, so it does /not/ extend 'lams'; nor is its body the outer
    -- binding's return, so 'inRet' drops to 'False'. Only the RHS's leading
    -- lambdas, seeded in 'goBind', are boundary parameters.
    go _ lams parents (Lam _ expr1) = go False lams parents expr1
    go inRet lams parents (Cast expr1 _) = go inRet lams parents expr1

    -- A 'Tick' (cost-centre, HPC, or debug/source note, present under -prof,
    -- -fhpc, or -g) wraps a sub-expression, which may itself contain matches or
    -- constructions, so descend into it.
    go inRet lams parents (Tick _ expr1) = go inRet lams parents expr1

    -- These carry no sub-expression to traverse.
    go _ _ _ (Lit _) = []
    go _ _ _ (Type _) = []
    go _ _ _ (Coercion _) = []

    -- A let-bound RHS is not the binding's return value (conservatively: we do
    -- not chase a let-bound construction that is later returned through its
    -- binder), so it is traversed with 'inRet' = 'False'.
    goLet :: VarSet -> [CoreBind] -> CoreBind -> [([CoreBind], Context)]
    goLet lams parents bndr@(NonRec _ expr1) = go False lams (bndr : parents) expr1
    goLet lams parents (Rec bs) =
        bs >>= (\(b, expr1) -> goLet lams parents $ NonRec b expr1)

contextTyConName :: Context -> Maybe Name
contextTyConName (CaseAlt _ (ALT_CONSTR(DataAlt dcon,_,_))) =
    Just (getName $ dataConTyCon dcon)
contextTyConName (CaseAlt _ _) = Nothing
contextTyConName (CaseScrut _ bndr) =
    getName <$> tyConAppTyConPicky_maybe (varType bndr)
contextTyConName (Constr _ con) = getName <$> constrTyCon con

-- | Like 'contextTyConName' but yields the fully-qualified @Module.Type@ name
-- (via 'qualifiedTyConName') used in reports rather than the raw 'Name'.
contextQualifiedName :: Context -> Maybe String
contextQualifiedName (CaseAlt _ (ALT_CONSTR(DataAlt dcon,_,_))) =
    Just (qualifiedTyConName (dataConTyCon dcon))
contextQualifiedName (CaseAlt _ _) = Nothing
contextQualifiedName (CaseScrut _ bndr) =
    qualifiedTyConName <$> tyConAppTyConPicky_maybe (varType bndr)
contextQualifiedName (Constr _ con) = qualifiedTyConName <$> constrTyCon con

-- | Drop any hit whose TyCon 'Name' is in the given exclusion list.
filterExcluded
    :: [Name] -> [([CoreBind], Context)] -> [([CoreBind], Context)]
filterExcluded excl =
    filter (\(_, ctx) -> maybe True (`notElem` excl) (contextTyConName ctx))

-- | True when the type occurs in a scrutinizing (pattern-match, i.e. @case@)
-- position -- a value being deconstructed. Note this is a /use/ of the value,
-- not an allocation: the box was allocated elsewhere (see 'isBoxedHit').
isPatternMatch :: Context -> Bool
isPatternMatch (CaseAlt _ _)   = True
isPatternMatch (CaseScrut _ _) = True
isPatternMatch (Constr _ _)    = False

-- | True when the hit is an unavoidable boundary pattern deconstruction.
isBoundary :: Context -> Bool
isBoundary (CaseAlt b _)   = b
isBoundary (CaseScrut b _) = b
isBoundary (Constr b _)    = b

-- | True when the type occurs in a constructing (allocating) position -- a
-- value being built here.
isConstruction :: Context -> Bool
isConstruction (Constr _ _)    = True
isConstruction (CaseAlt _ _)   = False
isConstruction (CaseScrut _ _) = False

-- | True for unboxed TyCons: unboxed primitives (e.g. 'Int#', 'State#'),
-- unboxed tuples and unboxed sums.
--
-- Note enumeration types (e.g. '()', 'Bool', 'SPEC') are /not/ included here.
-- Even though GHC allocates their values as global staic closures rather than
-- on the heap, they are still boxed -- deconstructing or using one goes
-- through a pointer indirection.
isUnboxedTyCon :: TyCon -> Bool
isUnboxedTyCon tycon =
    isPrimTyCon tycon
    || isUnboxedTupleTyCon tycon
    || isUnboxedSumTyCon tycon

-- | False for hits that can never represent boxing: a case match or
-- construction of an unboxed type (see 'isUnboxedTyCon'), or a bare reference
-- to something of function type (e.g. a primop like \"+#\", or a specialized
-- worker) which is not a data construction at all.
--
-- This covers all usage including case scrutiny as well as construction.
isBoxedHit :: Context -> Bool
isBoxedHit (CaseAlt _ (ALT_CONSTR(DataAlt dcon,_,_))) =
    not (isUnboxedTyCon (dataConTyCon dcon))
isBoxedHit (CaseAlt _ _) = True
isBoxedHit (CaseScrut _ bndr) =
    maybe True (not . isUnboxedTyCon)
        (tyConAppTyConPicky_maybe (varType bndr))
isBoxedHit (Constr _ con) =
    case isDataConId_maybe con of
        -- A genuine data-constructor 'Id' is never itself the "bare
        -- reference to something of function type" this guards against
        -- (that's a primop/worker, never a 'DataCon'), so the 'isFunTy'
        -- exclusion below does not apply here -- it would wrongly drop
        -- every non-nullary constructor (e.g. @(:)@), whose own type is a
        -- function over its fields.
        Just dcon -> not (isUnboxedTyCon (dataConTyCon dcon))
        Nothing ->
            not (isFunTy (varType con))
            && maybe True (not . isUnboxedTyCon)
                     (tyConAppTyConPicky_maybe (varType con))

-- | Drop the hits that can never represent boxing (see 'isBoxedHit').
keepBoxedOnly
    :: [([CoreBind], Context)] -> [([CoreBind], Context)]
keepBoxedOnly = filter (isBoxedHit . snd)

forbidding :: Bool -> UNIQ_FM -> [Name] -> Name -> Bool
forbidding forbidFused anns names n =
    n `elem` names || (forbidFused && isJust (lookupUFM anns n))

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
-- Inspect pattern matches and constructions
-------------------------------------------------------------------------------

-- | The position-normalized form of an inspection directive. Both
-- 'InspectPatternMatches' and 'InspectConstructions' are reduced to this common
-- shape so that the reporting machinery need not know which one it came from;
-- the two differ only in the position filter, the report labels, and the
-- banner text captured here.
data NormInspect = NormInspect
    { niInteresting :: Name -> Bool
    -- ^ The "isInteresting" predicate: which type 'Name's the directive cares
    -- about.
    , niExclusion   :: [Name]
    -- ^ Names to exclude from the reported hits (the allow list).
    , niExplicit    :: [Name]
    -- ^ Types the directive names explicitly (the forbid list for a "forbid"
    -- directive, the allow list for a "permit" one).
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
normPatternMatches
    :: Bool -> UNIQ_FM -> InspectPatternMatches -> CoreM NormInspect
normPatternMatches forbidFused anns d = do
    (interesting, excl, explicit, permit, banner) <- case d of
        ForbidPatternMatches thNames -> do
            names <- resolveTHNames thNames
            return
                ( forbidding forbidFused anns names
                , []
                , names
                , Nothing
                , "ForbidPatternMatches " ++ show thNames
                )
        PermitPatternMatches thAllow -> do
            allowed <- resolveTHNames thAllow
            return
                ( const True
                , allowed
                , allowed
                , Just ("PermitPatternMatches", thAllow)
                , "PermitPatternMatches " ++ show thAllow
                )
    return NormInspect
        { niInteresting = interesting
        , niExclusion = excl
        , niExplicit = explicit
        , niPosition = isPatternMatch
        , niPermit = permit
        , niForbidLabel = "forbidden pattern matches"
        , niBanner = banner
        }

-- | Normalize an 'InspectConstructions' directive. All of its constructors act
-- on the constructing (allocating) position.
normConstructions
    :: Bool -> UNIQ_FM -> InspectConstructions -> CoreM NormInspect
normConstructions forbidFused anns d = do
    (interesting, excl, explicit, permit, banner) <- case d of
        ForbidConstructions thNames -> do
            names <- resolveTHNames thNames
            return
                ( forbidding forbidFused anns names
                , []
                , names
                , Nothing
                , "ForbidConstructions " ++ show thNames
                )
        PermitConstructions thAllow -> do
            allowed <- resolveTHNames thAllow
            return
                ( const True
                , allowed
                , allowed
                , Just ("PermitConstructions", thAllow)
                , "PermitConstructions " ++ show thAllow
                )
    return NormInspect
        { niInteresting = interesting
        , niExclusion = excl
        , niExplicit = explicit
        , niPosition = isConstruction
        , niPermit = permit
        , niForbidLabel = "forbidden constructions"
        , niBanner = banner
        }

-- | Collect the fusible ('Fuse'-annotated) 'TyCon's that occur as the type of
-- a bare 'Var' /reference/ -- not a data constructor -- anywhere in the Core
-- of an expression. These are consumers of a fusible value: force-inlining
-- deliberately targets the enclosing binder so that case-of-case can eliminate
-- the value (see 'constructingBinders' item 5 in Fusion.Plugin.Fuse). A bare
-- Var reference is not an allocation, so 'containsAnns' does not record it as
-- a 'Constr' hit and it is not verified through 'PermitConstructions'; it is
-- only reported as an advisory note (see 'warnConsumedFusible'). Actual
-- constructors (nullary or applied) are excluded here -- those are already
-- reported as constructions.
--
consumedFusibleTyCons :: UNIQ_FM -> CoreExpr -> [TyCon]
consumedFusibleTyCons fuseAnns e0 =
    nonDetEltsUniqSet (go e0)

    where

    fusibleRef i
        | Nothing <- isDataConId_maybe i
        , Just tc <- tyConAppTyConPicky_maybe (varType i)
        , isJust (lookupUFM fuseAnns (getName tc)) = unitUniqSet tc
        | otherwise = emptyUniqSet

    go (Var i) = fusibleRef i
    go (Lit _) = emptyUniqSet
    go (App e1 e2) = go e1 `unionUniqSets` go e2
    go (Lam _ e) = go e
    go (Let b e) = goBind b `unionUniqSets` go e
    go (Case scrut _ _ alts) =
        go scrut
            `unionUniqSets`
                unionManyUniqSets (map (\(ALT_CONSTR(_,_,e)) -> go e) alts)
    go (Cast e _) = go e
    go (Tick _ e) = go e
    go (Type _) = emptyUniqSet
    go (Coercion _) = emptyUniqSet

    goBind (NonRec _ e) = go e
    goBind (Rec bs) = unionManyUniqSets (map (go . snd) bs)

-- | If the given top level bind's own binder carries an 'InspectPatternMatches'
-- and/or an 'InspectConstructions' annotation, print a report of interesting
-- types case-matched or constructed anywhere in its RHS, per the annotation's
-- rules. A binding may carry one of each; both are processed. No-op if the
-- binder is not annotated, or if the binding has no offending types.
--
-- The output honours the module-wide verbosity: at the default (or
-- @verbose=1@) a single terse @found forbidden types@ line is printed; at
-- @verbose=2@ and above the full "Inspecting ..." banner plus per-hit
-- @SCRUTINIZE@/@CONSTRUCT@ breakdown is printed.
-- Returns the number of directives (0, 1 or 2) that reported a violation, and
-- a flag that is 'True' when the (non-violation) consumed-fusible advisory
-- fired -- so the caller can dump the Core for it without counting it as a
-- violation.
reportInspected
    :: DynFlags -> ReportMode -> Bool -> Bool -> Bool -> UNIQ_FM
    -> INSPECT_PM_FM -> INSPECT_CONSTR_FM
    -> [(CoreBndr, CoreExpr)] -> CoreBind -> CoreM (Int, Bool)
reportInspected
        dflags reportMode forbidFused inspectUnboxed detectBoundary
        anns pmAnns constrAnns allBinds (NonRec b _)
    | subsumedBySameName allBinds b = return (0, False)
    | otherwise = do
        n1 <- maybe (return 0)
                (\d -> normPatternMatches forbidFused anns d >>= go)
                (lookupBinderAnn b pmAnns)
        (n2, advised) <- maybe (return (0, False))
                (\d -> do
                    ni <- normConstructions forbidFused anns d
                    r <- go ni
                    -- Advisory only: references to fusible values that fusion
                    -- would try to inline away. Not counted as a violation, but
                    -- the returned flag lets the caller dump the Core for it.
                    adv <- warnConsumedFusible ni
                    return (r, adv))
                (lookupBinderAnn b constrAnns)
        return (n1 + n2, advised)

    where

    go ni = do
        let isInteresting = niInteresting ni
            exclusion = niExclusion ni
            explicit = niExplicit ni
            inPosition = niPosition ni
            keep (_, ctx) =
                   inspectUnboxed
                || isBoxedHit ctx
                || maybe False (`elem` explicit) (contextTyConName ctx)
        let allHits = filter (inPosition . snd)
                    $ filter keep
                    $ filter (\(_, ctx) ->
                                  detectBoundary || not (isBoundary ctx))
                    -- Boundary detection runs on every closure member, worker
                    -- included -- not just the wrapper. A sum-typed argument is
                    -- not W/W-unpacked, so its boundary unpack lands in the
                    -- worker, and an inlined-away wrapper leaves the worker as
                    -- the only binder holding the arg unpacks.
                    $ concatMap
                        (\(v, e) ->
                            containsAnns dflags isInteresting
                                (boundaryEligibleBinder allBinds b v e)
                                (NonRec v e))
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
                let hits = if inspectUnboxed then allHits else keepBoxedOnly allHits
                    present = mapMaybe (contextTyConName . snd) hits
                    stale = filter (`notElem` present) allowed
                unless (null stale) $
                    putMsgS $ "fusion-plugin: "
                            ++ binderDisplayName b
                            ++ ": redundant " ++ label
                            ++ " entries (safe to remove): ["
                            ++ DL.intercalate ", " (map qualifiedName stale)
                            ++ "]"

    -- Returns 'True' if any consumed-but-not-constructed fusible type was
    -- reported, so the caller can dump the Core even though this is not a
    -- violation.
    warnConsumedFusible ni = do
        let excluded = niExclusion ni
            tycons = DL.nub
                   $ concatMap (consumedFusibleTyCons anns . snd)
                               (binderClosure allBinds b)
            reported = filter ((`notElem` excluded) . getName) tycons
        unless (null reported) $
            putMsgS $ "fusion-plugin: "
                    ++ binderDisplayName b
                    ++ ": note: fusible type(s) consumed as var references,"
                    ++ " not constructed:"
                    ++ " [" ++ DL.intercalate ", " (map qualifiedTyConName reported)
                    ++ "]"
        return (not (null reported))

    terse ni results =
        let names = DL.nub (mapMaybe (contextQualifiedName . snd) results)
        in putMsgS $ "fusion-plugin: "
                   ++ binderDisplayName b
                   ++ ": found " ++ niForbidLabel ni ++ " ["
                   ++ DL.intercalate ", " names ++ "]"

    detailed ni results = do
        putMsgS $ "fusion-plugin: "
                ++ binderDisplayName b
                ++ ": inspecting (" ++ niBanner ni ++ ")..."
        let getAlts x =
                case x of
                    (bs, CaseAlt _ alt) -> Just (bs, Left alt)
                    (bs, CaseScrut _ bndr) -> Just (bs, Right bndr)
                    _ -> Nothing
            patternMatches = mapMaybe getAlts results
            uniqBinders =
                DL.nub (map (getNonRecBinder . head . fst) patternMatches)

            getConstrs x =
                case x of
                    (bs, Constr _ con) -> Just (bs, con)
                    _ -> Nothing
            constrs = mapMaybe getConstrs results
            uniqConstr = DL.nub (map (getNonRecBinder . head . fst) constrs)

        showInfo b dflags reportMode "SCRUTINIZE"
            uniqBinders patternMatches showDetailsScrutinize
        showInfo b dflags reportMode "CONSTRUCT"
            uniqConstr constrs showDetailsConstr
reportInspected _ _ _ _ _ _ _ _ _ (Rec _) =
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
                            ++ binderDisplayName b
                            ++ ": inspecting (" ++ showSDoc dflags (ppr ispec) ++ ")..."
                    report ispec hits
            return 1

    report _ hits =
        let names = DL.nub (map qualifiedTyConName hits)
        in putMsgS $ "fusion-plugin: "
                   ++ binderDisplayName b
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
                    ++ binderDisplayName b
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
    :: ReportMode -> MAX_CORE_SIZE_FM
    -> [(CoreBndr, CoreExpr)] -> CoreBind -> CoreM Int
reportCoreSize reportMode sizeAnns allBinds (NonRec b _) =
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
                        ++ binderDisplayName b
                        ++ ": core size "
                        ++ show terms
                        ++ " terms (set of " ++ show (length clSet)
                        ++ " bindings)"
        if terms > maxSize
        then do
            putMsgS $ "fusion-plugin: "
                    ++ binderDisplayName b
                    ++ ": core size (" ++ show terms
                    ++ " terms) exceeds the specified size ("
                    ++ show maxSize ++ " terms)."
            return 1
        else return 0
reportCoreSize _ _ _ (Rec _) =
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
    , iaConstructions  :: INSPECT_CONSTR_FM
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
               ++ Map.keys (iaConstructions anns) ++ Map.keys (iaClasses anns)
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
-- annotation-check violation ('InspectPatternMatches', 'InspectConstructions',
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
    constrAnns <- getAnns "InspectConstructions"
    sizeAnns  <- getAnns "MaxCoreSize"
    dumpAnns  <- getAnns "DumpCore"
    classAnns <- getAnns "InspectTypeClasses"
    let inspectAnns = InspectAnns
            { iaPatternMatches = pmAnns
            , iaConstructions  = constrAnns
            , iaClasses        = classAnns
            , iaSizes          = sizeAnns
            , iaDumps          = dumpAnns
            }
    let anyInspect =
            runInspect && (not (Map.null pmAnns) || not (Map.null constrAnns))
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
            pkgName modName liveBndrs allBinds bind@(NonRec b rhs) = do
        let pmAnns    = iaPatternMatches iAnns
            constrAnns = iaConstructions iAnns
            classAnns = iaClasses iAnns
            sizeAnns  = iaSizes iAnns
            dumpAnns  = iaDumps iAnns
        (n1, advised) <- if runInspect
              then reportInspected
                       dflags reportMode (optionsForbidFused opts)
                       (optionsInspectUnboxed opts)
                       (optionsDetectBoundaryMatches opts)
                       anns pmAnns constrAnns allBinds bind
              else return (0, False)
        n2 <- if runInspect
              then reportInspectedClasses
                       dflags reportMode classAnns allBinds bind
              else return 0
        n3 <- if runInspect
              then reportCoreSize reportMode sizeAnns allBinds bind
              else return 0
        let hasViolationAnn =
                   isJust (lookupBinderAnn b pmAnns)
                || isJust (lookupBinderAnn b constrAnns)
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
                    || (optionsDumpCoreIfViolated opts
                            && (n1 + n2 + n3 > 0 || advised))
        when (shouldDump && notSubsumed) $
            dumpBindCore dflags pkgName modName allBinds b
        when (b `elemVarSet` liveBndrs) $ do
            -- Drop boundary unpacks here too (unless overridden), just as the
            -- annotated report does. With no annotated root, 'b' is its own
            -- root, so eligibility reduces to the cycle check (see
            -- 'boundaryEligibleBinder'): 'b's own arguments are boundaries
            -- unless 'b' is a loop.
            let detectBoundary = optionsDetectBoundaryMatches opts
                results = filter (\(_, ctx) ->
                                      detectBoundary || not (isBoundary ctx))
                        $ keepBoxedOnly
                        $ containsAnns dflags (isJust . lookupUFM anns)
                              (boundaryEligibleBinder allBinds b b rhs) bind

            let getAlts x =
                    case x of
                        (bs, CaseAlt _ alt) -> Just (bs, alt)
                        _ -> Nothing
            let patternMatches = mapMaybe getAlts results
            let uniqBinders = DL.nub (map (getNonRecBinder . head . fst)
                                          patternMatches)

            -- let constrs = constructingBinders anns bind
            let getConstrs x =
                    case x of
                        (bs, Constr _ con) -> Just (bs, con)
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
