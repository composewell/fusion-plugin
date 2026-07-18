#if MIN_VERSION_ghc(9,0,0)
#define IS_ACTIVE isActive (Phase 0)
#define UNIQ_FM UniqFM Name [Fuse]
#define GET_NAME getName
#define FMAP_SND fmap snd $
#else
#define IS_ACTIVE isActiveIn 0
#define UNIQ_FM UniqFM [Fuse]
#define GET_NAME getName
#define FMAP_SND
#endif

-- Keyed by 'OccName' string rather than by 'Name'/'Unique'. A top-level Id's
-- Unique -- and even its 'NameSort' -- is not guaranteed to survive the
-- Core-to-core passes while OccName stays the same.
#define INSPECT_PM_FM Map.Map String InspectPatternMatches
#define INSPECT_ALLOC_FM Map.Map String InspectAllocations
#define INSPECT_CLASSES_FM Map.Map String InspectTypeClasses
#define FUSE_TYPES_FM Map.Map String FuseTypes
#define NO_FUSE_TYPES_FM Map.Map String NoFuseTypes
#define NO_FUSE_FM Map.Map String NoFuse
#define MAX_CORE_SIZE_FM Map.Map String MaxCoreSize
#define DUMP_CORE_FM Map.Map String DumpCore
#define DUMP_CORE_PASSES_FM Map.Map String DumpCorePasses
-- GHC-9.6 renamed 'PrintUnqualified' to 'NamePprCtx'.
#if MIN_VERSION_ghc(9,6,0)
#define PRINT_UNQUAL NamePprCtx
#else
#define PRINT_UNQUAL PrintUnqualified
#endif
#if MIN_VERSION_ghc(9,2,0)
#define ALT_CONSTR(x,y,z) Alt (x) y z
#else
#define ALT_CONSTR(x,y,z) (x, y, z)
#endif
