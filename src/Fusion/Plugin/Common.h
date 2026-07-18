#if MIN_VERSION_ghc(8,6,0)
-- Imports for all compiler versions
import Control.Monad (when, unless, forM_, void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Data.Char (isDigit)
import Data.Data (Data)
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Word (Word8)
import Data.Generics.Schemes (everywhere)
import Data.Generics.Aliases (mkT)
import Debug.Trace (trace)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory, takeFileName)
import qualified Data.List as DL
import qualified Data.Map.Strict as Map
import qualified Language.Haskell.TH.Syntax as TH

-- Imports for specific compiler versions
#if MIN_VERSION_ghc(9,6,0)
import GHC.Core.Lint.Interactive (interactiveInScope)
import GHC.Core.Opt.Simplify.Env (SimplMode(..))
import GHC.Core.Opt.Simplify (SimplifyOpts(..))
import GHC.Driver.Config.Core.Opt.Simplify (initSimplMode, initSimplifyOpts)
#endif

#if MIN_VERSION_ghc(9,14,0)
import GHC.Unit.Env (ue_hpt)
import GHC.Unit.Home.ModInfo (HomeModInfo(..))
import GHC.Unit.Home.PackageTable (concatHpt)
import GHC.Unit.Module.ModDetails (ModDetails(..))
#endif

#if MIN_VERSION_ghc(9,6,0)
import Data.Char (isSpace)
import Text.Printf (printf)
import GHC.Core.Ppr (pprCoreBindingsWithSize, pprRules)
import GHC.Types.Name.Ppr (mkNamePprCtx)
import GHC.Utils.Logger (Logger)
#elif MIN_VERSION_ghc(9,2,0)
import Data.Char (isSpace)
import Text.Printf (printf)
import GHC.Core.Ppr (pprCoreBindingsWithSize, pprRules)
import GHC.Types.Name.Ppr (mkPrintUnqualified)
import GHC.Utils.Logger (Logger)
#endif

-- dump-core option related imports
#if MIN_VERSION_ghc(9,3,0)
import GHC.Utils.Logger (putDumpFile, logFlags, LogFlags(..))
#elif MIN_VERSION_ghc(9,2,0)
import GHC.Utils.Logger (putDumpMsg)
#elif MIN_VERSION_ghc(9,0,0)
import Data.Char (isSpace)
import Text.Printf (printf)
#else
import Data.Char (isSpace)
import Data.IORef (readIORef, writeIORef)
import Data.Time (getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.IO (Handle, IOMode(..), withFile, hSetEncoding, utf8)
import Text.Printf (printf)
import ErrUtils (mkDumpDoc, Severity(..))
import PprCore (pprCoreBindingsWithSize, pprRules)
import qualified Data.Set as Set
#endif
#endif

-- Implicit imports
#if MIN_VERSION_ghc(9,6,0)
import GHC.Plugins
import qualified GHC.Plugins as GhcPlugins
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Plugins
import qualified GHC.Plugins as GhcPlugins
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
