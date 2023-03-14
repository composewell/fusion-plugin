-- |
-- Module      : Fusion.Plugin.Ghc860
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Fusion.Plugin.Ghc860
    ( coreToDo
    , dumpCore
    , isPhase0MainTodo
    , defaultPurePlugin
    )
where

import Control.Monad (unless)
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

-- Implicit imports
import GhcPlugins

-------------------------------------------------------------------------------
-- Simplification pass after marking inline
-------------------------------------------------------------------------------

coreToDo :: HscEnv -> DynFlags -> CoreToDo
coreToDo _hsc_env dflags =
    let mode =
            SimplMode
            { sm_phase = InitialPhase
            , sm_names = ["Fusion Plugin Inlining"]
            , sm_dflags = dflags
            , sm_rules = gopt Opt_EnableRewriteRules dflags
            , sm_eta_expand = gopt Opt_DoLambdaEtaExpansion dflags
            , sm_inline = True
            , sm_case_case = True
            }
    in CoreDoSimplify
        (maxSimplIterations dflags) mode

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
    dumpPassResult
        dflags print_unqual (_suffix ++ "dump-simpl") hdr (text "") binds rules

    where

    hdr = text "["
        GhcPlugins.<> int counter
        GhcPlugins.<> text "] "
        GhcPlugins.<> todo

    _suffix = printf "%02d" counter ++ "-"
        ++ (map (\x -> if isSpace x then '-' else x)
               $ filterOutLast isSpace
               $ takeWhile (/= '(')
               $ showSDoc dflags todo)
        ++ "."

dumpCore :: Int -> SDoc -> ModGuts -> CoreM ()
dumpCore counter title guts = do
    dflags <- getDynFlags
    let print_unqual = mkPrintUnqualified dflags (mg_rdr_env guts)
    liftIO $ dumpResult dflags print_unqual counter
                title (mg_binds guts) (mg_rules guts)

isPhase0MainTodo :: CoreToDo -> Bool
isPhase0MainTodo (CoreDoSimplify _ SimplMode
    { sm_phase = Phase 0
    , sm_names = ["main"]
    }) = True
isPhase0MainTodo _ = False

defaultPurePlugin :: Plugin
defaultPurePlugin = defaultPlugin
    { pluginRecompile = purePlugin
    }
