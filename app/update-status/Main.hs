{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | krikit-update-status: print pending-update info for the agent
-- stack tools (codex, openclaw, claude) and macOS.
--
-- Two modes:
--
-- * Default: read the cache + query npm; print a report.
-- * @--refresh-cache@: run @softwareupdate -l@, write the cache,
--   print a brief summary. Runs as the invoking user; works
--   without sudo because @\/var\/lib\/krikit\/@ is owned by
--   @agentops:admin@ per PB 10 Phase 2.
module Main (main) where

import qualified Data.Text.IO                           as TIO

import           Effectful                              (runEff)
import           Options.Applicative
    ( Parser
    , execParser
    , fullDesc
    , header
    , help
    , helper
    , info
    , long
    , progDesc
    , switch
    , (<**>)
    )
import           System.Exit                            (exitFailure, exitSuccess)

import           Krikit.Agent.Ops.Effect.Proc           (runProcIO)
import           Krikit.Agent.Ops.UpdateStatus.MacOS    (readMacOSStatus)
import           Krikit.Agent.Ops.UpdateStatus.Refresh
    ( RefreshOutcome (..)
    , refreshCache
    )
import           Krikit.Agent.Ops.UpdateStatus.Report
    ( renderMacOSStatus
    , renderReport
    )
import           Krikit.Agent.Ops.UpdateStatus.Run      (checkAllTools)

newtype Opts = Opts Bool   -- True == --refresh-cache requested

optsParser :: Parser Opts
optsParser =
    Opts <$> switch
        ( long "refresh-cache"
        <> help "Run softwareupdate -l and refresh /var/lib/krikit/macos-updates.cache. \
                \No sudo required; runs as the invoking user."
        )

main :: IO ()
main = do
    Opts refresh <- execParser $
        info (optsParser <**> helper)
            (  fullDesc
            <> progDesc "Report installed-vs-latest versions for the agent stack."
            <> header   "krikit-update-status"
            )

    if refresh
        then runRefresh
        else runReport

runReport :: IO ()
runReport = do
    macos <- readMacOSStatus
    tools <- runEff . runProcIO $ checkAllTools
    TIO.putStrLn (renderReport tools macos)

runRefresh :: IO ()
runRefresh = do
    outcome <- runEff . runProcIO $ refreshCache
    case outcome of
        RefreshOk status -> do
            TIO.putStrLn "Cache refreshed."
            TIO.putStrLn (renderMacOSStatus status)
            exitSuccess
        RefreshSubprocessFailed reason -> do
            TIO.putStrLn ("FAILED: " <> reason)
            exitFailure
        RefreshWriteFailed reason -> do
            TIO.putStrLn ("FAILED: cache write: " <> reason)
            TIO.putStrLn
                "Most likely fix: stale ownership on the cache file."
            TIO.putStrLn
                "  sudo chown agentops:admin /var/lib/krikit/macos-updates.cache"
            exitFailure
