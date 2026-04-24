{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Exception             (IOException, try)
import qualified Data.ByteString.Lazy          as LBS
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.Time                     as Time
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
    , short
    , switch
    , (<**>)
    )
import           System.Exit                   (exitFailure, exitSuccess)

import           Effectful                     (runEff)

import           Krikit.Agent.Ops.Effect.Log   (runLogStdout)
import           Krikit.Agent.Ops.Effect.Probe (defaultManager, runProbeIO)
import           Krikit.Agent.Ops.Effect.Proc  (runProcIO)
import           Krikit.Agent.Ops.Smoke.Config
    ( Paths (..)
    , SmokeConfig (..)
    , defaultConfig
    )
import           Krikit.Agent.Ops.Smoke.Report
    ( buildSummary
    , historyLine
    , renderSummary
    )
import           Krikit.Agent.Ops.Smoke.Run    (Opts (..), runSmoke)
import           Krikit.Agent.Ops.Smoke.Tier   (TierResult, allPassed)

-- | Parse CLI options. Flags match the bash script's contract so the
-- operator-facing surface is stable.
optsParser :: Parser Opts
optsParser =
    Opts
        <$> switch
            (  long "fast"
            <> help "Skip slow tiers (thinker, builder)"
            )
        <*> switch
            (  long "verbose"
            <> short 'v'
            <> help "Print full agent responses"
            )
        <*> switch
            (  long "telegram"
            <> help "Include Telegram round-trip probe"
            )

main :: IO ()
main = do
    opts <- execParser $
        info (optsParser <**> helper)
            (  fullDesc
            <> progDesc "End-to-end smoke test of the krikit agent stack"
            <> header   "krikit-smoke - tier-based liveness + config check"
            )
    runStart opts

runStart :: Opts -> IO ()
runStart opts = do
    startedAt <- Time.getCurrentTime
    mgr       <- defaultManager
    let cfg = defaultConfig
    TIO.putStrLn ("krikit-smoke starting at " <> isoUtc startedAt)
    announceFlags opts

    results <- runEff
                 . runLogStdout
                 . runProcIO
                 . runProbeIO mgr
                 $ runSmoke cfg opts

    TIO.putStrLn (renderSummary results)
    appendHistory (pHistoryLog (scPaths cfg)) startedAt results

    if allPassed results
        then exitSuccess
        else exitFailure

announceFlags :: Opts -> IO ()
announceFlags opts = do
    TIO.putStrLn ("  fast     = " <> showBool (optFast opts))
    TIO.putStrLn ("  verbose  = " <> showBool (optVerbose opts))
    TIO.putStrLn ("  telegram = " <> showBool (optTelegram opts))

showBool :: Bool -> Text
showBool True  = "on"
showBool False = "off"

isoUtc :: Time.UTCTime -> Text
isoUtc t =
    T.pack (Time.formatTime Time.defaultTimeLocale "%FT%TZ" t)

-- | Append one JSON Lines history entry. If the log directory isn't
-- writable (on the laptop, where /var/log/krikit doesn't exist), silently
-- skip — the summary already printed to stdout, no value in aborting.
appendHistory :: FilePath -> Time.UTCTime -> [TierResult] -> IO ()
appendHistory path startedAt results = do
    let line = historyLine (buildSummary startedAt results)
    attempt <- try @IOException (LBS.appendFile path line)
    case attempt of
        Right ()   -> pure ()
        Left   _e  -> pure ()
