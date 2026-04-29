{-# LANGUAGE OverloadedStrings #-}

-- | @krikit-regen-summary@: emit a two-line summary of last
-- night's regen + verify chain status, suitable for inclusion
-- in the daily Telegram digest (PB 10 monitor.py).
--
-- Reads two directories:
--
--   * @--log-dir@ (default @\/var\/log\/krikit\/regen\/@): per-job
--     log files written by the cron-driven regenerators and
--     verifiers.
--   * @--state-dir@ (default @\/var\/lib\/krikit\/regen\/@):
--     auto-disable counters and markers from
--     'Krikit.Agent.Ops.Regen.AutoDisable'. A job with a marker
--     here surfaces as @DISABLED@ in the summary regardless of
--     log content.
module Main (main) where

import qualified Data.Text.IO                            as TIO
import           Options.Applicative
    ( Parser
    , execParser
    , fullDesc
    , header
    , help
    , helper
    , info
    , long
    , metavar
    , progDesc
    , short
    , strOption
    , value
    , (<**>)
    )

import           Krikit.Agent.Ops.Regen.AutoDisable      (defaultStateDir)
import           Krikit.Agent.Ops.RegenSummary.Run       (defaultLogDir,
                                                          summarize)

data Opts = Opts
    { oLogDir   :: !FilePath
    , oStateDir :: !FilePath
    }

optsParser :: Parser Opts
optsParser =
    Opts
        <$> strOption
            (  long  "log-dir"
            <> short 'l'
            <> metavar "DIR"
            <> value defaultLogDir
            <> help "Override the default /var/log/krikit/regen/ \
                    \(useful for ad-hoc testing)"
            )
        <*> strOption
            (  long  "state-dir"
            <> short 's'
            <> metavar "DIR"
            <> value defaultStateDir
            <> help "Override the default /var/lib/krikit/regen/ \
                    \(auto-disable markers + counters)"
            )

main :: IO ()
main = do
    opts <- execParser $
        info (optsParser <**> helper)
            (  fullDesc
            <> progDesc "Emit a two-line regen+verify status \
                        \summary for the daily digest"
            <> header   "krikit-regen-summary"
            )
    TIO.putStr =<< summarize (oLogDir opts) (oStateDir opts)
    TIO.putStr "\n"
