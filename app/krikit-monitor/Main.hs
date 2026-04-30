{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @krikit-monitor@: cron-style health monitor + Telegram
-- alerter for the kritick agent host. Runs every 5 minutes via
-- launchd; emits transition alerts and a once-daily digest.
--
-- Replaces @\/usr\/local\/bin\/krikit\/monitor.py@. Same
-- behaviour, typed.
--
-- Reads bot credentials from the environment:
--
--   * @TELEGRAM_BOT_TOKEN@
--   * @TELEGRAM_ALERT_CHAT_ID@
--
-- The wrapper script (Phase 4 of @macmini-monitoring-playbook.md@)
-- sources @channels.env@ before exec.
module Main (main) where

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
    , optional
    , progDesc
    , short
    , strOption
    , switch
    , (<**>)
    )
import           System.Exit                          (exitSuccess)

import           Effectful                            (runEff)

import           Krikit.Agent.Ops.Effect.Telegram
    ( botCredsFromEnv
    , runTelegramIO
    )
import           Krikit.Agent.Ops.Monitor.Digest      (ForceDigest (..))
import           Krikit.Agent.Ops.Monitor.Run
    ( RunConfig (..)
    , runConfigFromEnv
    , runOnceWith
    )

import qualified Network.HTTP.Client                  as HC
import qualified Network.HTTP.Client.TLS              as HTLS

-- | CLI overrides. Sensible defaults baked into 'defaultRunConfig'.
data Opts = Opts
    { optStatePath        :: !(Maybe FilePath)
    , optRegenSummaryBin  :: !(Maybe FilePath)
    , optUpdateStatusBin  :: !(Maybe FilePath)
    , optForceDigest      :: !Bool
    }

optsParser :: Parser Opts
optsParser =
    Opts
        <$> optional
            (strOption
                (  long  "state-path"
                <> metavar "PATH"
                <> help "Override /var/lib/krikit/monitor-state.json"))
        <*> optional
            (strOption
                (  long  "regen-summary-bin"
                <> metavar "PATH"
                <> help "Path to krikit-regen-summary (default: \
                        \/usr/local/bin/krikit/krikit-regen-summary)"))
        <*> optional
            (strOption
                (  long  "update-status-bin"
                <> short 'u'
                <> metavar "PATH"
                <> help "Path to krikit-update-status (default: \
                        \/usr/local/bin/krikit/krikit-update-status)"))
        <*> switch
            (  long "force-digest"
            <> help "Run checks AND send the daily digest right now, \
                    \regardless of the configured digest hour or \
                    \whether one already fired today. Does NOT update \
                    \last_digest_date, so the regularly scheduled \
                    \digest tomorrow morning still fires. Useful for \
                    \verifying digest formatting / Telegram delivery / \
                    \regen-summary integration without waiting for \
                    \07:00 local.")

main :: IO ()
main = do
    opts <- execParser $
        info (optsParser <**> helper)
            (  fullDesc
            <> progDesc "Health-check + Telegram alerter; \
                        \launchd-driven 5-min cron"
            <> header   "krikit-monitor")

    -- Start from env-var-resolved defaults (host name, digest
    -- hour), then layer per-flag CLI overrides on top.
    base <- runConfigFromEnv
    let cfg = base
                { rcStatePath       =
                    maybe (rcStatePath base) id (optStatePath opts)
                , rcRegenSummaryBin =
                    case optRegenSummaryBin opts of
                        Just _  -> optRegenSummaryBin opts
                        Nothing -> rcRegenSummaryBin base
                , rcUpdateStatusBin =
                    case optUpdateStatusBin opts of
                        Just _  -> optUpdateStatusBin opts
                        Nothing -> rcUpdateStatusBin base
                }

    let force = if optForceDigest opts then ForceDigest else NormalSchedule

    creds <- botCredsFromEnv
    mgr   <- HC.newManager HTLS.tlsManagerSettings

    runEff
        . runTelegramIO mgr creds
        $ runOnceWith force cfg

    -- Always exit 0: alerting is a side effect; launchd respawns
    -- us in 5 minutes regardless. A non-zero exit would only mask
    -- the next run.
    exitSuccess
