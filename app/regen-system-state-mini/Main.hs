{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @krikit-regen-system-state-mini@: regenerate the human-readable
-- mirror of @kritick-ecosystem\/config\/infrastructure-macmini.json@
-- as @krikit-agent-fabric\/context\/system-state-mini.generated.md@.
--
-- Idempotent: substantive content unchanged → file untouched (the
-- "Last generated" timestamp does not by itself trigger a rewrite).
-- Failures (input parse, output write) exit non-zero with a single
-- @FAIL:@ line on stderr.
--
-- All paths come from @agent-ops.json@; see
-- 'Krikit.Agent.Ops.Config'. Override the config location with
-- @--config@ or @KRIKIT_AGENT_OPS_CONFIG@.
module Main (main) where

import qualified Data.Text                                as T
import qualified Data.Text.IO                             as TIO
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
    , (<**>)
    )
import           System.Exit                              (exitFailure)
import           System.IO                                (hPutStrLn, stderr)

import           Effectful                                (runEff)
import           Effectful.Reader.Static                  (runReader)

import           Krikit.Agent.Ops.Config                  (loadConfig)
import           Krikit.Agent.Ops.Regen.Write
    ( WriteOutcome (..)
    , renderOutcome
    )
import           Krikit.Agent.Ops.SystemStateMini.Run     (regenerate)

-- | Single optional override for the agent-ops.json location.
-- Everything else flows from that file.
newtype Opts = Opts (Maybe FilePath)

optsParser :: Parser Opts
optsParser =
    Opts
        <$> optional
            ( strOption
                (  long  "config"
                <> short 'c'
                <> metavar "PATH"
                <> help "Override agent-ops.json location (else \
                        \KRIKIT_AGENT_OPS_CONFIG, else default)"
                )
            )

main :: IO ()
main = do
    Opts override <- execParser $
        info (optsParser <**> helper)
            (  fullDesc
            <> progDesc "Regenerate context/system-state-mini.generated.md"
            <> header   "krikit-regen-system-state-mini"
            )

    cfgE <- loadConfig override
    cfg  <- case cfgE of
        Right c  -> pure c
        Left err -> do
            hPutStrLn stderr ("FAIL: " <> T.unpack err)
            exitFailure

    result <- runEff . runReader cfg $ regenerate
    case result of
        Left err           -> do
            hPutStrLn stderr ("FAIL: " <> T.unpack err)
            exitFailure
        Right (path, outcome) -> do
            TIO.putStrLn (renderOutcome path outcome)
            case outcome of
                WriteError _ -> exitFailure
                _            -> pure ()
