{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @krikit-regen-system-state-mini@: regenerate the human-readable
-- mirror of @kritick-ecosystem\/config\/infrastructure-macmini.json@
-- as @krikit-agent-fabric\/context\/system-state-mini.generated.md@.
--
-- Idempotent: substantive content unchanged → file untouched (the
-- "Last generated" timestamp does not by itself trigger a rewrite).
--
-- Exit codes:
--
--   * 0 — success (UNCHANGED, WRITTEN, or CREATED)
--   * 2 — output write failed (hard error; counts toward auto-disable)
--   * 3 — input failure (config or canonical inputs missing/unparseable)
--
-- Wrapped in 'runWithAutoDisable': three consecutive non-zero
-- exits (>= 2) leave a marker file under
-- @\/var\/lib\/krikit\/regen\/@ that future runs short-circuit on
-- until manually cleared.
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
import           System.Exit                              (ExitCode (..))
import           System.IO                                (hPutStrLn, stderr)

import           Effectful                                (runEff)
import           Effectful.Reader.Static                  (runReader)

import           Krikit.Agent.Ops.Config                  (loadConfig)
import           Krikit.Agent.Ops.Regen.AutoDisable
    ( defaultStateDir
    , runWithAutoDisable
    )
import           Krikit.Agent.Ops.Regen.Write
    ( WriteOutcome (..)
    , renderOutcome
    )
import           Krikit.Agent.Ops.SystemStateMini.Run     (regenerate)

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
    runWithAutoDisable "regen-system-state-mini" defaultStateDir
                       (run override)

run :: Maybe FilePath -> IO ExitCode
run override = do
    cfgE <- loadConfig override
    case cfgE of
        Left err -> do
            hPutStrLn stderr ("FAIL: " <> T.unpack err)
            pure (ExitFailure 3)
        Right cfg -> do
            result <- runEff . runReader cfg $ regenerate
            case result of
                Left err -> do
                    hPutStrLn stderr ("FAIL: " <> T.unpack err)
                    pure (ExitFailure 3)
                Right (path, outcome) -> do
                    TIO.putStrLn (renderOutcome path outcome)
                    case outcome of
                        WriteError _ -> pure (ExitFailure 2)
                        _            -> pure ExitSuccess
