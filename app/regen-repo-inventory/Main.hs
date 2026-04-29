{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @krikit-regen-repo-inventory@: regenerate the categorized
-- agent-facing repo inventory at
-- @krikit-agent-fabric\/context\/repo-inventory.generated.md@.
--
-- Exit codes: 0 success / 2 hard write failure / 3 input failure.
-- Wrapped in 'runWithAutoDisable' (three-strikes auto-disable).
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
import           Krikit.Agent.Ops.RepoInventory.Run       (regenerate)

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
            <> progDesc "Regenerate context/repo-inventory.generated.md"
            <> header   "krikit-regen-repo-inventory"
            )
    runWithAutoDisable "regen-repo-inventory" defaultStateDir
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
