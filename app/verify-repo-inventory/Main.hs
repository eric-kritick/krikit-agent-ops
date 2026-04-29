{-# LANGUAGE OverloadedStrings #-}

-- | @krikit-verify-repo-inventory@: cross-check the canonical
-- kritick-* root inventory against what's cloned on disk.
--
-- Inputs come from the same @agent-ops.json@ the regenerators
-- read; override location with @--config@ or
-- @KRIKIT_AGENT_OPS_CONFIG@.
--
-- Exit codes (per 'Krikit.Agent.Ops.Verify.Common.exitForFindings'):
--
--   * 0  — clean (zero findings or info only)
--   * 1  — warnings only (action when convenient)
--   * 2  — at least one error (action required)
--   * 3  — input failure (could not read config or canonical inputs)
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
import           System.Exit                              (ExitCode (..),
                                                           exitWith)
import           System.IO                                (hPutStrLn, stderr)

import           Effectful                                (runEff)
import           Effectful.Reader.Static                  (runReader)

import           Krikit.Agent.Ops.Config                  (loadConfig)
import           Krikit.Agent.Ops.Verify.Common
    ( exitForFindings
    , renderFindings
    )
import           Krikit.Agent.Ops.Verify.RepoInventory    (verify)

-- | Single optional override for the agent-ops.json location.
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
            <> progDesc "Cross-check kritick-* roots against on-disk clones"
            <> header   "krikit-verify-repo-inventory"
            )

    cfgE <- loadConfig override
    cfg  <- case cfgE of
        Right c  -> pure c
        Left err -> do
            hPutStrLn stderr ("FAIL: " <> T.unpack err)
            exitWith (ExitFailure 3)

    result <- runEff . runReader cfg $ verify
    case result of
        Left err -> do
            hPutStrLn stderr ("FAIL: " <> T.unpack err)
            exitWith (ExitFailure 3)
        Right findings -> do
            TIO.putStr (renderFindings "krikit-verify-repo-inventory" findings)
            exitWith (exitForFindings findings)
