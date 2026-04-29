{-# LANGUAGE OverloadedStrings #-}

-- | @krikit-verify-reading-order@: every backtick-cited file
-- path in @AGENTS.md@ / @IDENTITY.md@ under
-- @krikit-agent-fabric\/agents\/@ must exist.
--
-- Inputs come from the same @agent-ops.json@ the regenerators
-- read; override location with @--config@ or
-- @KRIKIT_AGENT_OPS_CONFIG@.
--
-- Exit codes (per 'Krikit.Agent.Ops.Verify.Common.exitForFindings'):
--
--   * 0  — clean
--   * 1  — warnings only
--   * 2  — at least one error (broken citation)
--   * 3  — input failure (missing config or unreadable agents_dir)
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
import           Krikit.Agent.Ops.Verify.ReadingOrder     (verify)

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
            <> progDesc "Verify every backtick-cited path in AGENTS.md / IDENTITY.md exists"
            <> header   "krikit-verify-reading-order"
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
            TIO.putStr (renderFindings "krikit-verify-reading-order" findings)
            exitWith (exitForFindings findings)
