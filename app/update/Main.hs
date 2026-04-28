{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | krikit-update: unified updater. Subsumes the four bash
-- @krikit-update-*@ scripts. Usage:
--
-- @
-- krikit-update claude
-- krikit-update codex
-- krikit-update openclaw
-- krikit-update ollama
-- krikit-update all
-- @
module Main (main) where

import qualified Data.Text                              as T
import qualified Data.Text.IO                           as TIO
import           Options.Applicative
    ( Parser
    , ReadM
    , argument
    , execParser
    , fullDesc
    , header
    , help
    , helper
    , info
    , metavar
    , progDesc
    , readerError
    , str
    , (<**>)
    )
import           System.Exit                            (exitFailure, exitSuccess)

import           Effectful                              (runEff)

import           Krikit.Agent.Ops.Effect.Log            (runLogStdout)
import           Krikit.Agent.Ops.Effect.Proc           (runProcIO)
import           Krikit.Agent.Ops.Update.Run
    ( UpdateOutcome (..)
    , UpdateStatus (..)
    , UpdateTarget
    , allTargets
    , renderOutcome
    , renderOutcomes
    , targetFromShortName
    , targetShortName
    , updateAll
    , updateTarget
    )

-- | Parsed CLI selection: either one specific tool, or "all".
data Selection
    = SelectOne !UpdateTarget
    | SelectAll

newtype Opts = Opts Selection

optsParser :: Parser Opts
optsParser =
    Opts <$> argument readSelection
        ( metavar "TARGET"
        <> help "claude | codex | openclaw | ollama | all"
        )

readSelection :: ReadM Selection
readSelection = do
    raw <- str
    case raw of
        "all" -> pure SelectAll
        _ -> case targetFromShortName (T.pack raw) of
            Just t  -> pure (SelectOne t)
            Nothing -> readerError $
                "unknown target: " <> raw <> "\n  valid: "
                    <> T.unpack (T.intercalate ", "
                          (map targetShortName allTargets <> ["all"]))

main :: IO ()
main = do
    Opts sel <- execParser $
        info (optsParser <**> helper)
            (  fullDesc
            <> progDesc "Update agent-stack CLIs and restart any \
                        \dependent LaunchDaemon."
            <> header   "krikit-update - typed agent-stack updater"
            )

    case sel of
        SelectOne t -> do
            outcome <- runEff
                         . runLogStdout
                         . runProcIO
                         $ updateTarget t
            TIO.putStrLn ""
            TIO.putStrLn (renderOutcome outcome)
            if isFailure outcome then exitFailure else exitSuccess

        SelectAll -> do
            outcomes <- runEff
                          . runLogStdout
                          . runProcIO
                          $ updateAll
            TIO.putStrLn (renderOutcomes outcomes)
            if any isFailure outcomes then exitFailure else exitSuccess
  where
    isFailure o = case uoStatus o of
        UpdateUnchanged          -> False
        UpdateChanged            -> False
        UpdateRestartFailed _    -> True
        UpdateFailed _           -> True
