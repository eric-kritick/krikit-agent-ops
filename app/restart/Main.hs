{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | krikit-restart: clean restart of a single LaunchDaemon by short
-- name (@krikit-restart openclaw@, etc.). Replaces the bash version
-- of the same name; the operator-facing CLI shape is identical.
module Main (main) where

import qualified Data.Text                    as T
import qualified Data.Text.IO                 as TIO
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
import           System.Exit                  (exitFailure, exitSuccess)

import           Effectful                    (runEff)

import           Krikit.Agent.Ops.Effect.Log  (runLogStdout)
import           Krikit.Agent.Ops.Effect.Proc (runProcIO)
import           Krikit.Agent.Ops.Restart.Run
    ( RestartOutcome (..)
    , restartService
    )
import           Krikit.Agent.Ops.Service
    ( Service
    , allServices
    , serviceFromShortName
    , serviceShortName
    )

newtype Opts = Opts Service

optsParser :: Parser Opts
optsParser =
    Opts <$> argument readService
        ( metavar "SERVICE"
        <> help "short service name (e.g. openclaw, ollama, colima)"
        )

readService :: ReadM Service
readService = do
    raw <- str
    case serviceFromShortName (T.pack raw) of
        Just svc -> pure svc
        Nothing  -> readerError $
            "unknown service: " <> raw <> "\n  valid: "
                <> T.unpack (T.intercalate ", " (map serviceShortName allServices))

main :: IO ()
main = do
    Opts svc <- execParser $
        info (optsParser <**> helper)
            (  fullDesc
            <> progDesc "Stop, clean up, and re-bootstrap a krikit LaunchDaemon"
            <> header   "krikit-restart - typed service-restart wrapper"
            )

    outcome <- runEff
                . runLogStdout
                . runProcIO
                $ restartService svc

    case outcome of
        RestartOk           -> exitSuccess
        RestartFailed reason -> do
            TIO.putStrLn ("FAILED: " <> reason)
            exitFailure
