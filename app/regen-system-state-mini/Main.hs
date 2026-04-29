{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | @krikit-regen-system-state-mini@: regenerate the human-readable
-- mirror of @kritick-ecosystem\/config\/infrastructure-macmini.json@
-- as @krikit-agent-fabric\/context\/system-state-mini.generated.md@.
--
-- Idempotent: substantive content unchanged → file untouched (the
-- "Last generated" timestamp does not by itself trigger a rewrite).
-- Failures (input parse, output write) exit non-zero with a single
-- @FAIL:@ line on stderr.
module Main (main) where

import qualified Data.Text                                as T
import qualified Data.Text.IO                             as TIO
import qualified Data.Time                                as Time
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
import           System.Exit                              (exitFailure)
import           System.IO                                (hPutStrLn, stderr)

import           Krikit.Agent.Ops.Regen.Write
    ( WriteOutcome (..)
    , renderOutcome
    , writeIfChanged
    )
import           Krikit.Agent.Ops.SystemStateMini.Run
    ( readConfig
    , renderReport
    )

data Opts = Opts
    { optInputJson  :: !FilePath
    , optOutputMd   :: !FilePath
    }

optsParser :: Parser Opts
optsParser =
    Opts
        <$> strOption
            (  long  "input"
            <> short 'i'
            <> metavar "PATH"
            <> value defaultInput
            <> help "Path to canonical infrastructure-macmini.json"
            )
        <*> strOption
            (  long  "output"
            <> short 'o'
            <> metavar "PATH"
            <> value defaultOutput
            <> help "Path to system-state-mini.generated.md"
            )
  where
    defaultInput  = "/Users/opsadmin/Development/kritick-ecosystem/config/infrastructure-macmini.json"
    defaultOutput = "/Users/opsadmin/Development/krikit-agent-fabric/context/system-state-mini.generated.md"

main :: IO ()
main = do
    Opts{..} <- execParser $
        info (optsParser <**> helper)
            (  fullDesc
            <> progDesc "Regenerate context/system-state-mini.generated.md"
            <> header   "krikit-regen-system-state-mini"
            )
    today <- isoDateUTC
    cfgE  <- readConfig optInputJson
    case cfgE of
        Left err -> do
            hPutStrLn stderr ("FAIL: " <> T.unpack err)
            exitFailure
        Right cfg -> do
            let body = renderReport today cfg
            outcome <- writeIfChanged optOutputMd body
            TIO.putStrLn (renderOutcome optOutputMd outcome)
            case outcome of
                WriteError _ -> exitFailure
                _            -> pure ()

isoDateUTC :: IO T.Text
isoDateUTC = do
    -- @showGregorian day@ produces e.g. @"2026-04-29"@.
    now <- Time.getCurrentTime
    pure (T.pack (Time.showGregorian (Time.utctDay now)))
