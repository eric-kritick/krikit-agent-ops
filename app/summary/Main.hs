{-# LANGUAGE OverloadedStrings #-}

-- | krikit-summary: agent + model usage roll-up from the OpenClaw
-- detailed log. Replaces the bash version.
module Main (main) where

import qualified Data.Text.IO                 as TIO
import           Options.Applicative
    ( Parser
    , execParser
    , fullDesc
    , header
    , helper
    , info
    , metavar
    , option
    , progDesc
    , showDefault
    , value
    , (<**>)
    )
import qualified Options.Applicative          as OA

import           Effectful                    (runEff)

import           Krikit.Agent.Ops.Effect.Proc (runProcIO)
import           Krikit.Agent.Ops.Summary.Run
    ( SummaryConfig (..)
    , defaultSummaryConfig
    , renderReport
    , runSummary
    )

newtype Opts = Opts Int  -- line-count argument

optsParser :: Parser Opts
optsParser =
    Opts <$> option OA.auto
        (  OA.long "lines"
        <> OA.short 'n'
        <> metavar "N"
        <> OA.help "How many lines of the log to tail"
        <> value (scLineCount defaultSummaryConfig)
        <> showDefault
        )

main :: IO ()
main = do
    Opts n <- execParser $
        info (optsParser <**> helper)
            (  fullDesc
            <> progDesc "Roll up agent + model usage from today's OpenClaw log."
            <> header   "krikit-summary"
            )
    let cfg = defaultSummaryConfig { scLineCount = n }
    report <- runEff . runProcIO $ runSummary cfg
    TIO.putStrLn (renderReport report)
