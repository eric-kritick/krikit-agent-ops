{-# LANGUAGE OverloadedStrings #-}

-- | @krikit-regen-summary@: emit a two-line summary of last
-- night's regen + verify chain status, suitable for inclusion
-- in the daily Telegram digest (PB 10 monitor.py).
module Main (main) where

import qualified Data.Text.IO                         as TIO
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

import           Krikit.Agent.Ops.RegenSummary.Run    (defaultLogDir, summarize)

newtype Opts = Opts FilePath

optsParser :: Parser Opts
optsParser =
    Opts
        <$> strOption
            (  long  "log-dir"
            <> short 'l'
            <> metavar "DIR"
            <> value defaultLogDir
            <> help "Override the default /var/log/krikit/regen/ \
                    \(useful for ad-hoc testing)"
            )

main :: IO ()
main = do
    Opts logDir <- execParser $
        info (optsParser <**> helper)
            (  fullDesc
            <> progDesc "Emit a two-line regen+verify status \
                        \summary for the daily digest"
            <> header   "krikit-regen-summary"
            )
    TIO.putStr =<< summarize logDir
    TIO.putStr "\n"
