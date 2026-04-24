{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

-- | Structured log output effect.
--
-- Business logic calls 'logInfo', 'logOk', 'logFail', 'logSkip' — the
-- effect stack decides whether to print to stdout, collect for tests, or
-- drop to /dev/null.
module Krikit.Agent.Ops.Effect.Log
    ( -- * Effect
      Log
    , LogLevel (..)

      -- * Smart constructors
    , logInfo
    , logOk
    , logFail
    , logSkip

      -- * Handlers
    , runLogStdout
    , runLogSilent
    ) where

import           Data.Text                   (Text)
import qualified Data.Text.IO                as TIO

import           Effectful                   (Eff, Effect, IOE, liftIO, (:>))
import           Effectful.Dispatch.Dynamic  (interpret)
import           Effectful.TH                (makeEffect)

-- | Severity level for a log line.
data LogLevel = Info | Ok | Fail | Skip
    deriving stock (Eq, Show)

data Log :: Effect where
    LogLine :: LogLevel -> Text -> Log m ()

makeEffect ''Log

-- Smart constructors per level so business logic doesn't need to pass
-- an explicit 'LogLevel' argument.
logInfo, logOk, logFail, logSkip :: Log :> es => Text -> Eff es ()
logInfo = logLine Info
logOk   = logLine Ok
logFail = logLine Fail
logSkip = logLine Skip

-- | Real handler: write each line to stdout, formatted with a level tag.
runLogStdout :: IOE :> es => Eff (Log : es) a -> Eff es a
runLogStdout = interpret $ \_ -> \case
    LogLine lvl msg -> liftIO $ TIO.putStrLn (formatLine lvl msg)

-- | Discard handler for tests that don't care about log output.
runLogSilent :: Eff (Log : es) a -> Eff es a
runLogSilent = interpret $ \_ -> \case
    LogLine _ _ -> pure ()

formatLine :: LogLevel -> Text -> Text
formatLine lvl msg = case lvl of
    Info -> "    " <> msg
    Ok   -> "  OK:   " <> msg
    Fail -> "  FAIL: " <> msg
    Skip -> "  SKIP: " <> msg
