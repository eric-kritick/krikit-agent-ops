{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- Orphan ToJSON instances for 'Counts' and 'TierResult' live here
-- rather than in 'Krikit.Agent.Ops.Smoke.Tier'. Rationale: Tier.hs
-- intentionally has zero dependencies on aeson or IO concerns;
-- serialization is a "formatting" concern that belongs with the
-- formatters. Acceptable because the whole package is our code, and
-- no other module defines ToJSON for these types.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Output formatting for smoke-test results.
--
-- Two responsibilities:
--
-- * Pretty-print a summary block to stdout after all tiers have run.
-- * Append a one-line JSON Lines history record to disk so
--   @/var/log/krikit/smoke.log@ is greppable / parseable over time.
--
-- Both are pure-ish: the summary formatter returns 'Text', the history
-- writer returns a 'ByteString' that the caller appends to a file.
module Krikit.Agent.Ops.Smoke.Report
    ( -- * Summary text
      renderSummary

      -- * History (JSON Lines)
    , historyLine
    , Summary (..)
    , buildSummary
    ) where

import           Data.Aeson                   (ToJSON, (.=))
import qualified Data.Aeson                   as Aeson
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy.Char8   as LBSC
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time                    (UTCTime, defaultTimeLocale, formatTime)

import           Krikit.Agent.Ops.Smoke.Tier
    ( Counts (..)
    , TierResult (..)
    , TierStatus (..)
    , allPassed
    , countResults
    , tierKey
    , tierName
    )

-- | Rendered, human-readable summary block for stdout.
renderSummary :: [TierResult] -> Text
renderSummary results =
    T.intercalate "\n" $
        [ ""
        , "== Summary =="
        , "  duration: " <> T.pack (show (sum (map trElapsedMs results))) <> " ms"
        , "  results:"
        ]
        ++ map resultLine results
        ++ [ verdict results ]

resultLine :: TierResult -> Text
resultLine r =
    "    "
        <> statusTag (trStatus r)
        <> " "
        <> tierName (trTier r)
        <> "  (" <> T.pack (show (trElapsedMs r)) <> " ms)"
        <> reasonSuffix (trStatus r)

statusTag :: TierStatus -> Text
statusTag = \case
    Pass   -> "OK:  "
    Fail _ -> "FAIL:"
    Skip _ -> "SKIP:"

reasonSuffix :: TierStatus -> Text
reasonSuffix = \case
    Pass       -> ""
    Fail msg   -> " — " <> msg
    Skip msg   -> " — " <> msg

verdict :: [TierResult] -> Text
verdict results =
    if allPassed results
        then "\nAll tiers passed."
        else "\nFAILED: " <> T.pack (show (cFail (countResults results))) <> " tier(s)."

-- === History (JSON Lines) ===================================================

-- | Aggregate record serialized to the history log, one per run.
data Summary = Summary
    { sWhen       :: !UTCTime
    , sDurationMs :: !Int
    , sResults    :: ![TierResult]
    , sCounts     :: !Counts
    }
    deriving stock (Eq, Show)

buildSummary :: UTCTime -> [TierResult] -> Summary
buildSummary t rs =
    Summary
        { sWhen       = t
        , sDurationMs = sum (map trElapsedMs rs)
        , sResults    = rs
        , sCounts     = countResults rs
        }

-- | One JSON line terminated by a newline, suitable for append-only
-- history logging.
historyLine :: Summary -> LBS.ByteString
historyLine s = Aeson.encode s <> LBSC.pack "\n"

instance ToJSON Summary where
    toJSON s =
        Aeson.object
            [ "when"     .= formatISO8601 (sWhen s)
            , "duration_ms" .= sDurationMs s
            , "counts"   .= sCounts s
            , "results"  .= sResults s
            ]

instance ToJSON Counts where
    toJSON c =
        Aeson.object
            [ "pass" .= cPass c
            , "fail" .= cFail c
            , "skip" .= cSkip c
            ]

instance ToJSON TierResult where
    toJSON r =
        Aeson.object
            [ "tier"    .= tierKey (trTier r)
            , "status"  .= statusJson (trStatus r)
            , "reason"  .= statusReason (trStatus r)
            , "elapsed_ms" .= trElapsedMs r
            , "details" .= trDetails r
            ]

statusJson :: TierStatus -> Text
statusJson = \case
    Pass   -> "pass"
    Fail _ -> "fail"
    Skip _ -> "skip"

statusReason :: TierStatus -> Maybe Text
statusReason = \case
    Pass     -> Nothing
    Fail msg -> Just msg
    Skip msg -> Just msg

formatISO8601 :: UTCTime -> String
formatISO8601 = formatTime defaultTimeLocale "%FT%T%QZ"
