{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Pure transition + digest assembly for @krikit-monitor@.
--
-- Three responsibilities, all pure:
--
--   1. 'transitionAlerts' -- given previous and current statuses,
--      compute the per-check alert strings to emit. Only emits on
--      transitions: a check that was @crit@ yesterday and is
--      @crit@ today produces no new alert.
--   2. 'shouldEmitDigest' -- is now the moment to fire the daily
--      digest? (one per day, gated by configured hour and last
--      digest date stored in state).
--   3. 'buildDigest' -- assemble the human-readable digest body
--      from the live check results plus optional adjunct sections
--      (regen-summary, update-status -- shelled out by the
--      orchestrator and passed in here as 'Text' blobs).
module Krikit.Agent.Ops.Monitor.Digest
    ( -- * Transitions
      Alert (..)
    , transitionAlerts
    , renderAlert

      -- * Daily digest
    , DigestSchedule (..)
    , defaultDigestSchedule
    , shouldEmitDigest
    , buildDigest
    ) where

import qualified Data.Map.Strict                    as Map
import           Data.Map.Strict                    (Map)
import           Data.Text                          (Text)
import qualified Data.Text                          as T

import           Krikit.Agent.Ops.Monitor.Check     (CheckResult (..),
                                                     CheckStatus (..))
import           Krikit.Agent.Ops.Monitor.State     (MonitorState (..))

-- =============================================================================
-- Transitions
-- =============================================================================

-- | One alert worth sending: a check transitioned to a new
-- status (or is newly known).
data Alert = Alert
    { alertCheck   :: !Text
    , alertStatus  :: !CheckStatus     -- ^ the new (post-transition) status
    , alertMessage :: !Text
    }
    deriving stock (Eq, Show)

-- | Compute the alert list for a run. Emit only for checks whose
-- status changed since the previous run. Order: input order
-- preserved (stable for tests / log greppability).
--
-- Default for unseen names: 'Ok'. Matches @monitor.py@'s
-- @prev_alerts.get(name, "ok")@.
transitionAlerts
    :: Map Text CheckStatus  -- ^ previous-run statuses (from state)
    -> [CheckResult]         -- ^ current-run results
    -> [Alert]
transitionAlerts prev = go
  where
    go :: [CheckResult] -> [Alert]
    go []       = []
    go (r : rs) =
        let prevS = Map.findWithDefault Ok (crName r) prev
        in  if crStatus r /= prevS
                then mkAlert r : go rs
                else go rs

    mkAlert r = Alert
        { alertCheck   = crName r
        , alertStatus  = crStatus r
        , alertMessage = crMessage r
        }

-- | One-line alert text. Matches @monitor.py@'s shape:
-- @\"[OK] foo recovered: msg\"@ or @\"[CRIT] foo: msg\"@.
renderAlert :: Alert -> Text
renderAlert a = case alertStatus a of
    Ok   -> "[OK] "   <> alertCheck a <> " recovered: " <> alertMessage a
    Warn -> "[WARN] " <> alertCheck a <> ": " <> alertMessage a
    Crit -> "[CRIT] " <> alertCheck a <> ": " <> alertMessage a

-- =============================================================================
-- Daily digest
-- =============================================================================

-- | When should the daily digest fire?
data DigestSchedule = DigestSchedule
    { dsHourLocal :: !Int
      -- ^ local-time hour (0..23) at which the digest should
      --   fire.
    }
    deriving stock (Eq, Show)

defaultDigestSchedule :: DigestSchedule
defaultDigestSchedule = DigestSchedule { dsHourLocal = 7 }

-- | Should @main@ emit the digest right now? Gated by:
--
--   * The current hour matches 'dsHourLocal'.
--   * No digest has been emitted today yet
--     (i.e. 'msLastDigestDate' /= today).
--
-- Caller supplies @nowHourLocal@ and @todayIso@ rather than us
-- doing IO here, so the function is pure / testable.
shouldEmitDigest
    :: DigestSchedule
    -> Int           -- ^ current local hour, 0..23
    -> Text          -- ^ today's date in @YYYY-MM-DD@
    -> MonitorState
    -> Bool
shouldEmitDigest sched nowHour todayIso ms =
    nowHour == dsHourLocal sched
    && msLastDigestDate ms /= todayIso

-- | Render the daily digest body. Includes:
--
--   * A dated header line.
--   * Per-check status rows (renamed for at-a-glance scanning).
--   * Optional adjunct sections passed in as raw 'Text' --
--     intended for the regen-summary and update-status outputs
--     produced by other binaries.
--
-- Pure for testability; the orchestrator shells out to the
-- adjunct binaries and feeds their stdout in.
buildDigest
    :: Text                -- ^ today's date (@YYYY-MM-DD@)
    -> Text                -- ^ host name (e.g. @krikit-agent-001@)
    -> [CheckResult]
    -> [(Text, Text)]
      -- ^ adjunct sections: @[(\"Regen\", \"...two-line...\"), ...]@.
      --   Empty values are dropped.
    -> Text
buildDigest todayIso host results adjuncts =
    T.intercalate "\n" $
        [header]
        ++ checkLines results
        ++ concatMap section nonEmpty
  where
    header = "[DIGEST " <> todayIso <> "] " <> host <> " health"

    checkLines rs = ["  " <> tagFor (crStatus r)
                          <> "  " <> crName r <> ": " <> crMessage r
                    | r <- rs]

    tagFor = \case
        Ok   -> "  ok"
        Warn -> "warn"
        Crit -> "CRIT"

    nonEmpty = [ p | p@(_, v) <- adjuncts, not (T.null (T.strip v)) ]

    section (label, body) =
        "" : (label <> ":") : map ("  " <>) (T.lines (T.strip body))
