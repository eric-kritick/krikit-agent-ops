{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the digest's pure transition + assembly logic.
module Krikit.Agent.Ops.Monitor.DigestSpec (spec) where

import qualified Data.Map.Strict                    as Map
import qualified Data.Text                          as T
import           Test.Hspec

import           Krikit.Agent.Ops.Monitor.Check     (CheckResult (..),
                                                     CheckStatus (..))
import           Krikit.Agent.Ops.Monitor.Digest
import           Krikit.Agent.Ops.Monitor.State     (MonitorState (..),
                                                     emptyState)

spec :: Spec
spec = do
    describe "transitionAlerts" $ do
        it "emits nothing when nothing changed" $ do
            let prev = Map.fromList [("openclaw", Ok), ("ollama", Ok)]
                results =
                    [ CheckResult "openclaw" Ok "ok"
                    , CheckResult "ollama"   Ok "ok"
                    ]
            transitionAlerts prev results `shouldBe` []

        it "emits an alert when a check goes Ok -> Crit" $ do
            let prev = Map.fromList [("openclaw", Ok)]
                results = [CheckResult "openclaw" Crit "127.0.0.1:18789 not reachable"]
                got = transitionAlerts prev results
            length got           `shouldBe` 1
            map alertStatus got  `shouldBe` [Crit]
            map alertCheck got   `shouldBe` ["openclaw"]

        it "emits a recovery alert when a check goes Crit -> Ok" $ do
            let prev = Map.fromList [("openclaw", Crit)]
                results = [CheckResult "openclaw" Ok "127.0.0.1:18789 reachable"]
                got = transitionAlerts prev results
            map alertStatus got `shouldBe` [Ok]

        it "treats unseen names as Ok (matches monitor.py default)" $ do
            -- A brand-new check: no prior status => default Ok.
            -- If current is Ok, no alert. If current is Crit, alert.
            transitionAlerts Map.empty
                [CheckResult "newcheck" Ok "fine"] `shouldBe` []
            length (transitionAlerts Map.empty
                [CheckResult "newcheck" Crit "broken"]) `shouldBe` 1

    describe "renderAlert" $ do
        it "Crit format" $
            renderAlert (Alert "openclaw" Crit "down")
                `shouldBe` "[CRIT] openclaw: down"

        it "Warn format" $
            renderAlert (Alert "disk" Warn "85%")
                `shouldBe` "[WARN] disk: 85%"

        it "Ok format reads as a recovery" $
            renderAlert (Alert "openclaw" Ok "back up")
                `shouldBe` "[OK] openclaw recovered: back up"

    describe "shouldEmitDigest" $ do
        let sched = DigestSchedule { dsHourLocal = 7 }

        it "fires when the hour matches and we haven't fired today" $
            shouldEmitDigest sched 7 "2026-04-30" emptyState `shouldBe` True

        it "skips outside the configured hour" $
            shouldEmitDigest sched 6 "2026-04-30" emptyState `shouldBe` False

        it "skips a second invocation in the same hour the same day" $ do
            let s = emptyState { msLastDigestDate = "2026-04-30" }
            shouldEmitDigest sched 7 "2026-04-30" s `shouldBe` False

        it "fires the next day" $ do
            let s = emptyState { msLastDigestDate = "2026-04-29" }
            shouldEmitDigest sched 7 "2026-04-30" s `shouldBe` True

    describe "buildDigest" $ do
        it "emits a header + one line per check" $ do
            let txt = buildDigest "2026-04-30" "krikit-agent-001"
                        [ CheckResult "openclaw" Ok   "reachable"
                        , CheckResult "disk"     Warn "85%"
                        ]
                        []
            T.lines txt `shouldBe`
                [ "[DIGEST 2026-04-30] krikit-agent-001 health"
                , "    ok  openclaw: reachable"
                , "  warn  disk: 85%"
                ]

        it "appends adjunct sections separated by a blank line" $ do
            let txt = buildDigest "2026-04-30" "host"
                        [ CheckResult "openclaw" Ok "reachable" ]
                        [ ("Regen", "Regen: 4/4 ok\nVerify: 3/3 clean")
                        , ("Updates", "12 pending")
                        ]
            txt `shouldSatisfy` T.isInfixOf "Regen:"
            txt `shouldSatisfy` T.isInfixOf "Verify: 3/3 clean"
            txt `shouldSatisfy` T.isInfixOf "Updates:"
            txt `shouldSatisfy` T.isInfixOf "12 pending"

        it "drops adjunct sections whose body is empty / whitespace" $ do
            let txt = buildDigest "2026-04-30" "host"
                        [ CheckResult "x" Ok "ok" ]
                        [ ("Regen",   "")
                        , ("Updates", "   \n  ")
                        , ("Real",    "data here")
                        ]
            txt `shouldNotSatisfy` T.isInfixOf "Regen:"
            txt `shouldNotSatisfy` T.isInfixOf "Updates:"
            txt `shouldSatisfy`    T.isInfixOf "Real:"

    describe "decideDigest" $ do
        let sched = DigestSchedule { dsHourLocal = 7 }
            today = "2026-04-30"

        -- ---- NormalSchedule path ----------------------------------

        it "NormalSchedule: fires + marks emitted at the gate hour" $
            decideDigest NormalSchedule sched 7 today emptyState
                `shouldBe` DigestFire ScheduledFire

        it "NormalSchedule: skips when hour doesn't match" $
            decideDigest NormalSchedule sched 6 today emptyState
                `shouldBe` DigestSkip

        it "NormalSchedule: skips when already fired today" $ do
            let s = emptyState { msLastDigestDate = today }
            decideDigest NormalSchedule sched 7 today s
                `shouldBe` DigestSkip

        -- ---- ForceDigest path -------------------------------------

        it "ForceDigest: fires (as ForcedFire) outside the gate hour" $
            decideDigest ForceDigest sched 6 today emptyState
                `shouldBe` DigestFire ForcedFire

        it "ForceDigest: fires (as ForcedFire) even if already fired today" $ do
            let s = emptyState { msLastDigestDate = today }
            decideDigest ForceDigest sched 23 today s
                `shouldBe` DigestFire ForcedFire

        it "ForceDigest: still ForcedFire when the gate would have fired anyway" $
            -- Pathological case: operator runs --force-digest at 07:00
            -- on a day not yet emitted. We pick ForcedFire over
            -- ScheduledFire so the post-fire bookkeeping doesn't
            -- inadvertently mark the day emitted on a forced run.
            decideDigest ForceDigest sched 7 today emptyState
                `shouldBe` DigestFire ForcedFire
