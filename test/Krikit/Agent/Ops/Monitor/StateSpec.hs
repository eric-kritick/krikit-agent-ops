{-# LANGUAGE OverloadedStrings #-}

-- | Tests for monitor state load/save round-trip + pure update.
module Krikit.Agent.Ops.Monitor.StateSpec (spec) where

import qualified Data.Map.Strict                  as Map
import           System.Directory                 (createDirectoryIfMissing,
                                                   getTemporaryDirectory,
                                                   removePathForcibly)
import           System.FilePath                  ((</>))
import           Test.Hspec

import           Krikit.Agent.Ops.Monitor.Check   (CheckResult (..),
                                                   CheckStatus (..))
import           Krikit.Agent.Ops.Monitor.State

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "krikit-monitor-state-spec"
    removePathForcibly dir
    createDirectoryIfMissing True dir
    a <- action dir
    removePathForcibly dir
    pure a

spec :: Spec
spec = do
    describe "loadStateFrom on missing file" $ do
        it "returns the empty state" $ do
            withTempDir $ \dir -> do
                ms <- loadStateFrom (dir </> "nope.json")
                ms `shouldBe` emptyState

    describe "saveStateTo + loadStateFrom round-trip" $ do
        it "preserves alerts, last_run, last_digest_date" $ do
            withTempDir $ \dir -> do
                let path = dir </> "monitor-state.json"
                    initial = MonitorState
                        { msAlerts         = Map.fromList
                            [ ("openclaw", Crit)
                            , ("ollama",   Ok)
                            , ("disk",     Warn)
                            ]
                        , msLastRun        = "2026-04-30T07:00:00Z"
                        , msLastDigestDate = "2026-04-29"
                        }
                saveStateTo path initial
                roundtrip <- loadStateFrom path
                roundtrip `shouldBe` initial

    describe "loadStateFrom on garbage" $ do
        it "returns the empty state (does not crash)" $ do
            withTempDir $ \dir -> do
                let path = dir </> "garbage.json"
                writeFile path "this is not json {{{"
                ms <- loadStateFrom path
                ms `shouldBe` emptyState

    describe "recordResults" $ do
        it "replaces the alert map with the latest run's statuses" $ do
            let prev = MonitorState
                    { msAlerts = Map.fromList [("openclaw", Crit), ("retired", Warn)]
                    , msLastRun = ""
                    , msLastDigestDate = ""
                    }
                results =
                    [ CheckResult "openclaw" Ok   "127.0.0.1:18789 reachable"
                    , CheckResult "ollama"   Warn "rate-limited"
                    ]
                next = recordResults "2026-04-30T07:00:00Z" results prev
            msAlerts next        `shouldBe` Map.fromList
                [ ("openclaw", Ok), ("ollama", Warn) ]
            msLastRun next       `shouldBe` "2026-04-30T07:00:00Z"
            msLastDigestDate next `shouldBe` ""  -- preserved

    describe "markDigestEmitted" $ do
        it "stamps last_digest_date and leaves the rest" $ do
            let s0 = emptyState
                    { msAlerts = Map.fromList [("a", Ok)]
                    , msLastRun = "x"
                    }
                s1 = markDigestEmitted "2026-04-30" s0
            msLastDigestDate s1 `shouldBe` "2026-04-30"
            msAlerts s1         `shouldBe` Map.fromList [("a", Ok)]
            msLastRun s1        `shouldBe` "x"
