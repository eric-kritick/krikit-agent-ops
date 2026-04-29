{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the three-strikes auto-disable mechanism.
--
-- We exercise the pure helpers ('isStrike', 'advanceCounter')
-- exhaustively across the exit-code spectrum, then round-trip
-- 'readJobStatus' through real on-disk marker files in a temp
-- directory. The full 'runWithAutoDisable' wrapper terminates
-- the process via 'exitWith', so it's exercised in the binaries'
-- build/dry-run rather than as an inline hspec case.
module Krikit.Agent.Ops.Regen.AutoDisableSpec (spec) where

import qualified Data.Text.IO                            as TIO
import           System.Directory                        (createDirectoryIfMissing,
                                                          getTemporaryDirectory,
                                                          removePathForcibly)
import           System.Exit                             (ExitCode (..))
import           System.FilePath                         ((<.>), (</>))
import           Test.Hspec

import           Krikit.Agent.Ops.Regen.AutoDisable
    ( JobStatus (..)
    , advanceCounter
    , disableThreshold
    , isStrike
    , readJobStatus
    )

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "krikit-agent-ops-auto-disable-spec"
    removePathForcibly dir
    createDirectoryIfMissing True dir
    a <- action dir
    removePathForcibly dir
    pure a

spec :: Spec
spec = do
    describe "isStrike" $ do
        it "ExitSuccess is not a strike" $
            isStrike ExitSuccess `shouldBe` False

        it "ExitFailure 1 (warning-level) is not a strike" $
            isStrike (ExitFailure 1) `shouldBe` False

        it "ExitFailure 2 (hard error) is a strike" $
            isStrike (ExitFailure 2) `shouldBe` True

        it "ExitFailure 3 (input failure) is a strike" $
            isStrike (ExitFailure 3) `shouldBe` True

        it "ExitFailure 99 (uncategorised non-zero) is a strike" $
            isStrike (ExitFailure 99) `shouldBe` True

    describe "advanceCounter" $ do
        it "clean exit resets counter to 0 (was 0)" $
            advanceCounter 0 ExitSuccess `shouldBe` (0, False)

        it "clean exit resets counter to 0 (was 2)" $
            advanceCounter 2 ExitSuccess `shouldBe` (0, False)

        it "warning-level exit also resets the counter" $
            advanceCounter 2 (ExitFailure 1) `shouldBe` (0, False)

        it "first strike from 0 -> 1, not crossed" $
            advanceCounter 0 (ExitFailure 2) `shouldBe` (1, False)

        it "second strike: 1 -> 2, not crossed" $
            advanceCounter 1 (ExitFailure 2) `shouldBe` (2, False)

        it "third strike crosses the threshold exactly once" $
            advanceCounter 2 (ExitFailure 2) `shouldBe` (3, True)

        it "fourth+ strike does NOT re-cross (idempotent disable)" $ do
            advanceCounter 3 (ExitFailure 2) `shouldBe` (4, False)
            advanceCounter 4 (ExitFailure 3) `shouldBe` (5, False)

        it "threshold value is the documented 3" $
            disableThreshold `shouldBe` 3

    describe "readJobStatus" $ do
        it "returns JobEnabled when no marker file exists" $
            withTempDir $ \dir -> do
                status <- readJobStatus dir "nonexistent-job"
                status `shouldBe` JobEnabled

        it "returns JobDisabled with the first line of the marker body" $
            withTempDir $ \dir -> do
                let jobName = "regen-system-state-mini"
                    path    = dir </> "regen-system-state-mini" <.> "disabled"
                TIO.writeFile path
                    "2026-04-29T03:14:00Z — last exit: 2; counter reached 3 \
                    \(threshold 3)\nsecond line should be ignored\n"
                status <- readJobStatus dir jobName
                case status of
                    JobEnabled ->
                        expectationFailure "expected JobDisabled, got JobEnabled"
                    JobDisabled body ->
                        body `shouldBe`
                            "2026-04-29T03:14:00Z — last exit: 2; counter \
                            \reached 3 (threshold 3)"

        it "treats an empty marker file as JobDisabled with empty body" $
            withTempDir $ \dir -> do
                let jobName = "verify-reading-order"
                    path    = dir </> "verify-reading-order" <.> "disabled"
                TIO.writeFile path ""
                status <- readJobStatus dir jobName
                status `shouldBe` JobDisabled ""

        it "isolates per-job markers (one disabled, one not)" $
            withTempDir $ \dir -> do
                TIO.writeFile (dir </> "regen-repo-inventory" <.> "disabled")
                              "marker for repo-inventory\n"
                a <- readJobStatus dir "regen-repo-inventory"
                b <- readJobStatus dir "regen-cross-reference-index"
                case a of
                    JobDisabled _ -> pure ()
                    JobEnabled    ->
                        expectationFailure "regen-repo-inventory should be disabled"
                b `shouldBe` JobEnabled
