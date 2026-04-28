{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the pure 'Version' helpers.
module Krikit.Agent.Ops.UpdateStatus.VersionSpec (spec) where

import           Test.Hspec

import           Krikit.Agent.Ops.UpdateStatus.Version
    ( UpdateAvailability (..)
    , Version (..)
    , compareVersions
    , extractVersion
    )

spec :: Spec
spec = do
    describe "extractVersion" $ do
        it "pulls 1.2.3 out of bare semver" $ do
            extractVersion "1.2.3" `shouldBe` Just (Version "1.2.3")

        it "strips a leading v prefix" $ do
            extractVersion "v1.2.3" `shouldBe` Just (Version "1.2.3")

        it "finds semver embedded in a sentence" $ do
            extractVersion "claude version 1.0.45 (Claude Code)"
                `shouldBe` Just (Version "1.0.45")

        it "handles npm-style package prefixes" $ do
            extractVersion "@openai/codex v0.5.2"
                `shouldBe` Just (Version "0.5.2")

        it "returns Nothing on unparseable input" $ do
            extractVersion "garbage"     `shouldBe` Nothing
            extractVersion ""            `shouldBe` Nothing
            extractVersion "1.2"         `shouldBe` Nothing  -- not 3 parts
            extractVersion "alpha.beta.gamma" `shouldBe` Nothing

        it "accepts pre-release suffixes attached to the patch number" $ do
            extractVersion "1.2.3-rc1" `shouldBe` Just (Version "1.2.3-rc1")

    describe "compareVersions" $ do
        it "reports UpToDate when installed equals latest" $ do
            compareVersions (Version "1.2.3") (Version "1.2.3")
                `shouldBe` UpToDate

        it "reports UpdateAvailable when they differ" $ do
            compareVersions (Version "1.2.3") (Version "1.2.4")
                `shouldBe` UpdateAvailable (Version "1.2.4")
