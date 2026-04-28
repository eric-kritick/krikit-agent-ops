{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the pure 'parseMacOSCache' classifier.
module Krikit.Agent.Ops.UpdateStatus.MacOSSpec (spec) where

import           Test.Hspec

import           Krikit.Agent.Ops.UpdateStatus.MacOS
    ( MacOSStatus (..)
    , parseMacOSCache
    )

spec :: Spec
spec = do
    describe "parseMacOSCache" $ do

        it "recognizes 'No new software available'" $ do
            parseMacOSCache "Software Update Tool\n\nNo new software available\n" 1
                `shouldBe` MacOSUpToDate 1

        it "counts star-prefixed pending lines" $ do
            let body =
                    "Software Update Tool\n\
                    \Finding available software\n\
                    \\n\
                    \* Label: macOS Sequoia 15.5-23F79\n\
                    \\tTitle: macOS Sequoia 15.5...\n\
                    \* Label: Safari 19.0\n\
                    \\tTitle: Safari 19.0...\n"
            parseMacOSCache body 0 `shouldBe` MacOSUpdatesAvailable 2 0

        it "reports Unparseable when neither marker matches" $ do
            case parseMacOSCache "weird format\n" 3 of
                MacOSCacheUnparseable _ -> pure ()
                other -> expectationFailure
                            ("expected Unparseable, got " <> show other)
