{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the pure 'parseMacOSCache' classifier.
module Krikit.Agent.Ops.UpdateStatus.MacOSSpec (spec) where

import           Test.Hspec

import           Krikit.Agent.Ops.UpdateStatus.MacOS
    ( MacOSStatus (..)
    , MacOSUpdate (..)
    , extractUpdates
    , parseMacOSCache
    )

spec :: Spec
spec = do
    describe "parseMacOSCache" $ do

        it "recognizes 'No new software available'" $ do
            parseMacOSCache "Software Update Tool\n\nNo new software available\n" 1
                `shouldBe` MacOSUpToDate 1

        it "extracts label + title for each pending update" $ do
            let body =
                    "Software Update Tool\n\
                    \Finding available software\n\
                    \\n\
                    \* Label: macOS Sequoia 15.5-23F79\n\
                    \\tTitle: macOS Sequoia 15.5, Version: 15.5, Size: 14.2 GiB, Recommended: YES, Action: restart,\n\
                    \* Label: Safari 19.0\n\
                    \\tTitle: Safari 19.0, Version: 19.0, Size: 95 MiB, Recommended: YES,\n"
                expected =
                    [ MacOSUpdate "macOS Sequoia 15.5-23F79" "macOS Sequoia 15.5"
                    , MacOSUpdate "Safari 19.0"              "Safari 19.0"
                    ]
            parseMacOSCache body 0 `shouldBe` MacOSUpdatesAvailable expected 0
            extractUpdates body    `shouldBe` expected

        it "tolerates a label without a Title line" $ do
            let body =
                    "* Label: SomeUpdateLabel\n\
                    \(no title line)\n\
                    \* Label: AnotherLabel\n\
                    \\tTitle: Another Title, Version: 1.0,\n"
                expected =
                    [ MacOSUpdate "SomeUpdateLabel" ""
                    , MacOSUpdate "AnotherLabel"    "Another Title"
                    ]
            extractUpdates body `shouldBe` expected

        it "reports Unparseable when neither marker matches" $ do
            case parseMacOSCache "weird format\n" 3 of
                MacOSCacheUnparseable _ -> pure ()
                other -> expectationFailure
                            ("expected Unparseable, got " <> show other)
