{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the small pure helpers inside Monitor.Check.
-- The IO checks themselves (tcpProbe, curl ollama, etc.) are
-- exercised end-to-end on the mini and not here.
module Krikit.Agent.Ops.Monitor.CheckSpec (spec) where

import           Test.Hspec

import           Krikit.Agent.Ops.Monitor.Check (CheckResult (..),
                                                 CheckStatus (..),
                                                 isOk, statusFromText,
                                                 statusToText)

spec :: Spec
spec = do
    describe "statusToText / statusFromText" $ do
        it "round-trips every status" $ do
            mapM_ (\s -> statusFromText (statusToText s) `shouldBe` s)
                [Ok, Warn, Crit]

        it "unknown text clamps to Ok (matches monitor.py default)" $ do
            statusFromText "unknown"  `shouldBe` Ok
            statusFromText ""         `shouldBe` Ok

    describe "CheckStatus Ord" $ do
        it "orders Ok < Warn < Crit" $ do
            (Ok < Warn)   `shouldBe` True
            (Warn < Crit) `shouldBe` True
            maximum [Ok, Warn, Crit] `shouldBe` Crit

    describe "isOk" $ do
        it "is True only for Ok" $ do
            isOk (CheckResult "x" Ok   "msg") `shouldBe` True
            isOk (CheckResult "x" Warn "msg") `shouldBe` False
            isOk (CheckResult "x" Crit "msg") `shouldBe` False
