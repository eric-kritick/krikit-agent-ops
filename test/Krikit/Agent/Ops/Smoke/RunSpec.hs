{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration-shaped tests using mock Proc + Probe handlers. Demonstrates
-- the effect-system payoff: business logic runs unchanged with different
-- handlers swapped in.
module Krikit.Agent.Ops.Smoke.RunSpec (spec) where

import qualified Data.Map.Strict                 as Map
import           Test.Hspec

import           Effectful                       (runPureEff)

import           Krikit.Agent.Ops.Effect.Log     (runLogSilent)
import           Krikit.Agent.Ops.Effect.Probe   (Url (..), runProbeMock)
import           Krikit.Agent.Ops.Effect.Proc    (runProcMock)
import           Krikit.Agent.Ops.Smoke.Config   (defaultConfig)
import           Krikit.Agent.Ops.Smoke.Run      (tierOllama)
import           Krikit.Agent.Ops.Smoke.Tier     (TierStatus (..), trStatus)

spec :: Spec
spec = describe "tierOllama" $ do

    it "passes when /api/tags returns >= 4 models" $ do
        let happyBody =
                "{\"models\":[\
                \{\"name\":\"gemma4:e4b\"},\
                \{\"name\":\"gemma4:e2b\"},\
                \{\"name\":\"gemma4:26b\"},\
                \{\"name\":\"nomic-embed-text\"}]}"
            probes = Map.fromList
                [ ( Url "http://127.0.0.1:11434/api/tags", Right happyBody ) ]
            result = runPureEff
                   . runLogSilent
                   . runProcMock Map.empty
                   . runProbeMock probes
                   $ tierOllama defaultConfig
        trStatus result `shouldBe` Pass

    it "fails when /api/tags returns too few models" $ do
        let skinnyBody = "{\"models\":[{\"name\":\"only-one\"}]}"
            probes = Map.fromList
                [ ( Url "http://127.0.0.1:11434/api/tags", Right skinnyBody ) ]
            result = runPureEff
                   . runLogSilent
                   . runProcMock Map.empty
                   . runProbeMock probes
                   $ tierOllama defaultConfig
        case trStatus result of
            Fail _ -> pure ()
            other  -> expectationFailure
                        ("expected Fail, got: " <> show other)

    it "fails when /api/tags is unreachable" $ do
        let probes = Map.empty  -- no mock -> ProbeNetwork
            result = runPureEff
                   . runLogSilent
                   . runProcMock Map.empty
                   . runProbeMock probes
                   $ tierOllama defaultConfig
        case trStatus result of
            Fail _ -> pure ()
            other  -> expectationFailure
                        ("expected Fail, got: " <> show other)

    it "never touches IO in these tests" $ do
        -- If the effect stack leaked IOE, this would fail to typecheck.
        -- runPureEff signature guarantees no IO was performed.
        let _ = runPureEff
                  . runLogSilent
                  . runProcMock Map.empty
                  . runProbeMock Map.empty
                  $ tierOllama defaultConfig
        True `shouldBe` True
