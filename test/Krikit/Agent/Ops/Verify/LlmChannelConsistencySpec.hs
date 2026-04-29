{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the LLM-channel-consistency verifier (pure
-- cross-check core).
module Krikit.Agent.Ops.Verify.LlmChannelConsistencySpec (spec) where

import qualified Data.Text                                          as T
import           Test.Hspec

import           Krikit.Agent.Ops.Verify.Common                     (Finding (..),
                                                                     Severity (..))
import           Krikit.Agent.Ops.Verify.LlmChannelConsistency
    ( Agent (..)
    , Channel (..)
    , buildFindings
    )

spec :: Spec
spec = do
    let thinkerCh = Channel
            { chName            = "thinker"
            , chPrimaryModel    = Just "claude-cli/claude-opus-4-7"
            , chThinkingDefault = Just "max"
            , chOpenclawAgentId = Just "thinker"
            }
        thinkerAg = Agent
            { agId              = "thinker"
            , agModel           = Just "claude-cli/claude-opus-4-7"
            , agThinkingDefault = Just "max"
            }
        builderCh = Channel
            { chName            = "builder"
            , chPrimaryModel    = Just "claude-cli/claude-opus-4-7"
            , chThinkingDefault = Just "max"
            , chOpenclawAgentId = Just "builder"
            }
        builderAg = Agent
            { agId              = "builder"
            , agModel           = Just "claude-cli/claude-opus-4-7"
            , agThinkingDefault = Just "max"
            }
        sentryCh = Channel
            { chName            = "sentry"
            , chPrimaryModel    = Just "ollama/gemma4:e4b"
            , chThinkingDefault = Just "off"
            , chOpenclawAgentId = Nothing  -- not in V1 scope
            }

    describe "buildFindings" $ do
        it "is clean when matched channel + agent agree" $
            buildFindings [thinkerCh] [thinkerAg] `shouldBe` []

        it "is clean across multiple matched channels" $
            buildFindings [thinkerCh, builderCh] [thinkerAg, builderAg]
                `shouldBe` []

        it "skips channels without openclaw_agent_id (V1 scope)" $ do
            -- sentry has no openclaw_agent_id; missing-agent should
            -- NOT be reported because we never tried to look it up.
            buildFindings [sentryCh] [] `shouldBe` []

        it "Errors when channel.openclaw_agent_id has no matching agent" $ do
            let got = buildFindings [thinkerCh] []
            length got        `shouldBe` 1
            map fSeverity got `shouldBe` [Error]
            map fSubject got  `shouldBe` ["thinker"]
            case got of
                [Finding _ _ msg] ->
                    msg `shouldSatisfy` T.isInfixOf "no agent with that id"
                _ -> expectationFailure "expected one finding"

        it "Errors on model mismatch" $ do
            let badAgent = thinkerAg { agModel = Just "claude-cli/claude-opus-4-6" }
                got      = buildFindings [thinkerCh] [badAgent]
            map fSeverity got `shouldBe` [Error]
            case got of
                [Finding _ _ msg] -> do
                    msg `shouldSatisfy` T.isInfixOf "model mismatch"
                    msg `shouldSatisfy` T.isInfixOf "claude-opus-4-7"
                    msg `shouldSatisfy` T.isInfixOf "claude-opus-4-6"
                _ -> expectationFailure "expected one finding"

        it "Errors on thinking_default mismatch" $ do
            let badAgent = thinkerAg { agThinkingDefault = Just "high" }
                got      = buildFindings [thinkerCh] [badAgent]
            map fSeverity got `shouldBe` [Error]
            case got of
                [Finding _ _ msg] -> do
                    msg `shouldSatisfy` T.isInfixOf "thinking_default mismatch"
                    msg `shouldSatisfy` T.isInfixOf "max"
                    msg `shouldSatisfy` T.isInfixOf "high"
                _ -> expectationFailure "expected one finding"

        it "produces multiple findings when multiple fields drift" $ do
            let badAgent = thinkerAg
                    { agModel           = Just "claude-cli/old-model"
                    , agThinkingDefault = Just "low"
                    }
                got      = buildFindings [thinkerCh] [badAgent]
            length got `shouldBe` 2

        it "Errors when openclaw agent has no model field at all" $ do
            let badAgent = thinkerAg { agModel = Nothing }
                got      = buildFindings [thinkerCh] [badAgent]
            length got `shouldBe` 1
            case got of
                [Finding _ _ msg] ->
                    msg `shouldSatisfy` T.isInfixOf "no model field"
                _ -> expectationFailure "expected one finding"

        it "tolerates missing thinking values on either side" $ do
            -- Channel without thinking_default vs agent without
            -- thinkingDefault: both unset => no finding. Mixed
            -- (one set, one unset) => no finding either; the
            -- compareThinking logic only fires on present-and-mismatched.
            let chNoThink = thinkerCh { chThinkingDefault = Nothing }
                agNoThink = thinkerAg { agThinkingDefault = Nothing }
            buildFindings [chNoThink] [thinkerAg]   `shouldBe` []
            buildFindings [thinkerCh] [agNoThink]   `shouldBe` []
            buildFindings [chNoThink] [agNoThink]   `shouldBe` []

        it "subject is the channel name (operator can grep)" $ do
            let badAgent = thinkerAg { agModel = Just "x" }
                got      = buildFindings [thinkerCh] [badAgent]
            map fSubject got `shouldBe` ["thinker"]
