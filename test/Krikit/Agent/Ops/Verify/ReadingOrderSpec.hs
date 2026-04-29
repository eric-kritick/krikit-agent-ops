{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the reading-order verifier's pure helpers.
module Krikit.Agent.Ops.Verify.ReadingOrderSpec (spec) where

import qualified Data.Text                            as T
import           Test.Hspec

import           Krikit.Agent.Ops.Verify.Common       (Finding (..),
                                                       Severity (..))
import           Krikit.Agent.Ops.Verify.ReadingOrder
    ( buildFindingsForFile
    , looksLikePath
    , resolvePath
    , stripAnchor
    )

spec :: Spec
spec = do
    describe "looksLikePath" $ do
        it "accepts relative paths with a known extension" $ do
            looksLikePath "../../USER.md"                 `shouldBe` True
            looksLikePath "research/fp-best-practices.md" `shouldBe` True
            looksLikePath "./agents/foo/AGENTS.md"        `shouldBe` True
            looksLikePath "kritick-ecosystem/config/ecosystem.json"
                                                          `shouldBe` True

        it "accepts directory references (trailing slash)" $
            looksLikePath "krikit-agent-fabric/context/" `shouldBe` True

        it "rejects bare filenames (no slash)" $
            looksLikePath "USER.md" `shouldBe` False

        it "rejects code identifiers" $ do
            looksLikePath "Map.lookup"     `shouldBe` False
            looksLikePath "forkIO"         `shouldBe` False
            looksLikePath "T.text"         `shouldBe` False

        it "rejects LLM provider/model identifiers" $ do
            -- Critical: these contain `/` but are not paths.
            looksLikePath "claude-cli/claude-opus-4-7" `shouldBe` False
            looksLikePath "openai-codex/gpt-5.5"       `shouldBe` False
            looksLikePath "ollama/gemma4:e4b"          `shouldBe` False

        it "rejects URLs" $ do
            looksLikePath "https://github.com/eric-kritick/foo" `shouldBe` False
            looksLikePath "http://example.com"                  `shouldBe` False
            looksLikePath "ssh://git@github.com/foo"            `shouldBe` False

        it "rejects tokens with whitespace" $
            looksLikePath "two words/no" `shouldBe` False

        it "rejects CLI flag tokens (leading `-`)" $
            looksLikePath "-i path/foo.md" `shouldBe` False

        it "accepts absolute paths" $
            looksLikePath "/etc/foo/bar.conf" `shouldBe` True

        it "rejects empty / pure-whitespace tokens" $ do
            looksLikePath ""    `shouldBe` False
            looksLikePath " "   `shouldBe` False

    describe "stripAnchor" $ do
        it "drops a trailing anchor" $
            stripAnchor "doc.md#section" `shouldBe` "doc.md"

        it "is a no-op when there is no anchor" $
            stripAnchor "doc.md" `shouldBe` "doc.md"

        it "drops everything from the first '#' onward" $
            stripAnchor "a/b.md#x#y" `shouldBe` "a/b.md"

    describe "resolvePath" $ do
        it "resolves a relative path against the citing dir" $
            resolvePath "agents/stendahl" "research/fp-best-practices.md"
                `shouldBe` "agents/stendahl/research/fp-best-practices.md"

        it "collapses parent traversal segments" $
            resolvePath "agents/stendahl" "../../USER.md"
                `shouldBe` "USER.md"

        it "leaves absolute paths alone" $
            resolvePath "agents/stendahl" "/etc/foo.conf"
                `shouldBe` "/etc/foo.conf"

        it "strips the anchor before resolving" $
            resolvePath "agents/stendahl" "design.md#bootstrap"
                `shouldBe` "agents/stendahl/design.md"

    describe "buildFindingsForFile" $ do
        let citing = "agents/foo/AGENTS.md"

        it "is empty when every path exists" $ do
            let exists =
                    [ ("agents/foo/research/x.md", True)
                    , ("USER.md",                  True)
                    ]
            buildFindingsForFile citing "" exists `shouldBe` []

        it "produces one Error finding per missing path" $ do
            let exists =
                    [ ("agents/foo/research/x.md", True)
                    , ("agents/foo/missing.md",    False)
                    , ("typo/path.md",             False)
                    ]
                got = buildFindingsForFile citing "" exists
            length got            `shouldBe` 2
            map fSeverity got     `shouldBe` [Error, Error]

        it "subject is the citing file path; message names the missing target" $ do
            let exists = [("agents/foo/missing.md", False)]
            case buildFindingsForFile citing "" exists of
                [Finding _ subj msg] -> do
                    subj `shouldBe` T.pack citing
                    msg  `shouldSatisfy` T.isInfixOf "agents/foo/missing.md"
                _ -> expectationFailure "expected exactly one finding"
