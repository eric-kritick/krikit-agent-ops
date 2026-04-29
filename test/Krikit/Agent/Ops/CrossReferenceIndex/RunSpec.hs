{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the cross-reference-index generator's pure core.
module Krikit.Agent.Ops.CrossReferenceIndex.RunSpec (spec) where

import qualified Data.Text                                  as T
import           Test.Hspec

import           Krikit.Agent.Ops.CrossReferenceIndex.Run
    ( CrossRef (..)
    , extractRefsFromContent
    , renderIndex
    )

spec :: Spec
spec = do
    describe "extractRefsFromContent" $ do
        it "produces zero refs for content with no file-shaped citations" $ do
            let body = T.unlines
                    [ "## Heading"
                    , ""
                    , "Just prose with `Map.lookup` and `forkIO` mentions."
                    ]
            extractRefsFromContent "agents/foo/AGENTS.md" body `shouldBe` []

        it "captures every file-shaped backtick citation" $ do
            let body = T.unlines
                    [ "See `../../USER.md` for baseline."
                    , "Also `../../research/fp-best-practices.md`."
                    ]
                got = extractRefsFromContent "agents/foo/AGENTS.md" body
            length got `shouldBe` 2
            map xrTarget got `shouldBe`
                [ "../../USER.md"
                , "../../research/fp-best-practices.md"
                ]
            map xrSource got `shouldBe`
                [ "agents/foo/AGENTS.md"
                , "agents/foo/AGENTS.md"
                ]

        it "strips anchors from targets" $ do
            let body = "See `../docs/design.md#bootstrap` for the bootstrap step."
                got  = extractRefsFromContent "agents/foo/AGENTS.md" body
            map xrTarget got `shouldBe` ["../docs/design.md"]

        it "dedupes a target cited multiple times in one file" $ do
            let body = T.unlines
                    [ "See `../../research/fp.md`."
                    , "Again, `../../research/fp.md`."
                    ]
                got = extractRefsFromContent "agents/foo/AGENTS.md" body
            length got `shouldBe` 1

        it "skips contents of fenced code blocks" $ do
            let body = T.unlines
                    [ "Real cite: `../../USER.md`."
                    , "```haskell"
                    , "let x = `code/inside.md` -- not a citation"
                    , "```"
                    , "Real cite: `../research/foo.md`."
                    ]
                got = extractRefsFromContent "agents/foo/AGENTS.md" body
            map xrTarget got `shouldBe` ["../../USER.md", "../research/foo.md"]

        it "skips identifier-shaped tokens (no extension, no trailing slash)" $ do
            let body = T.unlines
                    [ "Provider: `claude-cli/claude-opus-4-7`."
                    , "Model: `openai-codex/gpt-5.5`."
                    , "Real cite: `../../USER.md`."
                    ]
                got = extractRefsFromContent "agents/foo/AGENTS.md" body
            map xrTarget got `shouldBe` ["../../USER.md"]

    describe "renderIndex" $ do
        it "produces a banner-prefixed sorted markdown table" $ do
            let refs =
                    [ CrossRef "z/last.md"  "../target-z.md"
                    , CrossRef "a/first.md" "../target-a.md"
                    , CrossRef "a/first.md" "../target-b.md"
                    ]
                txt = renderIndex "2026-04-30" refs
            -- Banner present
            txt `shouldSatisfy` T.isInfixOf "Auto-generated"
            -- Header row
            txt `shouldSatisfy` T.isInfixOf "| Source | Target |"
            -- Sort: a/first.md rows precede z/last.md row
            let aIdx = T.length (fst (T.breakOn "a/first.md" txt))
                zIdx = T.length (fst (T.breakOn "z/last.md"  txt))
            (aIdx < zIdx) `shouldBe` True

        it "is empty-table-but-rendered when there are no refs" $ do
            let txt = renderIndex "2026-04-30" []
            txt `shouldSatisfy` T.isInfixOf "| Source | Target |"
