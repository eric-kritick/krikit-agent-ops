{-# LANGUAGE OverloadedStrings #-}

-- | Tests for idempotent file writing.
module Krikit.Agent.Ops.Regen.WriteSpec (spec) where

import qualified Data.Text.IO                  as TIO
import           System.Directory              (createDirectoryIfMissing,
                                                doesFileExist,
                                                getTemporaryDirectory,
                                                removePathForcibly)
import           System.FilePath               ((</>))
import           Test.Hspec

import           Krikit.Agent.Ops.Regen.Write  (WriteOutcome (..),
                                                writeIfChanged)

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "krikit-agent-ops-write-spec"
    removePathForcibly dir
    createDirectoryIfMissing True dir
    a <- action dir
    removePathForcibly dir
    pure a

spec :: Spec
spec = do
    describe "writeIfChanged" $ do
        it "creates the file on first write" $ do
            withTempDir $ \dir -> do
                let path = dir </> "out.md"
                exists0  <- doesFileExist path
                exists0 `shouldBe` False
                outcome  <- writeIfChanged path "hello\n"
                outcome  `shouldBe` Created
                exists1  <- doesFileExist path
                exists1 `shouldBe` True

        it "leaves the file alone when content matches exactly" $ do
            withTempDir $ \dir -> do
                let path = dir </> "out.md"
                _        <- writeIfChanged path "hello\n"
                outcome  <- writeIfChanged path "hello\n"
                outcome `shouldBe` Unchanged

        it "leaves the file alone when only the timestamp differs" $ do
            withTempDir $ \dir -> do
                let path = dir </> "out.md"
                let v1 = "# Title\n> Last generated: 2026-04-29\nbody\n"
                let v2 = "# Title\n> Last generated: 2099-12-31\nbody\n"
                _        <- writeIfChanged path v1
                outcome  <- writeIfChanged path v2
                outcome `shouldBe` Unchanged
                contents <- TIO.readFile path
                contents `shouldBe` v1   -- original kept

        it "rewrites when substantive content differs" $ do
            withTempDir $ \dir -> do
                let path = dir </> "out.md"
                let v1 = "# Title\n> Last generated: 2026-04-29\nbody\n"
                let v2 = "# Title\n> Last generated: 2026-04-29\nDIFFERENT\n"
                _        <- writeIfChanged path v1
                outcome  <- writeIfChanged path v2
                outcome `shouldBe` Written
                contents <- TIO.readFile path
                contents `shouldBe` v2
