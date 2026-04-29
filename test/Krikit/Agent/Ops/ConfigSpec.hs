{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the agent-ops config loader.
module Krikit.Agent.Ops.ConfigSpec (spec) where

import qualified Data.Text                       as T
import           System.Directory                (createDirectoryIfMissing,
                                                  getTemporaryDirectory,
                                                  removePathForcibly)
import           System.FilePath                 ((</>))
import           Test.Hspec

import           Krikit.Agent.Ops.Config
    ( Config (..)
    , EcosystemPaths (..)
    , FabricPaths (..)
    , PathsConfig (..)
    , loadConfigFrom
    )

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "krikit-agent-ops-config-spec"
    removePathForcibly dir
    createDirectoryIfMissing True dir
    a <- action dir
    removePathForcibly dir
    pure a

writeJson :: FilePath -> String -> IO ()
writeJson p body = writeFile p body

validJson :: String
validJson = unlines
    [ "{"
    , "  \"workspace_root\": \"/ws\","
    , "  \"paths\": {"
    , "    \"ecosystem\": {"
    , "      \"infrastructure_macmini_json\": \"a/infra.json\","
    , "      \"ecosystem_json\":              \"a/eco.json\","
    , "      \"ecosystem_roots_md\":          \"a/roots.md\""
    , "    },"
    , "    \"fabric\": {"
    , "      \"system_state_mini_md\": \"b/mini.md\","
    , "      \"repo_inventory_md\":    \"b/inv.md\""
    , "    }"
    , "  }"
    , "}"
    ]

spec :: Spec
spec = do
    describe "loadConfigFrom" $ do
        it "parses a well-formed config" $ do
            withTempDir $ \dir -> do
                let path = dir </> "agent-ops.json"
                writeJson path validJson
                cfgE <- loadConfigFrom path
                case cfgE of
                    Left err  -> expectationFailure (T.unpack err)
                    Right cfg ->
                        cfgWorkspaceRoot cfg `shouldBe` "/ws"

        it "resolves relative paths against workspace_root" $ do
            withTempDir $ \dir -> do
                let path = dir </> "agent-ops.json"
                writeJson path validJson
                Right cfg <- loadConfigFrom path
                let eco = pcEcosystem (cfgPaths cfg)
                epInfrastructureMacminiJson eco `shouldBe` "/ws/a/infra.json"
                epEcosystemJson eco             `shouldBe` "/ws/a/eco.json"
                epEcosystemRootsMd eco          `shouldBe` "/ws/a/roots.md"
                let fab = pcFabric (cfgPaths cfg)
                fpSystemStateMiniMd fab `shouldBe` "/ws/b/mini.md"
                fpRepoInventoryMd fab   `shouldBe` "/ws/b/inv.md"

        it "preserves absolute paths verbatim" $ do
            withTempDir $ \dir -> do
                let path = dir </> "agent-ops.json"
                writeJson path $ unlines
                    [ "{"
                    , "  \"workspace_root\": \"/ws\","
                    , "  \"paths\": {"
                    , "    \"ecosystem\": {"
                    , "      \"infrastructure_macmini_json\": \"/abs/infra.json\","
                    , "      \"ecosystem_json\":              \"a/eco.json\","
                    , "      \"ecosystem_roots_md\":          \"a/roots.md\""
                    , "    },"
                    , "    \"fabric\": {"
                    , "      \"system_state_mini_md\": \"b/mini.md\","
                    , "      \"repo_inventory_md\":    \"b/inv.md\""
                    , "    }"
                    , "  }"
                    , "}"
                    ]
                Right cfg <- loadConfigFrom path
                let eco = pcEcosystem (cfgPaths cfg)
                epInfrastructureMacminiJson eco `shouldBe` "/abs/infra.json"

        it "returns Left on a missing file" $ do
            withTempDir $ \dir -> do
                cfgE <- loadConfigFrom (dir </> "nope.json")
                case cfgE of
                    Left _  -> pure ()
                    Right _ -> expectationFailure "expected an error"

        it "returns Left on malformed JSON" $ do
            withTempDir $ \dir -> do
                let path = dir </> "agent-ops.json"
                writeJson path "not json {"
                cfgE <- loadConfigFrom path
                case cfgE of
                    Left _  -> pure ()
                    Right _ -> expectationFailure "expected a parse error"
