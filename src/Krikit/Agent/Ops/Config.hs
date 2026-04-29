{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

-- | Runtime configuration shared by every @krikit-agent-ops@ binary.
--
-- Every regenerator / verifier reads canonical input paths and
-- writes derived output paths. Hard-coding those paths in each
-- 'Main' couples the binaries to the macmini layout and forces a
-- recompile to retarget. Instead, every binary reads a single JSON
-- config and threads it via @'Reader' 'Config'@.
--
-- Default config location:
-- @\/Users\/opsadmin\/Development\/kritick-ecosystem\/config\/agent-ops.json@.
-- Override with the env var @KRIKIT_AGENT_OPS_CONFIG@, or supply a
-- @--config@ flag at the executable boundary.
--
-- All path values under @paths@ are written *relative to*
-- @workspace_root@ and are resolved to absolute paths at load
-- time, so the in-memory 'Config' value carries only absolute
-- 'FilePath's. Generators don't need to know what was relative.
module Krikit.Agent.Ops.Config
    ( -- * Domain types
      Config (..)
    , PathsConfig (..)
    , EcosystemPaths (..)
    , FabricPaths (..)
    , OpenclawPaths (..)

      -- * Constants
    , defaultConfigPath
    , configEnvVar

      -- * Loading
    , resolveConfigPath
    , loadConfig
    , loadConfigFrom
    ) where

import           Control.Exception        (IOException, try)
import           Data.Aeson               (FromJSON (..), (.:))
import qualified Data.Aeson               as A
import qualified Data.ByteString.Lazy     as LBS
import           Data.Text                (Text)
import qualified Data.Text                as T
import           System.Environment       (lookupEnv)
import           System.FilePath          (isAbsolute, (</>))

-- =============================================================================
-- Domain types
-- =============================================================================

-- | Top-level config. Add new fields as the binary surface grows --
-- e.g. log levels, timeouts, feature gates -- without minting new
-- config files.
data Config = Config
    { cfgWorkspaceRoot :: !FilePath
    , cfgPaths         :: !PathsConfig
    }
    deriving stock (Eq, Show)

data PathsConfig = PathsConfig
    { pcEcosystem :: !EcosystemPaths
    , pcFabric    :: !FabricPaths
    , pcOpenclaw  :: !OpenclawPaths
    }
    deriving stock (Eq, Show)

-- | Inputs sourced from @kritick-ecosystem@.
data EcosystemPaths = EcosystemPaths
    { epInfrastructureMacminiJson :: !FilePath
    , epEcosystemJson             :: !FilePath
    , epEcosystemRootsMd          :: !FilePath
    , epDocsDir                   :: !FilePath
      -- ^ @kritick-ecosystem\/docs\/@. Walked recursively for every
      -- @*.md@ by the cross-reference-index generator.
    }
    deriving stock (Eq, Show)

-- | Paths inside @krikit-agent-fabric@ that the agent-ops binaries
-- read or write.
data FabricPaths = FabricPaths
    { fpRoot                  :: !FilePath
      -- ^ root of @krikit-agent-fabric\/@. Walked recursively for
      -- every @*.md@ by the cross-reference-index generator.
    , fpSystemStateMiniMd     :: !FilePath
    , fpRepoInventoryMd       :: !FilePath
    , fpCrossReferenceIndexMd :: !FilePath
    , fpAgentsDir             :: !FilePath
      -- ^ root of @krikit-agent-fabric\/agents\/@. Walked by the
      -- reading-order verifier to find every @AGENTS.md@ /
      -- @IDENTITY.md@.
    }
    deriving stock (Eq, Show)

-- | Paths to OpenClaw runtime files. Typically absolute under
-- @\/Users\/agentops\/.openclaw\/@ on the mini; the LLM-channel
-- verifier reads them. Missing files are tolerated (verifier
-- emits a 'Warning'), so this can carry mini-only paths in shared
-- config without breaking off-mini runs.
data OpenclawPaths = OpenclawPaths
    { opConfigJson :: !FilePath
      -- ^ absolute path to @openclaw.json@, e.g.
      -- @\/Users\/agentops\/.openclaw\/openclaw.json@.
    }
    deriving stock (Eq, Show)

-- =============================================================================
-- Constants
-- =============================================================================

-- | Default location read when neither @--config@ nor the env var
-- is supplied.
defaultConfigPath :: FilePath
defaultConfigPath =
    "/Users/opsadmin/Development/kritick-ecosystem/config/agent-ops.json"

-- | Env var consulted by 'resolveConfigPath' if @--config@ wasn't
-- supplied.
configEnvVar :: String
configEnvVar = "KRIKIT_AGENT_OPS_CONFIG"

-- =============================================================================
-- JSON decoding (relative form)
-- =============================================================================

-- | Intermediate decoding shape: paths in the on-disk JSON are
-- relative to @workspace_root@. We decode into this type, then
-- 'absolutize' to produce the canonical 'Config'.
data RawConfig = RawConfig
    { rcWorkspaceRoot :: !FilePath
    , rcPaths         :: !RawPaths
    }

data RawPaths = RawPaths
    { rpEcosystem :: !RawEcosystemPaths
    , rpFabric    :: !RawFabricPaths
    , rpOpenclaw  :: !RawOpenclawPaths
    }

data RawEcosystemPaths = RawEcosystemPaths
    { repInfrastructureMacminiJson :: !FilePath
    , repEcosystemJson             :: !FilePath
    , repEcosystemRootsMd          :: !FilePath
    , repDocsDir                   :: !FilePath
    }

data RawFabricPaths = RawFabricPaths
    { rfpRoot                  :: !FilePath
    , rfpSystemStateMiniMd     :: !FilePath
    , rfpRepoInventoryMd       :: !FilePath
    , rfpCrossReferenceIndexMd :: !FilePath
    , rfpAgentsDir             :: !FilePath
    }

newtype RawOpenclawPaths = RawOpenclawPaths
    { ropConfigJson :: FilePath
    }

instance FromJSON RawConfig where
    parseJSON = A.withObject "Config" $ \o ->
        RawConfig
            <$> o .: "workspace_root"
            <*> o .: "paths"

instance FromJSON RawPaths where
    parseJSON = A.withObject "Paths" $ \o ->
        RawPaths
            <$> o .: "ecosystem"
            <*> o .: "fabric"
            <*> o .: "openclaw"

instance FromJSON RawEcosystemPaths where
    parseJSON = A.withObject "EcosystemPaths" $ \o ->
        RawEcosystemPaths
            <$> o .: "infrastructure_macmini_json"
            <*> o .: "ecosystem_json"
            <*> o .: "ecosystem_roots_md"
            <*> o .: "docs_dir"

instance FromJSON RawFabricPaths where
    parseJSON = A.withObject "FabricPaths" $ \o ->
        RawFabricPaths
            <$> o .: "root"
            <*> o .: "system_state_mini_md"
            <*> o .: "repo_inventory_md"
            <*> o .: "cross_reference_index_md"
            <*> o .: "agents_dir"

instance FromJSON RawOpenclawPaths where
    parseJSON = A.withObject "OpenclawPaths" $ \o ->
        RawOpenclawPaths
            <$> o .: "config_json"

-- | Resolve every relative path in 'RawConfig' against the
-- workspace root, producing the canonical absolute-path 'Config'.
absolutize :: RawConfig -> Config
absolutize RawConfig{..} =
    Config
        { cfgWorkspaceRoot = rcWorkspaceRoot
        , cfgPaths = PathsConfig
            { pcEcosystem = EcosystemPaths
                { epInfrastructureMacminiJson = under (repInfrastructureMacminiJson rpEco)
                , epEcosystemJson             = under (repEcosystemJson rpEco)
                , epEcosystemRootsMd          = under (repEcosystemRootsMd rpEco)
                , epDocsDir                   = under (repDocsDir rpEco)
                }
            , pcFabric = FabricPaths
                { fpRoot                  = under (rfpRoot rpFab)
                , fpSystemStateMiniMd     = under (rfpSystemStateMiniMd rpFab)
                , fpRepoInventoryMd       = under (rfpRepoInventoryMd rpFab)
                , fpCrossReferenceIndexMd = under (rfpCrossReferenceIndexMd rpFab)
                , fpAgentsDir             = under (rfpAgentsDir rpFab)
                }
            , pcOpenclaw = OpenclawPaths
                { opConfigJson = under (ropConfigJson rpOc)
                }
            }
        }
  where
    rpEco = rpEcosystem rcPaths
    rpFab = rpFabric    rcPaths
    rpOc  = rpOpenclaw  rcPaths
    under p
        | isAbsolute p = p                       -- already absolute: trust it
        | otherwise    = rcWorkspaceRoot </> p

-- The wildcards rely on RecordWildCards which we enable below.

-- =============================================================================
-- Loading
-- =============================================================================

-- | Decide where to look for the config file:
--
-- 1. If the caller passes an explicit path, use it.
-- 2. Otherwise, consult the @KRIKIT_AGENT_OPS_CONFIG@ env var.
-- 3. Otherwise, fall back to 'defaultConfigPath'.
resolveConfigPath :: Maybe FilePath -> IO FilePath
resolveConfigPath = \case
    Just p  -> pure p
    Nothing -> do
        envVal <- lookupEnv configEnvVar
        pure (maybe defaultConfigPath id envVal)

-- | Read + decode the config from its resolved path.
loadConfig :: Maybe FilePath -> IO (Either Text Config)
loadConfig override = do
    path <- resolveConfigPath override
    loadConfigFrom path

-- | Read + decode from an explicit path. Errors come back as 'Text'
-- ('Left'); the caller decides exit semantics.
loadConfigFrom :: FilePath -> IO (Either Text Config)
loadConfigFrom path = do
    result <- try (LBS.readFile path)
    case result of
        Left (e :: IOException) ->
            pure (Left ("could not read " <> T.pack path
                        <> ": " <> T.pack (show e)))
        Right bytes -> case A.eitherDecode bytes of
            Left err  -> pure (Left ("could not parse " <> T.pack path
                                     <> ": " <> T.pack err))
            Right raw -> pure (Right (absolutize raw))
