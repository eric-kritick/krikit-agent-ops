{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

-- | @krikit-verify-repo-inventory@: cross-check the canonical
-- @kritick-*@ root inventory against what's actually cloned on
-- disk.
--
-- Inputs:
--
--   * @kritick-ecosystem\/docs\/ecosystem-roots.generated.md@
--     (canonical: every @kritick-*@ root the ecosystem expects)
--   * filesystem walk of @workspace_root@
--   * @kritick-ecosystem\/config\/ecosystem.json@ (the @ignore@
--     filter; we drop ignored entries from both sides before
--     comparing)
--
-- Findings:
--
--   * 'Error': a root listed in the inventory is *not* cloned
--     locally. Action: clone it (probably via @gh-sync.sh@).
--   * 'Warning': a @kritick-*@ directory exists locally but is
--     not in the inventory and not on the ignore list. Action:
--     either add the repo to the inventory (if it's a real new
--     ecosystem repo) or to @ignore@ (if it's intentional but
--     out-of-scope).
--
-- Scope: kritick-* roots only. Other prefix families
-- (@krikit-*@, @purescript-*@, @scala-*@, ...) are out of scope
-- for this verifier; a future @krikit-verify-workspace-vs-github@
-- could extend coverage by querying GitHub directly.
module Krikit.Agent.Ops.Verify.RepoInventory
    ( -- * Pure logic
      kritickPrefix
    , buildFindings

      -- * Effectful orchestrator
    , verify
    ) where

import           Data.Set                                 (Set)
import qualified Data.Set                                 as Set
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import qualified Data.Text.IO                             as TIO

import           Effectful                                (Eff, IOE, liftIO, (:>))
import           Effectful.Reader.Static                  (Reader, ask)

import           Krikit.Agent.Ops.Config
    ( Config (..)
    , EcosystemPaths (..)
    , PathsConfig (..)
    )
import           Krikit.Agent.Ops.Regen.FsWalk            (listRepoDirs)
import           Krikit.Agent.Ops.RepoInventory.Run
    ( EcosystemFilter (..)
    , applyFilter
    , extractEcosystemRoots
    , readEcosystemFilter
    )
import           Krikit.Agent.Ops.Verify.Common
    ( Finding (..)
    , SetDiff (..)
    , Severity (..)
    , compareSets
    )

-- | The kritick-* prefix; only repos starting with this are in
-- scope for this verifier.
kritickPrefix :: Text
kritickPrefix = "kritick-"

-- | Pure core: given the canonical roots, the on-disk repos
-- (post-filter), and the set of kritick-* repos that are
-- intentionally not roots (e.g. @kritick-ecosystem@ via
-- @auto_roots.exclude_repos@), produce findings.
--
-- @inventoryRoots@: kritick-* root names from
-- @ecosystem-roots.generated.md@.
--
-- @rootExceptions@: kritick-* repos that exist on disk but are
-- *intentionally* not in @inventoryRoots@. We subtract these
-- from the on-disk side before comparing, so they don't surface
-- as false-positive warnings.
--
-- @onDisk@: every directory under @workspace_root@ that has a
-- @.git@ entry. We narrow to the kritick-* prefix here so other
-- families don't pollute the comparison.
buildFindings
    :: Set Text  -- ^ inventoryRoots (already kritick-*-shaped)
    -> Set Text  -- ^ rootExceptions (intentional non-roots)
    -> Set Text  -- ^ onDisk (full set; we narrow to kritick-* here)
    -> [Finding]
buildFindings inventoryRoots rootExceptions onDisk =
    let onDiskKritick = Set.filter (kritickPrefix `T.isPrefixOf`) onDisk
        onDiskFiltered = onDiskKritick `Set.difference` rootExceptions
        SetDiff{..}    = compareSets inventoryRoots onDiskFiltered
    in  map missingClone   sdLeftOnly
     ++ map unexpectedRepo sdRightOnly
  where
    missingClone repo = Finding
        { fSeverity = Error
        , fSubject  = repo
        , fMessage  = "listed in ecosystem-roots.generated.md but not "
                   <> "cloned to workspace_root"
        }

    unexpectedRepo repo = Finding
        { fSeverity = Warning
        , fSubject  = repo
        , fMessage  = "kritick-* directory present in workspace_root "
                   <> "but not in ecosystem-roots.generated.md and "
                   <> "not in auto_roots.exclude_repos (add to "
                   <> "inventory or to ecosystem.json `ignore` / "
                   <> "`auto_roots.exclude_repos`)"
        }

-- | End-to-end verifier: pull paths from @'Reader' 'Config'@, read
-- both sides, narrow + filter, return findings. Caller renders
-- and chooses an exit code.
verify
    :: ( Reader Config :> es
       , IOE :> es
       )
    => Eff es (Either Text [Finding])
verify = do
    cfg <- ask
    let rootsPath = epEcosystemRootsMd . pcEcosystem . cfgPaths $ cfg
        ecoPath   = epEcosystemJson    . pcEcosystem . cfgPaths $ cfg
        wsRoot    = cfgWorkspaceRoot cfg

    rootsDoc <- liftIO (TIO.readFile rootsPath)
    fsRepos  <- liftIO (listRepoDirs wsRoot)
    filterE  <- liftIO (readEcosystemFilter ecoPath)
    case filterE of
        Left err -> pure (Left err)
        Right f  -> do
            let inventoryRoots = Set.fromList
                  $ applyFilter f (extractEcosystemRoots rootsDoc)
                onDisk         = Set.fromList (applyFilter f fsRepos)
                rootExceptions = efRootExceptions f
            pure (Right (buildFindings inventoryRoots rootExceptions onDisk))
