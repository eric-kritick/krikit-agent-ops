{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Domain types for the @krikit-update-status@ tool.
--
-- A 'Tool' is one of the agent-stack CLIs we want to track for update
-- availability. Each carries an 'UpstreamSource' describing how, if at
-- all, we can determine whether a newer version exists.
module Krikit.Agent.Ops.UpdateStatus.Tool
    ( Tool (..)
    , toolDisplayName
    , toolBinary
    , toolVersionArgs

    , UpstreamSource (..)
    , toolUpstream

    , allTools
    ) where

import Data.Text (Text)

-- | One of the CLIs the smoke + monitor stack relies on.
data Tool
    = ToolCodex      -- ^ @\@openai\/codex@ via npm
    | ToolOpenclaw   -- ^ @openclaw@ via npm
    | ToolClaude     -- ^ Claude Code; auto-updates, no public npm registry
    deriving stock (Eq, Show, Bounded, Enum)

-- | Iteration-friendly list of every tool. Mirrors @['Tool']@ but reads
-- nicer at call sites.
allTools :: [Tool]
allTools = [minBound .. maxBound]

-- | Human-readable label for the tool. Used in report output.
toolDisplayName :: Tool -> Text
toolDisplayName = \case
    ToolCodex    -> "codex"
    ToolOpenclaw -> "openclaw"
    ToolClaude   -> "claude"

-- | The CLI binary name, as exec'd to query the installed version.
toolBinary :: Tool -> FilePath
toolBinary = \case
    ToolCodex    -> "codex"
    ToolOpenclaw -> "openclaw"
    ToolClaude   -> "claude"

-- | Args passed to the binary to print its version. All three tools
-- accept @--version@; if that ever diverges, change here only.
toolVersionArgs :: Tool -> [String]
toolVersionArgs _ = ["--version"]

-- | How a tool publishes new versions, if at all.
data UpstreamSource
    = NpmRegistry !Text
    -- ^ Carry the npm package name; query via @npm show <pkg> version@.
    | AutoUpdates
    -- ^ The CLI manages its own updates (e.g. Claude Code's
    -- self-updater). No remote registry to compare against; report
    -- the installed version and stop.
    deriving stock (Eq, Show)

-- | Exhaustive on 'Tool'. New tool? You'll be forced to classify it.
toolUpstream :: Tool -> UpstreamSource
toolUpstream = \case
    ToolCodex    -> NpmRegistry "@openai/codex"
    ToolOpenclaw -> NpmRegistry "openclaw"
    ToolClaude   -> AutoUpdates
