{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Version + availability ADTs for update tracking.
--
-- 'Version' is a thin wrapper around 'Text' so we don't pass raw text
-- through the domain layer. 'UpdateAvailability' is the closed sum-type
-- summarizing what we learned about a tool: up to date, an update
-- available, no remote check possible (auto-updaters), or unknown
-- (couldn't determine one or both versions).
module Krikit.Agent.Ops.UpdateStatus.Version
    ( Version (..)
    , versionText
    , extractVersion

    , UpdateAvailability (..)
    , compareVersions
    ) where

import           Data.Char  (isDigit)
import           Data.Text  (Text)
import qualified Data.Text  as T

-- | A semantic-versioned string. Construction is unconstrained because
-- not every tool publishes strict semver, and we'd rather pass through
-- whatever the upstream said than reject novel formats.
newtype Version = Version Text
    deriving stock   (Eq, Show)
    deriving newtype (Ord)

-- | Project back to 'Text' for display.
versionText :: Version -> Text
versionText (Version t) = t

-- | Pull a semver-like substring from arbitrary CLI output.
--
-- Examples of inputs we want to cope with:
--
-- @
-- "1.2.3"                       -> Just "1.2.3"
-- "v1.2.3"                      -> Just "1.2.3"
-- "claude version 1.0.45 (Claude Code)" -> Just "1.0.45"
-- "@openai\/codex v0.5.2"       -> Just "0.5.2"
-- "garbage"                     -> Nothing
-- @
--
-- The strategy is "find the first run of @digits.digits.digits@ and
-- take through the next whitespace." It's deliberately lenient.
extractVersion :: Text -> Maybe Version
extractVersion =
    fmap Version . findFirstSemver . T.words
  where
    findFirstSemver :: [Text] -> Maybe Text
    findFirstSemver []         = Nothing
    findFirstSemver (w : rest) =
        let stripped = T.dropWhile (== 'v') w
        in  if looksLikeSemver stripped
                then Just stripped
                else findFirstSemver rest

    looksLikeSemver :: Text -> Bool
    looksLikeSemver t =
        case T.splitOn "." t of
            (a : b : c : _) ->
                T.all isDigit a && T.all isDigit b
                && (T.all isDigit (T.takeWhile isDigit c))
                && not (T.null c)
            _ -> False

-- | What we ultimately report for a single tool.
data UpdateAvailability
    = UpToDate
    -- ^ Installed version equals the upstream latest.
    | UpdateAvailable !Version
    -- ^ Newer version exists upstream; payload carries it.
    | NoRemoteCheck
    -- ^ The tool auto-updates and we don't query a public registry.
    | Unknown !Text
    -- ^ Couldn't determine one or both versions; payload is the reason.
    deriving stock (Eq, Show)

-- | Compare two versions textually. We don't try to do "real" semver
-- comparison (1.10 > 1.9) because the variants in the wild are too
-- many; an exact-match check is enough to flag an update.
compareVersions :: Version -> Version -> UpdateAvailability
compareVersions installed latest
    | installed == latest = UpToDate
    | otherwise           = UpdateAvailable latest
