{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Format the update-status results for terminal / Telegram output.
--
-- Pure: takes the data classified by "Krikit.Agent.Ops.UpdateStatus.Run"
-- and "Krikit.Agent.Ops.UpdateStatus.MacOS", returns plain 'Text'.
-- Caller does the IO.
module Krikit.Agent.Ops.UpdateStatus.Report
    ( renderReport
    , renderToolStatus
    , renderMacOSStatus
    , hasAnyUpdate
    ) where

import           Data.Text                              (Text)
import qualified Data.Text                              as T

import           Krikit.Agent.Ops.UpdateStatus.MacOS
    ( MacOSStatus (..)
    , defaultStaleThresholdDays
    )
import           Krikit.Agent.Ops.UpdateStatus.Run
    ( ToolStatus (..) )
import           Krikit.Agent.Ops.UpdateStatus.Tool     (toolDisplayName)
import           Krikit.Agent.Ops.UpdateStatus.Version
    ( UpdateAvailability (..)
    , Version (..)
    , versionText
    )

-- | Top-level renderer: header, per-tool lines, macOS line.
--
-- Output looks like:
--
-- @
-- Updates:
--     codex      0.5.2 (latest)
--   ! openclaw   1.2.3 -> 1.2.4
--     claude     1.0.45 (auto-updates)
--     macOS      no updates pending (cache 2d old)
-- @
--
-- A leading @!@ marks pending updates so they stand out at a glance.
renderReport :: [ToolStatus] -> MacOSStatus -> Text
renderReport tools macos =
    T.intercalate "\n" $
        [ "Updates:" ]
        ++ map renderToolStatus tools
        ++ [ renderMacOSStatus macos ]

-- | One line for one tool.
renderToolStatus :: ToolStatus -> Text
renderToolStatus ts =
    formatLine (toolMarker (tsAvailability ts)) (toolDisplayName (tsTool ts)) body
  where
    body = case tsAvailability ts of
        UpToDate ->
            renderInstalled (tsInstalled ts) <> " (latest)"
        UpdateAvailable latest ->
            renderInstalled (tsInstalled ts)
                <> " -> "
                <> versionText latest
        NoRemoteCheck ->
            renderInstalled (tsInstalled ts) <> " (auto-updates)"
        Unknown reason ->
            "? (" <> reason <> ")"

renderInstalled :: Maybe Version -> Text
renderInstalled = \case
    Just v  -> versionText v
    Nothing -> "?"

-- | Single-character "needs attention" marker per availability state.
-- Drives alignment: every line has exactly @<marker><space><name>...@.
toolMarker :: UpdateAvailability -> Char
toolMarker = \case
    UpToDate          -> ' '
    UpdateAvailable _ -> '!'
    NoRemoteCheck     -> ' '
    Unknown _         -> '?'

-- | Same alignment recipe used for both tool and macOS lines: one
-- marker char, one space, name padded to 'nameWidth', then the body.
formatLine :: Char -> Text -> Text -> Text
formatLine m name body =
    T.singleton m
        <> " "
        <> T.justifyLeft nameWidth ' ' name
        <> body

nameWidth :: Int
nameWidth = 12

-- | One line for macOS, using the same marker+name layout as tools.
renderMacOSStatus :: MacOSStatus -> Text
renderMacOSStatus = \case
    MacOSUpToDate days ->
        formatLine ' ' "macOS" ("no updates pending (cache " <> ageText days <> " old)")
    MacOSUpdatesAvailable count days ->
        formatLine '!' "macOS"
            (T.pack (show count) <> " pending (cache " <> ageText days <> " old)")
    MacOSCacheStale inner ->
        renderMacOSStatus inner
            <> "\n  ! cache stale (>" <> T.pack (show defaultStaleThresholdDays)
            <> "d); re-run krikit-macos-updates"
    MacOSCacheMissing ->
        formatLine ' ' "macOS" "(cache not built; run krikit-macos-updates)"
    MacOSCacheUnparseable reason ->
        formatLine '?' "macOS" ("cache unparseable: " <> reason)

ageText :: Int -> Text
ageText days = T.pack (show days) <> "d"

-- | True if at least one tool has an update available, or macOS has
-- updates pending. Useful as a one-shot "is there anything to do
-- today?" check that callers can branch on.
hasAnyUpdate :: [ToolStatus] -> MacOSStatus -> Bool
hasAnyUpdate tools macos =
    any toolHasUpdate tools || macosHasUpdate macos
  where
    toolHasUpdate ts = case tsAvailability ts of
        UpdateAvailable _ -> True
        _                 -> False

    macosHasUpdate = \case
        MacOSUpdatesAvailable _ _ -> True
        MacOSCacheStale inner     -> macosHasUpdate inner
        _                         -> False
