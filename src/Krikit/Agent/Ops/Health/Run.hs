{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

-- | krikit-health: one-shot multi-section health report for the
-- agent host. Replaces the bash version of the same name.
--
-- Each 'Section' is a (title, lines) pair produced by an effectful
-- runner. The orchestrator collects them in a fixed order and the
-- formatter prints them with banner separators.
module Krikit.Agent.Ops.Health.Run
    ( -- * Domain types
      Section (..)

      -- * Orchestrator
    , runHealth
    , renderSections

      -- * Per-section runners (testable individually)
    , sectionHostInfo
    , sectionLaunchdServices
    , sectionPortBindings
    , sectionDockerColima
    , sectionWorkspaceBackup
    , sectionMacOSUpdates
    , sectionOllamaModels
    , sectionTailscale
    , sectionDiskMemory
    , sectionRecentErrors
    ) where

import           Data.Aeson                     (Value (..))
import qualified Data.Aeson                     as Aeson
import qualified Data.Aeson.KeyMap              as AKM
import qualified Data.ByteString.Lazy.Char8     as LBSC
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           System.Directory               (doesPathExist)

import           Effectful                      (Eff, IOE, liftIO, (:>))

import           Krikit.Agent.Ops.Effect.Probe
    ( Probe
    , ProbeError (..)
    , Url (..)
    , httpGet
    )
import           Krikit.Agent.Ops.Effect.Proc
    ( Proc
    , ProcResult (..)
    , runCmd
    , runCmdAsUser
    )
import           Krikit.Agent.Ops.Service
    ( Service
    , allServices
    , serviceLaunchctlLabel
    )
import           Krikit.Agent.Ops.Units         (Seconds (..))
import           Krikit.Agent.Ops.UpdateStatus.MacOS
    ( MacOSStatus (..)
    , readMacOSStatus
    )

-- | One section of the report: a banner title plus pre-formatted lines.
data Section = Section
    { sectionTitle :: !Text
    , sectionLines :: ![Text]
    }
    deriving stock (Eq, Show)

-- | Standard short-command timeout for any one subprocess call within
-- a section. Anything that takes longer is a wedge or a network hang
-- and we'd rather flag it via missing output than let the whole report
-- block.
shortCmdTimeout :: Seconds
shortCmdTimeout = Seconds 10

-- | Run every section in order. IOE is needed by some runners
-- (filesystem reads, MacOS cache); the rest just need Proc + Probe.
runHealth
    :: (Proc :> es, Probe :> es, IOE :> es)
    => Eff es [Section]
runHealth = sequence
    [ sectionHostInfo
    , sectionLaunchdServices
    , sectionPortBindings
    , sectionDockerColima
    , sectionWorkspaceBackup
    , sectionMacOSUpdates
    , sectionOllamaModels
    , sectionTailscale
    , sectionDiskMemory
    , sectionRecentErrors
    ]

-- | Format the whole report.
renderSections :: [Section] -> Text
renderSections = T.intercalate "\n" . map renderSection
  where
    renderSection (Section title lines') =
        T.intercalate "\n" $
            ("\n== " <> title <> " ==") : map ("  " <>) lines'

-- === Section: host time / uptime =============================================

sectionHostInfo :: (Proc :> es) => Eff es Section
sectionHostInfo = do
    dateOut   <- firstLineOf <$> runShort "date"   []
    uptimeOut <- firstLineOf <$> runShort "uptime" []
    pure Section
        { sectionTitle = "Host time / uptime"
        , sectionLines =
            [ dateOut
            , uptimeOut
            ]
        }

-- === Section: launchd services ===============================================

sectionLaunchdServices :: (Proc :> es) => Eff es Section
sectionLaunchdServices = do
    rows <- mapM serviceRow allServices
    pure Section
        { sectionTitle = "launchd services"
        , sectionLines = rows
        }

serviceRow :: (Proc :> es) => Service -> Eff es Text
serviceRow svc = do
    let label = serviceLaunchctlLabel svc
    r <- runCmd "sudo"
            ["launchctl", "print", "system/" <> T.unpack label]
            shortCmdTimeout
    pure $ case r of
        Right ProcResult { prStdout = out } ->
            let st  = parseLaunchctlField "state" out
                pid = parseLaunchctlField "pid"   out
            in  T.justifyLeft 28 ' ' label
                    <> "state=" <> T.justifyLeft 12 ' ' (orQuestion st)
                    <> "pid="   <> orDash pid
        Left _ ->
            T.justifyLeft 28 ' ' label <> "(not loaded)"
  where
    orQuestion t = if T.null t then "?" else t
    orDash     t = if T.null t then "-" else t

-- | Pull a value out of @launchctl print@'s indented @key = value@ rows.
-- Returns the empty string if the key isn't found.
parseLaunchctlField :: Text -> Text -> Text
parseLaunchctlField key out =
    case [ T.strip val
         | line <- T.lines out
         , let (k, rest) = T.break (== '=') (T.strip line)
         , T.strip k == key
         , let val = T.drop 1 rest
         ] of
        (v : _) -> v
        []      -> ""

-- === Section: port bindings ==================================================

sectionPortBindings :: (Proc :> es) => Eff es Section
sectionPortBindings = do
    openclaw <- portRow "18789" "openclaw"
    ollama   <- portRow "11434" "ollama"
    pure Section
        { sectionTitle = "Port bindings"
        , sectionLines = [openclaw, ollama]
        }

portRow :: (Proc :> es) => Text -> Text -> Eff es Text
portRow port label = do
    r <- runCmd "sudo"
            ["lsof", "-i", ":" <> T.unpack port, "-P", "-n"]
            shortCmdTimeout
    pure $ case r of
        Right ProcResult { prStdout = out }
            | not (T.null (T.strip out)) ->
                let firstHit =
                        case drop 1 (T.lines out) of
                            (l : _) -> l
                            []      -> "(no rows)"
                in  port <> " (" <> label <> "): " <> T.strip firstHit
        _ ->
            port <> " (" <> label <> "): free"

-- === Section: Docker / Colima ================================================

sectionDockerColima :: (Proc :> es, IOE :> es) => Eff es Section
sectionDockerColima = do
    let socket = "/Users/agentops/.colima/default/docker.sock"
    socketPresent <- liftIO (existsAt socket)
    if not socketPresent
        then pure Section
            { sectionTitle = "Docker / Colima"
            , sectionLines = ["(colima socket not present at " <> T.pack socket <> ")"]
            }
        else do
            colimaStatus <- runColimaStatus
            dockerInfo   <- runDockerInfo socket
            pure Section
                { sectionTitle = "Docker / Colima"
                , sectionLines = [colimaStatus, dockerInfo]
                }

runColimaStatus :: (Proc :> es) => Eff es Text
runColimaStatus = do
    r <- runCmdAsUser "agentops" "bash" ["-lc", "colima status 2>&1"] shortCmdTimeout
    pure $ "colima:  " <> case r of
        Right ProcResult { prStdout = out } -> firstNonEmptyLine out
        Left _                              -> "(unreachable)"

runDockerInfo :: (Proc :> es) => FilePath -> Eff es Text
runDockerInfo socket = do
    let cmd =
            "DOCKER_HOST=unix://" <> socket
                <> " docker info --format '"
                <> "{{.ServerVersion}} containers={{.Containers}} images={{.Images}}"
                <> "'"
    r <- runCmdAsUser "agentops" "bash" ["-lc", cmd] shortCmdTimeout
    pure $ "docker:  " <> case r of
        Right ProcResult { prStdout = out } -> firstNonEmptyLine out
        Left _                              -> "(unreachable)"

-- === Section: workspace backup ===============================================

sectionWorkspaceBackup :: (Proc :> es, IOE :> es) => Eff es Section
sectionWorkspaceBackup = do
    let path = "/Users/agentops/workspace-backup/.git"
    present <- liftIO (existsAt path)
    if not present
        then pure Section
            { sectionTitle = "Workspace backup"
            , sectionLines = ["(backup repo not initialized; see Playbook 9 Phase 2)"]
            }
        else do
            r <- runCmd "sudo"
                    [ "git", "-c", "safe.directory=/Users/agentops/workspace-backup"
                    , "-C", "/Users/agentops/workspace-backup"
                    , "log", "-1", "--format=%ar  (%h %s)"
                    ]
                    shortCmdTimeout
            let line = case r of
                    Right ProcResult { prStdout = out } ->
                        "last snapshot: " <> firstNonEmptyLine out
                    Left _ ->
                        "(error reading log)"
            pure Section
                { sectionTitle = "Workspace backup"
                , sectionLines = [line]
                }

-- === Section: macOS updates ==================================================

sectionMacOSUpdates :: (IOE :> es) => Eff es Section
sectionMacOSUpdates = do
    status <- liftIO readMacOSStatus
    pure Section
        { sectionTitle = "macOS updates"
        , sectionLines = [renderShortMacOS status]
        }

renderShortMacOS :: MacOSStatus -> Text
renderShortMacOS = \case
    MacOSUpToDate days ->
        "up to date (cache " <> ageDays days <> " old)"
    MacOSUpdatesAvailable updates days ->
        T.pack (show (length updates))
            <> " update(s) pending (cache " <> ageDays days <> " old)"
    MacOSCacheStale inner ->
        renderShortMacOS inner <> "  ! cache stale; re-run krikit-update-status --refresh-cache"
    MacOSCacheMissing ->
        "(never checked; run krikit-update-status --refresh-cache)"
    MacOSCacheUnparseable reason ->
        "(cache unparseable: " <> reason <> ")"
  where
    ageDays d = T.pack (show d) <> "d"

-- === Section: Ollama models ==================================================

sectionOllamaModels :: (Probe :> es) => Eff es Section
sectionOllamaModels = do
    r <- httpGet (Url "http://127.0.0.1:11434/api/tags")
    let lines' = case r of
            Left e  -> ["(unreachable: " <> probeErrorMsg e <> ")"]
            Right b -> parseOllamaModels b
    pure Section
        { sectionTitle = "Ollama models"
        , sectionLines = lines'
        }

parseOllamaModels :: Text -> [Text]
parseOllamaModels body =
    case Aeson.decode @Value (LBSC.pack (T.unpack body)) of
        Just (Object o) | Just (Array xs) <- AKM.lookup "models" o ->
            map renderModel (toList xs)
        _ -> ["(api/tags response unparseable)"]
  where
    toList = foldr (:) []  -- Vector -> list

renderModel :: Value -> Text
renderModel (Object o) =
    let name    = case AKM.lookup "name" o of
                    Just (String s) -> s
                    _               -> "?"
        sizeGb  = case AKM.lookup "size" o of
                    Just (Number n) -> floor (n / 1_000_000_000) :: Int
                    _               -> 0
    in  T.justifyLeft 30 ' ' name <> "  " <> T.pack (show sizeGb) <> " GB"
renderModel _ = "(non-object model entry)"

probeErrorMsg :: ProbeError -> Text
probeErrorMsg = \case
    ProbeNetwork m -> m
    ProbeStatus  n -> "HTTP " <> T.pack (show n)

-- === Section: Tailscale ======================================================

sectionTailscale :: (Proc :> es, IOE :> es) => Eff es Section
sectionTailscale = do
    let bin = "/Applications/Tailscale.app/Contents/MacOS/Tailscale"
    present <- liftIO (existsAt bin)
    if not present
        then pure Section
            { sectionTitle = "Tailscale"
            , sectionLines = ["(tailscale binary not found)"]
            }
        else do
            r <- runCmd bin ["status", "--json"] shortCmdTimeout
            let lines' = case r of
                    Right ProcResult { prStdout = out } -> parseTailscaleJson out
                    Left _ -> ["(status command failed)"]
            pure Section
                { sectionTitle = "Tailscale"
                , sectionLines = lines'
                }

parseTailscaleJson :: Text -> [Text]
parseTailscaleJson body =
    case Aeson.decode @Value (LBSC.pack (T.unpack body)) of
        Just (Object o) ->
            let state    = case AKM.lookup "BackendState" o of
                            Just (String s) -> s
                            _               -> "?"
                selfDns  = case AKM.lookup "Self" o of
                            Just (Object self) ->
                                case AKM.lookup "DNSName" self of
                                    Just (String s) -> T.dropWhileEnd (== '.') s
                                    _               -> "?"
                            _ -> "?"
            in  ["state=" <> state, "self=" <> selfDns]
        _ -> ["(status JSON unparseable)"]

-- === Section: disk / memory ==================================================

sectionDiskMemory :: (Proc :> es) => Eff es Section
sectionDiskMemory = do
    diskOut <- runShort "df" ["-h", "/"]
    let diskLines = take 2 (T.lines diskOut)
    memOut <- runShort "memory_pressure" []
    let memLine =
            case [ T.strip l | l <- T.lines memOut, "free percentage" `T.isInfixOf` T.toLower l ] of
                (l : _) -> l
                []      -> "memory_pressure: (unavailable)"
    pure Section
        { sectionTitle = "Disk / memory"
        , sectionLines = diskLines ++ ["", memLine]
        }

-- === Section: recent errors ==================================================

sectionRecentErrors :: (Proc :> es) => Eff es Section
sectionRecentErrors = do
    openclawTail <- tailErrLog "/var/log/krikit/openclaw.err.log"
    ollamaTail   <- tailErrLog "/var/log/krikit/ollama.err.log"
    pure Section
        { sectionTitle = "Recent errors (last 5 of each err.log)"
        , sectionLines =
            [ "openclaw.err.log:" ]
            ++ openclawTail
            ++ [ "", "ollama.err.log:" ]
            ++ ollamaTail
        }

tailErrLog :: (Proc :> es) => FilePath -> Eff es [Text]
tailErrLog path = do
    r <- runCmd "sudo" ["tail", "-5", path] shortCmdTimeout
    pure $ case r of
        Right ProcResult { prStdout = out } ->
            let nonEmpty = filter (not . T.null . T.strip) (T.lines out)
            in  if null nonEmpty
                    then ["  (clean)"]
                    else map ("  " <>) nonEmpty
        Left _ -> ["  (log not readable)"]

-- === Helpers =================================================================

runShort :: (Proc :> es) => FilePath -> [String] -> Eff es Text
runShort cmd args = do
    r <- runCmd cmd args shortCmdTimeout
    pure $ case r of
        Right ProcResult { prStdout = out } -> out
        Left _                              -> ""

firstLineOf :: Text -> Text
firstLineOf t = case T.lines t of
    (l : _) -> l
    []      -> ""

firstNonEmptyLine :: Text -> Text
firstNonEmptyLine t = case dropWhile (T.null . T.strip) (T.lines t) of
    (l : _) -> T.strip l
    []      -> ""

-- | Filesystem existence check. Used by sections that need to know
-- whether a binary or socket is in place before trying to invoke it.
existsAt :: FilePath -> IO Bool
existsAt = doesPathExist
