{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Health-check primitives for @krikit-monitor@.
--
-- Each check returns a 'CheckResult' with a 'CheckStatus' tag
-- (@Ok | Warn | Crit@) and a one-line message. Six checks today,
-- mirroring @monitor.py@:
--
--   1. OpenClaw gateway TCP port (127.0.0.1:18789)
--   2. Ollama @\/api\/tags@ HTTP 200
--   3. Tailscale @BackendState == Running@
--   4. Docker via Colima socket
--   5. Disk usage on @/@
--   6. Memory pressure (@memory_pressure@ free percentage)
--
-- All check functions are effectful; pure transition logic lives
-- in 'Krikit.Agent.Ops.Monitor.Digest'.
module Krikit.Agent.Ops.Monitor.Check
    ( -- * Types
      CheckStatus (..)
    , CheckResult (..)
    , CheckName

      -- * Pure helpers
    , statusFromText
    , statusToText
    , isOk

      -- * Individual checks
    , checkOpenclawPort
    , checkOllama
    , checkTailscale
    , checkDocker
    , checkDisk
    , checkMemoryPressure

      -- * Convenience
    , runAllChecks
    ) where

import           Control.Exception                (IOException, bracket, try)
import           Data.Aeson                       (FromJSON (..), (.:?))
import qualified Data.Aeson                       as A
import qualified Data.ByteString.Lazy             as LBS
import           Data.List                        (find)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import qualified Data.Text.Encoding.Error         as TE
import           Network.Socket                   (AddrInfo (..),
                                                   AddrInfoFlag (..),
                                                   SocketType (..),
                                                   close, connect,
                                                   defaultHints, getAddrInfo,
                                                   socket)
import           System.Directory                 (doesFileExist)
import           System.Exit                      (ExitCode (..))
import qualified System.Process.Typed             as TP
import           System.Timeout                   (timeout)

import           Effectful                        (Eff, IOE, liftIO, (:>))

-- =============================================================================
-- Types
-- =============================================================================

type CheckName = Text

-- | Severity for a check result.
--
-- Ordering: 'Ok' < 'Warn' < 'Crit'. Used by transition logic
-- to decide whether to alert.
data CheckStatus = Ok | Warn | Crit
    deriving stock (Eq, Ord, Show)

-- | One check's outcome.
data CheckResult = CheckResult
    { crName    :: !CheckName
    , crStatus  :: !CheckStatus
    , crMessage :: !Text
    }
    deriving stock (Eq, Show)

-- =============================================================================
-- Pure helpers
-- =============================================================================

isOk :: CheckResult -> Bool
isOk = (== Ok) . crStatus

-- | Render a 'CheckStatus' as the lowercase short form used in
-- the on-disk state file (@"ok"@ \/ @"warn"@ \/ @"crit"@).
statusToText :: CheckStatus -> Text
statusToText = \case
    Ok   -> "ok"
    Warn -> "warn"
    Crit -> "crit"

-- | Inverse of 'statusToText'. Unknown values clamp to 'Ok' (the
-- least-alarming default), which matches @monitor.py@'s
-- @prev_alerts.get(name, "ok")@ behavior.
statusFromText :: Text -> CheckStatus
statusFromText = \case
    "ok"   -> Ok
    "warn" -> Warn
    "crit" -> Crit
    _      -> Ok

-- =============================================================================
-- Individual checks
-- =============================================================================

-- | TCP-port reachability. Used for the OpenClaw gateway probe.
checkOpenclawPort :: IOE :> es => Eff es CheckResult
checkOpenclawPort = liftIO $ do
    let host = "127.0.0.1"
        port = "18789"
        name = "openclaw"
    result <- try @IOException (tcpProbe host port 2_000_000)
    pure $ case result of
        Right True ->
            CheckResult name Ok ("127.0.0.1:18789 reachable")
        Right False ->
            CheckResult name Crit ("127.0.0.1:18789 timeout")
        Left e ->
            CheckResult name Crit
                ("127.0.0.1:18789 not reachable: " <> tshow e)

-- | Open a TCP connection within @timeoutMicros@ microseconds.
-- Returns @True@ on success. Uses 'bracket' so the socket is
-- closed even if 'connect' throws.
tcpProbe :: String -> String -> Int -> IO Bool
tcpProbe host port timeoutMicros = do
    let hints = defaultHints { addrSocketType = Stream
                             , addrFlags      = [AI_NUMERICHOST, AI_NUMERICSERV]
                             }
    addrs <- getAddrInfo (Just hints) (Just host) (Just port)
    case addrs of
        []    -> pure False
        (a:_) -> bracket
            (socket (addrFamily a) (addrSocketType a) (addrProtocol a))
            close
            (\sock -> do
                r <- timeout timeoutMicros (connect sock (addrAddress a))
                pure (r == Just ()))

-- | Liveness probe for Ollama: GET 127.0.0.1:11434/api/tags.
-- Implementation note: we shell to @curl@ rather than dragging
-- the full http-client into a check module that's otherwise pure
-- of HTTP. macOS ships @curl@; the mini's devenv playbook
-- installs it via Homebrew anyway.
checkOllama :: IOE :> es => Eff es CheckResult
checkOllama = liftIO $ do
    let name = "ollama"
        url  = "http://127.0.0.1:11434/api/tags"
    r <- try @IOException $
        TP.readProcessStdout
            (TP.proc "curl" ["-sS", "-o", "/dev/null", "-w", "%{http_code}",
                             "--max-time", "3", url])
    pure $ case r of
        Right (ExitSuccess, body) ->
            let code = T.strip (decodeLenient body)
            in  if code == "200"
                    then CheckResult name Ok ("api/tags 200")
                    else CheckResult name Crit ("api/tags HTTP " <> code)
        Right (ExitFailure n, _) ->
            CheckResult name Crit ("curl exit " <> tshow n)
        Left e ->
            CheckResult name Crit ("curl error: " <> tshow e)

-- | Tailscale BackendState == Running. Probes @tailscale status
-- --json@ via the first binary that exists on disk.
checkTailscale :: IOE :> es => Eff es CheckResult
checkTailscale = liftIO $ do
    let name = "tailscale"
    mbin <- firstExistingBinary
                [ "/Applications/Tailscale.app/Contents/MacOS/Tailscale"
                , "/usr/local/bin/tailscale"
                , "/opt/homebrew/bin/tailscale"
                ]
    case mbin of
        Nothing  ->
            pure (CheckResult name Warn "binary not found")
        Just bin -> do
            r <- try @IOException $
                TP.readProcessStdout (TP.proc bin ["status", "--json"])
            case r of
                Left e ->
                    pure (CheckResult name Crit
                           ("status error: " <> tshow e))
                Right (ExitFailure n, _) ->
                    pure (CheckResult name Crit ("status rc=" <> tshow n))
                Right (ExitSuccess, body) -> case parseTailscale body of
                    Just "Running" ->
                        pure (CheckResult name Ok "Running")
                    Just s ->
                        pure (CheckResult name Crit ("state=" <> s))
                    Nothing ->
                        pure (CheckResult name Crit
                                "could not parse BackendState")
  where
    firstExistingBinary :: [FilePath] -> IO (Maybe FilePath)
    firstExistingBinary [] = pure Nothing
    firstExistingBinary (p : ps) = do
        ok <- doesFileExist p
        if ok then pure (Just p) else firstExistingBinary ps

newtype TailscaleStatus = TailscaleStatus (Maybe Text)

instance FromJSON TailscaleStatus where
    parseJSON = A.withObject "TailscaleStatus" $ \o ->
        TailscaleStatus <$> (o .:? "BackendState")

parseTailscale :: LBS.ByteString -> Maybe Text
parseTailscale body = case A.eitherDecode body of
    Right (TailscaleStatus s) -> s
    Left _                    -> Nothing

-- | Docker reachable via the Colima-backed agentops socket.
-- Mirrors @monitor.py@: socket existence is the cheap precondition;
-- @docker info@ confirms the daemon is responsive.
checkDocker :: IOE :> es => Eff es CheckResult
checkDocker = liftIO $ do
    let name      = "docker"
        sockPath  = "/Users/agentops/.colima/default/docker.sock"
    sockOk <- doesFileExist sockPath
    if not sockOk
        then pure (CheckResult name Warn "colima socket missing")
        else do
            let env = [ ("DOCKER_HOST", "unix://" <> sockPath) ]
                cfg =
                    TP.setEnv env
                        (TP.proc "docker"
                            ["info", "--format", "{{.ServerVersion}}"])
            r <- try @IOException (TP.readProcessStdout cfg)
            case r of
                Left e ->
                    pure (CheckResult name Crit ("error: " <> tshow e))
                Right (ExitFailure n, _) ->
                    pure (CheckResult name Crit
                            ("docker info rc=" <> tshow n))
                Right (ExitSuccess, body) ->
                    let v = T.strip (decodeLenient body)
                    in  pure (CheckResult name Ok
                              (if T.null v then "reachable" else v))

-- | Disk usage on @/@ via @df -k /@. Capacity column is field 5;
-- we strip the trailing @%@.
checkDisk :: IOE :> es => Eff es CheckResult
checkDisk = liftIO $ do
    let name = "disk"
    r <- try @IOException $
        TP.readProcessStdout (TP.proc "df" ["-k", "/"])
    case r of
        Left e ->
            pure (CheckResult name Warn ("df error: " <> tshow e))
        Right (ExitFailure n, _) ->
            pure (CheckResult name Warn ("df rc=" <> tshow n))
        Right (ExitSuccess, body) ->
            case parseDfPercent (decodeLenient body) of
                Just pct
                    | pct >= 95 -> pure (CheckResult name Crit
                                          ("/ " <> tshow pct <> "% used"))
                    | pct >= 80 -> pure (CheckResult name Warn
                                          ("/ " <> tshow pct <> "% used"))
                    | otherwise -> pure (CheckResult name Ok
                                          ("/ " <> tshow pct <> "% used"))
                Nothing ->
                    pure (CheckResult name Warn "df unparseable")

-- | Pull the @Capacity@ percentage out of @df -k /@ output. The
-- header line precedes the data line; the data line's 5th
-- whitespace-separated field is e.g. @"42%"@.
parseDfPercent :: Text -> Maybe Int
parseDfPercent body = case reverse (filter (not . T.null) (T.lines body)) of
    (lastLine : _) -> case T.words lastLine of
        ws | length ws >= 5 ->
            let pctText = T.dropEnd 1 (ws !! 4)  -- strip "%"
            in  readInt pctText
        _ -> Nothing
    _ -> Nothing
  where
    readInt :: Text -> Maybe Int
    readInt t = case reads (T.unpack t) of
        [(n, "")] -> Just n
        _         -> Nothing

-- | Memory pressure: @memory_pressure@ prints a "free percentage"
-- line we can grep + parse.
checkMemoryPressure :: IOE :> es => Eff es CheckResult
checkMemoryPressure = liftIO $ do
    let name = "memory"
    r <- try @IOException $
        TP.readProcessStdout (TP.proc "memory_pressure" [])
    case r of
        Left e ->
            pure (CheckResult name Warn ("error: " <> tshow e))
        Right (ExitFailure n, _) ->
            pure (CheckResult name Warn ("rc=" <> tshow n))
        Right (ExitSuccess, body) ->
            case parseFreePercent (decodeLenient body) of
                Just pct
                    | pct < 5  -> pure (CheckResult name Crit
                                         (tshow pct <> "% free"))
                    | pct < 15 -> pure (CheckResult name Warn
                                         (tshow pct <> "% free"))
                    | otherwise -> pure (CheckResult name Ok
                                          (tshow pct <> "% free"))
                Nothing ->
                    pure (CheckResult name Warn "unparseable output")

-- | Find a line containing \"free percentage\" (case-insensitive)
-- and pull a digit-only suffix from after the colon.
parseFreePercent :: Text -> Maybe Int
parseFreePercent body =
    case find (T.isInfixOf "free percentage" . T.toLower) (T.lines body) of
        Nothing -> Nothing
        Just l  -> case T.breakOnEnd ":" l of
            (_, after) ->
                let cleaned = T.dropEnd 1 (T.strip after)  -- strip trailing %
                in  case reads (T.unpack cleaned) of
                        [(n, "")] -> Just n
                        _         -> Nothing

-- =============================================================================
-- Convenience
-- =============================================================================

-- | Run every check in canonical order. Effectful but pure-data
-- driven; transition logic / digest assembly happens downstream.
runAllChecks :: IOE :> es => Eff es [CheckResult]
runAllChecks = do
    a <- checkOpenclawPort
    b <- checkOllama
    c <- checkTailscale
    d <- checkDocker
    e <- checkDisk
    f <- checkMemoryPressure
    pure [a, b, c, d, e, f]

-- =============================================================================
-- Helpers
-- =============================================================================

decodeLenient :: LBS.ByteString -> Text
decodeLenient = TE.decodeUtf8With TE.lenientDecode . LBS.toStrict

-- | 'show' to 'Text'.
tshow :: Show a => a -> Text
tshow = T.pack . show
