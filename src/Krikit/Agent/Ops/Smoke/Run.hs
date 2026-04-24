{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

-- | Smoke-test orchestration and per-tier implementations.
--
-- Each tier is a small function with typed effect constraints (uses
-- Proc, Probe, Log as needed). The orchestrator 'runSmoke' picks which
-- tiers to run based on 'Opts' and collects the results.
--
-- This module stays effect-polymorphic where possible: only the
-- tier runners depend on specific effects, and the orchestrator just
-- composes them. The actual handler wiring happens in "Main".
module Krikit.Agent.Ops.Smoke.Run
    ( -- * Options + orchestration
      Opts (..)
    , defaultOpts
    , selectTiers
    , runSmoke

      -- * Per-tier runners
    , tierServices
    , tierDocker
    , tierOllama
    , tierAudit
    , tierAgent
    , tierConfig
    , tierErrLog
    , tierTelegram
    ) where

import           Control.Concurrent             (threadDelay)
import           Data.Aeson                     (Value (..))
import qualified Data.Aeson                     as Aeson
import qualified Data.Aeson.Key                 as AKey
import qualified Data.Aeson.KeyMap              as AKM
import qualified Data.ByteString.Lazy.Char8     as LBSC
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import qualified Data.Time.Clock.POSIX          as POSIX

import           Effectful                      (Eff, IOE, liftIO, (:>))

import           Krikit.Agent.Ops.Effect.Log
    ( Log
    , logFail
    , logInfo
    , logOk
    )
import           Krikit.Agent.Ops.Effect.Probe
    ( Probe
    , ProbeError (..)
    , Url (..)
    , httpGet
    , httpPost
    )
import           Krikit.Agent.Ops.Effect.Proc
    ( Proc
    , ProcError (..)
    , ProcResult (..)
    , runCmd
    , runCmdAsUser
    )
import           Krikit.Agent.Ops.Smoke.Config
    ( Paths (..)
    , SmokeConfig (..)
    , Thresholds (..)
    , Timeouts (..)
    , timeoutFor
    )
import           Krikit.Agent.Ops.Smoke.Tier
    ( Agent (..)
    , Service
    , ServiceExpectation (..)
    , Tier (..)
    , TierResult (..)
    , agentDisplayName
    , agentOpenclawName
    , agentToTier
    , failWith
    , pass
    , serviceExpectation
    , serviceLaunchctlLabel
    , skipWith
    )
import           Krikit.Agent.Ops.Units
    ( Milliseconds (..)
    , Seconds (..)
    , secondsInt
    )

-- | Runtime options from the command line.
data Opts = Opts
    { optFast     :: !Bool
    , optVerbose  :: !Bool
    , optTelegram :: !Bool
    }
    deriving stock (Eq, Show)

defaultOpts :: Opts
defaultOpts = Opts { optFast = False, optVerbose = False, optTelegram = False }

-- | The ordered set of tiers to run for the given options.
selectTiers :: Opts -> [Tier]
selectTiers opts =
    [ TierServices, TierDocker, TierOllama, TierAudit
    , TierSentry, TierWorkhorse
    ]
    ++ (if optFast opts then [] else [TierThinker, TierBuilder])
    ++ [ TierConfig, TierErrLog ]
    ++ (if optTelegram opts then [TierTelegram] else [])

-- | Run every selected tier and collect results in order.
runSmoke
    :: (Proc :> es, Probe :> es, Log :> es, IOE :> es)
    => SmokeConfig
    -> Opts
    -> Eff es [TierResult]
runSmoke cfg opts = mapM (runOne cfg) (selectTiers opts)

runOne
    :: (Proc :> es, Probe :> es, Log :> es, IOE :> es)
    => SmokeConfig
    -> Tier
    -> Eff es TierResult
runOne cfg = \case
    TierServices  -> tierServices  cfg
    TierDocker    -> tierDocker    cfg
    TierOllama    -> tierOllama    cfg
    TierAudit     -> tierAudit     cfg
    TierSentry    -> tierAgent     cfg AgentMain
    TierWorkhorse -> tierAgent     cfg AgentWorkhorse
    TierThinker   -> tierAgent     cfg AgentThinker
    TierBuilder   -> tierAgent     cfg AgentBuilder
    TierConfig    -> tierConfig    cfg
    TierErrLog    -> tierErrLog    cfg
    TierTelegram  -> tierTelegram  cfg

-- === Tier: launchd services =================================================

data ServiceObservation
    = Running        -- ^ launchctl reports @state = running@
    | Loaded         -- ^ launchctl found the service, but it's not running
    | NotFound       -- ^ launchctl couldn't find the service at all
    deriving stock (Eq, Show)

tierServices
    :: (Proc :> es, Log :> es)
    => SmokeConfig -> Eff es TierResult
tierServices cfg = do
    logInfo "checking launchd services..."
    observations <- mapM (observeService (scTimeouts cfg)) services
    let failures = [ (s, obs) | (s, obs) <- observations, not (meetsExpectation s obs) ]
    mapM_ (logServiceOutcome) observations
    if null failures
        then pure
            ( pass TierServices (Milliseconds 0)
                [ "all " <> T.pack (show (length services))
                    <> " services meet their expectations" ]
            )
        else pure
            ( failWith TierServices (Milliseconds 0)
                ( T.pack (show (length failures)) <> " service(s) failed expectations" )
                [ serviceLaunchctlLabel s <> ": expected "
                    <> T.pack (show (serviceExpectation s))
                    <> ", observed " <> T.pack (show obs)
                | (s, obs) <- failures ]
            )
  where
    services = [minBound .. maxBound :: Service]

-- | A service passes if its observed state satisfies its expectation.
meetsExpectation :: Service -> ServiceObservation -> Bool
meetsExpectation s obs = case (serviceExpectation s, obs) of
    (MustBeRunning, Running) -> True
    (MustBeRunning, _      ) -> False
    (MayBeWaiting,  Running) -> True
    (MayBeWaiting,  Loaded ) -> True
    (MayBeWaiting,  NotFound) -> False

logServiceOutcome
    :: (Log :> es)
    => (Service, ServiceObservation) -> Eff es ()
logServiceOutcome (s, obs) =
    let label = serviceLaunchctlLabel s
        exp'  = case serviceExpectation s of
                  MustBeRunning -> "must-run"
                  MayBeWaiting  -> "may-wait"
        msg   = label <> " [" <> exp' <> "]: " <> T.pack (show obs)
    in  if meetsExpectation s obs
            then logOk   msg
            else logFail msg

observeService
    :: (Proc :> es)
    => Timeouts -> Service -> Eff es (Service, ServiceObservation)
observeService to svc = do
    let label = serviceLaunchctlLabel svc
    r <- runCmd "sudo"
            ["launchctl", "print", "system/" <> T.unpack label]
            (toShortCmd to)
    pure (svc, classify r)
  where
    classify = \case
        Right ProcResult { prStdout = out }
            | "state = running" `T.isInfixOf` out -> Running
            | "state"           `T.isInfixOf` out -> Loaded
            | otherwise                           -> NotFound
        Left _ -> NotFound

-- === Tier: Docker / Colima ==================================================

tierDocker
    :: (Proc :> es, Log :> es)
    => SmokeConfig -> Eff es TierResult
tierDocker cfg = do
    let sock = pColimaSocket (scPaths cfg)
    logInfo ("probing docker via " <> T.pack sock <> "...")
    r <- runCmdAsUser "agentops" "bash"
            [ "-c"
            , "DOCKER_HOST=unix://" <> sock
                <> " docker info --format '{{.ServerVersion}}'"
            ]
            (toShortCmd (scTimeouts cfg))
    pure $ case r of
        Right ProcResult { prStdout = out }
            | not (T.null (T.strip out)) ->
                pass TierDocker (Milliseconds 0) ["server=" <> T.strip out]
        Right _ ->
            failWith TierDocker (Milliseconds 0) "docker info returned empty output" []
        Left e ->
            failWith TierDocker (Milliseconds 0) (procErrorMsg e) []

-- === Tier: Ollama ===========================================================

tierOllama
    :: (Probe :> es, Log :> es)
    => SmokeConfig -> Eff es TierResult
tierOllama cfg = do
    logInfo "querying ollama /api/tags..."
    r <- httpGet (Url "http://127.0.0.1:11434/api/tags")
    case r of
        Left e  -> pure (failWith TierOllama (Milliseconds 0) (probeErrorMsg e) [])
        Right b -> pure (parseOllama cfg b)

parseOllama :: SmokeConfig -> Text -> TierResult
parseOllama cfg body =
    case Aeson.decode @Value (LBSC.pack (T.unpack body)) of
        Nothing -> failWith TierOllama (Milliseconds 0) "unparseable JSON from /api/tags" []
        Just v  ->
            let n    = ollamaModelCount v
                need = thMinOllamaModels (scThresholds cfg)
            in  if n >= need
                    then pass TierOllama (Milliseconds 0)
                            ["models=" <> T.pack (show n)]
                    else failWith TierOllama (Milliseconds 0)
                            ( "only " <> T.pack (show n)
                                <> " models present, want >= "
                                <> T.pack (show need) )
                            []

ollamaModelCount :: Value -> Int
ollamaModelCount = \case
    Object o -> case AKM.lookup "models" o of
        Just (Array xs) -> length xs
        _               -> 0
    _        -> 0

-- === Tier: openclaw security audit ==========================================

tierAudit
    :: (Proc :> es, Log :> es)
    => SmokeConfig -> Eff es TierResult
tierAudit _ = do
    logInfo "running openclaw security audit..."
    r <- runCmdAsUser "agentops" "openclaw" ["security", "audit"] (Seconds 30)
    case r of
        Left e  -> pure (failWith TierAudit (Milliseconds 0) (procErrorMsg e) [])
        Right ProcResult { prStdout = out } -> pure (parseAudit out)

parseAudit :: Text -> TierResult
parseAudit out =
    case filter ("critical" `T.isInfixOf`) (T.lines out) of
        []      -> failWith TierAudit (Milliseconds 0) "no summary line found in audit output" []
        (s : _) ->
            let crit = countBefore "critical" s
                warn = countBefore "warn" s
            in  if crit == Just 0 && warn == Just 0
                    then pass TierAudit (Milliseconds 0) [T.strip s]
                    else failWith TierAudit (Milliseconds 0)
                            ( "audit: critical="
                                <> maybe "?" (T.pack . show) crit
                                <> " warn="
                                <> maybe "?" (T.pack . show) warn )
                            [T.strip s]

-- | Find the integer token immediately before the given keyword. Useful for
-- "N critical · M warn · 2 info"-style summary lines.
countBefore :: Text -> Text -> Maybe Int
countBefore keyword line =
    case break (== keyword) (T.words line) of
        (prefix, _matched : _) ->
            case safeLast prefix of
                Just n -> readIntMaybe n
                Nothing -> Nothing
        _ -> Nothing
  where
    safeLast xs = case xs of
        [] -> Nothing
        _  -> Just (last xs)

readIntMaybe :: Text -> Maybe Int
readIntMaybe t = case reads (T.unpack t) of
    [(n, "")] -> Just n
    _         -> Nothing

-- === Tier: agent liveness (Sentry/Workhorse/Thinker/Builder) ================

smokePrompt :: String
smokePrompt =
    "[smoke-test ping] Reply with only the single word PONG and nothing else."

tierAgent
    :: (Proc :> es, Log :> es)
    => SmokeConfig -> Agent -> Eff es TierResult
tierAgent cfg agent = do
    let to     = timeoutFor (scTimeouts cfg) agent
        toSec  = secondsInt to
        tier   = agentToTier agent
        name   = agentOpenclawName agent
    logInfo
        ( "probing " <> agentDisplayName agent
            <> " (--agent " <> name
            <> ", timeout " <> T.pack (show toSec) <> "s)..."
        )
    r <- runCmdAsUser "agentops" "openclaw"
            [ "agent", "--agent", T.unpack name
            , "-m", smokePrompt
            ]
            to
    pure $ case r of
        Left ProcTimeout ->
            failWith tier (Milliseconds 0)
                ("no response within " <> T.pack (show toSec) <> "s") []
        Left e ->
            failWith tier (Milliseconds 0) (procErrorMsg e) []
        Right ProcResult { prStdout = out, prElapsed = ms }
            | T.null (T.strip out) ->
                failWith tier ms "empty response" [out]
            | otherwise ->
                pass tier ms ["bytes=" <> T.pack (show (T.length out))]

-- === Tier: config assertions ================================================

tierConfig
    :: (Proc :> es, Log :> es)
    => SmokeConfig -> Eff es TierResult
tierConfig cfg = do
    let path = pOpenclawConfig (scPaths cfg)
    logInfo ("reading " <> T.pack path <> "...")
    r <- runCmd "sudo" ["cat", path] (toShortCmd (scTimeouts cfg))
    case r of
        Left e  -> pure (failWith TierConfig (Milliseconds 0) (procErrorMsg e) [])
        Right ProcResult { prStdout = out } -> pure (parseConfig out)

parseConfig :: Text -> TierResult
parseConfig body =
    case Aeson.decode @Value (LBSC.pack (T.unpack body)) of
        Nothing -> failWith TierConfig (Milliseconds 0) "config is not valid JSON" []
        Just v  ->
            let deny      = readPath v ["tools", "deny"]
                sandbox   = readPath v ["agents", "defaults", "sandbox", "mode"]
                denyOk    =
                    case deny of
                        Array xs -> elem (String "group:web") xs
                        _        -> False
                sandboxOk = sandbox == String "all"
            in case (denyOk, sandboxOk) of
                (True, True)   ->
                    pass TierConfig (Milliseconds 0)
                        [ "tools.deny contains group:web"
                        , "sandbox.mode=all"
                        ]
                (False, True)  ->
                    failWith TierConfig (Milliseconds 0) "tools.deny missing group:web" []
                (True, False)  ->
                    failWith TierConfig (Milliseconds 0)
                        ( "sandbox.mode != all (got " <> showValue sandbox <> ")" )
                        []
                (False, False) ->
                    failWith TierConfig (Milliseconds 0)
                        "tools.deny + sandbox.mode both wrong" []

readPath :: Value -> [Text] -> Value
readPath = foldl step
  where
    step (Object o) k = case AKM.lookup (AKey.fromText k) o of
        Just v  -> v
        Nothing -> Null
    step _ _ = Null

showValue :: Value -> Text
showValue = TE.decodeUtf8 . LBSC.toStrict . Aeson.encode

-- === Tier: sandbox err-log scan =============================================

tierErrLog
    :: (Proc :> es, Log :> es)
    => SmokeConfig -> Eff es TierResult
tierErrLog cfg = do
    let path = pOpenclawErrLog (scPaths cfg)
        n    = thErrLogScanLineCount (scThresholds cfg)
    logInfo
        ( "scanning last " <> T.pack (show n)
            <> " lines of " <> T.pack path
        )
    r <- runCmd "sudo" ["tail", "-" <> show n, path] (toShortCmd (scTimeouts cfg))
    case r of
        Left (ProcLaunchErr _) ->
            pure (skipWith TierErrLog (Milliseconds 0) "err log not readable" [])
        Left e ->
            pure (failWith TierErrLog (Milliseconds 0) (procErrorMsg e) [])
        Right ProcResult { prStdout = out } ->
            pure (analyzeErrLog out)

analyzeErrLog :: Text -> TierResult
analyzeErrLog out =
    let hits = [ l | l <- T.lines out
                   , any (`T.isInfixOf` l) sandboxErrors
               ]
    in  if null hits
            then pass TierErrLog (Milliseconds 0) ["no sandbox errors in recent err log"]
            else failWith TierErrLog (Milliseconds 0)
                    ( T.pack (show (length hits))
                        <> " sandbox error lines in recent log" )
                    hits
  where
    sandboxErrors =
        [ "Failed to inspect sandbox image"
        , "Cannot connect to the Docker daemon"
        , "sandbox image not found"
        ]

-- === Tier: Telegram round-trip ==============================================

tierTelegram
    :: (Proc :> es, Probe :> es, Log :> es, IOE :> es)
    => SmokeConfig -> Eff es TierResult
tierTelegram cfg = do
    let paths = scPaths cfg
    logInfo ("reading " <> T.pack (pChannelsEnv paths))
    envRes <- runCmd "sudo" ["cat", pChannelsEnv paths] (toShortCmd (scTimeouts cfg))
    case envRes of
        Left _ ->
            pure (skipWith TierTelegram (Milliseconds 0) "channels.env not readable" [])
        Right ProcResult { prStdout = envBody } ->
            case ( lookupEnv "TELEGRAM_BOT_TOKEN"     envBody
                 , lookupEnv "TELEGRAM_ALERT_CHAT_ID" envBody
                 ) of
                (Just token, Just chatId) ->
                    runTelegramProbe cfg token chatId
                _ ->
                    pure (skipWith TierTelegram (Milliseconds 0) "telegram creds missing" [])

runTelegramProbe
    :: (Proc :> es, Probe :> es, Log :> es, IOE :> es)
    => SmokeConfig -> Text -> Text -> Eff es TierResult
runTelegramProbe cfg token chatId = do
    nonce <- liftIO freshNonce
    let body = "[smoke] nonce=" <> nonce
                 <> " (krikit-smoke round-trip probe; safe to ignore)"
        url  = Url ("https://api.telegram.org/bot" <> token <> "/sendMessage")
    logInfo ("posting to telegram with nonce=" <> nonce)
    sendRes <- httpPost url [("chat_id", chatId), ("text", body)]
    case sendRes of
        Left e ->
            pure ( failWith TierTelegram (Milliseconds 0)
                     ("sendMessage: " <> probeErrorMsg e) [] )
        Right _ ->
            pollForNonce cfg nonce

-- | Poll the openclaw log dir for the nonce. Sleeps 1s between attempts,
-- gives up after 'toTelegramWait' seconds.
pollForNonce
    :: (Proc :> es, IOE :> es)
    => SmokeConfig -> Text -> Eff es TierResult
pollForNonce cfg nonce =
    loop 0
  where
    maxWait = toTelegramWait (scTimeouts cfg)

    loop i
        | i >= secondsInt maxWait =
            pure ( failWith TierTelegram (Milliseconds 0)
                     ( "nonce not observed in openclaw log after "
                         <> T.pack (show (secondsInt maxWait)) <> "s" )
                     [] )
        | otherwise = do
            liftIO (threadDelay 1_000_000)
            r <- runCmd "sudo" ["sh", "-c", grepCmd] (Seconds 5)
            case r of
                Right ProcResult { prStdout = out }
                    | nonce `T.isInfixOf` out ->
                        pure ( pass TierTelegram (Milliseconds (i * 1000))
                                 [ "nonce observed after "
                                     <> T.pack (show i) <> "s" ] )
                Right _ -> loop (i + 1)
                Left  _ -> loop (i + 1)

    grepCmd =
        "grep -l '" <> T.unpack nonce
            <> "' /tmp/openclaw/openclaw-*.log || true"

-- === Helpers ================================================================

freshNonce :: IO Text
freshNonce = do
    secs <- POSIX.getPOSIXTime
    pure ("smoke-" <> T.pack (show (floor secs :: Integer)))

procErrorMsg :: ProcError -> Text
procErrorMsg = \case
    ProcTimeout       -> "timeout"
    ProcNonZero n     -> "non-zero exit " <> T.pack (show n)
    ProcLaunchErr msg -> "launch error: " <> msg

probeErrorMsg :: ProbeError -> Text
probeErrorMsg = \case
    ProbeNetwork msg -> "network error: " <> msg
    ProbeStatus  n   -> "http status " <> T.pack (show n)

-- | Naive KEY=VALUE extractor from a channels.env-style string.
-- First match wins; whitespace around KEY and around VALUE is stripped.
lookupEnv :: Text -> Text -> Maybe Text
lookupEnv key body =
    headMay
        [ T.strip (T.drop (T.length prefix) stripped)
        | rawLine <- T.lines body
        , let stripped = T.strip rawLine
        , let prefix   = key <> "="
        , prefix `T.isPrefixOf` stripped
        ]
  where
    headMay xs = case xs of
        []    -> Nothing
        (x:_) -> Just x
