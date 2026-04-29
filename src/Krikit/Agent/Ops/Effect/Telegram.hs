{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

-- | Typed effect for sending Telegram messages via the Bot API.
--
-- Single operation: 'SendMessage'. Bot credentials (token + chat
-- id) are bound at handler initialization, not threaded through
-- the effect, because every send in this codebase targets the
-- same @TELEGRAM_ALERT_CHAT_ID@. If multi-channel support is
-- ever needed, switch the effect to take a 'ChatId' argument.
--
-- The IO handler uses @http-client@ + @http-client-tls@ directly
-- rather than depending on 'Krikit.Agent.Ops.Effect.Probe'.
-- Telegram's error model (rate-limit 429s, ApiError descriptions)
-- benefits from typed handling separate from the generic HTTP
-- probe abstraction.
module Krikit.Agent.Ops.Effect.Telegram
    ( -- * Types
      Telegram
    , BotToken (..)
    , ChatId (..)
    , MessageId (..)
    , BotCreds (..)
    , TelegramError (..)

      -- * Smart constructors
    , sendMessage

      -- * Handlers
    , runTelegramIO
    , runTelegramMock
    , runTelegramCapture

      -- * Bootstrap
    , botCredsFromEnv
    ) where

import           Control.Exception          (SomeException, try)
import           Data.Aeson                 ((.:), (.:?))
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Types           as AT
import qualified Data.ByteString.Lazy       as LBS
import           Data.IORef                 (IORef, modifyIORef')
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Encoding.Error   as TE

import           Effectful                  (Eff, Effect, IOE, liftIO, (:>))
import           Effectful.Dispatch.Dynamic (interpret)
import           Effectful.TH               (makeEffect)

import           Network.HTTP.Client        (Manager)
import qualified Network.HTTP.Client        as HC
import qualified Network.HTTP.Types         as HT

import           System.Environment         (lookupEnv)

-- =============================================================================
-- Types
-- =============================================================================

-- | A Telegram bot token, e.g. @123456:ABC-DEF...@. Newtype
-- wrapper so it can't accidentally be 'show'n into a log line --
-- the 'Show' instance redacts.
newtype BotToken = BotToken { unBotToken :: Text }
    deriving stock (Eq)

-- | Same redaction for show -- avoids leaking creds into traces /
-- error messages.
instance Show BotToken where
    show _ = "BotToken <redacted>"

-- | A Telegram chat or channel id. Stored as 'Text' (rather than
-- 'Int') because group/channel ids carry a leading minus sign and
-- are sometimes 64-bit in disguise; treating them opaquely sidesteps
-- format surprises.
newtype ChatId = ChatId { unChatId :: Text }
    deriving stock (Eq, Show)

-- | The id of a successfully-sent message, returned by the Bot API.
newtype MessageId = MessageId { unMessageId :: Int }
    deriving stock (Eq, Show)

-- | What the IO handler needs to make API calls.
data BotCreds = BotCreds
    { bcToken  :: !BotToken
    , bcChatId :: !ChatId
    }
    deriving stock (Eq, Show)

-- | Why a 'SendMessage' failed.
data TelegramError
    = TelegramNetwork !Text
      -- ^ transport-level (TLS, DNS, timeout, etc.); body is the
      --   exception text, capped to keep logs sane.
    | TelegramApi !Int !Text
      -- ^ the Bot API returned a non-2xx HTTP status. Carries the
      --   status code and the @description@ field from the body
      --   (or the raw body, capped, if JSON didn't parse).
    | TelegramMalformedReply !Text
      -- ^ HTTP succeeded but the response wasn't shape we expected.
    | TelegramMissingCreds
      -- ^ creds were absent at handler init; sends are no-ops with
      --   this error returned for visibility.
    deriving stock (Eq, Show)

-- =============================================================================
-- Effect
-- =============================================================================

data Telegram :: Effect where
    SendMessage :: Text -> Telegram m (Either TelegramError MessageId)

makeEffect ''Telegram

-- =============================================================================
-- Real handler
-- =============================================================================

-- | Real IO handler. Build a 'Manager' once at outer scope and
-- pass it in; reuse with other HTTP-using code is encouraged.
--
-- If creds are absent, every 'SendMessage' returns
-- 'TelegramMissingCreds'. This matches @monitor.py@'s behaviour
-- of logging \"telegram creds missing; skipping\" rather than
-- crashing.
runTelegramIO
    :: IOE :> es
    => Manager
    -> Maybe BotCreds
    -> Eff (Telegram : es) a
    -> Eff es a
runTelegramIO mgr mcreds = interpret $ \_ -> \case
    SendMessage text -> case mcreds of
        Nothing    -> pure (Left TelegramMissingCreds)
        Just creds -> liftIO (performSend mgr creds text)

-- | One actual API call.
performSend :: Manager -> BotCreds -> Text -> IO (Either TelegramError MessageId)
performSend mgr creds text = do
    let url = sendMessageUrl (bcToken creds)
    result <- try @SomeException $ do
        base <- HC.parseRequest url
        let req = HC.urlEncodedBody
                    [ ("chat_id", TE.encodeUtf8 (unChatId (bcChatId creds)))
                    , ("text",    TE.encodeUtf8 text)
                    , ("disable_web_page_preview", "true")
                    ]
                    base
        HC.httpLbs req mgr
    case result of
        Left e -> pure (Left (TelegramNetwork (capLength 500 (T.pack (show e)))))
        Right r ->
            let statusCode = HT.statusCode (HC.responseStatus r)
                bodyTxt    = decodeLenient (HC.responseBody r)
            in  if statusCode >= 200 && statusCode < 300
                    then pure (parseSendResult bodyTxt)
                    else pure (Left (TelegramApi statusCode
                                       (extractDescription bodyTxt)))

sendMessageUrl :: BotToken -> String
sendMessageUrl (BotToken t) =
    "https://api.telegram.org/bot" <> T.unpack t <> "/sendMessage"

-- | Pull @result.message_id@ out of a successful Bot API response.
-- The shape is @{"ok": true, "result": {"message_id": N, ...}}@.
parseSendResult :: Text -> Either TelegramError MessageId
parseSendResult body =
    case A.eitherDecode (LBS.fromStrict (TE.encodeUtf8 body)) of
        Left err -> Left (TelegramMalformedReply
                            ("not JSON: " <> capLength 200 (T.pack err)))
        Right v  -> case AT.parseEither sendResultParser v of
            Right mid -> Right (MessageId mid)
            Left  err -> Left (TelegramMalformedReply (T.pack err))
  where
    sendResultParser = A.withObject "Telegram.sendMessage reply" $ \o -> do
        ok <- o .: "ok"
        if not ok
            then fail "ok=false in reply"
            else do
                result <- o .: "result"
                A.withObject "Telegram message" (\m -> m .: "message_id") result

-- | Pull @description@ out of a Bot API error body, if it's JSON.
-- Otherwise return the raw body (capped). Either way this is
-- what gets surfaced to the operator.
extractDescription :: Text -> Text
extractDescription body =
    case A.eitherDecode (LBS.fromStrict (TE.encodeUtf8 body)) of
        Left _  -> capLength 200 body
        Right v -> case AT.parseEither descParser v of
            Right d -> d
            Left _  -> capLength 200 body
  where
    descParser = A.withObject "Telegram error" $ \o ->
        o .:? "description" >>= maybe (pure (capLength 200 body)) pure

-- =============================================================================
-- Mock + capture handlers
-- =============================================================================

-- | Discard handler: every send returns @Right (MessageId 0)@.
-- For tests that don't care what was sent.
runTelegramMock :: Eff (Telegram : es) a -> Eff es a
runTelegramMock = interpret $ \_ -> \case
    SendMessage _ -> pure (Right (MessageId 0))

-- | Capture handler: every sent message text is appended (in send
-- order) to the supplied 'IORef'. Returns success. Used by tests
-- that want to assert which messages went out.
runTelegramCapture
    :: IOE :> es => IORef [Text] -> Eff (Telegram : es) a -> Eff es a
runTelegramCapture ref = interpret $ \_ -> \case
    SendMessage text -> do
        liftIO (modifyIORef' ref (++ [text]))
        pure (Right (MessageId 0))

-- =============================================================================
-- Bootstrap helpers
-- =============================================================================

-- | Read 'BotCreds' from the canonical env-var pair set by the
-- @channels.env@ wrapper:
--
--   * @TELEGRAM_BOT_TOKEN@
--   * @TELEGRAM_ALERT_CHAT_ID@
--
-- Returns 'Nothing' when either variable is unset or empty;
-- 'runTelegramIO' propagates this as 'TelegramMissingCreds' so
-- the operator sees a clear log line.
botCredsFromEnv :: IO (Maybe BotCreds)
botCredsFromEnv = do
    mtok  <- lookupEnv "TELEGRAM_BOT_TOKEN"
    mchat <- lookupEnv "TELEGRAM_ALERT_CHAT_ID"
    pure $ do
        tok  <- nonEmpty mtok
        chat <- nonEmpty mchat
        pure BotCreds
            { bcToken  = BotToken (T.pack tok)
            , bcChatId = ChatId   (T.pack chat)
            }
  where
    nonEmpty (Just s) | not (null s) = Just s
    nonEmpty _                       = Nothing

-- =============================================================================
-- Helpers
-- =============================================================================

decodeLenient :: LBS.ByteString -> Text
decodeLenient = TE.decodeUtf8With TE.lenientDecode . LBS.toStrict

-- | Cap a 'Text' to at most @n@ characters with an ellipsis suffix.
-- Used everywhere a remote-error string flows into a log or
-- 'TelegramError' to keep digest messages bounded.
capLength :: Int -> Text -> Text
capLength n t
    | T.length t <= n = t
    | otherwise       = T.take n t <> "…"
