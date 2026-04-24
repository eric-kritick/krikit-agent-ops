{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

-- | HTTP GET and POST probes for the smoke test.
--
-- Two operations: 'HttpGet' for liveness probes (Ollama /api/tags),
-- 'HttpPost' for Telegram's sendMessage endpoint. Real handler uses
-- http-client + TLS; mock handler returns canned bodies for tests.
module Krikit.Agent.Ops.Effect.Probe
    ( -- * Effect
      Probe
    , ProbeError (..)
    , Url (..)
    , urlString

      -- * Smart constructors
    , httpGet
    , httpPost

      -- * Handlers
    , runProbeIO
    , runProbeMock
    , defaultManager
    ) where

import           Control.Exception           (SomeException, try)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import qualified Data.Text.Encoding.Error    as TE

import           Effectful                   (Eff, Effect, IOE, liftIO, (:>))
import           Effectful.Dispatch.Dynamic  (interpret)
import           Effectful.TH                (makeEffect)

import           Network.HTTP.Client         (Manager, Response)
import qualified Network.HTTP.Client         as HC
import qualified Network.HTTP.Client.TLS     as HTLS
import qualified Network.HTTP.Types          as HT

-- | Why an HTTP probe failed.
data ProbeError
    = ProbeNetwork !Text        -- ^ couldn't connect / transport error
    | ProbeStatus  !Int         -- ^ server returned non-2xx
    deriving stock (Eq, Show)

-- | Typed wrapper for a URL. Prevents accidentally passing a path or
-- a free-form string where a URL was expected. Constructor is exported
-- because URLs in this codebase always come from config + literal
-- concatenation — there's no need for smart validation.
newtype Url = Url Text
    deriving stock   (Eq, Show)
    deriving newtype (Ord)

urlString :: Url -> String
urlString (Url u) = T.unpack u

data Probe :: Effect where
    HttpGet  :: Url -> Probe m (Either ProbeError Text)

    -- | Form-encoded POST; for Telegram's sendMessage. (chat_id, text) pairs.
    HttpPost :: Url -> [(Text, Text)] -> Probe m (Either ProbeError Text)

makeEffect ''Probe

-- | Real handler: creates a TLS-capable Manager once at outer scope and
-- reuses it for every call. Takes a pre-built Manager so callers can share
-- it with other HTTP-using code if they want.
runProbeIO :: IOE :> es => Manager -> Eff (Probe : es) a -> Eff es a
runProbeIO mgr = interpret $ \_ -> \case
    HttpGet url ->
        liftIO (performGet mgr (urlString url))
    HttpPost url params ->
        liftIO (performPost mgr (urlString url) params)

-- | Convenience: build the default TLS Manager if the caller doesn't
-- want to manage one themselves.
defaultManager :: IO Manager
defaultManager = HC.newManager HTLS.tlsManagerSettings
{-# NOINLINE defaultManager #-}

performGet :: Manager -> String -> IO (Either ProbeError Text)
performGet mgr url = do
    result <- try @SomeException $ do
        req  <- HC.parseRequest url
        resp <- HC.httpLbs req mgr
        pure resp
    pure (interpretResponse result)

performPost
    :: Manager -> String -> [(Text, Text)]
    -> IO (Either ProbeError Text)
performPost mgr url params = do
    result <- try @SomeException $ do
        base <- HC.parseRequest url
        let req =
                HC.urlEncodedBody
                    [ (TE.encodeUtf8 k, TE.encodeUtf8 v)
                    | (k, v) <- params ]
                    base
        resp <- HC.httpLbs req mgr
        pure resp
    pure (interpretResponse result)

interpretResponse
    :: Either SomeException (Response LBS.ByteString)
    -> Either ProbeError Text
interpretResponse = \case
    Left e -> Left (ProbeNetwork (T.pack (show e)))
    Right r ->
        let statusCode = HT.statusCode (HC.responseStatus r)
        in  if statusCode >= 200 && statusCode < 300
                then Right (decodeLenient (HC.responseBody r))
                else Left (ProbeStatus statusCode)

decodeLenient :: LBS.ByteString -> Text
decodeLenient = TE.decodeUtf8With TE.lenientDecode . LBS.toStrict

-- === Mock handler ===========================================================

-- | Mock handler: responses keyed by URL. Covers both GET and POST
-- cases (POST bodies aren't asserted in v1 tests).
runProbeMock
    :: Map Url (Either ProbeError Text)
    -> Eff (Probe : es) a
    -> Eff es a
runProbeMock responses = interpret $ \_ -> \case
    HttpGet url ->
        pure (lookupMock url)
    HttpPost url _ ->
        pure (lookupMock url)
  where
    lookupMock k@(Url raw) =
        case Map.lookup k responses of
            Just r  -> r
            Nothing -> Left (ProbeNetwork ("no mock for " <> raw))

