{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Client
-- Description : Connection pooling, authentication, and endpoint configuration
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Client
    ( Client, ClientConfig (..), defaultClientConfig, newClient, newClientWithManager
    , clientManager, clientHub, clientInference, clientToken, clientTimeout
    , Endpoint, parseEndpoint, endpointText, defaultHubEndpoint, defaultInferenceEndpoint
    ) where

import Control.Monad (when)
import Data.Char (isDigit, ord)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client
    ( Manager, Request, defaultRequest, getUri, host, managerRetryableException
    , newManager, parseRequest, port, secure )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.URI (parseURI)
import qualified Text.Parsec as P
import HuggingFace.Error (HuggingFaceError (..))
import HuggingFace.Utils (TextParser, parseText, matched)
import HuggingFace.Types.Auth (Token)
import HuggingFace.Types.Parameters (PositiveInt, positiveIntValue, defaultTimeout)

-- | Reusable connection pool, parsed endpoints, and credentials. Construct a
-- client with 'newClient' or 'newClientWithManager'.
data Client = Client
    { clientManager :: Manager -- ^ Connection manager used by this client.
    , clientHub :: Request -- ^ Parsed Hub origin, without request credentials or endpoint-specific paths.
    , clientInference :: Request -- ^ Parsed inference origin, without request credentials or task-specific paths.
    , clientToken :: Maybe Token -- ^ Optional credential for this client's configured origins.
    , clientTimeout :: Int -- ^ Response timeout in microseconds.
    }

-- | Parsed HTTP(S) origin. The request representation is retained so client
-- construction never reparses URLs. Plain HTTP is restricted to loopback.
newtype Endpoint = Endpoint Request

instance Show Endpoint where
    show = show . endpointText

instance Eq Endpoint where
    a == b = endpointText a == endpointText b

-- | Parse an HTTPS origin (or loopback HTTP origin), with no credentials,
-- non-root path, query, or fragment. Parse errors never echo the input.
parseEndpoint :: Text -> Either HuggingFaceError Endpoint
parseEndpoint input = do
    (tls, parsedPort) <- parseText malformed origin input
    -- Delegate complete RFC URI / IPv6 syntax to network-uri (itself backed by
    -- Parsec), then adapt to http-client's required String API. The origin
    -- grammar has already consumed the entire input and bounded the port.
    _ <- maybe (Left malformed) Right (parseURI (T.unpack input))
    request <- maybe (Left malformed) Right (parseRequest (T.unpack input) :: Maybe Request)
    pure (Endpoint request {secure = tls, port = parsedPort})
  where
    malformed = InvalidInput "Endpoint requires an HTTPS origin, or loopback HTTP origin, with a port in 1–65535 and no credentials, path, query, or fragment"

origin :: TextParser (Bool, Int)
origin = do
    _ <- P.string "http"
    tls <- P.option False (True <$ P.char 's')
    _ <- P.string "://"
    hostname <- matched $
        P.between (P.char '[') (P.char ']') (P.skipMany1 (P.noneOf "]"))
            P.<|> P.skipMany1 (P.satisfy (\c -> c /= ':' && c /= '/' && c /= '?' && c /= '#' && c /= '@' && c /= '[' && c /= ']'))
    parsedPort <- P.option (if tls then 443 else 80) (P.char ':' *> portNumber)
    P.optional (P.char '/')
    if tls || hostname `elem` ["localhost", "127.0.0.1", "[::1]"]
        then pure (tls, parsedPort)
        else P.parserFail "Plain HTTP requires loopback"

-- | Render the parsed origin. A missing root slash is normalized to @/@.
endpointText :: Endpoint -> Text
endpointText (Endpoint request) = T.pack (show (getUri request))

-- | Public Hub origin.
defaultHubEndpoint :: Endpoint
defaultHubEndpoint = Endpoint defaultRequest {secure = True, host = "huggingface.co", port = 443}

-- | Public Inference Providers router origin.
defaultInferenceEndpoint :: Endpoint
defaultInferenceEndpoint = Endpoint defaultRequest {secure = True, host = "router.huggingface.co", port = 443}

-- | Trusted client configuration built from parsed endpoints and a positive
-- timeout. The same token is sent to both origins: only configure endpoints
-- that you trust with it.
data ClientConfig = ClientConfig
    { hubEndpoint :: Endpoint -- ^ Default: 'defaultHubEndpoint'.
    , inferenceEndpoint :: Endpoint -- ^ Default: 'defaultInferenceEndpoint'.
    , accessToken :: Maybe Token -- ^ Optional for public Hub reads.
    , timeoutMicroseconds :: PositiveInt -- ^ Positive timeout, default thirty seconds.
    } deriving (Show)

-- | Public Hugging Face endpoints with no token and a thirty-second timeout.
defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig defaultHubEndpoint defaultInferenceEndpoint Nothing defaultTimeout

-- | Create a reusable TLS connection pool. Configuration is already parsed;
-- constructing the client does not perform any network requests.
newClient :: ClientConfig -> IO Client
newClient config = do
    manager <- newManager tlsManagerSettings {managerRetryableException = const False}
    pure (newClientWithManager manager config)

-- | Use a caller-owned manager. The caller controls its TLS, proxy, and lifetime
-- settings; request-altering manager hooks must not reintroduce credentials.
-- Set @managerRetryableException = const False@ when creating it: http-client's
-- default retry policy can replay a POST if a reused connection loses a response.
-- The no-automatic-retry guarantee requires this setting for custom managers.
newClientWithManager :: Manager -> ClientConfig -> Client
newClientWithManager manager config =
    let Endpoint hub = hubEndpoint config
        Endpoint inference = inferenceEndpoint config
    in Client manager hub inference (accessToken config) (positiveIntValue (timeoutMicroseconds config))

-- Accumulate with a bounded Int; fail before a later digit could overflow.
-- Leading zeroes are allowed, but a zero port is not a usable endpoint.
portNumber :: TextParser Int
portNumber = do
    first <- digit
    number <- rest first
    when (number == 0) (P.parserFail "Zero port")
    pure number
  where
    digit = (\c -> ord c - ord '0') <$> P.satisfy isDigit
    rest n = P.option n $ do
        d <- digit
        let next = 10 * n + d
        when (next > 65535) (P.parserFail "Port out of range")
        rest next
