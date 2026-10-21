{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Internal.HTTP
-- Description : HTTP transport and credential-safe redirects
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
--
-- Transport implementation shared by the Hub and inference APIs. This module
-- is exposed for callers building additional operations; 'requestFor' prepares
-- requests using the client's configuration. Import it explicitly rather than
-- through the "HuggingFace" convenience facade.
module HuggingFace.Internal.HTTP
    ( requestFor, getJSON, postJSON, getPage, continuePage, withBody
    ) where

import Control.Exception (catch)
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Network.HTTP.Client hiding (TooManyRedirects)
import Network.HTTP.Types
import Network.URI (URI (..), URIAuth (..), parseURIReference, relativeTo)
import HuggingFace.Error (HuggingFaceError (..))
import HuggingFace.Monad
import HuggingFace.Types.Auth (tokenBytes)
import HuggingFace.Client (Client, clientManager, clientHub, clientToken, clientTimeout)
import HuggingFace.Utils (encodeSegments, readAtMost, sameOrigin, decodeJSON)
import HuggingFace.Types.Pagination (Page (..), NextPage, nextPageFromHeaders, nextPageURL)

-- | Prepare a request for a trusted origin. Encodes path segments, sets the
-- timeout and bearer token, and disables automatic redirects and status throws.
-- The supplied origin must be trusted with the client's credential.
requestFor :: Client -> Request -> [Text] -> Query -> Request
requestFor client base segments query = base
    { path = encodeSegments segments
    , queryString = renderQuery True query
    , redirectCount = 0
    , checkResponse = \_ _ -> pure ()
    , responseTimeout = responseTimeoutMicro (clientTimeout client)
    , cookieJar = Nothing
    , requestHeaders = [(hAccept, "application/json"), (hUserAgent, "huggingface-haskell/0.1.0.0")]
        ++ case clientToken client of
            Nothing -> []
            Just token -> [(hAuthorization, "Bearer " <> tokenBytes token)]
    }

-- | Execute a prepared request and decode JSON. GET redirects stay on-origin;
-- response and decoding failures use the library's redacted error types.
getJSON :: FromJSON a => Client -> Request -> HuggingFaceM a
getJSON client request = do
    (_, _, body) <- getBytes client request
    liftEither (decodeJSON body)

-- | Submit JSON with a prepared request and decode its response. POST redirects
-- are not followed; use a manager with transport retries disabled.
postJSON :: (ToJSON a, FromJSON b) => Client -> Request -> a -> HuggingFaceM b
postJSON client request payload = getJSON client request
    { method = methodPost
    , requestBody = RequestBodyLBS (encode payload)
    , requestHeaders = (hContentType, "application/json") : requestHeaders request
    }

getBytes :: Client -> Request -> HuggingFaceM (Request, ResponseHeaders, BL.ByteString)
getBytes client request = withBody False client request $ \response -> do
    chunks <- liftIO (brConsume (responseBody response))
    pure (getOriginalRequest response, responseHeaders response, BL.fromChunks chunks)

-- | Decode a JSON array and its Link continuation from a prepared request.
getPage :: FromJSON a => Client -> Request -> HuggingFaceM (Page a)
getPage client request = do
    (finalRequest, headers, body) <- getBytes client request
    items <- liftEither (decodeJSON body)
    pure (Page items (nextPageFromHeaders finalRequest headers))

-- | Fetch a continuation only if it resolves to the configured Hub origin.
continuePage :: FromJSON a => Client -> NextPage -> HuggingFaceM (Page a)
continuePage client next = case redirectTarget False (clientHub client) (nextPageURL next) of
    Right target | sameOrigin target (clientHub client) ->
        let request = requestFor client target [] []
        in getPage client request {path = path target, queryString = queryString target}
    _ -> throwError InvalidPagination

-- | Consume a successful response while its body reader is in scope. Setting
-- the first argument to 'True' allows external GET redirects and strips
-- credentials when crossing origins. HTTPS downgrades are always rejected.
-- The response and its socket are closed before following another redirect.
-- Only GET redirects are followed; replaying a billable POST is never implicit.
-- Typed callback errors close the response before propagating to the caller.
withBody :: Bool -> Client -> Request -> (Response BodyReader -> HuggingFaceM a) -> HuggingFaceM a
withBody allowExternal client initial consume = go (5 :: Int) initial
  where
    go remaining request = do
        outcome <- liftHuggingFaceM $ catchHTTP $
            withResponse request (clientManager client) $ \response -> runHuggingFaceM $ do
                let code = statusCode (responseStatus response)
                if code >= 200 && code < 300
                    then Right <$> consume response
                    else if method request == methodGet && code `elem` [301, 302, 303, 307, 308]
                        then if remaining <= 0 then throwError TooManyRedirects else
                            case lookup hLocation (responseHeaders response) of
                                Nothing -> throwError UnsafeRedirect
                                Just location -> Left <$> liftEither (redirectTarget allowExternal request location)
                        else liftIO (responseError response) >>= throwError
        case outcome of
            Right value -> pure value
            Left target -> go (remaining - 1) target

catchHTTP :: IO (Either HuggingFaceError a) -> IO (Either HuggingFaceError a)
catchHTTP action = action `catch` handler
  where
    handler :: HttpException -> IO (Either HuggingFaceError a)
    handler (HttpExceptionRequest _ ResponseTimeout) = pure (Left (TransportError "Response timeout"))
    handler (HttpExceptionRequest _ ConnectionTimeout) = pure (Left (TransportError "Connection timeout"))
    handler _ = pure (Left (TransportError "HTTP transport failure"))

responseError :: Response BodyReader -> IO HuggingFaceError
responseError response = do
    body <- readAtMost 65536 (responseBody response)
    let headers = responseHeaders response
    pure HttpError
        { errorStatus = statusCode (responseStatus response)
        , errorBody = body
        , errorRequestId = lookup "x-request-id" headers
        , errorRetryAfter = lookup "retry-after" headers
        }

redirectTarget :: Bool -> Request -> ByteString -> Either HuggingFaceError Request
redirectTarget allowExternal current location = do
    reference <- maybe (Left UnsafeRedirect) Right (parseURIReference (B8.unpack location))
    let uri = reference `relativeTo` getUri current
    case uriAuthority uri of
        Just auth | null (uriUserInfo auth) && uriScheme uri `elem` ["http:", "https:"] -> pure ()
        _ -> Left UnsafeRedirect
    target <- maybe (Left UnsafeRedirect) Right (parseRequest (show uri) :: Maybe Request)
    if (secure current && not (secure target)) || (not allowExternal && not (sameOrigin current target))
        then Left UnsafeRedirect
        else Right target
            { method = methodGet
            , redirectCount = 0
            , checkResponse = \_ _ -> pure ()
            , responseTimeout = responseTimeout current
            , cookieJar = Nothing
            , requestHeaders = if sameOrigin current target then requestHeaders current
                else filter (\(key, _) -> key /= hAuthorization && key /= hCookie) (requestHeaders current)
            }
