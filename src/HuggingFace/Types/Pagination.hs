{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Types.Pagination
-- Description : Pages and parsed Link header continuations
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Types.Pagination
    ( Page (..), NextPage, nextPageFromHeaders, nextPageURL
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Char (isAscii, isAlphaNum, ord, toLower)
import Data.Either (fromRight)
import Data.Maybe (catMaybes, listToMaybe)
import Network.HTTP.Client (Request, getUri)
import Network.HTTP.Types (ResponseHeaders)
import Network.URI (parseURIReference, relativeTo)
import qualified Text.Parsec as P
import qualified Text.Parsec.ByteString as P
import HuggingFace.Utils (matchedBytes)

-- | One server page. The caller explicitly decides whether to fetch another.
data Page a = Page
    { pageItems :: [a] -- ^ Items in this response.
    , pageNext :: Maybe NextPage -- ^ Opaque continuation supplied by the Hub.
    } deriving stock (Eq, Show)

-- | Opaque server-provided pagination continuation.
newtype NextPage = NextPage
    { nextPageURL :: ByteString
    -- ^ Recover the continuation URL as bytes. Query parameters may contain private
    -- state; the 'Show' instance redacts them. Consuming a continuation still requires
    -- checking its origin against the client's configured Hub endpoint.
    }
    deriving (Eq) via ByteString

instance Show NextPage where
    show _ = "<NextPage>"

-- A Link header is a comma-separated list of targets and parameters. Parsing
-- quoted strings as a unit prevents embedded commas, semicolons, and escaped
-- quotes from being mistaken for delimiters or relation parameters.
links :: ByteString -> [ByteString]
links = fromRight [] . P.parse (linkHeader <* P.eof) ""

linkHeader :: P.Parser [ByteString]
linkHeader = do
    ows
    -- HTTP list recipients ignore empty members (RFC 9110, section 5.6.1.2).
    entries <- P.optionMaybe link `P.sepBy1` (P.char ',' *> ows)
    pure [target | (target, parameters) <- catMaybes entries, maybe False (elem "next" . B8.words) (lookup "rel" parameters)]
  where
    link = do
        target <- P.between (P.char '<') (P.char '>') $
            matchedBytes (P.skipMany (P.satisfy (\c -> c > ' ' && c < '\DEL' && c /= '<' && c /= '>')))
        ows
        parameters <- P.many (P.char ';' *> ows *> parameter <* ows)
        pure (target, parameters)
    parameter = do
        name <- token <* ows
        value <- P.option "" (P.char '=' *> ows *> (quoted P.<|> token))
        pure (B8.map toLower name, value)
    token = matchedBytes (P.skipMany1 (P.satisfy tokenChar))
    tokenChar c = (isAscii c && isAlphaNum c) || B8.elem c "!#$%&'*+-.^_`|~"
    quoted = P.between (P.char '"') (P.char '"') (BS.concat <$> P.many chunk)
    chunk = (P.char '\\' *> (B8.singleton <$> P.satisfy quotedPair))
        P.<|> matchedBytes (P.skipMany1 (P.satisfy quotedText))
    quotedPair c = c == '\t' || (ord c >= 32 && c /= '\DEL')
    quotedText c = quotedPair c && c /= '"' && c /= '\\'

ows :: P.Parser ()
ows = P.skipMany (P.satisfy (\c -> c == ' ' || c == '\t'))

-- | Parse the first @rel=next@ target from response Link headers and resolve it
-- against the final request URL. Malformed Link headers contribute no target.
-- The resulting handle is untrusted until the transport checks its origin.
nextPageFromHeaders :: Request -> ResponseHeaders -> Maybe NextPage
nextPageFromHeaders request headers = do
    link <- listToMaybe (concatMap (links . snd) (filter ((== "link") . fst) headers))
    let resolved = case parseURIReference (B8.unpack link) of
            Just uri -> B8.pack (show (uri `relativeTo` getUri request))
            Nothing -> link
    pure (NextPage resolved)
