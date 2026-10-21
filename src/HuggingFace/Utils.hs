{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Utils
-- Description : Shared parsing, JSON, and HTTP utilities
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Utils
    ( TextParser, parseText, parseValue, parseTextJSON, parseLabel, fromEither, decodeJSON
    , matched, matchedBytes, takeWhile1P, label, asciiAlphaNum, inRange
    , encodeSegments, readAtMost, sameOrigin
    ) where

import Data.Aeson (FromJSON, Value, eitherDecode, withText)
import qualified Data.Aeson.Types as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isAscii, isControl, isSpace, toLower)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (BodyReader, Request, brRead, secure, port, host)
import Network.HTTP.Types (urlEncode)
import qualified Text.Parsec as P
import qualified Text.Parsec.ByteString as PB
import HuggingFace.Error (HuggingFaceError (..))

-- | Parsec parser over strict 'Text', without user state.
type TextParser = P.Parsec Text ()

-- | Require complete input consumption and replace parser errors with the
-- caller's diagnostic. Parsec errors may contain input
-- tokens, so they must never escape a parser that can receive credentials.
parseText :: e -> TextParser a -> Text -> Either e a
parseText err parser = either (const (Left err)) Right . P.parse (parser <* P.eof) ""

-- | Capture consumed Text without an intermediate String or rescanning the
-- remaining input. Parsec consumes complete characters, so the byte-length
-- difference always falls on a UTF-8 boundary; takeWord8 is the safe slicer.
matched :: TextParser a -> TextParser Text
matched parser = do
    before <- P.getInput
    _ <- parser
    after <- P.getInput
    pure (TF.takeWord8 (fromIntegral (TF.lengthWord8 before - TF.lengthWord8 after)) before)

-- | Consume one or more matching characters and return the consumed text.
takeWhile1P :: (Char -> Bool) -> TextParser Text
takeWhile1P predicate = matched (P.skipMany1 (P.satisfy predicate))

-- | Whether a character is an ASCII letter or decimal digit.
asciiAlphaNum :: Char -> Bool
asciiAlphaNum c = isAscii c && isAlphaNum c

-- | Parse an open label, preserving spaces, Unicode, and punctuation, with
-- at least one non-space character and no control characters.
label :: TextParser Text
label = do
    _ <- P.lookAhead (P.skipMany (P.satisfy isSpace) *> P.satisfy (not . isSpace))
    takeWhile1P (not . isControl)

-- | Decode a JSON string using a domain parser. The first argument names the
-- expected type in Aeson diagnostics.
parseTextJSON :: Text -> (Text -> Either HuggingFaceError a) -> Value -> JSON.Parser a
parseTextJSON name parse = withText (T.unpack name) (fromEither . parse)

-- | Lift a domain result into Aeson's parser.
fromEither :: Either HuggingFaceError a -> JSON.Parser a
fromEither = either (fail . show) pure

-- | Parse a complete value, reporting 'InvalidInput' with the supplied message.
parseValue :: Text -> TextParser a -> Text -> Either HuggingFaceError a
parseValue message = parseText (InvalidInput message)

-- | Parse a complete 'label'; the first argument identifies the domain type.
parseLabel :: Text -> Text -> Either HuggingFaceError Text
parseLabel name = parseValue (name <> " requires nonblank text without control characters") label

-- | Test a finite number against inclusive lower and upper bounds.
inRange :: Double -> Double -> Double -> Bool
inRange low high t = not (isNaN t || isInfinite t) && t >= low && t <= high

-- | Decode JSON without retaining potentially sensitive parser diagnostics.
decodeJSON :: FromJSON a => BL.ByteString -> Either HuggingFaceError a
decodeJSON = either (const (Left DecodeError)) Right . eitherDecode

-- | Capture the bytes consumed by a ByteString parser.
matchedBytes :: PB.Parser a -> PB.Parser ByteString
matchedBytes parser = do
    before <- P.getInput
    _ <- parser
    after <- P.getInput
    pure (BS.take (BS.length before - BS.length after) before)

-- | Encode each path segment as UTF-8 and percent-escape it, then join the
-- segments under a leading slash. Slashes inside a segment are escaped too.
encodeSegments :: [Text] -> ByteString
encodeSegments parts = "/" <> BS.intercalate "/" (map (urlEncode True . encodeUtf8) parts)

-- | Read at most the requested number of bytes, or none for a nonpositive limit.
-- The last chunk may exceed the limit; its excess bytes are consumed and
-- discarded. Use this for a bounded preview before closing the response.
readAtMost :: Int -> BodyReader -> IO ByteString
readAtMost limit reader = BS.concat . reverse <$> go limit []
  where
    go n chunks
        | n <= 0 = pure chunks
        | otherwise = do
            chunk <- brRead reader
            if BS.null chunk then pure chunks
                else go (n - BS.length chunk) (BS.take n chunk : chunks)

-- | Compare TLS, port, and case-insensitive hostname; paths are ignored.
sameOrigin :: Request -> Request -> Bool
sameOrigin a b = secure a == secure b && port a == port b
    && B8.map toLower (host a) == B8.map toLower (host b)
