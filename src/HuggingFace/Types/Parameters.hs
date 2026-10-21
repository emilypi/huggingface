{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Types.Parameters
-- Description : Constrained counts, timeouts, and sampling parameters
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Types.Parameters
    ( PositiveInt, mkPositiveInt, positiveIntValue, defaultTimeout
    , ChatTemperature, mkChatTemperature, chatTemperatureValue
    , TopP, mkTopP, topPValue
    , GenerationTemperature, mkGenerationTemperature, generationTemperatureValue
    ) where

import Data.Aeson
import HuggingFace.Error (HuggingFaceError (..))
import HuggingFace.Utils (fromEither, inRange)

-- | Strictly positive machine integer for counts, limits, and microseconds.
-- No 'Num' instance is supplied, so arithmetic cannot bypass construction.
newtype PositiveInt = PositiveInt
    { positiveIntValue :: Int
    -- ^ Recover a positive integer.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Int

-- | Refine an external integer into a positive count.
mkPositiveInt :: Int -> Either HuggingFaceError PositiveInt
mkPositiveInt n
    | n > 0 = Right (PositiveInt n)
    | otherwise = Left (InvalidInput "Expected a positive integer")

-- | Thirty seconds in microseconds.
defaultTimeout :: PositiveInt
defaultTimeout = PositiveInt 30000000

instance FromJSON PositiveInt where parseJSON v = parseJSON v >>= fromEither . mkPositiveInt

-- | A finite value in [0,2].
newtype ChatTemperature = ChatTemperature
    { chatTemperatureValue :: Double
    -- ^ Recover the numeric value.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Double

-- | Refine a number into a finite value in [0,2].
mkChatTemperature :: Double -> Either HuggingFaceError ChatTemperature
mkChatTemperature t
    | inRange 0 2 t = Right (ChatTemperature t)
    | otherwise = Left (InvalidInput "ChatTemperature requires a finite value in [0,2]")

instance FromJSON ChatTemperature where parseJSON v = parseJSON v >>= fromEither . mkChatTemperature

-- | A finite value in (0,1].
newtype TopP = TopP
    { topPValue :: Double
    -- ^ Recover the numeric value.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Double

-- | Refine a number into a finite value in (0,1].
mkTopP :: Double -> Either HuggingFaceError TopP
mkTopP t
    | inRange 0 1 t && t > 0 = Right (TopP t)
    | otherwise = Left (InvalidInput "TopP requires a finite value in (0,1]")

instance FromJSON TopP where parseJSON v = parseJSON v >>= fromEither . mkTopP

-- | A positive finite value.
newtype GenerationTemperature = GenerationTemperature
    { generationTemperatureValue :: Double
    -- ^ Recover the numeric value.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Double

-- | Refine a number into a positive finite value.
mkGenerationTemperature :: Double -> Either HuggingFaceError GenerationTemperature
mkGenerationTemperature t
    | not (isNaN t || isInfinite t) && t > 0 = Right (GenerationTemperature t)
    | otherwise = Left (InvalidInput "GenerationTemperature requires a positive finite value")

instance FromJSON GenerationTemperature where parseJSON v = parseJSON v >>= fromEither . mkGenerationTemperature
