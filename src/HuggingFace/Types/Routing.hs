{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Types.Routing
-- Description : Inference model identities and provider selection
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Types.Routing
    ( Provider, parseProvider, providerText
    , Routing (..), InferenceModel, parseInferenceModel, inferenceModelText
    , modelRepository, modelRouting, modelFromRepo, routeModel
    , ModelName, parseModelName, modelNameText
    ) where

import Data.Aeson
import Data.Text (Text)
import qualified Text.Parsec as P
import HuggingFace.Error (HuggingFaceError)
import HuggingFace.Utils (TextParser, parseValue, parseTextJSON, parseLabel, takeWhile1P, asciiAlphaNum)
import HuggingFace.Types.Repository (RepoId, repoIdText, repoIdParser)

-- | Provider-resolved model name; it need not be a Hub repository ID.
newtype ModelName = ModelName
    { modelNameText :: Text
    -- ^ Render a ModelName.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Text

-- | Parse a ModelName from external text.
parseModelName :: Text -> Either HuggingFaceError ModelName
parseModelName = fmap ModelName . parseLabel "ModelName"

instance FromJSON ModelName where parseJSON = parseTextJSON "ModelName" parseModelName

-- | Provider routing slug; policy names are reserved for Routing constructors.
newtype Provider = Provider
    { providerText :: Text
    -- ^ Render a Provider.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Text

-- | Parse a Provider from external text.
parseProvider :: Text -> Either HuggingFaceError Provider
parseProvider = parseValue "Expected a provider slug, not a routing policy" providerParser

instance FromJSON Provider where parseJSON = parseTextJSON "Provider" parseProvider

-- | Router selection policy or a named provider. An absent routing choice uses
-- the server default; it is distinct from explicitly selecting 'Fastest'.
data Routing = Fastest | Cheapest | Preferred | Via Provider deriving stock (Eq, Ord, Show)

-- | A Hub model repository paired with an optional parsed routing choice.
data InferenceModel = InferenceModel
    { modelRepository :: RepoId -- ^ The repository referenced by a routed model.
    , modelRouting :: Maybe Routing -- ^ Optional explicit routing choice.
    } deriving stock (Eq, Ord, Show)

-- | Parse @owner/model@ or @owner/model:provider-or-policy@.
parseInferenceModel :: Text -> Either HuggingFaceError InferenceModel
parseInferenceModel = parseValue "Expected a model repository with at most one routing suffix" $
    InferenceModel <$> repoIdParser <*> P.optionMaybe (P.char ':' *> routingParser)

routingParser :: TextParser Routing
routingParser = do
    slug <- takeWhile1P (\c -> asciiAlphaNum c || c == '-')
    pure $ case slug of
        "fastest" -> Fastest
        "cheapest" -> Cheapest
        "preferred" -> Preferred
        _ -> Via (Provider slug)

providerParser :: TextParser Provider
providerParser = do
    routing <- routingParser
    case routing of
        Via provider -> pure provider
        _ -> P.parserFail "Reserved routing policy"

-- | Choose the router's default policy for an existing repository identifier.
modelFromRepo :: RepoId -> InferenceModel
modelFromRepo repository = InferenceModel repository Nothing

-- | Select a routing policy without converting an identifier back to text.
routeModel :: RepoId -> Routing -> InferenceModel
routeModel repository routing = InferenceModel repository (Just routing)

-- | Render a model together with its optional routing suffix.
inferenceModelText :: InferenceModel -> Text
inferenceModelText (InferenceModel repository routing) = repoIdText repository <> maybe "" ((":" <>) . render) routing
  where
    render Fastest = "fastest"
    render Cheapest = "cheapest"
    render Preferred = "preferred"
    render (Via provider) = providerText provider

instance ToJSON InferenceModel where toJSON = toJSON . inferenceModelText
instance FromJSON InferenceModel where parseJSON = parseTextJSON "InferenceModel" parseInferenceModel
