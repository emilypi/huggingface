{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Inference
-- Description : Hosted chat completions and HF Inference tasks
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Inference
    ( ChatMessage (..), userMessage, systemMessage, assistantMessage
    , ChatRequest (..), defaultChatRequest, ChatCompletion (..), ChatChoice (..), Usage (..)
    , chatCompletion
    , GenerationParameters (..), defaultGenerationParameters, GeneratedText (..), textGeneration
    , Classification (..), classifyText, featureExtraction
    , infer
    ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import HuggingFace.Internal.HTTP
import HuggingFace.Client (Client, clientInference)
import HuggingFace.Error (HuggingFaceError (..))
import HuggingFace.Monad (HuggingFaceM, throwError)
import HuggingFace.Types.Chat
import HuggingFace.Types.Repository (RepoId, repoIdText)
import HuggingFace.Types.Tasks

-- | Submit a non-streaming chat completion. Calls may incur provider charges.
-- This operation is never automatically retried or replayed on a redirect.
chatCompletion
    :: Client
    -> ChatRequest
    -> HuggingFaceM ChatCompletion
chatCompletion client =
    postJSON client
      (requestFor
        client (clientInference client)
        ["v1", "chat", "completions"]
        []
        )

newtype Generations = Generations { _getGenerations :: [GeneratedText] }

instance FromJSON Generations where
    parseJSON value = Generations <$> (parseJSON value <|> ((: []) <$> parseJSON value))

-- | Generate text using a model served by @hf-inference@. Task availability
-- depends on the model/provider; use 'chatCompletion' for router-hosted LLMs.
textGeneration
    :: Client
    -> RepoId
    -> Text
    -> GenerationParameters
    -> HuggingFaceM [GeneratedText]
textGeneration client model prompt parameters =
    _getGenerations <$>
        infer client model (object ["inputs" .= prompt, "parameters" .= parameters, "stream" .= False])

-- | Classify one input. Accepts the flat and single-batch response shapes used
-- by HF Inference. Multiple batches for a single input are rejected.
classifyText :: Client -> RepoId -> Text -> HuggingFaceM [Classification]
classifyText client model input = do
    result <- infer client model (object ["inputs" .= input])
    either (const (throwError DecodeError)) pure (parseEither parseLabels result)
  where
    parseLabels value = parseJSON value <|> do
        batches <- parseJSON value :: Parser [[Classification]]
        case batches of
            [labels] -> pure labels
            _ -> fail "Expected one classification batch"

-- | Compute sentence embeddings for a nonempty batch. Uses the explicit
-- @/pipeline/feature-extraction@ route. Models returning token-level tensors
-- instead of one vector per input are outside this convenience function's schema.
featureExtraction :: Client -> RepoId -> NonEmpty Text -> HuggingFaceM [[Double]]
featureExtraction client model inputs =
    postJSON client
        (requestFor client (clientInference client)
            (["hf-inference", "models"] ++ T.splitOn "/" (repoIdText model) ++ ["pipeline", "feature-extraction"]) [])
        (object ["inputs" .= inputs])

-- | Send a caller-defined JSON request to a model's default HF Inference task.
-- Use this for task-specific request/response schemas beyond the typed helpers.
-- This does not route to arbitrary providers. Binary and SSE responses are not
-- decoded here. No model code executes locally, and requests are not retried.
infer :: (ToJSON request, FromJSON response) => Client -> RepoId -> request -> HuggingFaceM response
infer client model = postJSON client
    (requestFor client (clientInference client) (["hf-inference", "models"] ++ T.splitOn "/" (repoIdText model)) [])
