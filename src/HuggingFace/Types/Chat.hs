{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Types.Chat
-- Description : Chat identities, messages, requests, and completion responses
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Types.Chat
    ( ChatRole, parseChatRole, chatRoleText
    , ToolCallId, parseToolCallId, toolCallIdText
    , ParticipantName, parseParticipantName, participantNameText
    , CompletionId, parseCompletionId, completionIdText
    , FinishReason, parseFinishReason, finishReasonText
    , systemRole, userRole, assistantRole, toolRole, developerRole
    , ChatMessage (..), userMessage, systemMessage, assistantMessage
    , ChatRequest (..), defaultChatRequest
    , ChatCompletion (..), ChatChoice (..), Usage (..)
    ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Text.Parsec as P
import HuggingFace.Error (HuggingFaceError)
import HuggingFace.Utils (parseTextJSON, parseLabel, parseValue, label)
import HuggingFace.Types.Parameters (PositiveInt, ChatTemperature, TopP)
import HuggingFace.Types.Routing (InferenceModel, ModelName)

-- | Open chat role; common roles have named constants.
newtype ChatRole = ChatRole
    { chatRoleText :: Text
    -- ^ Render a ChatRole.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Text

-- | Parse a ChatRole from external text.
parseChatRole :: Text -> Either HuggingFaceError ChatRole
parseChatRole = fmap ChatRole . parseLabel "ChatRole"

instance FromJSON ChatRole where parseJSON = parseTextJSON "ChatRole" parseChatRole

-- | Opaque provider-assigned tool call identifier.
newtype ToolCallId = ToolCallId
    { toolCallIdText :: Text
    -- ^ Render a ToolCallId.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Text

-- | Parse a ToolCallId from external text.
parseToolCallId :: Text -> Either HuggingFaceError ToolCallId
parseToolCallId = fmap ToolCallId . parseLabel "ToolCallId"

instance FromJSON ToolCallId where parseJSON = parseTextJSON "ToolCallId" parseToolCallId

-- | Open chat participant name; provider-specific naming rules remain server-side.
newtype ParticipantName = ParticipantName
    { participantNameText :: Text
    -- ^ Render a ParticipantName.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Text

-- | Parse a ParticipantName from external text.
parseParticipantName :: Text -> Either HuggingFaceError ParticipantName
parseParticipantName = fmap ParticipantName . parseLabel "ParticipantName"

instance FromJSON ParticipantName where parseJSON = parseTextJSON "ParticipantName" parseParticipantName

-- | Opaque provider-assigned completion identifier. An empty identifier is
-- preserved because Hugging Face / TGI can return it on successful responses.
newtype CompletionId = CompletionId
    { completionIdText :: Text
    -- ^ Render a CompletionId.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Text

-- | Parse a CompletionId from external text.
parseCompletionId :: Text -> Either HuggingFaceError CompletionId
parseCompletionId = parseValue "Expected a completion identifier without controls" $
    CompletionId <$> (label P.<|> ("" <$ P.eof))

instance FromJSON CompletionId where parseJSON = parseTextJSON "CompletionId" parseCompletionId

-- | Open provider stop reason; unknown reasons are preserved.
newtype FinishReason = FinishReason
    { finishReasonText :: Text
    -- ^ Render a FinishReason.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Text

-- | Parse a FinishReason from external text.
parseFinishReason :: Text -> Either HuggingFaceError FinishReason
parseFinishReason = fmap FinishReason . parseLabel "FinishReason"

instance FromJSON FinishReason where parseJSON = parseTextJSON "FinishReason" parseFinishReason

-- | System instruction role.
systemRole :: ChatRole
systemRole = ChatRole "system"

-- | User message role.
userRole :: ChatRole
userRole = ChatRole "user"

-- | Assistant response role.
assistantRole :: ChatRole
assistantRole = ChatRole "assistant"

-- | Tool response role.
toolRole :: ChatRole
toolRole = ChatRole "tool"

-- | Developer instruction role.
developerRole :: ChatRole
developerRole = ChatRole "developer"

-- | A chat message. JSON content supports text, multimodal arrays, and null
-- content for tool calls. Provider-specific tool payloads are preserved as JSON.
data ChatMessage = ChatMessage
    { messageRole :: ChatRole -- ^ For example @system@, @user@, @assistant@, or @tool@.
    , messageContent :: Maybe Value -- ^ Text or multimodal content; absent for some tool calls.
    , messageToolCalls :: Maybe Value -- ^ Tool call array, when present.
    , messageToolCallId :: Maybe ToolCallId -- ^ Identifier for a tool response.
    , messageName :: Maybe ParticipantName -- ^ Optional participant name.
    } deriving stock (Eq, Show)

instance ToJSON ChatMessage where
    toJSON message = object $ ("role" .= messageRole message)
        : catMaybes
            [ ("content" .=) <$> messageContent message
            , ("tool_calls" .=) <$> messageToolCalls message
            , ("tool_call_id" .=) <$> messageToolCallId message
            , ("name" .=) <$> messageName message
            ]

instance FromJSON ChatMessage where
    parseJSON = withObject "ChatMessage" $ \o -> ChatMessage
        <$> o .: "role" <*> o .:? "content" <*> o .:? "tool_calls"
        <*> o .:? "tool_call_id" <*> o .:? "name"

-- | Construct a plain-text user message.
userMessage :: Text -> ChatMessage
userMessage = textMessage userRole

-- | Construct a plain-text system message.
systemMessage :: Text -> ChatMessage
systemMessage = textMessage systemRole

-- | Construct a plain-text assistant message.
assistantMessage :: Text -> ChatMessage
assistantMessage = textMessage assistantRole

textMessage :: ChatRole -> Text -> ChatMessage
textMessage role content = ChatMessage role (Just (String content)) Nothing Nothing Nothing

-- | Non-streaming chat request for the router's OpenAI-compatible endpoint.
-- Model names can include a provider suffix such as @:fastest@ or @:together@.
data ChatRequest = ChatRequest
    { chatModel :: InferenceModel -- ^ Hub model identifier and optional routing suffix.
    , chatMessages :: NonEmpty ChatMessage -- ^ A nonempty conversation.
    , chatMaxTokens :: Maybe PositiveInt -- ^ Positive output token limit.
    , chatTemperature :: Maybe ChatTemperature -- ^ Finite sampling temperature in @[0,2]@.
    , chatTopP :: Maybe TopP -- ^ Finite probability in @(0,1]@.
    , chatExtra :: Object -- ^ Additional fields, e.g. @tools@ or @response_format@.
    } deriving stock (Eq, Show)

-- | Construct a request with server-default sampling settings.
defaultChatRequest :: InferenceModel -> NonEmpty ChatMessage -> ChatRequest
defaultChatRequest model messages = ChatRequest model messages Nothing Nothing Nothing KM.empty

instance ToJSON ChatRequest where
    toJSON request = Object $ KM.union fields extras
      where
        fields = KM.fromList $ ["model" .= chatModel request, "messages" .= chatMessages request, "stream" .= False]
            ++ catMaybes
                [ ("max_tokens" .=) <$> chatMaxTokens request
                , ("temperature" .=) <$> chatTemperature request
                , ("top_p" .=) <$> chatTopP request
                ]
        -- Reserved fields cannot replace parsed fields or enable streaming.
        extras = foldr KM.delete (chatExtra request) ["model", "messages", "stream", "max_tokens", "temperature", "top_p"]

-- | Token accounting when reported by the provider.
data Usage = Usage
    { promptTokens :: Integer -- ^ Tokens in the prompt.
    , completionTokens :: Integer -- ^ Tokens generated.
    , totalTokens :: Integer -- ^ Total billed/reported tokens.
    } deriving stock (Eq, Show)

instance FromJSON Usage where
    parseJSON = withObject "Usage" $ \o -> Usage
        <$> o .: "prompt_tokens" <*> o .: "completion_tokens" <*> o .: "total_tokens"

-- | One completion candidate.
data ChatChoice = ChatChoice
    { choiceIndex :: Int -- ^ Candidate index.
    , choiceMessage :: ChatMessage -- ^ Assistant message, including tool calls.
    , choiceFinishReason :: Maybe FinishReason -- ^ Provider stop reason.
    } deriving stock (Eq, Show)

instance FromJSON ChatChoice where
    parseJSON = withObject "ChatChoice" $ \o -> ChatChoice
        <$> o .: "index" <*> o .: "message" <*> o .:? "finish_reason"

-- | Completed non-streaming response. Additional fields remain in 'completionRaw'.
data ChatCompletion = ChatCompletion
    { completionId :: CompletionId -- ^ Request identifier assigned by the provider.
    , completionModel :: ModelName -- ^ Resolved model.
    , completionChoices :: [ChatChoice] -- ^ Completion candidates.
    , completionUsage :: Maybe Usage -- ^ Optional token accounting.
    , completionRaw :: Object -- ^ Full response, including provider extensions.
    } deriving stock (Eq, Show)

instance FromJSON ChatCompletion where
    parseJSON = withObject "ChatCompletion" $ \o -> ChatCompletion
        <$> o .: "id" <*> o .: "model" <*> o .: "choices" <*> o .:? "usage" <*> pure o
