{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Types.Hub
-- Description : Hub discovery options and repository metadata
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Types.Hub
    ( HubTag, parseHubTag, hubTagText
    , PipelineTag, parsePipelineTag, pipelineTagText
    , LibraryName, parseLibraryName, libraryNameText
    , SpaceSdk, parseSpaceSdk, spaceSdkText
    , SortKey, parseSortKey, sortKeyText
    , RepoInfo (..), RepoFile (..), UserInfo (..)
    , ListOptions (..), defaultListOptions
    ) where

import Data.Aeson
import Data.Text (Text)
import HuggingFace.Error (HuggingFaceError)
import HuggingFace.Utils (parseTextJSON, parseLabel)
import HuggingFace.Types.Parameters (PositiveInt)
import HuggingFace.Types.Repository (AccountName, RepoId, CommitSha, RepoPath)

-- | Open Hub metadata tag, including namespaced tags.
newtype HubTag = HubTag
    { hubTagText :: Text
    -- ^ Render a HubTag.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Text

-- | Parse a HubTag from external text.
parseHubTag :: Text -> Either HuggingFaceError HubTag
parseHubTag = fmap HubTag . parseLabel "HubTag"

instance FromJSON HubTag where parseJSON = parseTextJSON "HubTag" parseHubTag

-- | Open model task identifier.
newtype PipelineTag = PipelineTag
    { pipelineTagText :: Text
    -- ^ Render a PipelineTag.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Text

-- | Parse a PipelineTag from external text.
parsePipelineTag :: Text -> Either HuggingFaceError PipelineTag
parsePipelineTag = fmap PipelineTag . parseLabel "PipelineTag"

instance FromJSON PipelineTag where parseJSON = parseTextJSON "PipelineTag" parsePipelineTag

-- | Open model library identifier.
newtype LibraryName = LibraryName
    { libraryNameText :: Text
    -- ^ Render a LibraryName.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Text

-- | Parse a LibraryName from external text.
parseLibraryName :: Text -> Either HuggingFaceError LibraryName
parseLibraryName = fmap LibraryName . parseLabel "LibraryName"

instance FromJSON LibraryName where parseJSON = parseTextJSON "LibraryName" parseLibraryName

-- | Open Space SDK identifier.
newtype SpaceSdk = SpaceSdk
    { spaceSdkText :: Text
    -- ^ Render a SpaceSdk.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Text

-- | Parse a SpaceSdk from external text.
parseSpaceSdk :: Text -> Either HuggingFaceError SpaceSdk
parseSpaceSdk = fmap SpaceSdk . parseLabel "SpaceSdk"

instance FromJSON SpaceSdk where parseJSON = parseTextJSON "SpaceSdk" parseSpaceSdk

-- | Open Hub sort field.
newtype SortKey = SortKey
    { sortKeyText :: Text
    -- ^ Render a SortKey.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Text

-- | Parse a SortKey from external text.
parseSortKey :: Text -> Either HuggingFaceError SortKey
parseSortKey = fmap SortKey . parseLabel "SortKey"

instance FromJSON SortKey where parseJSON = parseTextJSON "SortKey" parseSortKey

-- | Shared Hub metadata. Optional fields may be absent from list responses.
-- Unknown fields remain available in 'repoRaw' for forward compatibility.
data RepoInfo = RepoInfo
    { repoId :: RepoId -- ^ Fully qualified repository name.
    , repoSha :: Maybe CommitSha -- ^ Revision commit SHA when supplied.
    , repoPrivate :: Maybe Bool -- ^ Visibility, when included by the server.
    , repoDownloads :: Maybe Integer -- ^ Download count, when available.
    , repoLikes :: Maybe Integer -- ^ Like count, when available.
    , repoTags :: [HubTag] -- ^ Hub metadata tags.
    , repoPipelineTag :: Maybe PipelineTag -- ^ Model task, when available.
    , repoLibraryName :: Maybe LibraryName -- ^ Associated model library.
    , repoSdk :: Maybe SpaceSdk -- ^ Space SDK, when available.
    , repoSiblings :: [RepoFile] -- ^ Files included in the metadata response.
    , repoRaw :: Object -- ^ Complete JSON object, including provider extensions.
    } deriving stock (Eq, Show)

instance FromJSON RepoInfo where
    parseJSON = withObject "RepoInfo" $ \o -> RepoInfo
        <$> o .: "id" <*> o .:? "sha" <*> o .:? "private"
        <*> o .:? "downloads" <*> o .:? "likes" <*> o .:? "tags" .!= []
        <*> o .:? "pipeline_tag" <*> o .:? "library_name" <*> o .:? "sdk"
        <*> o .:? "siblings" .!= [] <*> pure o

-- | A file returned in repository metadata. Size requires @blobs=true@.
data RepoFile = RepoFile
    { fileName :: RepoPath -- ^ Relative path within the repository.
    , fileSize :: Maybe Integer -- ^ Byte count, when supplied.
    } deriving stock (Eq, Show)

instance FromJSON RepoFile where
    parseJSON = withObject "RepoFile" $ \o -> RepoFile <$> o .: "rfilename" <*> o .:? "size"

-- | Identity returned by @/api/whoami-v2@.
data UserInfo = UserInfo
    { userName :: AccountName -- ^ Account name.
    , userFullName :: Maybe Text -- ^ Display name.
    , userRaw :: Object -- ^ Full identity response; may contain private account data.
    } deriving stock (Eq, Show)

instance FromJSON UserInfo where
    parseJSON = withObject "UserInfo" $ \o -> UserInfo <$> o .: "name" <*> o .:? "fullname" <*> pure o

-- | Options common to the Hub's repository listing endpoints. 'listLimit' is
-- the requested page size, not a total across pages. The server may cap it.
data ListOptions = ListOptions
    { listSearch :: Maybe Text -- ^ Search repository names.
    , listAuthor :: Maybe AccountName -- ^ Restrict to an account or organization.
    , listFilter :: [HubTag] -- ^ Repeated @filter@ query parameters.
    , listSort :: Maybe SortKey -- ^ Server sort key, such as @downloads@ or @lastModified@.
    , listLimit :: Maybe PositiveInt -- ^ Positive page size.
    , listFull :: Bool -- ^ Ask for expanded metadata.
    } deriving stock (Eq, Show)

-- | Unfiltered first page, with the server's default size and ordering.
defaultListOptions :: ListOptions
defaultListOptions = ListOptions Nothing Nothing [] Nothing Nothing False
