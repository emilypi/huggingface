{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Hub
-- Description : Repository discovery, metadata, and streamed file downloads
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Hub
    ( ListOptions (..), defaultListOptions
    , listModels, listDatasets, listSpaces, nextPage
    , repositoryInfo, modelInfo, datasetInfo, spaceInfo, whoAmI
    , downloadFile, withDownload
    ) where

import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (BodyReader, brConsume, requestHeaders, responseBody)
import Network.HTTP.Types (Query, hAccept)
import HuggingFace.Internal.HTTP
import HuggingFace.Client (Client, clientHub)
import HuggingFace.Monad (HuggingFaceM, liftIO)
import HuggingFace.Types.Hub
import HuggingFace.Types.Parameters (positiveIntValue)
import HuggingFace.Types.Pagination (Page, NextPage)
import HuggingFace.Types.Repository

-- | Fetch one page of models.
listModels :: Client -> ListOptions -> HuggingFaceM (Page RepoInfo)
listModels = listRepositories Model

-- | Fetch one page of datasets.
listDatasets :: Client -> ListOptions -> HuggingFaceM (Page RepoInfo)
listDatasets = listRepositories Dataset

-- | Fetch one page of Spaces.
listSpaces :: Client -> ListOptions -> HuggingFaceM (Page RepoInfo)
listSpaces = listRepositories Space

listRepositories :: RepoType -> Client -> ListOptions -> HuggingFaceM (Page RepoInfo)
listRepositories kind client options = getPage client (requestFor client (clientHub client) ["api", repoPlural kind] (listQuery options))

listQuery :: ListOptions -> Query
listQuery options = catMaybes
    [ field "search" (listSearch options)
    , field "author" (accountNameText <$> listAuthor options)
    , field "sort" (sortKeyText <$> listSort options)
    , (\n -> ("limit", Just (B8.pack (show (positiveIntValue n))))) <$> listLimit options
    ] ++ [("filter", Just (encodeUtf8 (hubTagText f))) | f <- listFilter options]
      ++ [("full", Just "true") | listFull options]
  where
    field key = fmap (\value -> (key, Just (encodeUtf8 value)))

-- | Fetch a continuation obtained from a page. Only the configured Hub origin
-- is accepted, even when the continuation came from another client's page.
nextPage :: FromJSON a => Client -> NextPage -> HuggingFaceM (Page a)
nextPage = continuePage

-- | Repository metadata at the default branch or an explicit revision.
-- Requests file sizes through @blobs=true@.
repositoryInfo :: Client -> RepoType -> RepoId -> Maybe Revision -> HuggingFaceM RepoInfo
repositoryInfo client kind repository revision = getJSON client $
    requestFor client (clientHub client)
        (["api", repoPlural kind] ++ T.splitOn "/" (repoIdText repository)
            ++ maybe [] (\r -> ["revision", revisionText r]) revision)
        [("blobs", Just "true")]

-- | Model metadata, optionally pinned to a revision.
modelInfo :: Client -> RepoId -> Maybe Revision -> HuggingFaceM RepoInfo
modelInfo client = repositoryInfo client Model

-- | Dataset metadata, optionally pinned to a revision.
datasetInfo :: Client -> RepoId -> Maybe Revision -> HuggingFaceM RepoInfo
datasetInfo client = repositoryInfo client Dataset

-- | Space metadata, optionally pinned to a revision.
spaceInfo :: Client -> RepoId -> Maybe Revision -> HuggingFaceM RepoInfo
spaceInfo client = repositoryInfo client Space

-- | Identify the token's owner. Requires an access token.
whoAmI :: Client -> HuggingFaceM UserInfo
whoAmI client = getJSON client (requestFor client (clientHub client) ["api", "whoami-v2"] [])

-- | Buffer a file in memory. Use 'withDownload' for large model weights.
downloadFile :: Client -> RepoType -> RepoId -> Revision -> RepoPath -> HuggingFaceM BL.ByteString
downloadFile client kind repository revision file =
    withDownload client kind repository revision file (liftIO . fmap BL.fromChunks . brConsume)

-- | Stream a repository file to a callback. The body reader is valid only while
-- the callback runs. Connections close on success, typed error, or exception.
-- HTTP failures use the monad's error channel; other IO exceptions propagate.
-- No filesystem paths are chosen or written by the library. Credentials are removed on CDN redirects;
-- an HTTPS request can never redirect to HTTP. At most five redirects are followed.
withDownload :: Client -> RepoType -> RepoId -> Revision -> RepoPath -> (BodyReader -> HuggingFaceM a) -> HuggingFaceM a
withDownload client kind repository revision file consume =
    withBody True client request (consume . responseBody)
  where
    prefix = case kind of Model -> []; _ -> [repoPlural kind]
    baseRequest = requestFor client (clientHub client)
        (prefix ++ T.splitOn "/" (repoIdText repository) ++ ["resolve", revisionText revision]
            ++ T.splitOn "/" (repoPathText file)) []
    request = baseRequest
        { requestHeaders = (hAccept, "*/*") : filter ((/= hAccept) . fst) (requestHeaders baseRequest)
        }
