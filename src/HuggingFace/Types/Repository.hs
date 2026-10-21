{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Types.Repository
-- Description : Repository names, revisions, paths, and commit identities
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Types.Repository
    ( RepoId, parseRepoId, repoIdText, repoOwner, repoName, qualifiedRepo
    , RepoName, parseRepoName, repoNameText
    , AccountName, parseAccountName, accountNameText
    , Revision, parseRevision, revisionText, mainRevision, commitRevision
    , RepoPath, parseRepoPath, repoPathText
    , CommitSha, parseCommitSha, commitShaText
    , RepoType (..)
    , repoIdParser, repoPlural
    ) where

import Control.Monad (replicateM_, void)
import Data.Aeson
import Data.Char (isAscii, isHexDigit, isControl)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Parsec as P
import HuggingFace.Error (HuggingFaceError)
import HuggingFace.Utils (TextParser, parseValue, parseTextJSON, matched, asciiAlphaNum, takeWhile1P)

-- | Repository basename, distinct from its optional owner.
newtype RepoName = RepoName
    { repoNameText :: Text
    -- ^ Render a repository basename.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Text

-- | Parse a Hub repository basename.
parseRepoName :: Text -> Either HuggingFaceError RepoName
parseRepoName = parseValue "Expected a Hub repository basename" (RepoName <$> nameParser)

instance FromJSON RepoName where parseJSON = parseTextJSON "RepoName" parseRepoName

-- | Repository identity with an optional owner and a basename. Parsers and
-- smart constructors assemble it from parsed names.
data RepoId = RepoId
    { repoOwner :: Maybe AccountName -- ^ Optional owner of a repository.
    , repoName :: RepoName -- ^ Repository basename.
    }
    deriving stock (Eq, Ord, Show)

-- | Parse either a bare repository name or @owner/name@.
parseRepoId :: Text -> Either HuggingFaceError RepoId
parseRepoId = parseValue "Expected a repository name or owner/name" repoIdParser

-- | Compose a repository parser with a larger grammar, such as a routing suffix.
-- Use 'parseRepoId' when parsing a complete external value.
repoIdParser :: TextParser RepoId
repoIdParser = do
    first <- nameParser
    P.option (RepoId Nothing (RepoName first)) $
        qualifiedRepo (AccountName first) . RepoName <$> (P.char '/' *> nameParser)

-- | Assemble a qualified repository from parsed components without reparsing.
qualifiedRepo :: AccountName -> RepoName -> RepoId
qualifiedRepo owner = RepoId (Just owner)

-- | Render a repository identity for display or persistence.
repoIdText :: RepoId -> Text
repoIdText (RepoId owner name) = maybe "" ((<> "/") . accountNameText) owner <> repoNameText name

instance ToJSON RepoId where toJSON = toJSON . repoIdText
instance FromJSON RepoId where parseJSON = parseTextJSON "RepoId" parseRepoId

-- | Branch, tag, or commit reference; slash-separated components are retained.
newtype Revision = Revision (NonEmpty Text)
    deriving stock (Show)
    deriving (Eq, Ord) via (NonEmpty Text)

-- | Parse a revision. Slashes are encoded together as one URL segment.
parseRevision :: Text -> Either HuggingFaceError Revision
parseRevision = parseValue "Expected nonempty revision segments without traversal, controls, or backslashes" (Revision <$> components)

-- | Render a revision.
revisionText :: Revision -> Text
revisionText (Revision parts) = T.intercalate "/" (NE.toList parts)

-- | The default Hub branch.
mainRevision :: Revision
mainRevision = Revision ("main" :| [])

-- | Pin a revision to an already parsed full commit SHA.
commitRevision :: CommitSha -> Revision
commitRevision sha = Revision (commitShaText sha :| [])

instance ToJSON Revision where toJSON = toJSON . revisionText
instance FromJSON Revision where parseJSON = parseTextJSON "Revision" parseRevision

-- | Nonempty relative file path with no empty, dot, or parent segments.
newtype RepoPath = RepoPath (NonEmpty Text)
    deriving stock (Show)
    deriving (Eq, Ord) via (NonEmpty Text)

-- | Parse a repository-relative path, preserving Unicode and spaces.
parseRepoPath :: Text -> Either HuggingFaceError RepoPath
parseRepoPath = parseValue "Expected a relative repository file path without traversal, controls, or backslashes" (RepoPath <$> components)

-- | Render a repository-relative path.
repoPathText :: RepoPath -> Text
repoPathText (RepoPath parts) = T.intercalate "/" (NE.toList parts)

instance ToJSON RepoPath where toJSON = toJSON . repoPathText
instance FromJSON RepoPath where parseJSON = parseTextJSON "RepoPath" parseRepoPath

-- | Full SHA-1 commit identifier, stored in canonical lowercase form.
newtype CommitSha = CommitSha
    { commitShaText :: Text -- ^ Render a full commit identifier.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Text

-- | Parse exactly forty ASCII hexadecimal digits. Use 'parseRevision' for
-- abbreviated commit references, branches, or tags.
parseCommitSha :: Text -> Either HuggingFaceError CommitSha
parseCommitSha = parseValue "CommitSha requires forty hexadecimal digits" $
    CommitSha . T.toLower <$> matched (replicateM_ 40 (P.satisfy (\c -> isAscii c && isHexDigit c)))

instance FromJSON CommitSha where parseJSON = parseTextJSON "CommitSha" parseCommitSha

-- | Account or organization namespace.
newtype AccountName = AccountName
    { accountNameText :: Text -- ^ Render an account or organization name.
    }
    deriving stock (Show)
    deriving (Eq, Ord, ToJSON) via Text

-- | Parse a AccountName from external text.
parseAccountName :: Text -> Either HuggingFaceError AccountName
parseAccountName = parseValue "Expected a Hub account or organization name" (AccountName <$> nameParser)

instance FromJSON AccountName where parseJSON = parseTextJSON "AccountName" parseAccountName

-- | Hub repository kinds.
data RepoType = Model | Dataset | Space
    deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Hub collection path component for a repository kind.
repoPlural :: RepoType -> Text
repoPlural Model = "models"
repoPlural Dataset = "datasets"
repoPlural Space = "spaces"

-- Hub name components start/end with a word character, cannot repeat '.' or
-- '-', and cannot end in '.git'. Length is bounded while consuming input.
nameParser :: TextParser Text
nameParser = matched $ word *> rest (95 :: Int)
  where
    word = P.satisfy (\c -> asciiAlphaNum c || c == '_')
    nameChar = P.satisfy (\c -> asciiAlphaNum c || c == '_' || c == '-' || c == '.')
    next = void word P.<|> do
        P.notFollowedBy (P.try (P.string ".git" <* P.notFollowedBy nameChar))
        punctuation <- P.char '-' P.<|> P.char '.'
        P.notFollowedBy (P.char punctuation)
        void (P.lookAhead nameChar)
    rest 0 = P.notFollowedBy nameChar
    rest n = P.optional (next *> rest (n - 1))

components :: TextParser (NonEmpty Text)
components = (:|) <$> component <*> P.many (P.char '/' *> component)
  where
    component = do
        part <- takeWhile1P (\c -> c /= '/' && c /= '\\' && not (isControl c))
        case part of
            "." -> P.parserFail "Dot segment"
            ".." -> P.parserFail "Parent segment"
            _ -> pure part
