{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : Small command-line examples of the Hugging Face client
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module Main (main) where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import HuggingFace
import qualified Options.Applicative as Options
import System.Environment (lookupEnv)
import System.Exit (die)

data Command
    = SearchModels Text
    | ShowModel RepoId
    | RunChat InferenceModel Text

commandInfo :: Options.ParserInfo Command
commandInfo = Options.info (Options.helper <*> commandParser)
    ( Options.fullDesc
    <> Options.progDesc "Query the Hugging Face Hub or run a hosted chat completion"
    <> Options.footer "Authentication: set HF_TOKEN for authenticated requests. The chat command may incur provider charges."
    )

commandParser :: Options.Parser Command
commandParser = Options.hsubparser
    ( Options.command "models" (Options.info searchModels (Options.progDesc "Search models (up to ten results)"))
    <> Options.command "model" (Options.info showModel (Options.progDesc "Show repository metadata"))
    <> Options.command "chat" (Options.info runChat (Options.progDesc "Run a hosted chat completion"))
    )
  where
    searchModels = SearchModels <$> Options.strArgument
        (Options.metavar "SEARCH" <> Options.help "Model search text")
    showModel = ShowModel <$> Options.argument (domainReader parseRepoId)
        (Options.metavar "OWNER/NAME" <> Options.help "Hub repository ID (a bare name is also accepted)")
    runChat = RunChat
        <$> Options.argument (domainReader parseInferenceModel)
            (Options.metavar "MODEL" <> Options.help "Model repository with an optional provider or routing suffix")
        <*> Options.strArgument (Options.metavar "PROMPT" <> Options.help "Prompt text; quote multiword prompts")

domainReader :: (Text -> Either HuggingFaceError a) -> Options.ReadM a
domainReader parse = Options.eitherReader (first show . parse . T.pack)

-- | Run a Hub query or an explicitly requested hosted chat completion.
main :: IO ()
main = do
    command <- Options.customExecParser (Options.prefs Options.showHelpOnEmpty) commandInfo
    result <- runHuggingFaceM $ do
        tokenText <- liftIO (lookupEnv "HF_TOKEN")
        token <- liftEither (traverse (parseToken . T.pack) tokenText)
        client <- liftIO $ newClient defaultClientConfig
          { accessToken = token
          }
        case command of
            SearchModels query -> do
                limit <- liftEither (mkPositiveInt 10)
                page <- listModels client defaultListOptions {listSearch = Just query, listLimit = Just limit}
                liftIO $ mapM_ (T.putStrLn . repoIdText . repoId) (pageItems page)
            ShowModel repository -> do
                info <- modelInfo client repository Nothing
                liftIO (print info)
            RunChat target prompt -> do
                completion <- chatCompletion client (defaultChatRequest target (userMessage prompt :| []))
                liftIO $ mapM_ (print . choiceMessage) (completionChoices completion)
    either (die . show) pure result
