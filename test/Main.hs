{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (AsyncException (UserInterrupt), IOException, throwIO, try)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import Data.IORef
import Data.List (isInfixOf, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)
import qualified Data.Text as T
import HuggingFace.Monad
import HuggingFace.Client
import HuggingFace.Error
import HuggingFace.Hub
    ( listModels, listDatasets, listSpaces, nextPage
    , modelInfo, datasetInfo, spaceInfo, whoAmI, downloadFile, withDownload
    )
import HuggingFace.Inference (chatCompletion, textGeneration, classifyText, featureExtraction, infer)
import qualified HuggingFace.Internal.HTTP as Transport
import HuggingFace.Types.Auth
import HuggingFace.Types.Chat
import HuggingFace.Types.Hub
import HuggingFace.Types.Pagination
import HuggingFace.Types.Parameters
import HuggingFace.Types.Repository
import HuggingFace.Types.Routing
import HuggingFace.Types.Tasks
import qualified HuggingFace.Utils as Utils
import Network.HTTP.Client (brRead, defaultManagerSettings, managerRetryableException, managerModifyResponse, newManager)
import qualified Network.HTTP.Client.Internal as HTTP
import Network.HTTP.Types
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

data Observed = Observed
    { observedMethod :: Method
    , observedPath :: ByteString
    , observedQuery :: Query
    , observedHeaders :: RequestHeaders
    , observedBody :: BL.ByteString
    } deriving (Show)

type Reply = (Status, ResponseHeaders, BL.ByteString)

withServer :: (Observed -> IO Reply) -> (Int -> IO [Observed] -> IO a) -> IO a
withServer handler action = do
    seen <- newIORef []
    let app request respond = do
            body <- Wai.strictRequestBody request
            let observed = Observed (Wai.requestMethod request) (Wai.rawPathInfo request)
                    (Wai.queryString request) (Wai.requestHeaders request) body
            atomicModifyIORef' seen (\values -> (observed : values, ()))
            (status, headers, payload) <- handler observed
            respond (Wai.responseLBS status headers payload)
    testWithApplication (pure app) $ \port -> action port (reverse <$> readIORef seen)

withFixture :: Reply -> (Client -> IO [Observed] -> IO a) -> IO a
withFixture reply action = withServer (const (pure reply)) $ \port seen -> do
    client <- clientAt port (Just token)
    action client seen

clientAt :: Int -> Maybe Token -> IO Client
clientAt port tokenValue = do
    manager <- newManager defaultManagerSettings {managerRetryableException = const False}
    pure $ newClientWithManager manager defaultClientConfig
        { hubEndpoint = endpointAt port, inferenceEndpoint = endpointAt port, accessToken = tokenValue }

origin :: Int -> Text
origin port = "http://127.0.0.1:" <> T.pack (show port)

endpointAt :: Int -> Endpoint
endpointAt = valid . parseEndpoint . origin

expectRight :: Show e => Either e a -> IO a
expectRight = either (\err -> assertFailure (show err) >> fail "unreachable") pure

valid :: Show e => Either e a -> a
valid = either (error . show) id

token :: Token
token = valid (parseToken "hf_test_token")

repository :: RepoId
repository = valid (parseRepoId "owner/model")

repoJSON :: BL.ByteString
repoJSON = "{\"id\":\"owner/model\",\"sha\":\"0123456789abcdef0123456789abcdef01234567\",\"private\":false,\"downloads\":42,\"likes\":2,\"siblings\":[{\"rfilename\":\"config.json\",\"size\":17}],\"futureField\":true}"

chatJSON :: BL.ByteString
chatJSON = "{\"id\":\"chat-1\",\"model\":\"owner/model\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"Hello!\"},\"finish_reason\":\"stop\"}],\"usage\":{\"prompt_tokens\":2,\"completion_tokens\":3,\"total_tokens\":5}}"

jsonReply :: BL.ByteString -> Reply
jsonReply = (,,) status200 [(hContentType, "application/json")]

main :: IO ()
main = defaultMain $ testGroup "huggingface"
    [ identifierTests, domainTests, grammarTests, moduleTests, monadTests, hubTests, transportTests, inferenceTests ]

monadTests :: TestTree
monadTests = testGroup "Concrete client computations"
    [ testCase "parser errors prevent any request" $ withFixture (jsonReply repoJSON) $ \client seen -> do
        result <- runHuggingFaceM $ do
            parsed <- liftEither (parseRepoId "owner/../model")
            modelInfo client parsed Nothing
        case result of
            Left InvalidInput{} -> pure ()
            _ -> assertFailure (show result)
        length <$> seen >>= (@?= 0)
    , testCase "metadata and downloads compose without intermediate Either handling" $
        withServer (\request -> pure $ if "/api/" `BS.isPrefixOf` observedPath request
            then jsonReply repoJSON else (status200, [], "content")) $ \port seen -> do
                client <- clientAt port Nothing
                result <- runHuggingFaceM $ do
                    info <- modelInfo client repository Nothing
                    file <- liftEither (parseRepoPath "config.json")
                    downloadFile client Model (repoId info) mainRevision file
                result @?= Right "content"
                map observedPath <$> seen >>= (@?=
                    ["/api/models/owner/model", "/owner/model/resolve/main/config.json"])
    , testCase "catchError can recover from an HTTP error with another operation" $
        withServer (\request -> pure $ if observedPath request == "/api/models"
            then jsonReply "[]" else (status404, [], "missing")) $ \port seen -> do
                client <- clientAt port Nothing
                result <- runHuggingFaceM $
                    (modelInfo client repository Nothing >> pure [repository]) `catchError` \err ->
                        case err of
                            HttpError {errorStatus = 404} -> map repoId . pageItems <$> listModels client defaultListOptions
                            _ -> throwError err
                result @?= Right []
                map observedPath <$> seen >>= (@?= ["/api/models/owner/model", "/api/models"])
    , testCase "decode errors stop a subsequent inference request" $ withFixture (jsonReply "invalid") $ \client seen -> do
        result <- runHuggingFaceM $ do
            info <- modelInfo client repository Nothing
            infer client (repoId info) (object []) :: HuggingFaceM Value
        result @?= Left DecodeError
        map observedPath <$> seen >>= (@?= ["/api/models/owner/model"])
    , testCase "IO adapters run once and participate in typed recovery" $ do
        calls <- newIORef (0 :: Int)
        let action = liftHuggingFaceM $ do
                modifyIORef' calls (+ 1)
                pure (Left DecodeError :: Either HuggingFaceError ())
        readIORef calls >>= (@?= 0)
        result <- runHuggingFaceM $ (action >> liftIO (modifyIORef' calls (+ 100)))
            `catchError` \err -> liftIO (err @?= DecodeError)
        result @?= Right ()
        readIORef calls >>= (@?= 1)
    , testCase "typed callback errors release the response before outer recovery" $
        withServer (const (pure (jsonReply "payload"))) $ \port seen -> do
            closed <- newIORef False
            manager <- newManager defaultManagerSettings
                { managerRetryableException = const False
                , managerModifyResponse = \response -> do
                    let HTTP.ResponseClose close = HTTP.responseClose' response
                    pure response {HTTP.responseClose' = HTTP.ResponseClose (close >> writeIORef closed True)}
                }
            let client = newClientWithManager manager defaultClientConfig {hubEndpoint = endpointAt port}
                aborted = InvalidInput "Stopped download"
            result <- runHuggingFaceM $
                withDownload client Model repository mainRevision (valid (parseRepoPath "file"))
                    (\_ -> do
                        liftIO (readIORef closed >>= (@?= False))
                        throwError aborted)
                `catchError` \err -> do
                    liftIO (err @?= aborted)
                    liftIO (readIORef closed >>= (@?= True))
                    pure ("recovered" :: Text)
            result @?= Right "recovered"
            length <$> seen >>= (@?= 1)
    ]

moduleTests :: TestTree
moduleTests = testGroup "Exposed extension modules"
    [ testCase "transport supports custom operations with parsed client configuration" $
        withFixture (jsonReply repoJSON) $ \client seen -> do
            let request = Transport.requestFor client (clientHub client) ["api", "models", "owner", "model"] []
            response <- runHuggingFaceM (Transport.getJSON client request) >>= expectRight
            repoId response @?= repository
            requests <- seen
            map observedPath requests @?= ["/api/models/owner/model"]
            map (lookup hAuthorization . observedHeaders) requests @?= [Just "Bearer hf_test_token"]
    , testCase "header-created continuations still require the configured origin" $
        withFixture (jsonReply "[]") $ \client seen -> do
            let request = Transport.requestFor client (clientHub client) ["api", "models"] []
                headers = [("link", "<https://other.example/api/models?cursor=PRIVATE>; rel=next")]
            case nextPageFromHeaders request headers of
                Nothing -> assertFailure "missing parsed continuation"
                Just next -> do
                    nextPageURL next @?= "https://other.example/api/models?cursor=PRIVATE"
                    show next @?= "<NextPage>"
                    (runHuggingFaceM (Transport.continuePage client next) :: IO (Either HuggingFaceError (Page RepoInfo)))
                        >>= (@?= Left InvalidPagination)
            requests <- seen
            length requests @?= 0
    , testCase "shared parser runners consume complete values and redact failures" $ do
        Utils.parseValue "Malformed repository" repoIdParser "owner/model" @?= Right repository
        Utils.parseValue "Malformed repository" repoIdParser "owner/model?secret=PRIVATE"
            @?= Left (InvalidInput "Malformed repository")
        (Utils.decodeJSON "{\"id\":\"PRIVATE/invalid/id\"}" :: Either HuggingFaceError RepoInfo)
            @?= Left DecodeError
    ]

identifierTests :: TestTree
identifierTests = testGroup "Parsed domain values"
    [ testCase "token is redacted in configuration" $ do
        show token @?= "<Token>"
        assertBool "config leaked token" (not ("hf_test_token" `isInfixOf` show defaultClientConfig {accessToken = Just token}))
    , testCase "invalid tokens never echo their input" $ mapM_ (\value -> do
        let result = parseToken value
        assertBool "accepted malformed token" (isLeft result)
        assertBool "echoed secret" (not ("SECRET" `isInfixOf` show result)))
        ["", "hf_SECRET\r\nInjected: true", "hf_SECRET token", "hf_SECRET\955"]
    , testGroup "invalid repositories"
        [ testCase (show bad) $ assertBool "accepted invalid repo" (isLeft (parseRepoId bad))
        | bad <- ["", "/name", "name/", "a/b/c", "a/../b", "a?x=1", "https://example.com", "-name", "a..b", "a--b", "name.git", T.replicate 97 "x"] ]
    , testProperty "repository identity round-trips" $ forAll (listOf1 (elements ['a' .. 'z'])) $ \name ->
        let input = "owner/" <> T.pack (take 40 name)
        in fmap repoIdText (parseRepoId input) === Right input
    , testCase "branches may include slashes" $
        fmap revisionText (parseRevision "refs/pr/42") @?= Right "refs/pr/42"
    , testCase "revisions reject empty and dot-only segments" $
        mapM_ (\input -> assertBool "unsafe revision" (isLeft (parseRevision input))) ["../main", "refs/../main", "/main", "main/"]
    , testGroup "unsafe file paths"
        [ testCase (show bad) $ assertBool "accepted unsafe path" (isLeft (parseRepoPath bad))
        | bad <- ["", "/config.json", "../config.json", "a/../b", "a/./b", "a//b", "a/", "a\\b", "a\NULb"] ]
    , testCase "Unicode file names remain intact" $
        fmap repoPathText (parseRepoPath "sub/λ weights.json") @?= Right "sub/λ weights.json"
    , testCase "unsafe endpoints fail to parse" $
        mapM_ (\endpoint -> assertBool "accepted unsafe endpoint" (isLeft (parseEndpoint endpoint)))
            ["http://example.com", "https://user:secret@example.com", "https://example.com/api", "https://example.com?token=x", "https://example.com#fragment", "file:///tmp/data"]
    , testCase "endpoint ports cannot overflow or wrap to another destination" $
        mapM_ (\portText -> assertBool "invalid port parsed" (isLeft (parseEndpoint ("https://example.com:" <> portText))))
            ["", "0", "-1", "65536", "65979", "18446744073709552059", "not-a-port"]

    ]

domainTests :: TestTree
domainTests = testGroup "Domain boundaries"
    [ testCase "repository components compose without reparsing" $ do
        owner <- expectRight (parseAccountName "owner")
        name <- expectRight (parseRepoName "model")
        qualifiedRepo owner name @?= repository
        repoOwner repository @?= Just owner
        repoName repository @?= name
        repoOwner <$> parseRepoId "bert" @?= Right Nothing
    , testProperty "parsed repositories survive JSON encoding" $
        forAll (listOf1 (elements ['a' .. 'z'])) $ \name ->
            let value = valid (parseRepoId ("owner/" <> T.pack (take 40 name)))
            in eitherDecode (encode value) === Right value
    , testCase "qualified fields parse during JSON decoding" $ do
        mapM_ (\value -> assertBool "invalid response identity accepted" $
            isLeft (eitherDecode (encode value) :: Either String RepoInfo))
            [ object ["id" .= ("a/b/c" :: Text)]
            , object ["id" .= repository, "sha" .= ("abc" :: Text)]
            , object ["id" .= repository, "siblings" .= [object ["rfilename" .= ("../weights" :: Text)]]]
            , object ["id" .= repository, "pipeline_tag" .= ("" :: Text)]
            ]
        assertBool "invalid account accepted" $
            isLeft (eitherDecode "{\"name\":\"owner/name\"}" :: Either String UserInfo)
    , testCase "commit SHAs normalize and become revisions without reparsing" $ do
        sha <- expectRight (parseCommitSha "0123456789ABCDEF0123456789ABCDEF01234567")
        commitShaText sha @?= "0123456789abcdef0123456789abcdef01234567"
        revisionText (commitRevision sha) @?= commitShaText sha
        eitherDecode (encode sha) @?= Right sha
        mapM_ (\raw -> assertBool "malformed SHA parsed" (isLeft (parseCommitSha raw)))
            ["", "abc", T.replicate 40 "g", T.replicate 39 "a", T.replicate 41 "a"]
    , testCase "all router policies and named providers retain their structure" $ do
        provider <- expectRight (parseProvider "together")
        let targets = modelFromRepo repository : map (routeModel repository) [Fastest, Cheapest, Preferred, Via provider]
        mapM_ (\target -> do
            modelRepository target @?= repository
            parseInferenceModel (inferenceModelText target) @?= Right target
            eitherDecode (encode target) @?= Right target) targets
        map modelRouting targets @?= [Nothing, Just Fastest, Just Cheapest, Just Preferred, Just (Via provider)]
        assertBool "policy accepted as provider" (isLeft (parseProvider "fastest"))
        mapM_ (\raw -> assertBool "malformed model parsed" (isLeft (parseInferenceModel raw)))
            ["", " ", "owner/model:", ":fastest", "owner/model:fastest:cheapest", "owner/model:bad provider", "owner/model:provider/path"]
    , testCase "parsed paths and revisions retain escaped and Unicode content" $ do
        let path = valid (parseRepoPath "λ/a %20.json")
            revision = valid (parseRevision "refs/pr/42")
        eitherDecode (encode path) @?= Right path
        eitherDecode (encode revision) @?= Right revision
    , testCase "open namespaces retain future server values" $ do
        info <- expectRight (eitherDecode "{\"id\":\"owner/model\",\"tags\":[\"license:future-1.0\"],\"pipeline_tag\":\"future-task\",\"library_name\":\"future-library\",\"sdk\":\"future-sdk\"}" :: Either String RepoInfo)
        map hubTagText (repoTags info) @?= ["license:future-1.0"]
        fmap pipelineTagText (repoPipelineTag info) @?= Just "future-task"
        fmap libraryNameText (repoLibraryName info) @?= Just "future-library"
        fmap spaceSdkText (repoSdk info) @?= Just "future-sdk"
        response <- expectRight (eitherDecode "{\"id\":\"provider:completion:1\",\"model\":\"provider/deployment/model-v2\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"future_role\"},\"finish_reason\":\"future_reason\"}]}" :: Either String ChatCompletion)
        completionIdText (completionId response) @?= "provider:completion:1"
        modelNameText (completionModel response) @?= "provider/deployment/model-v2"
        map (chatRoleText . messageRole . choiceMessage) (completionChoices response) @?= ["future_role"]
        map (fmap finishReasonText . choiceFinishReason) (completionChoices response) @?= [Just "future_reason"]
    , testCase "message identifiers parse on input and retain their JSON shape" $ do
        call <- expectRight (parseToolCallId "call-1")
        name <- expectRight (parseParticipantName "search_tool")
        let message = ChatMessage toolRole (Just (String "result")) Nothing (Just call) (Just name)
        toJSON message @?= object ["role" .= ("tool" :: Text), "content" .= ("result" :: Text), "tool_call_id" .= ("call-1" :: Text), "name" .= ("search_tool" :: Text)]
        eitherDecode (encode message) @?= Right message
        mapM_ (\raw -> do
            participant <- expectRight (parseParticipantName raw)
            eitherDecode (encode participant) @?= Right participant)
            ["Zoë Reviewer", T.replicate 128 "x"]
        mapM_ (\payload -> assertBool "invalid message identifier accepted" $
            isLeft (eitherDecode payload :: Either String ChatMessage))
            ["{\"role\":\"\"}", "{\"role\":\"tool\",\"tool_call_id\":\"\"}", "{\"role\":\"user\",\"name\":\"\"}"]
    , testCase "numeric constraints produce usable types at construction" $ do
        positiveIntValue <$> mkPositiveInt 1 @?= Right 1
        mapM_ (\n -> assertBool "nonpositive count parsed" (isLeft (mkPositiveInt n))) [minBound, -1, 0]
        mapM_ (\n -> assertBool "invalid temperature parsed" (isLeft (mkChatTemperature n))) [-0.1, 2.1, 0/0, 1/0, -1/0]
        mapM_ (\n -> assertBool "invalid top_p parsed" (isLeft (mkTopP n))) [0, -1, 1.1, 0/0, 1/0]
        mapM_ (\n -> assertBool "invalid generation temperature parsed" (isLeft (mkGenerationTemperature n))) [0, -1, 0/0, 1/0]
        mapM_ (\n -> chatTemperatureValue <$> mkChatTemperature n @?= Right n) [0, 2]
        topPValue <$> mkTopP 1 @?= Right 1
        generationTemperatureValue <$> mkGenerationTemperature 3 @?= Right 3
        assertBool "numeric JSON bypassed smart constructor" (isLeft (eitherDecode "0" :: Either String PositiveInt))
        eitherDecode (encode (valid (mkTopP 0.7))) @?= Right (valid (mkTopP 0.7))
    , testCase "endpoint parser retains a renderable origin and hides bad credentials" $ do
        mapM_ (\raw -> do
            endpoint <- expectRight (parseEndpoint raw)
            parseEndpoint (endpointText endpoint) @?= Right endpoint)
            ["https://huggingface.co", "https://router.huggingface.co/", "https://example.com:1", "https://example.com:65535", "http://localhost:8080", "http://127.0.0.1:8000", "http://[::1]:8000"]
        parseEndpoint (endpointText defaultHubEndpoint) @?= Right defaultHubEndpoint
        parseEndpoint (endpointText defaultInferenceEndpoint) @?= Right defaultInferenceEndpoint
        assertBool "parser echoed credentials" (not ("SECRET" `isInfixOf` show (parseEndpoint "https://user:SECRET@example.com")))
    ]

grammarTests :: TestTree
grammarTests = testGroup "Parsec grammars"
    [ testCase "Hub names handle punctuation and the exact length boundary" $ do
        mapM_ (\raw -> fmap repoNameText (parseRepoName raw) @?= Right raw)
            ["a", "_", "a_b", "a-b.c", "a-.b", "model.gitx", "model.git-more", T.replicate 96 "x"]
        mapM_ (\raw -> assertBool "malformed name parsed" (isLeft (parseRepoName raw)))
            [".a", "a.", "a-", "a..b", "a--b", "model.git", T.replicate 97 "x", "owner/name", "model:fastest"]
        assertBool "invalid owner suffix parsed" (isLeft (parseRepoId "owner.git/model"))
        assertBool "oversized owner parsed" (isLeft (parseRepoId (T.replicate 97 "x" <> "/model")))
    , testCase "routing policy prefixes do not consume provider slugs" $ do
        mapM_ (\raw -> do
            target <- expectRight (parseInferenceModel ("owner/model:" <> raw))
            case modelRouting target of
                Just (Via provider) -> providerText provider @?= raw
                other -> assertFailure (show other))
            ["fastest-provider", "cheapest-provider", "preferred-provider"]
        mapM_ (\raw -> assertBool "trailing input parsed" (isLeft (parseInferenceModel raw)))
            ["owner/model:fastest/extra", "owner/model:fastest:extra", "owner/model.git:fastest", "owner/model/extra:fastest", "owner/model:fastest\n"]
    , testCase "tokens and complete identifiers reject trailing input" $ do
        assertBool "token prefix accepted" (isLeft (parseToken "hf_secret!"))
        assertBool "SHA prefix accepted" (isLeft (parseCommitSha (T.replicate 40 "a" <> "extra")))
        mapM_ (\raw -> assertBool "repository prefix accepted" (isLeft (parseRepoId raw)))
            ["owner/model?key=value", "owner/model\n", "owner/model trailing"]
    , testCase "component parsing preserves non-BMP text and excludes traversal" $ do
        let raw = "🦄/λ weights/文件.json"
        fmap repoPathText (parseRepoPath raw) @?= Right raw
        fmap revisionText (parseRevision raw) @?= Right raw
        mapM_ (\bad -> assertBool "bad segment accepted" (isLeft (parseRepoPath bad)))
            ["🦄/../file", "🦄/./file", "🦄//file", "🦄/file\r\n"]
    , testCase "open label grammars preserve spaces without accepting blank input" $ do
        fmap participantNameText (parseParticipantName "  Zoë Reviewer 🦄  ") @?= Right "  Zoë Reviewer 🦄  "
        mapM_ (\raw -> assertBool "blank label parsed" (isLeft (parseHubTag raw))) ["", " ", "\x2003", "abc\NUL"]
        fmap completionIdText (parseCompletionId "") @?= Right ""
        assertBool "blank nonempty completion ID parsed" (isLeft (parseCompletionId " "))
    , testCase "origin grammar consumes only a complete origin" $ do
        fmap endpointText (parseEndpoint "https://example.com:000443/") @?= Right "https://example.com/"
        mapM_ (\raw -> assertBool "non-origin parsed" (isLeft (parseEndpoint raw)))
            [ "https://example.com//", "https://example.com/?secret=1", "https://example.com/#fragment"
            , "https://example.com:443garbage", "https://example.com:443:80"
            , "https://[::1", "https://[not-an-ip]", "http://localhost.evil.example"
            , "https://example.com\n", "https://example.com@other.example"
            ]
    , testProperty "routed repository components render and reparse" $
        forAll (listOf1 (elements ['a' .. 'z'])) $ \suffix ->
            let raw = "owner/model-" <> T.pack (take 40 suffix) <> ":fastest-provider"
            in fmap inferenceModelText (parseInferenceModel raw) === Right raw
    ]

hubTests :: TestTree
hubTests = testGroup "Hub API"
    [ testCase "lists models with encoded query parameters and authentication" $
        withFixture (jsonReply ("[" <> repoJSON <> "]")) $ \client seen -> do
            page <- runHuggingFaceM (listModels client defaultListOptions
                { listSearch = Just "hello & λ", listAuthor = Just (valid (parseAccountName "owner")), listFilter = map (valid . parseHubTag) ["text-generation", "pytorch"], listLimit = Just (valid (mkPositiveInt 2)), listFull = True })
                >>= expectRight
            map repoId (pageItems page) @?= [repository]
            requests <- seen
            case requests of
                [request] -> do
                    observedPath request @?= "/api/models"
                    observedMethod request @?= methodGet
                    lookup hAuthorization (observedHeaders request) @?= Just "Bearer hf_test_token"
                    sort (observedQuery request) @?= sort
                        [("search", Just "hello & \206\187"), ("author", Just "owner"), ("filter", Just "text-generation"), ("filter", Just "pytorch"), ("limit", Just "2"), ("full", Just "true")]
                _ -> assertFailure (show requests)
    , testCase "lists datasets and Spaces" $ withFixture (jsonReply "[]") $ \client seen -> do
        _ <- runHuggingFaceM (listDatasets client defaultListOptions) >>= expectRight
        _ <- runHuggingFaceM (listSpaces client defaultListOptions) >>= expectRight
        map observedPath <$> seen >>= (@?= ["/api/datasets", "/api/spaces"])
    , testCase "metadata routes preserve slash-containing revisions" $ withFixture (jsonReply repoJSON) $ \client seen -> do
        let revision = Just (valid (parseRevision "refs/pr/42"))
        model <- runHuggingFaceM (modelInfo client repository revision) >>= expectRight
        repoSha model @?= Just (valid (parseCommitSha "0123456789abcdef0123456789abcdef01234567"))
        repoDownloads model @?= Just 42
        repoSiblings model @?= [RepoFile (valid (parseRepoPath "config.json")) (Just 17)]
        KM.lookup "futureField" (repoRaw model) @?= Just (Bool True)
        _ <- runHuggingFaceM (datasetInfo client repository Nothing) >>= expectRight
        _ <- runHuggingFaceM (spaceInfo client repository Nothing) >>= expectRight
        map observedPath <$> seen >>= (@?= ["/api/models/owner/model/revision/refs%2Fpr%2F42", "/api/datasets/owner/model", "/api/spaces/owner/model"])
    , testCase "metadata tolerates absent and null optional fields" $ do
        info <- expectRight (eitherDecode "{\"id\":\"owner/model\",\"tags\":null,\"siblings\":null}" :: Either String RepoInfo)
        repoTags info @?= []
        repoSiblings info @?= []
        repoPrivate info @?= Nothing
    , testCase "metadata identifiers feed subsequent requests without parsing" $
        withServer (\request -> pure $ if "/api/" `BS.isPrefixOf` observedPath request
            then jsonReply repoJSON else (status200, [], "content")) $ \port seen -> do
                client <- clientAt port Nothing
                info <- runHuggingFaceM (modelInfo client repository Nothing) >>= expectRight
                case (repoSha info, repoSiblings info) of
                    (Just sha, file : _) -> do
                        runHuggingFaceM (downloadFile client Model (repoId info) (commitRevision sha) (fileName file)) >>= (@?= Right "content")
                        let target = routeModel (repoId info) Fastest
                        modelRepository target @?= repoId info
                    _ -> assertFailure "fixture must include a commit and file"
                map observedPath <$> seen >>= (@?=
                    ["/api/models/owner/model", "/owner/model/resolve/0123456789abcdef0123456789abcdef01234567/config.json"])
    , testCase "whoami uses authenticated identity endpoint" $ withFixture (jsonReply "{\"name\":\"emilypi\",\"fullname\":\"Emily\"}") $ \client seen -> do
        user <- runHuggingFaceM (whoAmI client) >>= expectRight
        userName user @?= valid (parseAccountName "emilypi")
        map observedPath <$> seen >>= (@?= ["/api/whoami-v2"])
    , testCase "public requests omit Authorization" $ withServer (const (pure (jsonReply "[]"))) $ \port seen -> do
        client <- clientAt port Nothing
        _ <- runHuggingFaceM (listModels client defaultListOptions) >>= expectRight
        map (lookup hAuthorization . observedHeaders) <$> seen >>= (@?= [Nothing])
    , testCase "Link pagination handles commas, quoted parameters and relative URLs" $
        withServer (\request -> pure $ if null (observedQuery request)
            then (status200, [("link", "</api/models?cursor=a,b>; title=\"one,two\"; rel=\"next last\", </api/models?cursor=old>; rel=\"prev\"")], "[]")
            else jsonReply ("[" <> repoJSON <> "]")) $ \port seen -> do
                client <- clientAt port (Just token)
                first <- runHuggingFaceM (listModels client defaultListOptions) >>= expectRight
                case pageNext first of
                    Nothing -> assertFailure "missing continuation"
                    Just next -> do
                        second <- runHuggingFaceM (nextPage client next) >>= expectRight
                        map repoId (pageItems second) @?= [repository]
                        pageNext second @?= Nothing
                map observedQuery <$> seen >>= (@?= [[], [("cursor", Just "a,b")]])
    , testCase "cross-origin continuations are rejected" $
        withFixture (status200, [("link", "<https://example.invalid/api/models?cursor=x>; rel=next")], "[]") $ \client seen -> do
            first <- runHuggingFaceM (listModels client defaultListOptions) >>= expectRight
            case pageNext first of
                Just next -> (runHuggingFaceM (nextPage client next) :: IO (Either HuggingFaceError (Page RepoInfo))) >>= (@?= Left InvalidPagination)
                Nothing -> assertFailure "missing continuation"
            length <$> seen >>= (@?= 1)
    , testCase "Link parameters accept HTTP whitespace around names and values" $
        withFixture (status200, [("link", "</api/models?cursor=x>;\trel\t=\t\"next\"\t")], "[]") $ \client seen -> do
            first <- runHuggingFaceM (listModels client defaultListOptions) >>= expectRight
            case pageNext first of
                Nothing -> assertFailure "missing tab-separated continuation"
                Just next -> do
                    second <- runHuggingFaceM (nextPage client next) :: IO (Either HuggingFaceError (Page RepoInfo))
                    _ <- expectRight second
                    map observedQuery <$> seen >>= (@?= [[], [("cursor", Just "x")]])
    , testCase "quoted Link extensions keep escaped quotes and delimiters inside values" $
        withFixture (status200, [("link", "</api/models?cursor=a,b>; title=\"one\\\",two;three\"; rel=\"next last\"")], "[]") $ \client seen -> do
            first <- runHuggingFaceM (listModels client defaultListOptions) >>= expectRight
            case pageNext first of
                Nothing -> assertFailure "missing quoted continuation"
                Just next -> do
                    second <- runHuggingFaceM (nextPage client next) :: IO (Either HuggingFaceError (Page RepoInfo))
                    _ <- expectRight second
                    map observedQuery <$> seen >>= (@?= [[], [("cursor", Just "a,b")]])
    , testCase "HTTP list grammar ignores empty Link members" $
        withFixture (status200, [("link", " , , </api/models?cursor=x>; rel=next, , ")], "[]") $ \client seen -> do
            first <- runHuggingFaceM (listModels client defaultListOptions) >>= expectRight
            case pageNext first of
                Nothing -> assertFailure "missing continuation between empty members"
                Just next -> do
                    second <- runHuggingFaceM (nextPage client next) :: IO (Either HuggingFaceError (Page RepoInfo))
                    _ <- expectRight second
                    map observedQuery <$> seen >>= (@?= [[], [("cursor", Just "x")]])
    , testGroup "Link grammar cannot mistake extensions or malformed input for next"
        [ testCase (show header) $ withFixture (status200, [("link", header)], "[]") $ \client _ -> do
            page <- runHuggingFaceM (listModels client defaultListOptions) >>= expectRight
            pageNext page @?= Nothing
        | header <- [ "</api/models?cursor=x>; title=\"ignore; rel=next\"; rel=prev"
                    , "</api/models?cursor=x>; rel=prev; rel=next"
                    , "</api/models?cursor=x>; rel=\"next\" garbage"
                    , "</api/models?cursor=x>; rel=\"next"
                    , "</api/models?cursor=x; rel=next"
                    ] ]
    , testCase "relative pagination uses the final redirected URL" $
        withServer (\request -> pure $ case observedPath request of
            "/api/models" -> (status302, [(hLocation, "/relocated/models")], "")
            "/relocated/models" | null (observedQuery request) -> (status200, [("link", "<?cursor=next>; rel=next")], "[]")
            "/relocated/models" -> jsonReply ("[" <> repoJSON <> "]")
            _ -> (status404, [], "")) $ \port seen -> do
                client <- clientAt port Nothing
                page <- runHuggingFaceM (listModels client defaultListOptions) >>= expectRight
                case pageNext page of
                    Nothing -> assertFailure "missing continuation"
                    Just next -> do
                        result <- runHuggingFaceM (nextPage client next) >>= expectRight
                        map repoId (pageItems result) @?= [repository]
                map observedPath <$> seen >>= (@?= ["/api/models", "/relocated/models", "/relocated/models"])
    , testCase "downloads encode file names and repository prefixes" $ withFixture (jsonReply "bytes") $ \client seen -> do
        let revision = valid (parseRevision "branch/name")
            file = valid (parseRepoPath "sub/λ weights?.json")
        mapM_ (\kind -> runHuggingFaceM (downloadFile client kind repository revision file) >>= (@?= Right "bytes")) [Model, Dataset, Space]
        map (lookup hAccept . observedHeaders) <$> seen >>= (@?= replicate 3 (Just "*/*"))
        map observedPath <$> seen >>= (@?=
            [ "/owner/model/resolve/branch%2Fname/sub/%CE%BB%20weights%3F.json"
            , "/datasets/owner/model/resolve/branch%2Fname/sub/%CE%BB%20weights%3F.json"
            , "/spaces/owner/model/resolve/branch%2Fname/sub/%CE%BB%20weights%3F.json" ])
    ]

transportTests :: TestTree
transportTests = testGroup "Transport"
    [ testGroup "HTTP errors"
        [ testCase (show code) $ withFixture (mkStatus code "error", [("x-request-id", "request-42"), ("retry-after", "7")], "{\"error\":\"hf_test_token\"}") $ \client seen -> do
            result <- runHuggingFaceM (modelInfo client repository Nothing)
            case result of
                Left err@HttpError{} -> do
                    errorStatus err @?= code
                    errorRequestId err @?= Just "request-42"
                    errorRetryAfter err @?= Just "7"
                    assertBool "error body lost" ("hf_test_token" `BS.isInfixOf` errorBody err)
                    assertBool "error display leaked token" (not ("hf_test_token" `isInfixOf` show err))
                _ -> assertFailure (show result)
            length <$> seen >>= (@?= 1)
        | code <- [401, 403, 404, 429, 500, 503] ]
    , testCase "error bodies are bounded" $ withFixture (status500, [], BL.replicate 100000 120) $ \client _ -> do
        result <- runHuggingFaceM (modelInfo client repository Nothing)
        case result of
            Left err@HttpError{} -> BS.length (errorBody err) @?= 65536
            _ -> assertFailure (show result)
    , testGroup "JSON errors"
        [ testCase (B8.unpack (BL.toStrict payload)) $ withFixture (jsonReply payload) $ \client _ ->
            runHuggingFaceM (modelInfo client repository Nothing) >>= (@?= Left DecodeError)
        | payload <- ["<html>error</html>", "{\"not_id\":true}", "{\"id\":3}"] ]
    , testCase "same-origin GET redirects retain token" $
        withServer (\request -> pure $ if observedPath request == "/api/models/owner/model"
            then (status302, [(hLocation, "/canonical")], "") else jsonReply repoJSON) $ \port seen -> do
                client <- clientAt port (Just token)
                _ <- runHuggingFaceM (modelInfo client repository Nothing) >>= expectRight
                map (lookup hAuthorization . observedHeaders) <$> seen >>= (@?= replicate 2 (Just "Bearer hf_test_token"))
    , testCase "API redirects cannot cross origins" $ withFixture (status302, [(hLocation, "https://example.invalid/data")], "") $ \client _ ->
        runHuggingFaceM (modelInfo client repository Nothing) >>= (@?= Left UnsafeRedirect)
    , testCase "redirect loops terminate" $ withFixture (status302, [(hLocation, "/loop")], "") $ \client seen -> do
        runHuggingFaceM (modelInfo client repository Nothing) >>= (@?= Left TooManyRedirects)
        length <$> seen >>= (@?= 6)
    , testCase "download redirects remove credentials across ports" $
        withServer (const (pure (jsonReply "download"))) $ \cdnPort cdnSeen ->
            withFixture (status302, [(hLocation, encodeUtf8 (origin cdnPort <> "/blob"))], "") $ \client _ -> do
                runHuggingFaceM (downloadFile client Model repository mainRevision (valid (parseRepoPath "weights.bin"))) >>= (@?= Right "download")
                map (lookup hAuthorization . observedHeaders) <$> cdnSeen >>= (@?= [Nothing])
    , testCase "download callbacks can stop early and subsequent requests work" $ withFixture (jsonReply "payload") $ \client _ -> do
        result <- runHuggingFaceM (withDownload client Model repository mainRevision (valid (parseRepoPath "file")) (liftIO . brRead))
        result @?= Right "payload"
        result2 <- runHuggingFaceM (withDownload client Model repository mainRevision (valid (parseRepoPath "file")) (const (pure ())))
        result2 @?= Right ()
    , testCase "callback exceptions and asynchronous cancellation propagate" $ withFixture (jsonReply "payload") $ \client _ -> do
        ioResult <- try (runHuggingFaceM (withDownload client Model repository mainRevision (valid (parseRepoPath "file")) (const (liftIO (ioError (userError "callback")))))) :: IO (Either IOException (Either HuggingFaceError ()))
        assertBool "swallowed callback exception" (isLeft ioResult)
        asyncResult <- try (runHuggingFaceM (withDownload client Model repository mainRevision (valid (parseRepoPath "file")) (const (liftIO (throwIO UserInterrupt))))) :: IO (Either AsyncException (Either HuggingFaceError ()))
        asyncResult @?= Left UserInterrupt
    ]

inferenceTests :: TestTree
inferenceTests = testGroup "Hosted inference"
    [ testCase "chat request, extensions, and typed response" $ withFixture (jsonReply chatJSON) $ \client seen -> do
        let request = (defaultChatRequest (routeModel repository Fastest) (systemMessage "Be concise" :| [userMessage "Hi"]))
                { chatMaxTokens = Just (valid (mkPositiveInt 20)), chatTemperature = Just (valid (mkChatTemperature 0.2))
                , chatExtra = KM.fromList [("stream", Bool True), ("model", String "wrong"), ("seed", Number 42)] }
        response <- runHuggingFaceM (chatCompletion client request) >>= expectRight
        completionUsage response @?= Just (Usage 2 3 5)
        map (messageContent . choiceMessage) (completionChoices response) @?= [Just (String "Hello!")]
        requests <- seen
        case requests of
            [sent] -> do
                observedPath sent @?= "/v1/chat/completions"
                observedMethod sent @?= methodPost
                lookup hContentType (observedHeaders sent) @?= Just "application/json"
                payload <- expectRight (eitherDecode (observedBody sent) :: Either String Object)
                KM.lookup "model" payload @?= Just (String "owner/model:fastest")
                KM.lookup "stream" payload @?= Just (Bool False)
                KM.lookup "seed" payload @?= Just (Number 42)
                KM.lookup "max_tokens" payload @?= Just (Number 20)
                KM.member "top_p" payload @?= False
            _ -> assertFailure (show requests)
    , testCase "tool-call messages allow absent content" $ do
        let payload = "{\"role\":\"assistant\",\"content\":null,\"tool_calls\":[{\"id\":\"call-1\"}]}"
        message <- expectRight (eitherDecode payload :: Either String ChatMessage)
        messageContent message @?= Nothing
        assertBool "lost tool calls" (messageToolCalls message /= Nothing)
        eitherDecode (encode message) @?= Right message
    , testCase "successful TGI chat responses can have an empty completion ID" $
        withFixture (jsonReply "{\"id\":\"\",\"model\":\"tgi\",\"choices\":[]}") $ \client _ -> do
            response <- runHuggingFaceM (chatCompletion client (defaultChatRequest (modelFromRepo repository) (userMessage "Hi" :| []))) >>= expectRight
            completionIdText (completionId response) @?= ""
    , testCase "chat redirects are returned without replaying POST" $ withFixture (status307, [(hLocation, "/other")], "") $ \client seen -> do
        response <- runHuggingFaceM (chatCompletion client (defaultChatRequest (modelFromRepo repository) (userMessage "Hi" :| [])))
        case response of Left err@HttpError{} -> errorStatus err @?= 307; _ -> assertFailure (show response)
        length <$> seen >>= (@?= 1)
    , testCase "lost response on a reused connection does not replay a billed POST" $ do
        calls <- newIORef (0 :: Int)
        let app request respond
                | Wai.requestMethod request == methodGet = respond (Wai.responseLBS status200 [] "[]")
                | otherwise = do
                    _ <- Wai.strictRequestBody request
                    atomicModifyIORef' calls (\n -> (n + 1, ()))
                    -- Drop the connection after receiving the POST, before any
                    -- response headers. A default http-client manager retries it.
                    respond (Wai.responseRaw (\_ _ -> pure ()) (Wai.responseLBS status500 [] ""))
        testWithApplication (pure app) $ \port -> do
            client <- newClient defaultClientConfig {hubEndpoint = endpointAt port, inferenceEndpoint = endpointAt port}
            _ <- runHuggingFaceM (listModels client defaultListOptions) >>= expectRight
            result <- runHuggingFaceM (chatCompletion client (defaultChatRequest (modelFromRepo repository) (userMessage "Hi" :| [])))
            case result of
                Left (TransportError _) -> pure ()
                _ -> assertFailure (show result)
            readIORef calls >>= (@?= 1)
    , testCase "feature extraction uses explicit task route and batch" $ withFixture (jsonReply "[[0.1,0.2],[0.3,0.4]]") $ \client seen -> do
        runHuggingFaceM (featureExtraction client repository ("one" :| ["two"])) >>= (@?= Right [[0.1,0.2],[0.3,0.4]])
        requests <- seen
        case requests of
            [request] -> do
                observedPath request @?= "/hf-inference/models/owner/model/pipeline/feature-extraction"
                eitherDecode (observedBody request) @?= Right (object ["inputs" .= (["one", "two"] :: [Text])])
            _ -> assertFailure (show requests)
    , testGroup "generation response shapes"
        [ testCase (show index) $ withFixture (jsonReply payload) $ \client seen -> do
            runHuggingFaceM (textGeneration client repository "Hello" defaultGenerationParameters {generationMaxNewTokens = Just (valid (mkPositiveInt 4))}) >>= (@?= Right [GeneratedText "world" Nothing])
            requests <- seen
            case requests of
                [request] -> do
                    observedPath request @?= "/hf-inference/models/owner/model"
                    eitherDecode (observedBody request) @?= Right (object ["inputs" .= ("Hello" :: Text), "parameters" .= object ["max_new_tokens" .= (4 :: Int)], "stream" .= False])
                _ -> assertFailure (show requests)
        | (index, payload) <- zip [1 :: Int ..] ["[{\"generated_text\":\"world\"}]", "{\"generated_text\":\"world\"}"] ]
    , testGroup "classification response shapes"
        [ testCase (show index) $ withFixture (jsonReply payload) $ \client _ ->
            runHuggingFaceM (classifyText client repository "great") >>= (@?= Right [Classification "POSITIVE" 0.9])
        | (index, payload) <- zip [1 :: Int ..] ["[{\"label\":\"POSITIVE\",\"score\":0.9}]", "[[{\"label\":\"POSITIVE\",\"score\":0.9}]]"] ]
    , testCase "generic inference preserves custom JSON" $ withFixture (jsonReply "{\"answer\":42}") $ \client _ -> do
        result <- runHuggingFaceM (infer client repository (object ["inputs" .= ("custom task" :: Text)])) :: IO (Either HuggingFaceError Value)
        result @?= Right (object ["answer" .= (42 :: Int)])
    ]
