-- | User space held in Haskell for want of a surface: @IO.env@, @IO.httpPost@ and @IO.llm@ are
-- declared in @stack\/env@, @stack\/http@ and @stack\/llm@, but a Catln @#test@ is built with no
-- @io@, no environment and no network to mock, so what each sends and reads can only be run here.
-- The pure parts of @llm@ are print goldens of its own page.
--
--------------------------------------------------------------------
-- |
-- Module    :  Eval.SystemTests
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- The IO methods that reach the system a program runs on: @IO.env@, @IO.httpPost@ and
-- @IO.readLine@.
--
-- Each program is run against a mock, which is what says what the environment holds and what a
-- request answers with, so the programs are checked without depending on the machine running the
-- tests. The one thing a mock cannot check is the transport itself, so 'realHttpPost' is checked on
-- its own against a server started here.
--------------------------------------------------------------------

module Eval.SystemTests (systemTests) where

import           Control.Concurrent        (forkIO, killThread, threadDelay)
import           Control.Exception         (bracket)
import           Control.Monad             (forM_)
import           CRes
import           CtService
import qualified Data.ByteString.Lazy.UTF8 as LBU
import qualified Data.HashMap.Strict       as H
import           Data.IORef
import           Data.List                 (isInfixOf)
import qualified Data.Text.Lazy            as TL
import           Eval.Common               (HttpFailure (..), HttpPost, MockIO,
                                            getMockIOOutput, newMockIO,
                                            newMockIOWith)
import           Eval.Runtime.System       (realHttpPost)
import           Network.HTTP.Types        (status500)
import           Semantics                 (testCtssConfig)
import           Syntax.Ct.Formatter       (formatRootPrgm)
import           Syntax.Parsers            (mkDesCanonicalImportStr)
import           System.Directory          (copyFile, createDirectoryIfMissing,
                                            getTemporaryDirectory,
                                            removePathForcibly)
import           System.FilePath           ((</>))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Web.Scotty                (body, header, post, raw, scotty,
                                            status)

systemTests :: TestTree
systemTests = withResource fixtures (const $ return ()) $ \getCtss -> testGroup "SystemTests"
  [ testEnvReadsTheMockEnvironment getCtss
  , testHttpPostSendsThroughTheHandle getCtss
  , testMockRefusesTheNetwork getCtss
  , testReadLineWithoutInput getCtss
  , testLlmAsksTheConfiguredModel getCtss
  , testLlmReportsAFailedRequest getCtss
  , testLlmWithoutAModelAsksNothing getCtss
  , testProcessAnswersQueries getCtss
  , testProcessWithoutAModelAnswersNothing getCtss
  , fileProcessorTests
  , testRealHttpPost
  ]
  where
    -- One service for every fixture, so the library they import is built once rather than once
    -- per test.
    fixtures = ctssBaseFiles testCtssConfig [fixture "env.ct", fixture "http.ct", fixture "readline.ct", fixture "llm.ct", fixture "query.ct"] :: IO CTSS

fixture :: String -> FilePath
fixture name = "test/Integration/code/artifacts/system/" ++ name

-- | Runs a fixture's @main@ against the mock given: its exit code, or why it failed, and what it
-- printed.
runWith :: IO CTSS -> FilePath -> MockIO -> IO (CRes Integer, String)
runWith getCtss page mockIO = do
  ctss <- getCtss
  name <- mkDesCanonicalImportStr testCtssConfig page
  ctssBuildFrom ctss name
  res <- runCResT $ getEvaluatedMock ctss mockIO name "main"
  out <- getMockIOOutput mockIO
  return (res, out)

testEnvReadsTheMockEnvironment :: IO CTSS -> TestTree
testEnvReadsTheMockEnvironment getCtss = testCase "IO.env reads the environment the run was given" $ do
  mockIO <- newMockIOWith (H.singleton "CATLN_TEST_VAR" "hello") refuse
  (res, out) <- runWith getCtss (fixture "env.ct") mockIO
  case res of
    CErr notes -> assertFailure $ "the run failed: " ++ prettyCNotes notes
    CRes _ code -> do
      assertEqual "it exits cleanly" 0 code
      assertEqual "a set variable is its value, and an unset one is nothing" "set to hello\nnot set\n" out
  where
    refuse _ _ _ = return $ Left HttpNotSent

testHttpPostSendsThroughTheHandle :: IO CTSS -> TestTree
testHttpPostSendsThroughTheHandle getCtss = testCase "IO.httpPost sends through the handle and answers with the body" $ do
  sent <- newIORef []
  let answer :: HttpPost
      answer url headers reqBody = do
        modifyIORef sent ((url, headers, reqBody):)
        return $ Right "pong"
  mockIO <- newMockIOWith H.empty answer
  (res, out) <- runWith getCtss (fixture "http.ct") mockIO
  requests <- readIORef sent
  case res of
    CErr notes -> assertFailure $ "the run failed: " ++ prettyCNotes notes
    CRes _ _ -> do
      assertEqual "exactly the one request is sent, as written" [("http://example.test/echo", [("X-Test", "tok")], "héllo")] requests
      assertEqual "and what it answered is what was printed" "pong\n" out

testMockRefusesTheNetwork :: IO CTSS -> TestTree
testMockRefusesTheNetwork getCtss = testCase "a run with the default mock makes no request, and says so" $ do
  mockIO <- newMockIO
  (res, out) <- runWith getCtss (fixture "http.ct") mockIO
  case res of
    CErr notes -> assertFailure $ "the run failed: " ++ prettyCNotes notes
    CRes _ _ -> assertEqual "the program is answered with why there is no response" "error: the request to http://example.test/echo was not sent, as this run reaches no network\n" out

-- | A mocked run has no input, and a program reading a line is answered with that rather than
-- failing.
testReadLineWithoutInput :: IO CTSS -> TestTree
testReadLineWithoutInput getCtss = testCase "IO.readLine in a run with no input answers why" $ do
  mockIO <- newMockIO
  (res, out) <- runWith getCtss (fixture "readline.ct") mockIO
  case res of
    CErr notes -> assertFailure $ "the run failed: " ++ prettyCNotes notes
    CRes _ code -> do
      assertEqual "it exits cleanly" 0 code
      assertEqual "it is answered with why there is no line" "error: this run has no input to read\n" out

-- | @IO.llm@ end to end: the environment it is configured by, the request it sends, and the reply
-- it reads back out of the response. The response is a chat completion as the API answers one.
testLlmAsksTheConfiguredModel :: IO CTSS -> TestTree
testLlmAsksTheConfiguredModel getCtss = testCase "IO.llm asks the configured model and prints its reply" $ do
  sent <- newIORef []
  let answer :: HttpPost
      answer url headers reqBody = do
        modifyIORef sent ((url, headers, reqBody):)
        return $ Right "{\"id\": \"x\", \"choices\": [{\"index\": 0, \"message\": {\"role\": \"assistant\", \"content\": \"Hi there!\"}}]}"
  let env = H.fromList [("OPENAI_BASE_URL", "http://model.test/v1"), ("OPENAI_MODEL", "m-1"), ("OPENAI_API_KEY", "sk-test")]
  mockIO <- newMockIOWith env answer
  (res, out) <- runWith getCtss (fixture "llm.ct") mockIO
  requests <- readIORef sent
  case res of
    CErr notes -> assertFailure $ "the run failed: " ++ prettyCNotes notes
    CRes _ _ -> do
      assertEqual "one request, to the chat completions of the configured address, as JSON and with the key"
        [ ( "http://model.test/v1/chat/completions"
          , [("Content-Type", "application/json"), ("Authorization", "Bearer sk-test")]
          , "{\"model\":\"m-1\",\"messages\":[{\"role\":\"user\",\"content\":\"Say hi\"}]}" ) ]
        requests
      assertEqual "and the reply is what the model said" "Hi there!\n" out

-- | A request answered with a failure is the reply's failure, with the status and the body the API
-- said what was wrong in.
testLlmReportsAFailedRequest :: IO CTSS -> TestTree
testLlmReportsAFailedRequest getCtss = testCase "IO.llm answered with a failing status says so" $ do
  let answer :: HttpPost
      answer _ _ _ = return $ Left $ HttpStatus 401 "{\"error\": {\"message\": \"Incorrect API key\"}}"
  let env = H.fromList [("OPENAI_BASE_URL", "http://model.test/v1"), ("OPENAI_MODEL", "m-1")]
  mockIO <- newMockIOWith env answer
  (res, out) <- runWith getCtss (fixture "llm.ct") mockIO
  case res of
    CErr notes -> assertFailure $ "the run failed: " ++ prettyCNotes notes
    CRes _ _   -> assertEqual "the status and body are the reply's failure" "error: the model could not be asked: the request to http://model.test/v1/chat/completions answered with status 401: {\"error\": {\"message\": \"Incorrect API key\"}}\n" out

-- | Without a model to ask there is nothing to send, and the answer says which setting is missing.
testLlmWithoutAModelAsksNothing :: IO CTSS -> TestTree
testLlmWithoutAModelAsksNothing getCtss = testCase "IO.llm without OPENAI_MODEL sends nothing" $ do
  mockIO <- newMockIOWith (H.singleton "OPENAI_BASE_URL" "http://model.test/v1") (\_ _ _ -> return $ Left HttpNotSent)
  (res, out) <- runWith getCtss (fixture "llm.ct") mockIO
  case res of
    CErr notes -> assertFailure $ "the run failed: " ++ prettyCNotes notes
    CRes _ _   -> assertEqual "the unset variable is named" "error: OPENAI_MODEL is not set, so there is no model to ask\n" out

-- | Processes a fixture's annotations against the mock given ('getProcessedMock'): the page as it is
-- written back, how many annotations line processors updated and how many of those came back
-- unchanged, and how many times file processors changed the file.
processWith :: IO CTSS -> FilePath -> MockIO -> IO (CRes (String, Int, Int, Int))
processWith getCtss page mockIO = do
  ctss <- getCtss
  name <- mkDesCanonicalImportStr testCtssConfig page
  ctssBuildFrom ctss name
  res <- runCResT $ getProcessedMock ctss mockIO name
  return $ fmap (\ProcessReport{prPage, prLines, prLinesUnchanged, prFileRounds} -> (formatRootPrgm prPage, prLines, prLinesUnchanged, prFileRounds)) res

-- | @catln process@ end to end: each unanswered @q>@ is asked of the model with the page it is
-- written on, and written back with the reply as its @>>>@ lines. One already answered is not asked.
testProcessAnswersQueries :: IO CTSS -> TestTree
testProcessAnswersQueries getCtss = testCase "processing a page asks each unanswered q> and writes the reply under it" $ do
  sent <- newIORef []
  let answer :: HttpPost
      answer _ _ reqBody = do
        modifyIORef sent (reqBody:)
        return $ Right "{\"choices\": [{\"message\": {\"content\": \"Twice x.\\nAlways.\"}}]}"
  let env = H.fromList [("OPENAI_BASE_URL", "http://model.test/v1"), ("OPENAI_MODEL", "m-1")]
  mockIO <- newMockIOWith env answer
  res <- processWith getCtss (fixture "query.ct") mockIO
  requests <- readIORef sent
  case res of
    CErr notes -> assertFailure $ "processing failed: " ++ prettyCNotes notes
    CRes _ (page, processed, unchanged, _) -> do
      assertEqual "only the unanswered query is asked" 1 (length requests)
      assertBool "it is asked with the page it is written on" (all ("double(x)= x + x" `isInfixOf`) requests)
      assertBool "and with the question" (all ("What does double do?" `isInfixOf`) requests)
      assertEqual "one annotation is processed, and it changed" (1, 0) (processed, unchanged)
      assertBool ("the reply is written under the question:\n" ++ page) ("q> What does double do?\n  >>> Twice x.\n  >>> Always.\n" `isInfixOf` page)
      assertBool ("the answered query is left as it was:\n" ++ page) ("q> Is double fast?\n  >>> Yes.\n" `isInfixOf` page)

-- | Without a model to ask, nothing is sent, the query is written back as it was, and why it was
-- not answered is reported.
testProcessWithoutAModelAnswersNothing :: IO CTSS -> TestTree
testProcessWithoutAModelAnswersNothing getCtss = testCase "processing a page without a model leaves its queries unanswered and says why" $ do
  mockIO <- newMockIOWith H.empty (\_ _ _ -> return $ Left HttpNotSent)
  res <- processWith getCtss (fixture "query.ct") mockIO
  case res of
    CErr notes -> assertFailure $ "processing failed: " ++ prettyCNotes notes
    CRes notes (page, processed, unchanged, _) -> do
      assertEqual "no annotation is processed" (0, 0) (processed, unchanged)
      assertBool ("it is written back unanswered:\n" ++ page) ("q> What does double do?\nq> Is double fast?" `isInfixOf` page)
      assertBool ("the unset variable is reported: " ++ prettyCNotes notes) ("OPENAI_BASE_URL is not set" `isInfixOf` prettyCNotes notes)

-- | File processors rewrite the file they are written in, so each is run on a copy of its fixture in
-- a directory of its own rather than on the fixture.
fileProcessorTests :: TestTree
fileProcessorTests = withResource copies (const $ return ()) $ \getCopies -> testGroup "file processors"
  [ testFindReplaceRenames getCopies
  , testFindReplaceIsDoneOnceRun getCopies
  , testFindReplaceWithNothingToReplace getCopies
  , testFileProcessorAfterLineProcessor getCopies
  ]
  where
    pages = ["findReplace.ct", "findReplaceGrows.ct", "findReplaceAbsent.ct", "findReplaceAfterQuery.ct"]
    -- One service for every copy, so the library they import is built once rather than once per
    -- test.
    copies = do
      tmp <- getTemporaryDirectory
      let dir = tmp </> "catln-file-processors"
      removePathForcibly dir
      createDirectoryIfMissing True dir
      forM_ pages $ \page -> copyFile (fixture page) (dir </> page)
      ctss <- ctssBaseFiles testCtssConfig (map (dir </>) pages)
      return (ctss, dir)

-- | Processes a copied fixture ('processWith'), and reads back the file it was written to.
processCopy :: IO (CTSS, FilePath) -> FilePath -> MockIO -> IO (CRes (String, Int, Int, Int), String)
processCopy getCopies page mockIO = do
  (ctss, dir) <- getCopies
  res <- processWith (return ctss) (dir </> page) mockIO
  file <- readFile (dir </> page)
  length file `seq` return (res, file)

-- | A @#findReplace@ replaces what it finds throughout the page, removes itself, and the page it
-- leaves compiles.
testFindReplaceRenames :: IO (CTSS, FilePath) -> TestTree
testFindReplaceRenames getCopies = testCase "#findReplace replaces throughout the page and removes itself" $ do
  mockIO <- newMockIO
  (res, file) <- processCopy getCopies "findReplace.ct" mockIO
  case res of
    CErr notes -> assertFailure $ "processing failed: " ++ prettyCNotes notes
    CRes _ (page, _, _, rounds) -> do
      assertEqual "the file is changed once" 1 rounds
      assertBool ("nothing is left to replace:\n" ++ file) (not $ "oldName" `isInfixOf` file)
      assertBool ("the definition is renamed:\n" ++ file) ("newName(x)= x + 1" `isInfixOf` file)
      assertBool ("its uses are renamed:\n" ++ file) ("newName(x= newName(x= x))" `isInfixOf` file)
      assertBool ("the annotation removes itself:\n" ++ file) (not $ "#findReplace" `isInfixOf` file)
      assertEqual "the page processed last is the file written" file page

-- | A @#findReplace@ whose replacement holds what it finds is run once, as it is not run on what it
-- wrote.
testFindReplaceIsDoneOnceRun :: IO (CTSS, FilePath) -> TestTree
testFindReplaceIsDoneOnceRun getCopies = testCase "#findReplace is not run on what it wrote" $ do
  mockIO <- newMockIO
  (res, file) <- processCopy getCopies "findReplaceGrows.ct" mockIO
  case res of
    CErr notes -> assertFailure $ "processing failed: " ++ prettyCNotes notes
    CRes _ (_, _, _, rounds) -> do
      assertEqual "the file is changed once" 1 rounds
      assertBool ("each occurrence is replaced once:\n" ++ file) ("incinc(x -> Integer)= x + 1" `isInfixOf` file)
      assertBool ("and no more than once:\n" ++ file) (not $ "incincinc" `isInfixOf` file)

-- | A @#findReplace@ which finds nothing still removes itself, which is a change to the page.
testFindReplaceWithNothingToReplace :: IO (CTSS, FilePath) -> TestTree
testFindReplaceWithNothingToReplace getCopies = testCase "#findReplace finding nothing still removes itself" $ do
  mockIO <- newMockIO
  (res, file) <- processCopy getCopies "findReplaceAbsent.ct" mockIO
  case res of
    CErr notes -> assertFailure $ "processing failed: " ++ prettyCNotes notes
    CRes _ (_, _, _, rounds) -> do
      assertEqual "the file is changed once" 1 rounds
      assertBool ("the annotation removes itself:\n" ++ file) (not $ "#findReplace" `isInfixOf` file)
      assertBool ("the rest is as it was:\n" ++ file) ("inc(x -> Integer)= x + 1" `isInfixOf` file)

-- | Processors run top to bottom: a question above a @#findReplace@ is answered first, the rewrite is
-- of the page with the answer, and the page processed again from the top does not ask it again.
testFileProcessorAfterLineProcessor :: IO (CTSS, FilePath) -> TestTree
testFileProcessorAfterLineProcessor getCopies = testCase "a file processor rewrites the page with the line processors above it applied" $ do
  sent <- newIORef []
  let answer :: HttpPost
      answer _ _ reqBody = do
        modifyIORef sent (reqBody:)
        return $ Right "{\"choices\": [{\"message\": {\"content\": \"Twice x.\"}}]}"
  let env = H.fromList [("OPENAI_BASE_URL", "http://model.test/v1"), ("OPENAI_MODEL", "m-1")]
  mockIO <- newMockIOWith env answer
  (res, file) <- processCopy getCopies "findReplaceAfterQuery.ct" mockIO
  requests <- readIORef sent
  case res of
    CErr notes -> assertFailure $ "processing failed: " ++ prettyCNotes notes
    CRes _ (_, _, _, rounds) -> do
      assertEqual "the question is asked once" 1 (length requests)
      assertBool "it is asked of the page before the rewrite" (all ("double(x)= x + x" `isInfixOf`) requests)
      assertEqual "the file is changed once" 1 rounds
      assertBool ("the answer is kept by the rewrite:\n" ++ file) ("q> What does twice do?\n  >>> Twice x.\n" `isInfixOf` file)
      assertBool ("the page is rewritten:\n" ++ file) ("twice(x)= x + x" `isInfixOf` file)

-- | The transport, against a server answering on a port of this machine: the header and the UTF-8
-- body arrive as sent, and a status that is not a success is reported with the body it came with.
testRealHttpPost :: TestTree
testRealHttpPost = testCase "realHttpPost sends a request and reads the response" $
  bracket (forkIO $ scotty port app) killThread $ \_ -> do
    ok <- untilAnswered 50 $ realHttpPost (base ++ "/echo") [("X-Test", "tok")] "héllo"
    assertEqual "the header and body arrive as sent" (Right "tok|héllo") ok
    failed <- realHttpPost (base ++ "/fail") [] ""
    assertEqual "a failing status is reported with its body" (Left $ HttpStatus 500 "broken") failed
    invalid <- realHttpPost "not a url" [] ""
    case invalid of
      Left HttpInvalidUrl{} -> return ()
      other -> assertFailure $ "a URL that is not one was not reported as one: " ++ show other
  where
    port = 38472
    base = "http://127.0.0.1:" ++ show port
    app = do
      post "/echo" $ do
        h <- header "X-Test"
        b <- body
        raw $ LBU.fromString $ maybe "" TL.unpack h ++ "|" ++ LBU.toString b
      post "/fail" $ do
        status status500
        raw "broken"
    -- The server is started on another thread, so the first requests can arrive before it listens.
    untilAnswered :: Int -> IO (Either HttpFailure String) -> IO (Either HttpFailure String)
    untilAnswered tries req = do
      r <- req
      case r of
        Left HttpUnreachable{} | tries > 0 -> threadDelay 100000 >> untilAnswered (tries - 1) req
        _ -> return r
