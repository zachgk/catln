--------------------------------------------------------------------
-- |
-- Module    :  Eval.SystemTests
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- The IO methods that reach the system a program runs on: @IO.env@ and @IO.httpPost@.
--
-- Each program is run against a mock, which is what says what the environment holds and what a
-- request answers with, so the programs are checked without depending on the machine running the
-- tests. The one thing a mock cannot check is the transport itself, so 'realHttpPost' is checked on
-- its own against a server started here.
--------------------------------------------------------------------

module Eval.SystemTests (systemTests) where

import           Control.Concurrent        (forkIO, killThread, threadDelay)
import           Control.Exception         (bracket)
import           CRes
import           CtService
import qualified Data.ByteString.Lazy.UTF8 as LBU
import qualified Data.HashMap.Strict       as H
import           Data.IORef
import           Data.List                 (isInfixOf)
import qualified Data.Text.Lazy            as TL
import           Eval.Common               (HttpPost, MockIO, getMockIOOutput,
                                            newMockIO, newMockIOWith)
import           Eval.Runtime.System       (realHttpPost)
import           Network.HTTP.Types        (status500)
import           Semantics                 (testCtssConfig)
import           Syntax.Ct.Formatter       (formatRootPrgm)
import           Syntax.Parsers            (mkDesCanonicalImportStr)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Web.Scotty                (body, header, post, raw, scotty,
                                            status)

systemTests :: TestTree
systemTests = withResource fixtures (const $ return ()) $ \getCtss -> testGroup "SystemTests"
  [ testEnvReadsTheMockEnvironment getCtss
  , testHttpPostSendsThroughTheHandle getCtss
  , testMockRefusesTheNetwork getCtss
  , testLlmAsksTheConfiguredModel getCtss
  , testLlmWithoutAModelAsksNothing getCtss
  , testProcessAnswersQueries getCtss
  , testProcessWithoutAModelAnswersNothing getCtss
  , testRealHttpPost
  ]
  where
    -- One service for every fixture, so the library they import is built once rather than once
    -- per test.
    fixtures = ctssBaseFiles testCtssConfig [fixture "env.ct", fixture "http.ct", fixture "llm.ct", fixture "query.ct"] :: IO CTSS

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
    refuse _ _ _ = return $ Left "no requests expected"

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
testMockRefusesTheNetwork getCtss = testCase "a run with the default mock makes no request" $ do
  mockIO <- newMockIO
  (res, _) <- runWith getCtss (fixture "http.ct") mockIO
  case res of
    CRes _ _ -> assertFailure "the request was answered, so something was sent"
    CErr notes -> assertBool ("the refusal is what failed the run: " ++ prettyCNotes notes) ("makes no network requests" `isInfixOf` prettyCNotes notes)

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

-- | Without a model to ask there is nothing to send, and the answer says there is no reply.
testLlmWithoutAModelAsksNothing :: IO CTSS -> TestTree
testLlmWithoutAModelAsksNothing getCtss = testCase "IO.llm without OPENAI_MODEL sends nothing" $ do
  mockIO <- newMockIOWith (H.singleton "OPENAI_BASE_URL" "http://model.test/v1") (\_ _ _ -> return $ Left "no request should be sent")
  (res, out) <- runWith getCtss (fixture "llm.ct") mockIO
  case res of
    CErr notes -> assertFailure $ "the run failed: " ++ prettyCNotes notes
    CRes _ _   -> assertEqual "no reply" "no reply\n" out

-- | Processes a fixture's annotations against the mock given ('getProcessedMock'): the page as it is
-- written back, and how many annotations were processed and came back unchanged.
processWith :: IO CTSS -> FilePath -> MockIO -> IO (CRes (String, Int, Int))
processWith getCtss page mockIO = do
  ctss <- getCtss
  name <- mkDesCanonicalImportStr testCtssConfig page
  ctssBuildFrom ctss name
  res <- runCResT $ getProcessedMock ctss mockIO name
  return $ fmap (\(prgm, processed, unchanged) -> (formatRootPrgm prgm, processed, unchanged)) res

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
    CRes _ (page, processed, unchanged) -> do
      assertEqual "only the unanswered query is asked" 1 (length requests)
      assertBool "it is asked with the page it is written on" (all ("double(x)= x + x" `isInfixOf`) requests)
      assertBool "and with the question" (all ("What does double do?" `isInfixOf`) requests)
      assertEqual "one annotation is processed, and it changed" (1, 0) (processed, unchanged)
      assertBool ("the reply is written under the question:\n" ++ page) ("q> What does double do?\n  >>> Twice x.\n  >>> Always.\n" `isInfixOf` page)
      assertBool ("the answered query is left as it was:\n" ++ page) ("q> Is double fast?\n  >>> Yes.\n" `isInfixOf` page)

-- | Without a model to ask, nothing is sent, and the query is written back as it was.
testProcessWithoutAModelAnswersNothing :: IO CTSS -> TestTree
testProcessWithoutAModelAnswersNothing getCtss = testCase "processing a page without OPENAI_MODEL leaves its queries unanswered" $ do
  mockIO <- newMockIOWith H.empty (\_ _ _ -> return $ Left "no request should be sent")
  res <- processWith getCtss (fixture "query.ct") mockIO
  case res of
    CErr notes -> assertFailure $ "processing failed: " ++ prettyCNotes notes
    CRes _ (page, processed, unchanged) -> do
      assertEqual "the query is processed, and comes back unchanged" (1, 1) (processed, unchanged)
      assertBool ("it is written back unanswered:\n" ++ page) ("q> What does double do?\nq> Is double fast?" `isInfixOf` page)

-- | The transport, against a server answering on a port of this machine: the header and the UTF-8
-- body arrive as sent, and a status that is not a success is reported with the body it came with.
testRealHttpPost :: TestTree
testRealHttpPost = testCase "realHttpPost sends a request and reads the response" $
  bracket (forkIO $ scotty port app) killThread $ \_ -> do
    ok <- untilAnswered 50 $ realHttpPost (base ++ "/echo") [("X-Test", "tok")] "héllo"
    assertEqual "the header and body arrive as sent" (Right "tok|héllo") ok
    failed <- realHttpPost (base ++ "/fail") [] ""
    case failed of
      Right r -> assertFailure $ "a failing status was answered as a success: " ++ r
      Left err -> assertBool ("the status and body are reported: " ++ err) ("500" `isInfixOf` err && "broken" `isInfixOf` err)
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
    untilAnswered :: Int -> IO (Either String String) -> IO (Either String String)
    untilAnswered tries req = do
      r <- req
      case r of
        Left err | "could not be made" `isInfixOf` err && tries > 0 -> threadDelay 100000 >> untilAnswered (tries - 1) req
        _ -> return r
