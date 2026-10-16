--------------------------------------------------------------------
-- |
-- Module    :  Eval.Runtime.System
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- What a program can reach of the system it runs on: its environment and the network.
--
-- Every primitive here is an 'EIOPrim', a method of the Catln IO object, and reaches the system only
-- through the 'IOHandle' of the IO value it was called on. So what a run can see is decided by
-- whoever started it: 'realIOHandle' for a program being run, and a mock for a test or a debugger,
-- which says what the environment holds and whether a request is sent at all.
--------------------------------------------------------------------

module Eval.Runtime.System (
  systemPrims,
  realIOHandle,
  realHttpPost,
  ) where

import           Control.Exception         (displayException, try)
import           CtConstants
import qualified Data.ByteString.Lazy.UTF8 as LBU
import qualified Data.ByteString.UTF8      as BU
import qualified Data.HashMap.Strict       as H
import           Data.String               (fromString)
import           Eval.Common
import           Network.HTTP.Simple       (HttpException, getResponseBody,
                                            getResponseStatusCode, httpLBS,
                                            parseRequest, setRequestBodyLBS,
                                            setRequestHeaders)
import           System.Environment        (lookupEnv)
import           System.IO                 (hFlush, isEOF, stdout)
import           Text.Printf

-- | The primitives a program reaches the system through.
systemPrims :: [EPrim]
systemPrims = [ioEnv, ioHttpPost]

-- | The process's own stdin, stdout, environment and network.
--
-- Output is flushed before a line is read, so a prompt printed just before asking is on screen by
-- the time the program waits for the answer.
realIOHandle :: IOHandle
realIOHandle = IOHandle
  { iohPutLine = putStrLn
  , iohGetLine = getLine'
  , iohLookupEnv = lookupEnv
  , iohHttpPost = realHttpPost
  }
  where
    getLine' = do
      hFlush stdout
      eof <- isEOF
      if eof
        then return $ Left EndOfInput
        else Right <$> getLine

-- | Sends a POST request over the network.
--
-- The body is sent and read as UTF-8, which is what a Catln String is written out as and what a JSON
-- API answers in. A URL that is not one, a request that could not be made, and a response whose
-- status is not a success are each reported rather than raised, with the response body kept in the
-- report, since that is where an API says what was wrong.
realHttpPost :: HttpPost
realHttpPost url headers body = do
  parsed <- try $ parseRequest ("POST " ++ url)
  case parsed of
    Left e -> return $ Left $ HttpInvalidUrl $ displayException (e :: HttpException)
    Right req -> do
      let req' = setRequestBodyLBS (LBU.fromString body) $ setRequestHeaders [(fromString k, BU.fromString v) | (k, v) <- headers] req
      res <- try $ httpLBS req'
      return $ case res of
        Left e -> Left $ HttpUnreachable $ displayException (e :: HttpException)
        Right resp
          | status >= 200 && status < 300 -> Right $ LBU.toString $ getResponseBody resp
          | otherwise -> Left $ HttpStatus status (LBU.toString $ getResponseBody resp)
          where status = getResponseStatusCode resp

-- | An IO primitive's answer: its value in the io it was called in.
ioContextVal' :: H.HashMap String Val -> Val -> Val
ioContextVal' args v = ioContextVal v (H.lookupDefault unitVal ioStr args)

-- | Backs @env{io -> IO}(name -> String) -> Maybe[String]{IO}@: an environment variable's value, or
-- @Nothing@ where it is not set.
ioEnv :: EPrim
ioEnv = EIOPrim "ioEnv" prim
  where
    prim handle args = case valStrArg "IO.env" "name" args of
      Left err   -> return $ Left err
      Right name -> Right . ioContextVal' args . maybeStr <$> iohLookupEnv handle name
    maybeStr Nothing  = TupleVal ctMaybeNothing H.empty
    maybeStr (Just v) = TupleVal ctMaybeSome (H.singleton (runtimeArgKey ctMaybeSomeValue) (StrVal v))

-- | Backs @httpPost{io -> IO}(url -> String, headers -> List[Http/Header], body -> String) ->
-- Result[String, Http/HttpErr]{IO}@: the body of the response, or why there is none, in the io it is
-- called in.
--
-- Arguments of the wrong shape still fail the evaluation, since a program that type-checked cannot
-- pass them and so one that does is a fault of the compiler's rather than of the request.
ioHttpPost :: EPrim
ioHttpPost = EIOPrim "ioHttpPost" prim
  where
    prim handle args = case request args of
      Left err -> return $ Left err
      Right (url, headers, body) -> Right . ioContextVal' args . resultVal . either (Left . httpErrVal url) (Right . StrVal) <$> iohHttpPost handle url headers body
    request args = do
      url <- valStrArg "IO.httpPost" "url" args
      body <- valStrArg "IO.httpPost" "body" args
      headerVals <- valList =<< maybe (Left "IO.httpPost has no /headers") Right (H.lookup (runtimeArgKey "headers") args)
      headers <- mapM header headerVals
      return (url, headers, body)
    header (TupleVal name hArgs) = (,) <$> valStrArg name "name" hArgs <*> valStrArg name "value" hArgs
    header v = Left $ printf "%s is not an HTTP header" (show v)

-- | The @\/Http\/HttpErr@ saying why the request to @url@ has no response.
httpErrVal :: String -> HttpFailure -> Val
httpErrVal url failure = case failure of
  HttpInvalidUrl detail   -> err ctHttpErrInvalidUrl [(ctHttpErrDetail, StrVal detail)]
  HttpUnreachable detail  -> err ctHttpErrUnreachable [(ctHttpErrDetail, StrVal detail)]
  HttpStatus code resBody -> err ctHttpErrStatus [(ctHttpErrCode, IntVal $ toInteger code), (ctHttpErrBody, StrVal resBody)]
  HttpNotSent             -> err ctHttpErrNotSent []
  where
    err name fields = TupleVal name $ H.fromList [(runtimeArgKey k, v) | (k, v) <- (ctHttpErrUrl, StrVal url) : fields]
