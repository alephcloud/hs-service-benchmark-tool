-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Main
--
-- Please feel free to contact us at licensing@pivotmail.com with any
-- contributions, additions, or other feedback; we would love to hear from
-- you.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain a
-- copy of the License at http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.

-- |
-- Module: Main
-- Description: benchmarks for http-client and http-streams
-- Copyright: Copyright (c) 2013-2014 PivotCloud, Inc.
-- License: Apache License, Version 2.0
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
-- Here are the results for best performance when the number of threads
-- is larger than the number of cores:
--
-- > http-client-local-manager ---\                          /--- http-streamsIORef ---\    /--- http-streams-ioref-no-catchany
-- >                               -- http-streams-mvar -----                           ----
-- > http-client-global-manager --/                          \--- http-streams-state --/    \--- http-streams-mvar-no-catchany
--
-- When the number of cores matches the number of concurrent threads the
-- performance of all benchmarks is in the same order with the "no-catchany"
-- variants somewhat better than all others.
--

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main
( main
) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception.Enclosed
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Control
import Control.Monad.State.Strict

import Criterion.Main

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.Either
import Data.IORef
import qualified Data.List as L
import Data.Monoid.Unicode
import Data.String
import qualified Data.Text as T
import Data.Word

import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as WARP

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP
import qualified Network.Http.Client as HS

import Prelude.Unicode

import qualified System.IO.Streams as S
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomIO)

-- -------------------------------------------------------------------------- --
-- Utils

sshow ∷ (Show a, IsString s) ⇒ a → s
sshow = fromString ∘ show

server ∷ Int → IO ()
server port = WARP.run port $ \req respond → do
        body ← WAI.strictRequestBody req
        respond $ WAI.responseLBS HTTP.status200 [] body

-- -------------------------------------------------------------------------- --
-- Benchmark Parameters

data BenchmarkType
    = HttpClientLocalManager
    | HttpClientLocalManagerNoTimeout
    | HttpClientGlobalManager
    | HttpStreamsMVar
    | HttpStreamsMVarNoCatchAny
    | HttpStreamsIORef
    | HttpStreamsIORefNoCatchAny
    | HttpStreamsState
    deriving (Show, Read, Eq, Ord, Bounded, Enum)

benchmarkTypeText
    ∷ IsString s
    ⇒ BenchmarkType
    → s
benchmarkTypeText HttpClientLocalManager = "http-client-local-manager"
benchmarkTypeText HttpClientLocalManagerNoTimeout = "http-client-local-manager-no-timeout"
benchmarkTypeText HttpClientGlobalManager = "http-client-global-manager"
benchmarkTypeText HttpStreamsMVar = "http-streams-mvar"
benchmarkTypeText HttpStreamsMVarNoCatchAny = "http-streams-mvar-no-catchany"
benchmarkTypeText HttpStreamsIORef = "http-streams-ioref"
benchmarkTypeText HttpStreamsIORefNoCatchAny = "http-streams-ioref-no-catchany"
benchmarkTypeText HttpStreamsState = "http-streams-state"

data BenchmarkParams = BenchmarkParams
    { _bparamsRequestSize ∷ ![Int]
        -- ^ The size of the request body. If this @0@ the request is made
        -- as a @GET@ request. Otherwise a @POST@ request is performed.

    , _bparamsTotalRequestsN ∷ ![Int]
        -- ^ The total number of requests made in each benchmark.
        --
        -- Note that he number of requests per thread is computed as @_pbaramsTotalRequestsN
        -- `div` _bparamsThreadsN@. Therefor, with a poor choice of the parameters, the
        -- actually executed number of requests can be off from
        -- @_bparamsTotalRequestsN@ by up to @_bparamsThreadsN@.

    , _bparamsThreadsN ∷ ![Int]
        -- ^ number of independent threads making concurrent requests.

    , _bparamsTypes ∷ ![BenchmarkType]
        -- ^ the set of different benchmark setups that is used.

    , _bparamsUrl ∷ !T.Text
        -- ^ The HTTP URL of the request.
    }
    deriving (Show, Read, Eq, Ord)

-- -------------------------------------------------------------------------- --
-- Some predefined benchmark groups

benchmarksAll ∷ [BenchmarkType]
benchmarksAll =
    [ HttpClientLocalManager
    , HttpClientLocalManagerNoTimeout
    , HttpClientGlobalManager
    , HttpStreamsMVar
    , HttpStreamsMVarNoCatchAny
    , HttpStreamsIORef
    , HttpStreamsIORefNoCatchAny
    , HttpStreamsState
    ]

benchmarksHttpClient ∷ [BenchmarkType]
benchmarksHttpClient =
    [ HttpClientLocalManager
    , HttpClientLocalManagerNoTimeout
    , HttpClientGlobalManager
    ]

benchmarksHttpStreams ∷ [BenchmarkType]
benchmarksHttpStreams =
    [ HttpStreamsMVar
    , HttpStreamsMVarNoCatchAny
    , HttpStreamsIORef
    , HttpStreamsIORefNoCatchAny
    , HttpStreamsState
    ]

benchmarksBest ∷ [BenchmarkType]
benchmarksBest =
    [ HttpClientLocalManager
    , HttpStreamsIORef
    ]

-- -------------------------------------------------------------------------- --
-- Main

main ∷ IO ()
main = do
    if withWarp
      then
        race (server 8282) (benchmark params) >>= \case
            Right _ → return ()
            Left e → error $ "HTTP server exited unexpectedly: " ⊕ show e
      else
        benchmark params
  where
    params = BenchmarkParams
        { _bparamsRequestSize = [0]
        , _bparamsTotalRequestsN = [4096]
        , _bparamsThreadsN = [1,2,4,8,16,32,64]
        -- , _bparamsThreadsN = [128]
        , _bparamsTypes = [HttpClientGlobalManager, HttpClientLocalManager, HttpStreamsMVar, HttpStreamsIORef]
        -- , _bparamsTypes = [HttpClientGlobalManager, HttpClientLocalManager, HttpStreamsMVar, HttpStreamsIORef, HttpStreamsIORefNoCatchAny]
        , _bparamsUrl = "http://localhost:8282"
        }
    withWarp = True

-- -------------------------------------------------------------------------- --
-- Execute Benchmarks

benchmark ∷ BenchmarkParams → IO ()
benchmark BenchmarkParams{..} = defaultMain $
    [ createBenchmarkGroup httpClientReq httpStreamsReq _bparamsTypes (reqN `div` threadN) threadN size
    | reqN ← _bparamsTotalRequestsN
    , threadN ← _bparamsThreadsN
    , size ← _bparamsRequestSize
    ]
  where
    -- 1 MB of static random bytes
    body ∷ B.ByteString
    body = unsafePerformIO $ fromString <$> replicateM (1024 * 1024) randomIO
    {-# NOINLINE body #-}

    rawRequest = either (error ∘ show) id ∘ HTTP.parseUrl $ T.unpack _bparamsUrl
    path = HTTP.path rawRequest
    host = HTTP.host rawRequest
    port = fromIntegral $ HTTP.port rawRequest

    httpClientReq size = rawRequest
        { HTTP.requestBody = HTTP.RequestBodyBS $ B.take size body
        , HTTP.method = if size ≡ 0 then "GET" else "POST"
        }

    httpStreamsReq size = (host, port, req, reqBody)
      where
        method = if size ≡ 0 then HS.GET else HS.POST
        reqBody = if size ≡ 0 then Nothing else Just (B.take size body)
    #if MIN_VERSION_http_streams(0,8,0)
        req = HS.buildRequest $ do
            HS.http method path
            HS.setAccept "*/*"
    #else
        req = unsafePerformIO ∘ HS.buildRequest $ do
            HS.http method path
            HS.setAccept "*/*"
        {-# NOINLINE req #-}
    #endif

-- -------------------------------------------------------------------------- --
-- Create Benchmarks

createBenchmark
    ∷ (Int → HTTP.Request)
    → (Int → (B8.ByteString, Word16, HS.Request, Maybe B8.ByteString))
    → BenchmarkType
    → Int
        -- ^ number of requests
    → Int
        -- ^ number of threads
    → Int
        -- ^ size of body
    → Benchmark
createBenchmark httpClientReq httpStreamsReq typ reqN threadN size = case typ of
        HttpClientLocalManager → bcClient benchHttpClient
        HttpClientLocalManagerNoTimeout → bcClient benchHttpClientNoTimeout
        HttpClientGlobalManager → bcClient benchHttpClient2
        HttpStreamsMVar → bcStreams benchHttpStreamsMVar
        HttpStreamsMVarNoCatchAny → bcStreams benchHttpStreamsMVarNoCatchAny
        HttpStreamsIORef → bcStreams benchHttpStreamsIORef
        HttpStreamsIORefNoCatchAny → bcStreams benchHttpStreamsIORefNoCatchAny
        HttpStreamsState → bcStreams benchHttpStreamsState
  where
    bcClient f = bench (benchmarkTypeText typ) ∘ nfIO $ f reqN threadN (httpClientReq size)
    bcStreams f = bench (benchmarkTypeText typ) ∘ nfIO $ f reqN threadN (httpStreamsReq size)

createBenchmarkGroup
    ∷ (Int → HTTP.Request)
    → (Int → (B8.ByteString, Word16, HS.Request, Maybe B8.ByteString))
    → [BenchmarkType]
    → Int
        -- ^ number of requests
    → Int
        -- ^ number of threads
    → Int
        -- ^ size of body
    → Benchmark
createBenchmarkGroup httpClientReq httpStreamsReq typs reqN threadN size =
    bgroup label $ map create typs
  where
    label = L.intercalate "/" ["KB " ⊕ show size, "#req " ⊕ show reqN, "#threads " ⊕ show threadN]
    create typ = createBenchmark httpClientReq httpStreamsReq typ reqN threadN size

-- -------------------------------------------------------------------------- --
-- Tools for Creating Benchmarks

check
    ∷ MonadIO m
    ⇒ (Either T.Text α → Bool)
    → ExceptT T.Text m α
    → m ()
check c x = runExceptT x >>= \r →
    unless (c r) $ error "check failed"

-- | http-streams with connection stored in MVar
--
benchHttpStreamsMVar
    ∷ Int
        -- ^ number of threads
    → Int
        -- ^ number requests per thread
    → (B8.ByteString, Word16, HS.Request, Maybe B8.ByteString)
    → IO ()
benchHttpStreamsMVar n p (host, port, req, body) = void ∘ mapConcurrently run $ replicate p req
  where
    run r = withConnection host port $ \useConnection reset →
        replicateM_ n ∘ check isRight $ httpStreamsRequest useConnection reset body r

-- | http-streams with connection stored in MVar without catchAny
--
benchHttpStreamsMVarNoCatchAny
    ∷ Int
        -- ^ number of threads
    → Int
        -- ^ number requests per thread
    → (B8.ByteString, Word16, HS.Request, Maybe B8.ByteString)
    → IO ()
benchHttpStreamsMVarNoCatchAny n p (host, port, req, body) = void ∘ mapConcurrently run $ replicate p req
  where
    run r = withConnection host port $ \useConnection reset →
        replicateM_ n ∘ check isRight $ httpStreamsRequestNoCatchAny useConnection reset body r

-- | http-streams with connection stored in IORef
--
benchHttpStreamsIORef
    ∷ Int
        -- ^ number of threads
    → Int
        -- ^ number requests per thread
    → (B8.ByteString, Word16, HS.Request, Maybe B8.ByteString)
    → IO ()
benchHttpStreamsIORef n p (host, port, req, body) = void ∘ mapConcurrently run $ replicate p req
  where
    run r = withConnection_ host port $ \useConnection reset →
        replicateM_ n ∘ check isRight $ httpStreamsRequest useConnection reset body r

-- | http-streams with connection stored in IORef without catchAny
--
benchHttpStreamsIORefNoCatchAny
    ∷ Int
        -- ^ number of threads
    → Int
        -- ^ number requests per thread
    → (B8.ByteString, Word16, HS.Request, Maybe B8.ByteString)
    → IO ()
benchHttpStreamsIORefNoCatchAny n p (host, port, req, body) = void ∘ mapConcurrently run $ replicate p req
  where
    run r = withConnection_ host port $ \useConnection reset →
        replicateM_ n ∘ check isRight $ httpStreamsRequestNoCatchAny useConnection reset body r

-- | http-streams with connection in a state monad
--
benchHttpStreamsState
    ∷ Int
        -- ^ number of threads
    → Int
        -- ^ number requests per thread
    → (B8.ByteString, Word16, HS.Request, Maybe B8.ByteString)
    → IO ()
benchHttpStreamsState n p (host, port, req, body) = void ∘ mapConcurrently run $ replicate p req
  where
    run ∷ HS.Request → IO ()
    run r = do
        connection ← newConnectionS host port
        (_,connection') ← flip runStateT connection $
            replicateM_ n ∘ check isRight $ httpStreamsRequestS host port body r
        closeConnectionS connection' -- FIXME make this exception safe

-- | http-client with thread local manager
--
benchHttpClient
    ∷ Int
        -- ^ number of threads
    → Int
        -- ^ number requests per thread
    → HTTP.Request
    → IO ()
benchHttpClient n p = void ∘ mapConcurrently run ∘ replicate p
  where
    run req = HTTP.withManager settings $ \mgr → replicateM_ n ∘ check isRight $
        httpClientRequest mgr req
    settings = HTTP.defaultManagerSettings
        { HTTP.managerResponseTimeout = Just 5000000
        , HTTP.managerConnCount = 1
        }

-- | http-client with thread local manager
--
benchHttpClientNoTimeout
    ∷ Int
        -- ^ number requests per thread
    → Int
        -- ^ number of threads
    → HTTP.Request
    → IO ()
benchHttpClientNoTimeout n p = void ∘ mapConcurrently run ∘ replicate p
  where
    run req = HTTP.withManager settings $ \mgr → replicateM_ n ∘ check isRight $
        httpClientRequest mgr req
    settings = HTTP.defaultManagerSettings
        { HTTP.managerResponseTimeout = Nothing
        , HTTP.managerConnCount = 1
        }

-- | http-client global manager
--
benchHttpClient2
    ∷ Int
        -- ^ number of threads
    → Int
        -- ^ number requests per thread
    → HTTP.Request → IO ()
benchHttpClient2 n p req = HTTP.withManager settings $ \mgr →
    void ∘ mapConcurrently (run mgr) $ replicate p req
  where
    run mgr = replicateM_ n ∘ check isRight ∘ httpClientRequest mgr
    settings = HTTP.defaultManagerSettings
        -- { HTTP.managerResponseTimeout = Just 5000000
        { HTTP.managerResponseTimeout = Nothing
        , HTTP.managerConnCount = p
        }

-- -------------------------------------------------------------------------- --
-- http-client
--

httpClientRequest
    ∷ (MonadIO m, MonadError T.Text m)
    ⇒ HTTP.Manager
        -- ^ connection manager
    → HTTP.Request
        -- ^ request
    → m B.ByteString
httpClientRequest mgr req = do
    r ← liftIO $ HTTP.httpLbs req mgr
    case HTTP.responseStatus r of
        s   | HTTP.statusIsSuccessful s → return ∘ LB.toStrict $ HTTP.responseBody r
            | HTTP.statusIsServerError s → throwError $ "server error: " ⊕ sshow s
            | HTTP.statusIsClientError s → throwError $ "client error: " ⊕ sshow s
            | otherwise → throwError $ "unexpected response status: " ⊕ sshow s

-- -------------------------------------------------------------------------- --
-- http-streams

-- | This is not a production level interface to http-streams. It is only
-- meant to roughly exhibit similar performance as an production grade
-- implementation would have.
--
newtype HttpConnection = HttpConnection
    { httpConnectionVar ∷ MVar HS.Connection
    }

newConnection
    ∷ (MonadIO m, Functor m)
    ⇒ B8.ByteString
        -- ^ host name
    → Word16
        -- ^ port
    → m HttpConnection
newConnection host port = HttpConnection
    <$> liftIO (newMVar =<< HS.openConnection host port)

-- | This will deadlock when the connection had been closed before!
--
closeConnection
    ∷ MonadIO m
    ⇒ HttpConnection
    → m ()
closeConnection conn = liftIO $ do
    HS.closeConnection =<< takeMVar (httpConnectionVar conn)

resetConnection
    ∷ MonadIO m
    ⇒ B8.ByteString
        -- ^ host name
    → Word16
        -- ^ port
    → HttpConnection
    → m ()
resetConnection host port conn = do
    closeConnection conn
    liftIO $ putMVar (httpConnectionVar conn) =<< HS.openConnection host port

-- | It is up to the user to explicitly reset the connection if needed.
--
withConnection
    ∷ ∀ m n o α β . (MonadIO m, MonadBaseControl IO m, MonadIO n, MonadBaseControl IO n, MonadIO o)
    ⇒ B8.ByteString
        -- ^ host name
    → Word16
        -- ^ port
    → (((HS.Connection → n β) → n β) → o () →  m α)
    → m α
withConnection host port inner =
    bracket (newConnection host port) closeConnection $ \conn@(HttpConnection var) → do
        let useConnection = bracket (liftIO $ takeMVar var) (liftIO ∘ putMVar var)
        inner useConnection (resetConnection host port conn)

-- |
--
-- No redirections are performed!
--
httpStreamsRequest
    ∷ (MonadIO m, MonadBaseControl IO m, MonadError T.Text m)
    ⇒ ((HS.Connection → m (HTTP.Status, B.ByteString)) → m (HTTP.Status, B.ByteString))
        -- ^ use connection
    → m ()
        -- ^ reset connection
    → Maybe B.ByteString
    → HS.Request
        -- ^ HTTP request
    → m B.ByteString
httpStreamsRequest useConnection reset body req = do
    -- When an error occurred we don't know if the connection is clean. We
    -- could do a little better, but we take a conservative approach and
    -- always reset the connection.
    --
    -- We wrap this to into a 'catchAny' and reset the connection
    -- when an exception occurs.
    (status, responseBody) ← tryAnyText ∘ useConnection $ \connection →
        liftIO $ do
            b ← bodyStream
            HS.sendRequest connection req b
            HS.receiveResponse connection handler
    case status of
        s   | HTTP.statusIsSuccessful s → return responseBody
            | HTTP.statusIsServerError s → throwError $ "server error: " ⊕ sshow s
            | HTTP.statusIsClientError s → throwError $ "client error: " ⊕ sshow s
            | otherwise → throwError $ "unexpected response status: " ⊕ sshow s
  where

    bodyStream = case body of
        Nothing → return HS.emptyBody
        Just b → HS.inputStreamBody <$> S.fromByteString b

    -- with this there is a significant difference between http-streams-mvar and http-streams-ioref
    tryAnyText f = f `catchAny` \e → reset >> throwError (sshow e)

    -- using this makes the differences between http-streams-mvar and http-streams-ioref almost
    -- disappear.
    -- tryAnyText = id

    handler ∷ HS.Response → S.InputStream B.ByteString → IO (HTTP.Status, B.ByteString)
    handler response istream = do
        responseBody ← HS.concatHandler response istream
        return (HTTP.mkStatus (HS.getStatusCode response) (HS.getStatusMessage response), responseBody)

httpStreamsRequestNoCatchAny
    ∷ (MonadIO m, MonadBaseControl IO m, MonadError T.Text m)
    ⇒ ((HS.Connection → m (HTTP.Status, B.ByteString)) → m (HTTP.Status, B.ByteString))
        -- ^ use connection
    → m ()
        -- ^ reset connection
    → Maybe B.ByteString
    → HS.Request
        -- ^ HTTP request
    → m B.ByteString
httpStreamsRequestNoCatchAny useConnection reset body req = do
    -- When an error occurred we don't know if the connection is clean. We
    -- could do a little better, but we take a conservative approach and
    -- always reset the connection.
    --
    -- We wrap this to into a 'catchAny' and reset the connection
    -- when an exception occurs.
    (status, responseBody) ← tryAnyText ∘ useConnection $ \connection →
        liftIO $ do
            b ← bodyStream
            HS.sendRequest connection req b
            HS.receiveResponse connection handler
    case status of
        s   | HTTP.statusIsSuccessful s → return responseBody
            | HTTP.statusIsServerError s → throwError $ "server error: " ⊕ sshow s
            | HTTP.statusIsClientError s → throwError $ "client error: " ⊕ sshow s
            | otherwise → throwError $ "unexpected response status: " ⊕ sshow s
  where

    bodyStream = case body of
        Nothing → return HS.emptyBody
        Just b → HS.inputStreamBody <$> S.fromByteString b

    -- using this makes the differences between http-streams-mvar and http-streams-ioref almost
    -- disappear.
    tryAnyText = id

    handler ∷ HS.Response → S.InputStream B.ByteString → IO (HTTP.Status, B.ByteString)
    handler response istream = do
        responseBody ← HS.concatHandler response istream
        return (HTTP.mkStatus (HS.getStatusCode response) (HS.getStatusMessage response), responseBody)

-- -------------------------------------------------------------------------- --
-- http-streams with IORef

-- | This is not a production level interface to http-streams. It is only
-- meant to roughly exhibit similar performance as an production grade
-- implementation would have.
--
newtype HttpConnection_ = HttpConnection_
    { httpConnectionVar_ ∷ IORef HS.Connection
    }

newConnection_
    ∷ (MonadIO m, Functor m)
    ⇒ B8.ByteString
        -- ^ host name
    → Word16
        -- ^ port
    → m HttpConnection_
newConnection_ host port = HttpConnection_
    <$> liftIO (newIORef =<< HS.openConnection host port)

-- | This will deadlock when the connection had been closed before!
--
closeConnection_
    ∷ MonadIO m
    ⇒ HttpConnection_
    → m ()
closeConnection_ conn = liftIO $ do
    HS.closeConnection =<< readIORef (httpConnectionVar_ conn)

resetConnection_
    ∷ MonadIO m
    ⇒ B8.ByteString
        -- ^ host name
    → Word16
        -- ^ port
    → HttpConnection_
    → m ()
resetConnection_ host port conn = do
    closeConnection_ conn
    liftIO $ writeIORef (httpConnectionVar_ conn) =<< HS.openConnection host port

-- | It is up to the user to explicitly reset the connection if needed.
--
withConnection_
    ∷ ∀ m n o α β . (MonadIO m, MonadBaseControl IO m, MonadIO n, MonadBaseControl IO n, MonadIO o)
    ⇒ B8.ByteString
        -- ^ host name
    → Word16
        -- ^ port
    → (((HS.Connection → n β) → n β) → o () →  m α)
    → m α
withConnection_ host port inner =
    bracket (newConnection_ host port) closeConnection_ $ \conn@(HttpConnection_ var) → do
        let useConnection f = liftIO (readIORef var) >>= f
        inner useConnection (resetConnection_ host port conn)

-- -------------------------------------------------------------------------- --
-- http-streams with connection in state monad

newConnectionS
    ∷ MonadIO m
    ⇒ B8.ByteString
        -- ^ host name
    → Word16
        -- ^ port
    → m HS.Connection
newConnectionS host port = liftIO $ HS.openConnection host port

-- | This will deadlock when the connection had been closed before!
--
closeConnectionS
    ∷ MonadIO m
    ⇒ HS.Connection
    → m ()
closeConnectionS connection = liftIO $ HS.closeConnection connection

resetConnectionS
    ∷ (MonadIO m, MonadState HS.Connection m)
    ⇒ B8.ByteString
        -- ^ host name
    → Word16
        -- ^ port
    → m ()
resetConnectionS host port = do
    get >>= closeConnectionS
    newConnectionS host port >>= put

-- |
--
-- No redirections are performed!
--
httpStreamsRequestS
    ∷ (MonadIO m, MonadBaseControl IO m, MonadError T.Text m, MonadState HS.Connection m)
    ⇒ B8.ByteString
        -- ^ host name
    → Word16
        -- ^ port
    → Maybe B.ByteString
        -- ^ the request body
    → HS.Request
        -- ^ HTTP request
    → m B.ByteString
httpStreamsRequestS host port body req = do

    connection ← get

    -- When an error occurred we don't know if the connection is clean. We
    -- could do a little better, but we take a conservative approach and
    -- always reset the connection.
    --
    -- We wrap this to into a 'catchAny' and reset the connection
    -- when an exception occurs.
    (status, responseBody) ← tryAnyText ∘ liftIO $ do
        b ← bodyStream
        HS.sendRequest connection req b
        HS.receiveResponse connection handler
    case status of
        s   | HTTP.statusIsSuccessful s → return responseBody
            | HTTP.statusIsServerError s → throwError $ "server error: " ⊕ sshow s
            | HTTP.statusIsClientError s → throwError $ "client error: " ⊕ sshow s
            | otherwise → throwError $ "unexpected response status: " ⊕ sshow s
  where

    bodyStream = case body of
        Nothing → return HS.emptyBody
        Just b → HS.inputStreamBody <$> S.fromByteString b

    tryAnyText f = f `catchAny` \e → do
        resetConnectionS host port
        throwError $ sshow e

    handler ∷ HS.Response → S.InputStream B.ByteString → IO (HTTP.Status, B.ByteString)
    handler response istream = do
        responseBody ← HS.concatHandler response istream
        return (HTTP.mkStatus (HS.getStatusCode response) (HS.getStatusMessage response), responseBody)

