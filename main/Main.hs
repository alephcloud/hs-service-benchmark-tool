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
-- Copyright: Copyright (c) 2013-2014 PivotCloud, Inc.
-- License: Apache License, Version 2.0
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
-- TODO

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main
( main
) where

import Configuration.Utils hiding (Lens, Lens', action)
import Configuration.Utils.Validation

import Control.Concurrent
import Control.Exception.Lifted (bracket)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Control

import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import Data.IORef.Lifted
import Data.Monoid
import Data.Monoid.Unicode
import Data.String
import qualified Data.Text as T
import Data.Typeable

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP

import qualified Options.Applicative as O

import Prelude.Unicode

#ifdef WITH_HTTP_STREAMS
-- http-streams
import qualified "http-streams" Network.Http.Client as HS
import qualified System.IO.Streams as HS
#endif

-- internal modules

import Network.Benchmark
import Network.Benchmark.Logger
import Network.Benchmark.Utils
import Network.Benchmark.ColorOption

import PkgInfo

-- -------------------------------------------------------------------------- --
-- HTTP Client Backend

data HttpClient
    = HttpClient
    | HttpClientGlobalPool
#ifdef WITH_HTTP_STREAMS
    | HttpStreams
#endif
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

readHttpClient
    ∷  (Monad m, Eq a, Show a, CI.FoldCase a, IsString a, IsString e, Monoid e, MonadError e m)
    ⇒ a
    → m HttpClient
readHttpClient x = case CI.mk x of
    "http-client" → return HttpClient
    "http-client-global-pool" → return HttpClientGlobalPool
#ifdef WITH_HTTP_STREAMS
    "http-streams" → return HttpStreams
#endif
    e → throwError $ "unexpected http client value: "
        ⊕ fromString (show e)
        ⊕ ", expected \"http-client\", \"http-client-global-pool\", or \"http-streams\""

httpClientText
    ∷ IsString a
    ⇒ HttpClient
    → a
httpClientText HttpClient = "http-client"
httpClientText HttpClientGlobalPool = "http-client-global-pool"
#ifdef WITH_HTTP_STREAMS
httpClientText HttpStreams = "http-streams"
#endif

instance ToJSON HttpClient where
    toJSON = String ∘ httpClientText

instance FromJSON HttpClient where
    parseJSON = withText "HttpClient" $ either fail return ∘ readHttpClient

pHttpClient ∷ O.Parser HttpClient
pHttpClient = option (eitherReader readHttpClient)
    × long "http-client"
    ⊕ help "HTTP client backend that is used in the tests"
    ⊕ metavar "http-client|http-client-global-pool|http-streams"

-- -------------------------------------------------------------------------- --
-- Configuration

data MainConfiguration = MainConfiguration
    { _mainConfigTestParams ∷ !TestParams
    , _mainConfigColor ∷ !ColorOption
    , _mainConfigLogLevel ∷ !LogLevel
    , _mainConfigUrl ∷ !T.Text
        -- ^ not sure if this is too specific. We may reduce this again later...
    , _mainConfigHttpClient ∷ !HttpClient
    }
    deriving (Show, Read, Eq, Ord, Typeable)

mainConfigTestParams ∷ Lens' MainConfiguration TestParams
mainConfigTestParams = lens _mainConfigTestParams $ \a b → a { _mainConfigTestParams = b }

mainConfigColor ∷ Lens' MainConfiguration ColorOption
mainConfigColor = lens _mainConfigColor $ \a b → a { _mainConfigColor = b }

mainConfigLogLevel ∷ Lens' MainConfiguration LogLevel
mainConfigLogLevel = lens _mainConfigLogLevel $ \a b → a { _mainConfigLogLevel = b }

mainConfigUrl ∷ Lens' MainConfiguration T.Text
mainConfigUrl = lens _mainConfigUrl $ \a b → a { _mainConfigUrl = b }

mainConfigHttpClient ∷ Lens' MainConfiguration HttpClient
mainConfigHttpClient = lens _mainConfigHttpClient $ \a b → a { _mainConfigHttpClient = b }

defaultMainConfiguration ∷ MainConfiguration
defaultMainConfiguration = MainConfiguration
    { _mainConfigTestParams = defaultTestParams
    , _mainConfigColor = defaultColorOption
    , _mainConfigLogLevel = Warn
    , _mainConfigUrl = "http://localhost/"
#ifdef WITH_HTTP_STREAMS
    , _mainConfigHttpClient = HttpStreams
#else
    , _mainConfigHttpClient = HttpClient
#endif
    }

validateMainConfiguration ∷ ConfigValidation MainConfiguration λ
validateMainConfiguration MainConfiguration{..} = do
    validateTestParams _mainConfigTestParams
    validateHttpUrl "url" (T.unpack _mainConfigUrl)

instance ToJSON MainConfiguration where
    toJSON MainConfiguration{..} = object
        [ "test" .= _mainConfigTestParams
        , "color" .= _mainConfigColor
        , "loglevel" .= _mainConfigLogLevel
        , "url" .= _mainConfigUrl
        , "http_client" .= _mainConfigHttpClient
        ]

instance FromJSON (MainConfiguration → MainConfiguration) where
    parseJSON = withObject "MainConfiguration" $ \o → id
        <$< mainConfigTestParams %.: "test" × o
        <*< mainConfigColor ..: "color" × o
        <*< mainConfigLogLevel ..: "loglevel" × o
        <*< mainConfigUrl ..: "url" × o
        <*< mainConfigHttpClient ..: "http_client" × o

pMainConfiguration ∷ MParser MainConfiguration
pMainConfiguration = id
    <$< mainConfigTestParams %:: pTestParams
    <*< mainConfigColor .:: pColorOption
    <*< mainConfigLogLevel .:: pLogLevel
    <*< mainConfigUrl .:: fmap T.pack × strOption
        × long "url"
        ⊕ help "url that is used for the tests"
        ⊕ metavar "HTTP_URL"
    <*< mainConfigHttpClient .:: pHttpClient

-- -------------------------------------------------------------------------- --
-- Main

mainInfo ∷ ProgramInfo MainConfiguration
mainInfo = programInfoValidate
    "Benchmark services"
    pMainConfiguration
    defaultMainConfiguration
    validateMainConfiguration

main ∷ IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo $ \MainConfiguration{..} → do

    let backend = handleLoggerBackend defaultLoggerBackendConfig
            { _loggerBackendConfigHandle = StdOut
            , _loggerBackendConfigColor = _mainConfigColor
            }
        loggerConfig = defaultLoggerConfig
            { _loggerConfigThreshold = _mainConfigLogLevel
            }

    withLoggerCtx loggerConfig backend $ flip runLoggerT $

        case _mainConfigHttpClient of

            HttpClient →
                -- http-client, thread-local manager
                runTest "testname" _mainConfigTestParams $ httpRequestThreadAction _mainConfigUrl

            HttpClientGlobalPool →
                -- http-client, global manager
                withManager settings $ \mgr →
                    runTest "testname" _mainConfigTestParams $ httpRequestThreadAction' mgr _mainConfigUrl

#ifdef WITH_HTTP_STREAMS
            HttpStreams → do
                -- http-streams, one connection per thread
                runTest "testname" _mainConfigTestParams $ httpStreamsRequestThreadAction _mainConfigUrl
#endif

  where
    settings = HTTP.defaultManagerSettings
        { HTTP.managerConnCount = 100 -- FIXME (use number of threads + 1)
        , HTTP.managerResponseTimeout = Nothing
        -- , HTTP.managerResponseTimeout = Just 5000000
        }

-- -------------------------------------------------------------------------- --
-- HTTP service tests

-- | Lifted version of 'HTTP.withManager'
withManager
    ∷ (MonadBaseControl IO m)
    ⇒ HTTP.ManagerSettings
    → (HTTP.Manager → m α)
    → m α
withManager settings = liftBaseOp (HTTP.withManager settings)

makeRequest
    ∷ (MonadIO m, MonadError T.Text m)
    ⇒ HTTP.Manager
    → Int
        -- ^ thread id
    → Int
        -- ^ action id
    → HTTP.Request
        -- ^ HTTP request
    → m ()
makeRequest mgr _i _j req = do
    r ← {-# SCC call_httpLbs #-} liftIO $ HTTP.responseStatus <$> HTTP.httpLbs req mgr
    if
        | HTTP.statusIsSuccessful r → return ()
        | HTTP.statusIsServerError r → throwError $ "server error: " ⊕ sshow r
        | HTTP.statusIsClientError r → throwError $ "client error: " ⊕ sshow r
        | otherwise → throwError $ "unexpected response status: " ⊕ sshow r

-- | A test action generator for testing simple GET Urls.
-- That is useful to test the performance of the service.
--
httpRequestThreadAction
    ∷ (MonadIO m, MonadBaseControl IO m, MonadLog T.Text m)
    ⇒ T.Text
        -- ^ HTTP URL
    → Int
        -- ^ thread id
    → ThreadTestAction m
httpRequestThreadAction url i f = withLabel ("function","httpRequestThreadAction") $
    withManager settings $ \mgr →
        f $ \j → TestAction $ makeRequest mgr i j req
  where
    -- We give every thread its own connection manager
    settings = HTTP.defaultManagerSettings
        { HTTP.managerConnCount = 2
        -- , HTTP.managerResponseTimeout = Just 5000000
        , HTTP.managerResponseTimeout = Nothing
        }
    req = either (error ∘ show) id ∘ HTTP.parseUrl $ T.unpack url

-- | A test action generator for testing simple GET Urls.
-- That is useful to test the performance of the service.
--
-- This uses a gloabl connection manager for all threads.
--
httpRequestThreadAction'
    ∷ MonadLog T.Text m
    ⇒ HTTP.Manager
        -- ^ connection manager
    → T.Text
        -- ^ GET URL
    → Int
        -- ^ thread id
    → ThreadTestAction m
httpRequestThreadAction' mgr url i f = withLabel ("function","httpRequestThreadAction'") $
    f $ \j → TestAction $ makeRequest mgr i j req
  where
    req = either (error ∘ show) id ∘ HTTP.parseUrl $ T.unpack url

-- | A trivial test action generator for testing and debugging.
--
trivialThreadAction
    ∷ MonadLog T.Text m
    ⇒ Int
    → ThreadTestAction m
trivialThreadAction i f = withLabel ("function","trivialThreadAction") $
    f $ \j → TestAction $ action j
  where
    action
        ∷ ∀ m . (MonadIO m, MonadError T.Text m)
        ⇒ Int
        → m ()
    action j = if i ≡ j
      then do
        liftIO (threadDelay 100)
        throwError "failure"
      else do
        liftIO (threadDelay 10)
        return ()

#ifdef WITH_HTTP_STREAMS
-- -------------------------------------------------------------------------- --
-- http-streams

-- | A test action generator for testing simple GET Urls.
-- For each thread one TCP connection is allocated.
--
-- Note that no redirects are performed!
--
httpStreamsRequestThreadAction
    ∷ (MonadIO m, MonadLog T.Text m, MonadBaseControl IO m)
    ⇒ T.Text
        -- ^ HTTP URL
    → Int
        -- ^ thread id
    → ThreadTestAction m
httpStreamsRequestThreadAction url _i f = withLabel ("function", "httpStreamsRequestThreadAction") $ do
    bracket initConnection closeConnection $ \connRef → do
#if !MIN_VERSION_http_streams(0,8,0)
        req ← buildRequest
#endif
        f $ \_j → TestAction $ do
            -- We wrap this to into a 'catchAny' and reset the connection
            -- when an exception occurs.
            result ← fmap join ∘ tryAnyText $ do
                connection ← readIORef connRef
                liftIO $ HS.sendRequest connection req HS.emptyBody
                logg Debug $ "finished send"
                liftIO $ HS.receiveResponse connection handler
            case result of
                Right _ → return ()
                Left e → do
                    -- When an error occurred we don't know if the connection is clean. We
                    -- could do a little better, but we take and conservative approach and
                    -- always reset the connection.
                    resetConnection connRef
                    throwError e
  where

    initConnection = liftIO $ newIORef =<< HS.openConnection host port
    closeConnection ref = liftIO $ HS.closeConnection =<< readIORef ref
    resetConnection ref = do
        logg Debug $ "httpStreamsRequestThreadAction: reset connection"
        closeConnection ref
        liftIO $ writeIORef ref =<< HS.openConnection host port

    rawReq = either (error ∘ show) id ∘ HTTP.parseUrl $ T.unpack url
    path = HTTP.path rawReq
    host = HTTP.host rawReq
    port = fromIntegral $ HTTP.port rawReq

#if MIN_VERSION_http_streams(0,8,0)
    req = HS.buildRequest $ do
        HS.http HS.GET path
        HS.setAccept "*/*"
#else
    buildRequest = liftIO ∘ HS.buildRequest $ do
        HS.http HS.GET path
        HS.setAccept "*/*"
#endif

    handler ∷ HS.Response → HS.InputStream B.ByteString → IO (Either T.Text B.ByteString)
    handler response istream = if
        | HTTP.statusIsSuccessful status → Right <$> HS.concatHandler response istream
        | HTTP.statusIsServerError status → return ∘ Left $ "server error: " ⊕ sshow status
        | HTTP.statusIsClientError status → return ∘ Left $ "client error: " ⊕ sshow status
        | otherwise → return ∘ Left $ "unexpected response status: " ⊕ sshow status
      where
        status = HTTP.mkStatus (HS.getStatusCode response) (HS.getStatusMessage response)

#endif

