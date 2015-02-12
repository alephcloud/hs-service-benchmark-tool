-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Network.Benchmark
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
-- Module: Network.Benchmark
-- Copyright: Copyright (c) 2013-2014 PivotCloud, Inc.
-- License: Apache License, Version 2.0
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Network.Benchmark
( TestAction(..)
, ThreadTestAction
, runTest
, runThread
, runAction

-- * Test Parameters
, TestParams(..)
, paramThreadCount
, paramThreadDelay
, paramActionCount
, paramRetryCount
, paramDataFilePrefix
#ifdef WITH_CHART
, paramChartFilePrefix
#endif
, defaultTestParams
, pTestParams
, validateTestParams
) where

import Configuration.Utils hiding (Lens', Lens, action)
import Configuration.Utils.Validation

import Control.Concurrent.Async.Lifted
import Control.Exception.Enclosed
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Either

import Data.EitherR
import Data.Monoid
import Data.Monoid.Unicode
import qualified Data.Text as T
import Data.Thyme (toSeconds)
import Data.Typeable

import Prelude.Unicode

import System.Logger

-- internal modules

import Network.Benchmark.Utils

-- -------------------------------------------------------------------------- --
-- Running Tests

--
-- TODO:
--
-- I am not sure if it is a good idea to require 'MonadBaseControl' and support
-- 'MonadLog'.
--
-- What is the benefit here of quantifying over the context?
--
newtype TestAction = TestAction
    { runTestAction
        ∷ ∀ m .
            ( Functor m
            , Applicative m
            , Monad m
            , MonadIO m
            , MonadBaseControl IO m
            , MonadError T.Text m
            , MonadLog T.Text m
            )
        ⇒ m ()
    }

instance Show TestAction where
    show _ = "testaction"

-- | A bracket style function that provides the test
-- actions for the thread.
--
-- The type allows the action to contain some initialization
-- and cleanup code for a test thread.
--
type ThreadTestAction m = ((Int → TestAction) → m Stat) → m Stat

runThread
    ∷ (MonadIO m, MonadBaseControl IO m, MonadLog T.Text m)
    ⇒ TestParams
    → ThreadTestAction m
    → m Stat
runThread TestParams{..} provideActions = provideActions $ \action →
    foldM (runAction _paramRetryCount) mempty $
        action <$> [0 .. _paramActionCount - 1]
{-# INLINEABLE runThread #-}

runAction
    ∷ ∀ m .
        ( Functor m
        , MonadLog T.Text m
        , MonadBaseControl IO m
        , MonadIO m
        )
    ⇒ Int -- ^ number of retries
    → Stat
    → TestAction
    → m Stat
runAction retries stat action = do
    (t, result) ← timeT $ runEitherT run
    return ∘ seq stat ∘ (stat ⊕) $
        case result of
            Right () → successStat $ toSeconds t * 1000
            Left e → failStat (toSeconds t * 1000) (sshow e)
  where
    run ∷ EitherT TestException m ()
    run = withLabel ("function", "runAction") ∘ retryT retries $
        fmapLT TestException (runTestAction action)
            `catchAny` (throwError ∘ UnexpectedException)
{-# INLINEABLE runAction #-}

-- TODO add support for delay
--
runTest
    ∷ (MonadIO m, MonadBaseControl IO m, MonadLog T.Text m)
    ⇒ T.Text
        -- ^ test name
    → TestParams
        -- ^ test parameters
    → (Int → ThreadTestAction m)
        -- ^ test action generator
    → m ()
runTest testName params@TestParams{..} threadAction = do
    logg Info $ "Start test \"" ⊕ testName ⊕ "\""
    (t, stats) ← timeT $
        mapConcurrently (runThread params) $ threadAction <$> [0 .. _paramThreadCount - 1]

    -- report results
    let stat = mconcat stats
    liftIO $ printStat testName t stat
    whenJust _paramDataFilePrefix $ \prefix ->
        liftIO $ writeStatFiles prefix testName stat
#ifdef WITH_CHART
    whenJust _paramChartFilePrefix $ \prefix ->
        liftIO $ writeStatChart prefix testName stat
#endif
{-# INLINEABLE runTest #-}

-- -------------------------------------------------------------------------- --
-- Parameters

data TestParams = TestParams
    { _paramThreadCount ∷ !Int
        -- ^ number of concurrent threads
    , _paramThreadDelay ∷ !Int
        -- ^ delay in the startup of threads (in milliseconds)
    , _paramActionCount ∷ !Int
        -- ^ number actions (sequentially) per thread
    , _paramRetryCount ∷ !Int
        -- ^ number of retries for each action
    , _paramDataFilePrefix ∷ !(Maybe String)
#ifdef WITH_CHART
    , _paramChartFilePrefix ∷ !(Maybe String)
#endif
    }
    deriving (Show, Read, Eq, Ord, Typeable)

paramThreadCount ∷ Lens' TestParams Int
paramThreadCount = lens _paramThreadCount $ \a b → a { _paramThreadCount = b }

paramThreadDelay ∷ Lens' TestParams Int
paramThreadDelay = lens _paramThreadDelay $ \a b → a { _paramThreadDelay = b }

paramActionCount ∷ Lens' TestParams Int
paramActionCount = lens _paramActionCount $ \a b → a { _paramActionCount = b }

paramRetryCount ∷ Lens' TestParams Int
paramRetryCount = lens _paramRetryCount $ \a b → a { _paramRetryCount = b }

paramDataFilePrefix ∷ Lens' TestParams (Maybe String)
paramDataFilePrefix = lens _paramDataFilePrefix $ \a b → a { _paramDataFilePrefix = b }

#ifdef WITH_CHART
paramChartFilePrefix ∷ Lens' TestParams (Maybe String)
paramChartFilePrefix = lens _paramChartFilePrefix $ \a b → a { _paramChartFilePrefix = b }
#endif

defaultTestParams ∷ TestParams
defaultTestParams = TestParams
    { _paramThreadCount = 1
    , _paramThreadDelay = 0
    , _paramActionCount = 1
    , _paramRetryCount = 1
    , _paramDataFilePrefix = Nothing
#ifdef WITH_CHART
    , _paramChartFilePrefix = Nothing
#endif
    }

validateTestParams ∷ ConfigValidation TestParams λ
validateTestParams TestParams{..} = do
    validatePositive "thread_count" _paramThreadCount
    validateNonNegative "thread_delay" _paramThreadDelay
    validatePositive "action_count" _paramThreadCount
    validateNonNegative "retry_count" _paramThreadDelay
    maybe (return ()) (validateNonEmpty "data_file_prefix") _paramDataFilePrefix
#ifdef WITH_CHART
    maybe (return ()) (validateNonEmpty "chart_file_prefix") _paramChartFilePrefix
#endif

instance ToJSON TestParams where
    toJSON TestParams{..} = object
        [ "thread_count" .= _paramThreadCount
        , "thread_delay" .= _paramThreadDelay
        , "action_count" .= _paramActionCount
        , "retry_count" .= _paramRetryCount
        , "data_file_prefix" .= _paramDataFilePrefix
#ifdef WITH_CHART
        , "chart_file_prefix" .= _paramChartFilePrefix
#endif
        ]

instance FromJSON (TestParams → TestParams) where
    parseJSON = withObject "test_params" $ \o → id
        <$< paramThreadCount ..: "thread_count" × o
        <*< paramThreadDelay ..: "thread_delay" × o
        <*< paramActionCount ..: "action_count" × o
        <*< paramRetryCount ..: "retry_count" × o
        <*< paramDataFilePrefix ..: "data_file_prefix" × o
#ifdef WITH_CHART
        <*< paramChartFilePrefix ..: "chart_file_prefix" × o
#endif

pTestParams ∷ MParser TestParams
pTestParams = id
    <$< paramThreadCount .:: option auto
        × long "thread-count"
        ⊕ metavar "INT"
        ⊕ help "number of threads"
    <*< paramThreadCount .:: option auto
        × long "thread-delay"
        ⊕ metavar "MILLISECONDS"
        ⊕ help "time the startup of a thread is delayed"
    <*< paramActionCount .:: option auto
        × long "action-count"
        ⊕ metavar "INT"
        ⊕ help "number of actions PER THREAD"
    <*< paramRetryCount .:: option auto
        × long "retry-count"
        ⊕ metavar "INT"
        ⊕ help "number of retries per action"
    <*< paramDataFilePrefix .:: fmap Just × strOption
        × long "data-file-prefix"
        ⊕ metavar "STRING"
        ⊕ help "if present raw latency data is written to files with this prefix."
#ifdef WITH_CHART
    <*< paramChartFilePrefix .:: fmap Just × strOption
        × long "chart-file-prefix"
        ⊕ metavar "STRING"
        ⊕ help "if present latency density chargts are written to files with this prefix."
#endif

