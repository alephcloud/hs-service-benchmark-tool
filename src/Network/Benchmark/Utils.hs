-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Network.Benchmark.Utils
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
-- Module: Network.Benchmark.Utils
-- Copyright: Copyright (c) 2013-2014 PivotCloud, Inc.
-- License: Apache License, Version 2.0
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module Network.Benchmark.Utils
(
-- * Test Parameters
  testDataPrefix

-- * Exceptions
, TestException(..)
, testThrowT
, toE
, catchET
, fromEitherET
, fromEitherET_

-- * General Utils
, sshow
, tryAnyText
, tryAnyError
, tryAnyErrorText
, retryT
, retryT_
, testData
, whenJust
, exceptT

-- * Time Measurement
, getTime
, time
, timeT
, timeoutT

-- * Connection Statistics
, Stat(..)
, statFailure
, statSuccess
, statRetry
, statSuccessLatency
, statFailureLatency
, statRetrySuccessLatency
, statRetryFailureLatency
, statFailureMessages
, successStat
, failStat
, logSuccess
, logFailure
, pruneHttpError
, printStat
, writeStatFiles
, writeSample
, readSample
#ifdef WITH_CHART
, writeStatChart
#endif
) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Exception.Enclosed
import qualified Control.Exception.Lifted as LE
import Control.Error hiding (syncIO)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Control.Monad.Error.Hoist
import Control.Monad.Except
import Control.Monad.Trans.Control

import Data.AffineSpace ((.-.))
import qualified Data.CaseInsensitive as CI
import Data.Default
import qualified Data.DList as D
import Data.IORef
import qualified Data.List as L
import Data.Monoid
import Data.Monoid.Unicode
import qualified Data.Set as S
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.Thyme
import qualified Data.Vector.Unboxed as V
import Data.Typeable

import qualified Network.HTTP.Client as HTTP

import Prelude.Unicode

import qualified Statistics.Function as ST
import qualified Statistics.Sample as ST

import System.IO
import System.Timeout

import Text.Printf

#ifdef WITH_CHART
-- Used for plotting
import Control.Arrow ((***))

import Data.Colour
import Data.Colour.Names

import Graphics.Rendering.Chart hiding (label)
import Graphics.Rendering.Chart.Backend.Cairo

import qualified Statistics.Sample.KernelDensity as ST
#endif

-- -------------------------------------------------------------------------- --
-- Static Test parameters
--

-- | This prefix is used for the IDs and names of all entities that are
-- created in the AWS account.
--
testDataPrefix
    ∷ IsString a
    ⇒ a
testDataPrefix = "__TEST_AWSHASKELLBINDINGS__"

-- -------------------------------------------------------------------------- --
-- Test Exceptions

data TestException
    = TestException T.Text
    | UnexpectedException  LE.SomeException
    | RetryException Int LE.SomeException
    deriving (Show, Typeable)

instance LE.Exception TestException

testThrowT
    ∷ Monad m
    ⇒ T.Text
    → EitherT LE.SomeException m a
testThrowT = left ∘ LE.toException ∘ TestException
{-# INLINE testThrowT #-}

-- | Generalize Exceptions within an 'EitherT' to 'SomeException'
--
toE
    ∷ (Monad m, LE.Exception e)
    ⇒ EitherT e m a
    → EitherT LE.SomeException m a
toE = fmapLT LE.toException
{-# INLINE toE #-}

catchET
    ∷ (Monad m, LE.Exception e)
    ⇒ EitherT LE.SomeException m a
    → (e → EitherT LE.SomeException m a)
    → EitherT LE.SomeException m a
catchET f handler = f `catchT` \e  → maybe (left e) handler $ LE.fromException e
{-# INLINE catchET #-}

fromEitherET
    ∷ (Monad m, LE.Exception e)
    ⇒ EitherT LE.SomeException m a
    → (Maybe e → m a)
    → m a
fromEitherET f handler = eitherT (handler ∘ LE.fromException) return f
{-# INLINE fromEitherET #-}

fromEitherET_
    ∷ (Monad m, LE.Exception e)
    ⇒ EitherT LE.SomeException m a
    → (Either LE.SomeException e → m a)
    → m a
fromEitherET_ f handler = eitherT
    (\e → handler ∘ maybe (Left e) Right $ LE.fromException e)
    return
    f
{-# INLINE fromEitherET_ #-}

-- -------------------------------------------------------------------------- --
-- General Utils

-- | Catches all exceptions from the enclosed computation
--
tryAnyErrorText
    ∷ (MonadBaseControl IO m, MonadError T.Text m)
    ⇒ m α
    → m α
tryAnyErrorText = tryAny >=> either (throwError ∘ sshow) return
{-# INLINE tryAnyErrorText #-}

-- | Catches all exceptions from the enclosed computation
--
tryAnyText
    ∷ (MonadBaseControl IO m)
    ⇒ m α
    → m (Either T.Text α)
tryAnyText f = fmapL sshow <$> tryAny f
{-# INLINE tryAnyText #-}

-- | Catches all exceptions from the enclosed computation
--
tryAnyError
    ∷ (MonadBaseControl IO m, MonadError LE.SomeException m)
    ⇒ m α
    → m α
tryAnyError = tryAny >=> either throwError return
{-# INLINE tryAnyError #-}

testData
    ∷ (IsString a, Monoid a)
    ⇒ a
    → a
testData a = testDataPrefix ⊕ a
{-# INLINE testData #-}

retryT
    ∷ (LE.Exception e, MonadIO m)
    ⇒ Int
    → EitherT e m a
    → EitherT TestException m a
retryT n f = snd <$> retryT_ n f
{-# INLINE retryT #-}

retryT_
    ∷ (LE.Exception e, MonadIO m)
    ⇒ Int
    → EitherT e m a
    → EitherT TestException m (Int, a)
retryT_ n f = go 1
  where
    go x
        | x >= n = fmapLT (RetryException x ∘ LE.toException) ((x,) <$> f)
        | otherwise = ((x,) <$> f) `catchT` \_ → do
            liftIO $ threadDelay (1000000 * min 60 (2^(x-1)))
            go (succ x)
{-# INLINE retryT_ #-}

sshow
    ∷ (Show a, IsString b)
    ⇒ a
    → b
sshow = fromString ∘ show
{-# INLINE sshow #-}

whenJust
    ∷ Monad m
    ⇒ Maybe a
    → (a → m ())
    → m ()
whenJust (Just x) = ($ x)
whenJust Nothing = const $ return ()
{-# INLINE whenJust #-}

instance (m ~ n, MonadError e' m) ⇒ HoistError m (ExceptT e n) e e' where
    hoistError f = exceptT (throwError ∘ f) return
    {-# INLINE hoistError #-}

exceptT
    ∷ Monad m
    ⇒ (e → m b)
    → (a → m b)
    → ExceptT e m a
    → m b
exceptT f g = runExceptT >=> either f g
{-# INLINE exceptT #-}

-- -------------------------------------------------------------------------- --
-- Time Measurment

getTime ∷ IO UTCTime
getTime = getCurrentTime

time
    ∷ IO a
    → IO (NominalDiffTime, a)
time action = do
  start ← getTime
  !result ← action
  end ← getTime
  let !delta = end .-. start
  return (delta, result)

timeT
    ∷ MonadIO m
    ⇒ m a
    → m (NominalDiffTime, a)
timeT action = do
  start ← liftIO getTime
  !result ← action
  end ← liftIO getTime
  let !delta = end .-. start
  return (delta, result)

timeoutT
    ∷ (MonadBaseControl IO m)
    ⇒ T.Text
        -- ^ label
    → (T.Text → b)
        -- ^ exception constructor
    → NominalDiffTime
        -- ^ timeout
    → EitherT b m a
        -- ^ action
    → EitherT b m a
timeoutT label exConstr t a = do
    r ← liftBaseWith $ \runInBase ->
        timeout (fromIntegral $ view microseconds t) (runInBase a)
    case r of
        Nothing → left $ exConstr $ label ⊕ " timed out after " ⊕ sshow t
        Just x → restoreM x

-- -------------------------------------------------------------------------- --
-- Connection Statistics

-- TODO:
--
-- * add support for tagging all values. Tagging could be done
--   through a ReaderMonad similar to the LogScope. We may even consider
--   using the same tags?
--
-- * We may abstract out the system to record the statistics. We may just
--   use a conduit, pipe, or similar interface and leave it to the
--   user to chose the best implementation. For example a pair of
--   static buffers could be a good data-structure along with a backend
--   process that processes the currently inactive buffer in the background.
--
data Stat = Stat
    { _statFailure ∷ !Int
        -- ^ number of failures (after retries)
    , _statSuccess ∷ !Int
        -- ^ number of successes (after retries)
    , _statRetry ∷ !Int
        -- ^ number of retries
    , _statFailureLatency ∷ !(D.DList Double)
        -- ^ latency in milliseconds of failures (including time for retries)
    , _statSuccessLatency ∷ !(D.DList Double)
        -- ^ latency in milliseconds of successes (including time for retries)
    , _statRetryFailureLatency ∷ !(D.DList Double)
        -- ^ latency in milliseconds of failures in initial or retry attempts
    , _statRetrySuccessLatency ∷ !(D.DList Double)
        -- ^ latency in milliseconds of successes in initial or retry attempts
    , _statFailureMessages ∷ !(S.Set T.Text)
    }
    deriving (Show, Eq, Ord, Typeable)

statFailure ∷ Lens' Stat Int
statFailure = lens _statFailure $ \a b → a { _statFailure = b }
{-# INLINE statFailure #-}

statSuccess ∷ Lens' Stat Int
statSuccess = lens _statSuccess $ \a b → a { _statSuccess = b }
{-# INLINE statSuccess #-}

statRetry ∷ Lens' Stat Int
statRetry = lens _statRetry $ \a b → a { _statRetry = b }
{-# INLINE statRetry #-}

statFailureLatency ∷ Lens' Stat (D.DList Double)
statFailureLatency = lens _statFailureLatency $ \a b → a { _statFailureLatency = b }
{-# INLINE statFailureLatency #-}

statSuccessLatency ∷ Lens' Stat (D.DList Double)
statSuccessLatency = lens _statSuccessLatency $ \a b → a { _statSuccessLatency = b }
{-# INLINE statSuccessLatency #-}

statRetryFailureLatency ∷ Lens' Stat (D.DList Double)
statRetryFailureLatency = lens _statRetryFailureLatency $ \a b → a { _statRetryFailureLatency = b }
{-# INLINE statRetryFailureLatency #-}

statRetrySuccessLatency ∷ Lens' Stat (D.DList Double)
statRetrySuccessLatency = lens _statRetrySuccessLatency $ \a b → a { _statRetrySuccessLatency = b }
{-# INLINE statRetrySuccessLatency #-}

statFailureMessages ∷ Lens' Stat (S.Set T.Text)
statFailureMessages = lens _statFailureMessages $ \a b → a { _statFailureMessages = b }
{-# INLINE statFailureMessages #-}

instance Monoid Stat where
    mempty = Stat 0 0 0 mempty mempty mempty mempty mempty
    (Stat a0 a1 a2 a3 a4 a5 a6 a7) `mappend` (Stat b0 b1 b2 b3 b4 b5 b6 b7) = Stat
        (a0 + b0)
        (a1 + b1)
        (a2 + b2)
        (a3 ⊕ b3)
        (a4 ⊕ b4)
        (a5 ⊕ b5)
        (a6 ⊕ b6)
        (a7 ⊕ b7)

    {-# INLINE mempty #-}

successStat
    ∷ Double -- milliseconds
    → Stat
successStat l = Stat 0 1 0 mempty (D.singleton l) mempty mempty mempty
{-# INLINE successStat #-}

failStat
    ∷ Double -- millisconds
    → T.Text -- failure message
    → Stat
failStat l e = Stat 1 0 0 (D.singleton l) mempty mempty mempty (S.singleton e)
{-# INLINE failStat #-}

logSuccess
    ∷ IORef Stat
    → NominalDiffTime
    → IO ()
logSuccess ref t = atomicModifyIORef' ref $ \stat → stat `seq`
    (stat ⊕ successStat (toSeconds t * 1000), ())
{-# INLINE logSuccess #-}

logFailure
    ∷ IORef Stat
    → NominalDiffTime
    → T.Text
    → IO ()
logFailure ref t e = atomicModifyIORef' ref $ \stat → stat `seq`
    (stat ⊕ failStat (toSeconds t * 1000) e, ())
{-# INLINE logFailure #-}

-- | Prune HTTP error such that similar exceptions become equal and are
-- logged only once.
--
pruneHttpError
    ∷ HTTP.HttpException
    → HTTP.HttpException
pruneHttpError (HTTP.StatusCodeException s h _) = HTTP.StatusCodeException s (deleteDateHeader h) def
pruneHttpError (HTTP.TooManyRedirects _ ) = HTTP.TooManyRedirects []
pruneHttpError e = e
{-# INLINE pruneHttpError #-}

deleteDateHeader
    ∷ (Eq a, IsString a, CI.FoldCase a)
    ⇒ [(CI.CI a, b)]
    → [(CI.CI a, b)]
deleteDateHeader = L.filter ((`notElem` ["X-Request-URL", "date"]) ∘ fst)
{-# INLINE deleteDateHeader #-}

toSample
    ∷ D.DList Double
    → ST.Sample
toSample = V.fromList ∘ D.toList
{-# INLINE toSample #-}

printStat
    ∷ T.Text
    → NominalDiffTime
    → Stat
    → IO ()
printStat testName totalTime Stat{..} = do

    -- Overview
    let total = _statSuccess + _statFailure
        secs = toSeconds totalTime ∷ Double
    printf "Test \"%v\" completed %v requests (%v successes, %v failures, %v retries) in %.2fs (%.2f req/sec)\n\n"
        (T.unpack testName)
        total
        _statSuccess
        _statFailure
        _statRetry
        secs
        (fromIntegral total / secs)

    -- Successes
    let (succMin, succMax) = ST.minMax succSample
        succMean = ST.mean succSample
        succStdDev = ST.stdDev succSample
    printf "Success latencies\n"
    printf "    min: %.2fms, max %.2fms\n" succMin succMax
    printf "    mean: %.2fms, standard deviation: %.2fms\n\n" succMean succStdDev

    -- Failures
    unless (_statFailure == 0) $ do
        let (failMin, failMax) = ST.minMax failSample
            failMean = ST.mean failSample
            failStdDev = ST.stdDev failSample
        printf "Failure latencies\n"
        printf "    min: %.2fms, max %.2fms\n" failMin failMax
        printf "    mean: %.2fms, standard deviation %.2fms\n\n" failMean failStdDev

        -- Failure Messages
        printf "Failure Messages:\n"
        forM_ (S.toList _statFailureMessages) $ \e ->
            T.putStrLn $ "    " ⊕ sshow e
        printf "\n"

    -- Retry Successes
    let (rsuccMin, rsuccMax) = ST.minMax rsuccSample
        rsuccMean = ST.mean rsuccSample
        rsuccStdDev = ST.stdDev rsuccSample
    printf "Retry Success latencies\n"
    printf "    min: %.2fms, max %.2fms\n" rsuccMin rsuccMax
    printf "    mean: %.2fms, standard deviation: %.2fms\n\n" rsuccMean rsuccStdDev

    -- Retry Failures
    let (rfailMin, rfailMax) = ST.minMax rfailSample
        rfailMean = ST.mean rfailSample
        rfailStdDev = ST.stdDev rfailSample
    printf "Retry Failures latencies\n"
    printf "    min: %.2fms, max %.2fms\n" rfailMin rfailMax
    printf "    mean: %.2fms, standard deviation: %.2fms\n\n" rfailMean rfailStdDev

  where
    succSample = toSample _statSuccessLatency
    failSample = toSample _statFailureLatency
    rsuccSample = toSample _statRetrySuccessLatency
    rfailSample = toSample _statRetryFailureLatency

writeStatFiles
    ∷ String -- ^ file name prefix
    → T.Text -- ^ test name
    → Stat -- ^ results
    → IO ()
writeStatFiles prefix testName Stat{..} = do
    writeSample (prefix ⊕ "-" ⊕ T.unpack testName ⊕ "-success.txt") (toSample _statSuccessLatency)
    writeSample (prefix ⊕ "-" ⊕ T.unpack testName ⊕ "-failure.txt") (toSample _statFailureLatency)
    writeSample (prefix ⊕ "-" ⊕ T.unpack testName ⊕ "-retry-success.txt") (toSample _statRetrySuccessLatency)
    writeSample (prefix ⊕ "-" ⊕ T.unpack testName ⊕ "-retry-failure.txt") (toSample _statRetryFailureLatency)

#ifdef WITH_CHART
-- -------------------------------------------------------------------------- --
-- Plotting of Connection Statistics

-- We should add support for code for filtering and plotting statics out of
-- serializied data.

chart
    ∷ T.Text -- ^ title of the chart
    → [(String, Colour Double, [(LogValue, Double)])] -- ^ title color and data for each plot
    → Renderable ()
chart chartTitle dats = toRenderable layout
  where
    pl (title, color, results) = def
        & plot_points_title .~ title
        & plot_points_style ∘ point_color .~ opaque color
        & plot_points_style ∘ point_radius .~ 1
        & plot_points_values .~ results

    layout = def
        & layout_title .~ T.unpack chartTitle
        & layout_background .~ solidFillStyle (opaque white)
        & layout_left_axis_visibility ∘ axis_show_ticks .~ False
#if !MIN_VERSION_Chart(1,3,0)
        & setLayoutForeground (opaque black)
#endif
        & layout_plots .~ [ toPlot (pl d) | d ← dats ]

densityChart
    ∷ V.Vector Double
    → V.Vector Double
    → Renderable ()
densityChart successes failures = chart "Density" $
    if V.null successes then [] else [("success", blue, succDat)]
    ⊕
    if V.null failures then [] else [("failures", red, failDat)]
  where
    succDat,failDat ∷ [(LogValue, Double)]
    succDat = uncurry zip ∘ (map LogValue ∘ V.toList *** map (* 2048) ∘ V.toList) $ ST.kde 2048 successes
    failDat = uncurry zip ∘ (map LogValue ∘ V.toList *** map (* 2048) ∘ V.toList) $ ST.kde 2048 failures

writeStatChart
    ∷ String -- ^ file name prefix
    → T.Text -- ^ test name
    → Stat -- ^ results
    → IO ()
writeStatChart prefix testName Stat{..} = void $
#if MIN_VERSION_Chart_cairo(1,3,0)
    renderableToFile opts path render
#else
    renderableToFile opts render path
#endif
  where
    path = prefix ⊕ "-" ⊕ T.unpack testName ⊕ "-density.pdf"
    opts = FileOptions (800,600) PDF
    render = densityChart (toSample _statSuccessLatency) (toSample _statFailureLatency)

#endif

-- -------------------------------------------------------------------------- --
-- Serialize Connection Statistics

writeSample
    ∷ FilePath
    → ST.Sample
    → IO ()
writeSample file sample = withFile file WriteMode $ \h ->
    V.forM_ sample $ T.hPutStrLn h ∘ sshow

readSample
    ∷ FilePath
    → IO ST.Sample
readSample file = withFile file ReadMode $ fmap V.fromList ∘ go
  where
    go h = hIsEOF h >>= \x → if x
        then return []
        else do
            r ← either error fst ∘ T.double <$> T.hGetLine h
            (:) r <$> go h

