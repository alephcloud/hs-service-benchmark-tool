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
-- Description: benchmarks for concurrent use of 'timeout'
-- Copyright: Copyright (c) 2013-2014 PivotCloud, Inc.
-- License: Apache License, Version 2.0
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
--
-- These benchmarks evaluate the performance of 'timeout' when used for many
-- short-running sequential actions from a moderate number of concurrent
-- threads.
--
-- In concrete the benchmarks spawn 16 concurrent threads and have each
-- sequentially repeat the same action 1000 times. The action consists @n@ of
-- sequential calls to 'getCurrentTime'. The benchmarks are executed for
-- different values for @n@.
--
-- We compare are four types of benchmarks:
--
-- 1. Just execute the setup described above as is.
--
-- 2. Wraps each action of @n@ calls to 'getCurrentTime' into a call to
--    'timeout'. The timeout parameter is chosen large enought so that it will
--    trigger.
--
-- 3. Each action of @n@ calls to 'getCurrentTime' is started
--    only after updating an 'MVar' that is shared by all threads. The update
--    is a cheap, but not yet trivial pure function.
--
-- 4. Each action of @n@ calls to 'getCurrentTime' is started only atomically
--    updating an 'IORef' that is shared by all threads. The update is a cheap,
--    but not yet trivial pure function.
--
-- Note that the third and the fourth scenario force a synchronization between
-- *all* threads, whereas, to the best of my knowledge, the call to timeout
-- only requires synchronization between all threads on the same capability.
--
-- Here are the results: <<Timeout.html>>
--
-- I wonder how the observed behavior relates to <<https://ghc.haskell.org/trac/ghc/ticket/3838>>.
--
-- Usage:
--
-- > ghc --make -threaded Timeout.hs -rtsopts -O -fforce-recomp -Wall
-- > ./Timeout -o Timeout.html +RTS -N8

module Main
( main
) where

import Control.Concurrent.MVar (modifyMVar_, newMVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Control.Monad (replicateM, replicateM_)
import Data.IORef (newIORef, atomicModifyIORef')
import Data.Time.Clock (getCurrentTime)
import System.Timeout (timeout)
import Criterion.Main (bgroup, bench, defaultMain, nfIO)

threadN :: Int
threadN = 16

main :: IO ()
main = defaultMain $
    [ benchmark "plain" run1
    , benchmark "with-timeout" run2
    , benchmark "with-mvar" run3
    , benchmark "with-ioref" run4
    ]
  where
    params = [5,10,50,100,500,1000]
    benchmark label action = bgroup label
        [ bench (show i) . nfIO $ action i | i <- params ]

-- | Run 'threadN' threads which each call 'getCurrentTime'
-- 1000 times.
--
run1 :: Int -> IO ()
run1 n = forkN threadN $
    replicateM_ 1000 . replicateM_ n $ getCurrentTime

-- | Run 'threadN' threads which each call 'getCurrentTime'
-- 1000 times with a timeout of 10 seconds (which never triggers).
--
run2 :: Int -> IO ()
run2 n = forkN threadN $
    replicateM_ 1000 . timeout 10000000 . replicateM_ n $ getCurrentTime

-- | All threads share an 'MVar' that each thread updates
-- 1000 times.
--
run3 :: Int -> IO ()
run3 n = do
    mvar <- newMVar [0 :: Int]
    forkN threadN $
        replicateM_ 1000 $ do
            modifyMVar_ mvar (return . func)
            replicateM_ n $ getCurrentTime

-- | All threads share an 'IOref' that each thread updates
-- 1000 times.
--
run4 :: Int -> IO ()
run4 n = do
    var <- newIORef [0 :: Int]
    forkN threadN $
        replicateM_ 1000 $ do
            atomicModifyIORef' var (\a -> (func a, ()))
            replicateM_ n $ getCurrentTime

-- | Some pure function, that is not completely trivial
-- but yet cheap.
--
func :: [Int] -> [Int]
func a = take 40 $ a ++ [0]

-- | fork n processes and let them do some work in IO
--
forkN :: Int -> IO () -> IO ()
forkN n f = do
    vars <- replicateM n $ do
        v <- newEmptyMVar
        _ <- forkIO $ f `finally` putMVar v ()
        return v
    mapM_ takeMVar vars

module Timeout
(
) where
