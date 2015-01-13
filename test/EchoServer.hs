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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main
( main
, server
) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Lens ((&))
import Control.Monad.IO.Class

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.Monoid.Unicode
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Encoding as LT

import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as Warp

import qualified Web.Scotty as Scotty

-- internal modules
import Network.Benchmark.Utils

main ∷ IO ()
main = server 8282

server ∷ Int → IO ()
server port = do
    T.putStrLn $ "Run HTTP echo server on port " ⊕ sshow port ⊕ " of localhost"
    Scotty.scottyOpts opts $ do

        -- Scotty.post "/random/:" $
        --    b ← Scotty.body

        Scotty.matchAny "/ping" $ Scotty.json ()

        Scotty.matchAny "/ping/delay/:milliseconds" $ do
            delay ∷ Int ← Scotty.param "milliseconds"
            liftIO $ threadDelay (delay * 1000)
            Scotty.json ()

        Scotty.matchAny "/echo" $
            echo Nothing >>= Scotty.json

        Scotty.matchAny "/echo/delay/:milliseconds" $ do
            delay ∷ Int ← Scotty.param "milliseconds"
            echo (Just delay) >>= Scotty.json

        Scotty.matchAny (Scotty.function $ \_ → Just []) $
            echo Nothing >>= Scotty.json
  where
    opts = def
        { Scotty.verbose = 0
        , Scotty.settings = Warp.defaultSettings
            & Warp.setPort port
            & Warp.setOnException (\_ e → T.putStrLn $ "[HTTP Echo Server] Exception: " ⊕ sshow e)
        }

    hdr2pair ∷ (CI.CI B.ByteString, B.ByteString) → (T.Text, A.Value)
    hdr2pair (n, v) = (T.decodeUtf8 (CI.original n), A.String (T.decodeUtf8 v))

    echo delay = do
        hdrs ∷ [(T.Text, A.Value)] ← map hdr2pair . WAI.requestHeaders <$> Scotty.request
        b ← Scotty.body
        case delay of
            Nothing → return ()
            Just milliseconds → liftIO $ threadDelay (milliseconds * 1000)
        return $ A.object
            [ "headers" A..= A.object hdrs
            , "body" A..= LT.decodeUtf8 b
            ]

