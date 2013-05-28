{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat, mappend)

import Network.Wai.Middleware.RequestLogger
import Web.Scotty
import Network.HTTP.Types.Status (badRequest400)

text' x = text (x `mappend` "\n")

main = scotty 3000 $ do
    middleware logStdoutDev

    chan <- liftIO $ newChan
    globalChan  <- liftIO $ newChan

    get "/" $ do
        value <- liftIO $ globalChan chan
        text $ mconcat [value]

    post "/" $ do {
        key <- param "key";
        text' key
    } `rescue` (\msg -> status badRequest400 >> text' msg)

    post "/:key" $ do
        key <- param "key"
        liftIO $ writeChan chan key
        text' "thanx"

    get "/:key" $ do
        key <- param "key"
        liftIO $ threadDelay 2000000
        text $ mconcat ["Scotty, ", key, " me up!\n"]
