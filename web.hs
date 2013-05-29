{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Lazy
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)

import Control.Concurrent (ThreadId, threadDelay, myThreadId)
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat, mappend)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Network.Wai.Middleware.RequestLogger
import Web.Scotty
import Network.HTTP.Types.Status (badRequest400)

text' x = text (x `mappend` "\n")

-- todo: weakrefs
type PubSubQ = TVar (Map.Map Text (Bool, Set.Set ThreadId))

main = scotty 3000 $ do
    middleware logStdoutDev

    -- global channel is used for / requests (single writer, random reader)
    -- 
    -- % curl -X POST -d key=value -v :3000/
    -- % curl -v :3000/

    globalChan <- liftIO $ (newChan :: IO (Chan Text))

    get "/" $ do
        value <- liftIO $ readChan globalChan
        text $ mconcat ["*global*: ", value, "\n"]

    post "/" $ do {
        key <- param "key";
        liftIO $ writeChan globalChan key;
        text' "ok"
    } `rescue` (\msg -> status badRequest400 >> text' msg)

    -- pub/sub by :key (single writer, multiple simultaneous readers)
    
    waitq <- liftIO $ atomically $ newTVar Map.empty :: ScottyM PubSubQ

    post "/waitq/:key" $ do
        key <- param "key"
        waitlist <- liftIO $ atomically $ do
            wq <- readTVar waitq
            case Map.lookup key wq of
                Nothing -> return Set.empty
                Just (_, threads) -> do
                        writeTVar waitq $ Map.insert key (True, threads) wq
                        return threads
        text' $ mconcat ["waitlist: ", pack $ show waitlist]

    get "/waitq/:key" $ do
        key <- param "key"
        me <- liftIO $ myThreadId
        liftIO $ atomically $ do
            wq <- readTVar waitq
            let subscribe threads = (writeTVar waitq $ Map.insert key (False, Set.insert me threads) wq)
                in case Map.lookup key wq of
                        Nothing -> subscribe Set.empty
                        Just (False, threads) -> subscribe threads
                        Just (True, threads) -> retry

        liftIO $ atomically $ do
            wq <- readTVar waitq
            case Map.lookup key wq of
                Nothing -> retry
                Just (False, threads) -> retry
                Just (True, threads) -> do
                    let set = Set.delete me threads
                    writeTVar waitq $ Map.insert key (if set == Set.empty then False else True, set) wq

        text' $ mconcat [key, ": yay! ", pack $ show me]

    get "/waitq" $ do
        wq <- liftIO $ atomically $ readTVar waitq
        text' $ pack $ show $ wq
