{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Lazy
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)

import GHC.Conc
import Control.Concurrent (ThreadId, threadDelay, myThreadId)
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Monad (sequence)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat, mappend)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

import Network.Wai.Middleware.RequestLogger
import Web.Scotty
import Network.HTTP.Types.Status (badRequest400)

import qualified System.Remote.Monitoring as Mon

text' x = text (x `mappend` "\n")

show' :: (Show a) => a -> ActionM ()
show' = text' . pack . show

-- todo: weakrefs
type PubSubQ = Map.Map Text (Bool, Set.Set ThreadId)
type QThreadStatus = (Text, ThreadId, ThreadStatus)

threadStatus' :: Text -> ThreadId -> IO QThreadStatus
threadStatus' k t = threadStatus t >>= \x -> return (k, t, x)

qstats :: PubSubQ -> [IO QThreadStatus]
qstats wq = List.concatMap (\(key, (_, threads)) -> List.map (threadStatus' key) $ Set.toList threads) $ Map.toList wq

runScotty = scotty 3000 $ do
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
    
    waitq <- liftIO $ atomically $ newTVar Map.empty :: ScottyM (TVar PubSubQ)

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

        text' $ mconcat [key, ": response ", pack $ show me]

    get "/waitq" $ do
        wq <- liftIO $ atomically $ readTVar waitq
        wql <- liftIO $ sequence $ qstats wq
        show' $ wql

    get "/bug" $ do
        show' $ Prelude.head ([] :: [Text])

main = do
        Mon.forkServer "localhost" 3001
        runScotty
