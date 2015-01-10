{-# LANGUAGE OverloadedStrings #-}

module Web.Haskbot.Responder where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar)
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, runReaderT, asks, liftIO)

import qualified Network.Connection   as N
import qualified Network.HTTP.Conduit as N
import qualified Network.HTTP.Types   as N
import qualified Data.ByteString.Lazy as LB

import Network.HTTP.Conduit

type RespM = ReaderT RespEnv IO

data RespEnv = RespEnv
  { resp_queue  :: TVar [LB.ByteString]
  , resp_sender :: LB.ByteString -> RespM ()
  }

-- public functions

runResponseQueue :: String -> IO (LB.ByteString -> IO ())
runResponseQueue url = do
    queue <- newTVarIO []
    let env = RespEnv queue $ initSender url
    _ <- forkIO $ runReaderT sendFromQueue env
    return $ enqueue queue

-- private functions

dequeue :: RespM (Maybe LB.ByteString)
dequeue = do
    queue  <- asks resp_queue
    liftIO . atomically $ do
        msgs <- readTVar queue
        case msgs of
            (m:_) -> do
                modifyTVar' queue $ \q -> tail q
                return $ Just m
            _ -> return Nothing

enqueue :: TVar [LB.ByteString] -> LB.ByteString -> IO ()
enqueue queue msg = atomically $ modifyTVar' queue $ \q -> q ++ [msg]

handleSlack :: LB.ByteString -> Response a -> RespM ()
handleSlack resp slack
    | allGood   = return ()
    | otherwise = do
        queue <- asks resp_queue
        liftIO $ enqueue queue resp
  where
    allGood = responseStatus slack == N.status200

initNetConn :: IO N.Manager
initNetConn = N.newManager $ N.mkManagerSettings tlsInfo Nothing
  where tlsInfo = N.TLSSettingsSimple False False False

initSender :: String -> LB.ByteString -> RespM ()
initSender url resp = do
    conn     <- liftIO initNetConn
    template <- liftIO $ formatReq url
    let toSend = template { requestBody = RequestBodyLBS resp }
    liftIO (httpLbs toSend conn) >>= handleSlack resp

formatReq :: String -> IO Request
formatReq url = do
    req <- liftIO $ parseUrl url
    return $ req
      { method         = N.methodPost
      , rawBody        = True
      , requestHeaders = [(N.hContentType, "application/json")]
      }

sendFromQueue :: RespM ()
sendFromQueue = forever $ dequeue >>= sendNext >> wait1Sec

sendNext :: Maybe LB.ByteString -> RespM ()
sendNext (Just resp) = do
    sender <- asks resp_sender
    sender resp
sendNext _ = return ()

wait1Sec :: RespM ()
wait1Sec = liftIO $ threadDelay 1000000
