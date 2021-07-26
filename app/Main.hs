{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | 早期的一个demo,随库一起提交吧
-- TODO
module Main where

import Lib
import Web.AppM as M
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import Web.Static.Static
import Control.Applicative
import Web.WebSocket.WebSocket
import Data.Monoid
import Control.Exception
import Control.Concurrent.MVar
import System.IO.Unsafe
import GHC.Generics
import Data.Aeson
-- import Diqye.Moon
-- import Diqye.ServantTest



type Msgs = [(String,TL.Text)]
data UserSign = CurrentStatus {total::Int,displayUsers::[String]}
  | Recivice {sendUser::String,content::TL.Text}
  | InitMsgs {initMsgs::Msgs}
  deriving (Show,Generic)

instance ToJSON UserSign 
instance FromJSON UserSign 
  

-- 匿名用户列表
userLists :: MVar [(String,Connection)]
userLists = unsafePerformIO $ newMVar []

-- 10条历史纪录
historyMsgs :: MVar Msgs
historyMsgs = unsafePerformIO $ newMVar []



setting = setPort 8888
  $ setOnException (\ mreq e -> (putStrLn $ ("**OnException:\n" ++) $ displayException e)>>putStrLn "\n" >> putStrLn (show mreq))
  $ setOnExceptionResponse exceptionResponseForDebug
  -- $ setTimeout (30*60*60)
  $ setTimeout 10
  $ setFdCacheDuration 2
  $ defaultSettings
main :: IO ()
main = runSettings setting $ toApplication $ myapp


wsapp :: ServerApp
wsapp pending_conn = do
  conn <- acceptRequest pending_conn
  sendTextData conn ("wsapp"::TL.Text)
  forever $ do
    a <- (receiveData conn :: IO TL.Text) `catch` errorfn
    sendTextData conn $ a <> "  haved send"
  where errorfn :: ConnectionException -> IO TL.Text
        errorfn e = do
          putStrLn $ show $ e
          pure ""

modifyUserLists fn = do
  -- take使userLists置空
  users <- takeMVar userLists
  let users' = fn users
  -- 只有空的MVar才能进行put操作
  putMVar userLists users'
  sendAll users'
    $ encode
    $ CurrentStatus (length users')
    $ map fst users'
  pure users'

reciveLogic name conn = forever $ do
  a <- receiveData conn 
  users <- readMVar userLists
  let text =  encode $ Recivice name a
  sendAll users text
  msgs <- takeMVar historyMsgs
  let n = if length msgs > 11 then 10 else length msgs
  putMVar historyMsgs $ ((name,a):take n msgs)

socketApp :: AppIO
socketApp = do
  name <- consumV
  respSocket' $ \ pending_conn -> do
    conn <- acceptRequest pending_conn
    users' <- modifyUserLists $ ((name,conn):)
    let logic = reciveLogic name conn
    msgs <- readMVar historyMsgs
    sendTextData conn $ encode $ InitMsgs msgs
    -- forkPingThread conn 30
    logic `catch` removeConn (name,conn) logic
    where removeConn :: (String,Connection) -> IO () -> ConnectionException -> IO ()
          removeConn (name,conn) logic (CloseRequest _ _) = do
            putStrLn $ "closerequest"
            removeConnMVar name
            pure ()
          removeConn (name,conn) logic (ConnectionClosed) = do
            removeConnMVar name
            putStrLn $ name <> "关闭"
          removeConn (name,conn) logic e = do
            putStrLn $ "111:" ++ show e
            throwIO e
            
          removeConnMVar name = do
            modifyUserLists $ filter ((/= name) . fst)

            
            
            
    

sendAll users text = do
  putStrLn "===================="
  bads <- loop users
  if null bads then pure () else (pure () <* (modifyUserLists $ filter (not . (`elem` bads) . fst )))
  putStrLn "============end======"
  where loop [] = pure []
        loop ((name,conn):xs) = do
          putStrLn $ "send text:" <> name
          a <- (pure Nothing <* sendTextData conn text) `catch` \ e -> do putStrLn $ displayException $ (e::SomeException)
                                                                          pure $ Just name
          lr <- loop xs
          case a of Nothing -> pure $ lr
                    (Just name) -> pure $ (name:lr)
  
myapp :: AppIO
myapp = msum
  [ consum "dirserve" >> (dirBrowseJSON "/Users/zhenlong.qin" <|> dirServe "/Users/zhenlong.qin" ["package.yaml"])
  , do consum "dirserve1"
       a <- authBasicValue
       let r = dirBrowse "/Users/zhenlong.qin" <|> dirServe "/Users/zhenlong.qin" ["package.yaml"]
       if a == Just ("your sis","you") then  r else authBasic
  , consum "websocket" >> socketApp
  , consum "throw" >> (liftIO $ readFile "saf/asfdas/d") >> dirBrowse "."
  ]




