{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Web.WebSocket.WebSocket
-- Copyright   :  (C) 2019-8-9 Edward Kmett
--
-- @Web.WebSocket.WebSocket@  提供websockets支持
-- 感谢 websockets提供websockets功能
-- 感谢 wai-websockets提供wai运行websocket的能力
----------------------------------------------------------------------------
module Web.WebSocket.WebSocket
  ( module Network.WebSockets
  , module Network.WebSockets.Connection
  , module Network.Wai.Handler.WebSockets
  , respSocket
  , respSocket'
  ) where

import Web.AppM
import Network.Wai.Handler.WebSockets
import Network.WebSockets (ServerApp,defaultConnectionOptions,ConnectionException(..))
import Network.Wai.Handler.WebSockets
import Network.WebSockets.Connection

-- | Socket支持 ServerApp参见webSockets库
-- 本模块导出 常用的webSockets 无需单独引入
-- 
-- @
-- respSocket defaultConnectionOptions  $ \  pending_conn -> do
--   conn <- acceptRequest pending_conn
--   ....
-- @    
respSocket :: Monad m => ConnectionOptions -> ServerApp -> AppT m Application
respSocket opts wsapp = do
  req <- getRequest
  guard $ isWebSocketsReq req
  pure $ websocketsOr opts wsapp bkapp
  -- 这段代码永远走不到
  where bkapp _ respond = respond $ responseLBS status401 [] ""

-- | 同respSocket,不同的是提供了默认的ConnectionOptions
--
-- >>> respSocket' = respSocket defaultConnectionOptions
respSocket' :: Monad m => ServerApp -> AppT m Application
respSocket' = respSocket defaultConnectionOptions
