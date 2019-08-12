{-# LANGUAGE OverloadedStrings #-}

-- | 关键逻辑
-- 感谢trasforms提供的高度抽象，省去了大量的代码
-- 吾生也有涯，而知也无涯。以有涯随无涯，殆已！已而为知者，殆而已矣！
module Web.Core.Core
  ( AppT
  , AppM
  , AppIO
  , getRequest
  , putRequest
  , putHeader
  , getResponseHeaders
  , consum
  , pureResp
  , respLBS
  , respLTS
  , respStream
  , respFile
  , toApplication
  ) where
import  Network.Wai
  ( Application
  , Request
  , Response
  , responseLBS
  , StreamingBody
  , responseStream
  , FilePart
  , requestMethod
  )
import Control.Monad.Trans.State
  ( get
  , put
  , runStateT
  , StateT
  )
import Control.Monad.Trans.Maybe
 ( MaybeT(..)
 , runMaybeT
 )
import Network.Wai as W
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as LE
import qualified Network.HTTP.Types as HT
import qualified Network.Wai.Handler.Warp as HW
import Control.Monad.Trans.Class
import Control.Monad
import Data.String
import Control.Monad.IO.Class(liftIO)
import Control.Exception(SomeException,catch,displayException)

-- | 所需要的State
type AppState = (Request,HT.ResponseHeaders)

-- | AppT m a 用于处理Web服务相关入口东西
type AppT m = MaybeT (StateT AppState m)


type AppM a = AppT IO a
type AppIO = AppM Application


-- | 获取Request
getRequest :: Monad m => AppT m Request
getRequest = do
  (req,_) <- lift $ get
  pure req

-- | 替换Request
putRequest :: Monad m => Request -> AppT m ()
putRequest req = do
  (_,state) <- lift $ get
  lift $ put $ (req,state)

-- | 添加ResponseHeader
putHeader :: Monad m => HT.HeaderName -> B.ByteString -> AppT m ()
putHeader name val = do
  (req,headers) <- lift $ get
  lift $ put $ (req,(name,val):headers)

-- | 获取ResponseHeaders
getResponseHeaders :: Monad m => AppT m HT.ResponseHeaders
getResponseHeaders = do
  (_,headers) <- lift $ get
  pure headers

-- | 消费一个path
consum :: Monad m => T.Text -> AppT m ()
consum path = do
  request <- getRequest
  let paths = W.pathInfo request
  guard $ length paths /= 0 && head paths == path
  putRequest $ request {W.pathInfo = tail paths}

pureResp :: Monad m => Response -> m Application
pureResp resp = pure $ result
  where result req respond = respond resp

respLBS :: Monad m => HT.Status -> BL.ByteString -> AppT m Application
respLBS status content = do
  headers <- getResponseHeaders
  pureResp $ responseLBS status headers content

respLTS :: Monad m => HT.Status -> TL.Text -> AppT m Application
respLTS status = respLBS status . LE.encodeUtf8

respStream :: Monad m => HT.Status -> StreamingBody -> AppT m Application
respStream status body = do
  headers <- getResponseHeaders
  pureResp $ responseStream status headers body

respFile :: Monad m => HT.Status -> FilePath ->Maybe FilePart -> AppT m Application
respFile status filepath part = do
  headers <- getResponseHeaders
  pureResp $ responseFile status headers filepath part

-- | HTTP中默认的Header
commonHeader = [("Server","AppM/1.0.0")]

-- | AppIO 转为 WAI中的Application
toApplication :: AppIO -> Application
toApplication appIO request respod = do
  let runT = runStateT . runMaybeT
  (maybeApp,(req,_)) <- runT appIO (request,commonHeader)
  case maybeApp of Nothing -> noapp
                   (Just app) -> runApp $ app req
  where noapp = respod $ responseLBS HT.status404 [] $ ""
        runApp appfn = appfn respod

