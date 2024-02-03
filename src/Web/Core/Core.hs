{-# LANGUAGE OverloadedStrings #-}
module Web.Core.Core
  ( AppT
  , AppM
  , AppIO
  , AppState
  , getRequest
  , putRequest
  , putHeader
  , getResponseHeaders
  , consume
  , home
  , getAppState
  , putAppState
  , appTry
  , appmsum
  , pureResp
  , respLBS
  , respLTS
  , respStream
  , respFile
  , toApplication
  , appTToApplication
  , appm_version
  , respJSON
  ) where
import  Network.Wai
  ( Application
  , Request
  , Response
  , responseLBS
  , StreamingBody
  , responseStream
  , FilePart
  , requestHeaders
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
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as LE
import qualified Network.HTTP.Types as HT
import qualified Network.Wai.Handler.Warp as HW
import Control.Monad.Trans.Class
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Control.Applicative(empty,(<|>))
import Data.String
import Control.Monad.IO.Class(liftIO)
import Control.Exception(SomeException,catch,displayException)

appm_version = "1.0.0"


putJSONHeader :: Monad m => AppT m ()
putJSONHeader  = putHeader "Content-Type" "application/json"
-- | A state with 'Request' and 'HT.ResponseHeaders
type AppState = (Request,HT.ResponseHeaders)

-- | A `MaybeT` including `StateT` wrapping
type AppT m = MaybeT (StateT AppState m)


type AppM a = AppT IO a
type AppIO = AppM Application


getAppState :: Monad m => AppT m AppState
getAppState = lift $ get

putAppState :: Monad m => AppState -> AppT m ()
putAppState state = lift $ put $ state

-- | Restore state
-- @
-- appTry $ consum "hello" >> empty
-- consum "hello" -> respLBS status200 "hello"
-- @ 
-- If you visit `/hello` it will be response string `"hello"`, when remove 'appTry` it will be empty
appTry :: Monad m => AppT m a -> AppT m a
appTry appt = do
  state <- getAppState
  appt <|> do 
    putAppState state
    empty


-- | A safer version of  'msum' 
appmsum xs = msum $ map appTry xs

getRequest :: Monad m => AppT m Request
getRequest = do
  (req,_) <- lift $ get
  pure req

putRequest :: Monad m => Request -> AppT m ()
putRequest req = do
  (_,state) <- lift $ get
  lift $ put $ (req,state)

putHeader :: Monad m => HT.HeaderName -> B.ByteString -> AppT m ()
putHeader name val = do
  (req,headers) <- lift $ get
  lift $ put $ (req,(name,val):headers)

getRequestHeader :: Monad m => HT.HeaderName -> AppT m (Maybe B.ByteString)
getRequestHeader name = do
  req <- getRequest
  let headers = requestHeaders req
  pure $ lookup name headers

getResponseHeaders :: Monad m => AppT m HT.ResponseHeaders
getResponseHeaders = do
  (_,headers) <- lift $ get
  pure headers

-- | Route a path
-- @
-- consum "hello" >> respLBS status200 "hello"
-- @
-- Visit `/hello` will respond with `"hello"`
consume :: Monad m => T.Text -> AppT m ()
consume path = do
  request <- getRequest
  let paths = W.pathInfo request
  guard $ length paths /= 0 && head paths == path
  putRequest $ request {W.pathInfo = tail paths}

-- | A home router eg `/`
-- / -> home
-- home >> respLBS status200 "home"
home :: Monad m => AppT m ()
home = do
  request <- getRequest
  let paths = W.pathInfo request
  guard $ null paths

pureResp :: Monad m => Response -> m Application
pureResp resp = pure $ result
  where result req respond = respond resp

respLBS :: Monad m => HT.Status -> BL.ByteString -> AppT m Application
respLBS status content = do
  pureResp $ responseLBS status [] content

respJSON :: (Monad m,A.ToJSON v) => HT.Status -> v -> AppT m Application
respJSON status v = do
  putJSONHeader
  pureResp $ responseLBS status [] (A.encode v)

respLTS :: Monad m => HT.Status -> TL.Text -> AppT m Application
respLTS status = respLBS status . LE.encodeUtf8

respStream :: Monad m => HT.Status -> StreamingBody -> AppT m Application
respStream status body = do
  pureResp $ responseStream status [] body

respFile :: Monad m => HT.Status -> FilePath ->Maybe FilePart -> AppT m Application
respFile status filepath part = do
  pureResp $ responseFile status [] filepath part

-- | Default Header
commonHeader = [("Server","version " <> appm_version <> " " <> "https://github.com/diqye/appm")]



-- | runApplication
runAppT :: Monad m => AppT m a -> AppState -> m (Maybe a,AppState)
runAppT appt state = runStateT (runMaybeT $ appt) state


-- | AappT m Application 转 Application
appTToApplication :: Monad m => AppT m Application -> (m (Maybe Application,AppState) -> IO (Application,AppState)) -> Application
appTToApplication appT trans req respod = do
  let runT = runStateT . runMaybeT
  (app,(req',headers)) <- trans $ runT  appT $ (req,commonHeader)
  modifyResponse (mapResponseHeaders  (<> headers)) app  req' respod


-- | AppIO 转为 WAI中的Application
toApplication :: AppIO -> Application
toApplication appIO = appTToApplication appIO trans 
  where trans ior = do
          (mapp,a) <- ior
          case mapp of Nothing -> pure $ (noapp,a)
                       Just app -> pure $ (app,a)
        noapp req respod = respod $ responseLBS HT.status404 [] $ ""

