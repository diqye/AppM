# AppM
高性能的warp,睿智的mtl结合在一起，就是AppM。
## 简单示例
```haskell
import Web.AppM
import Web.Static.Static
import Web.WebSocket.WebSocket

myapp :: AppIO
myapp = msum
  -- 文件服务和目录浏览
  [ consum "dirserve" >> (dirServe "/Users/diqye" ["package.yaml"] <|> dirBrowse "/Users/diqye" )
  -- websocket
  , consum "websocket" >> respSocket' (socketApp::ServerApp)
  -- IO和抛错示例
  , consum "throw" >> (liftIO $ readFile "saf/asfdas/d") >> dirBrowse "."
  ]

setting = setPort 8899
  $ setOnException (\ _ e -> putStrLn $ ("**OnException:\n" ++) $ displayException e)
  $ setOnExceptionResponse exceptionResponseForDebug
  $ setTimeout (30*60*60)
  $ defaultSettings

main :: IO ()
main = runSettings setting $ toApplication $ myapp
```
## servant
```haskell
{-# LANGUAGE OverloadedStrings,OverloadedStrings,DataKinds,TypeOperators,DeriveGeneric #-}
import Data.String(fromString)
import Data.String.Conversions(cs)
import Control.Monad.IO.Class(liftIO)
-- ------------------------------------------------
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API

-- | app :: Monad m => AppT m Application
-- Or app:: AppIO
app = pure $ serve (Proxy::Proxy API) serverAPI

type API = Get '[JSON] (String,String)
  :<|> "cat" :> Capture "name" String :> Get '[JSON] String

getJSON = do
  liftIO $ putStrLn "getJSON"
  pure ("a","b")

getJSONWithName name = do
  liftIO $ putStrLn name
  pure "getJSONWithName"

serverAPI :: Server API
serverAPI = getJSON :<|> getJSONWithName
```
