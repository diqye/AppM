# AppM
基于Wai的Monad
## 使用示例
```haskell
import Web.AppM
import Web.Static.Static
import Web.WebSocket.WebSocket

myapp :: AppIO
myapp = msum
  -- 文件服务和目录浏览
  [ consum "dirserve" >> (dirServe "/Users/diqye" ["package.yaml"] <|> dirBrowse "/Users/diqye" )
  -- websocket
  , consum "websocket" >> socketApp
  -- 抛错示例
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
