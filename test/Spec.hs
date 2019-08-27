import Web.AppM

main :: IO ()
main = do
  putStrLn "Test suite not yet implemented"

testApp :: Application -> Request -> (Response -> IO Bool) -> IO Bool
testApp req app test = do
  mr <- newEmptyMVar
  app req $ \ resp -> test resp >>= putMar mr >> pure ResponseReceived
  r <- takeMVar mr
  return r




