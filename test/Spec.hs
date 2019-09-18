import Web.AppM

main :: IO ()
main = putStrLn "Test suite not yet implemented"


testApplication :: Application -> (Response -> IO ResponseReceived) -> IO Bool
testApplication app respdo = app 
