{-# LANGUAGE OverloadedStrings #-}

-- | 所有和HTTP method相关的东西
module Web.Core.Method where

import qualified Network.HTTP.Types as HT
import Web.Core.Core
import Control.Monad
import Network.Wai

-- | HTTP method guard
methodHTTP :: Monad m => HT.Method -> AppT m ()
methodHTTP method = do
  req <- getRequest
  let method' = requestMethod req
  guard $ method' == method

-- | HTTP method get
mGet :: Monad m => AppT m ()
mGet = methodHTTP HT.methodGet

-- | HTTP method Post
mPost :: Monad m => AppT m ()
mPost = methodHTTP HT.methodPost

-- | HTTP method Put
mPut :: Monad m => AppT m ()
mPut = methodHTTP HT.methodPut

-- | HTTP method Head
mHead :: Monad m => AppT m ()
mHead = methodHTTP HT.methodHead

-- | HTTP method Delete
mDelete :: Monad m => AppT m ()
mDelete = methodHTTP HT.methodDelete

-- | HTTP method Trace
mTrace :: Monad m => AppT m ()
mTrace = methodHTTP HT.methodTrace

-- | HTTP method Connect
mConnect :: Monad m => AppT m ()
mConnect = methodHTTP HT.methodConnect

-- | HTTP method Options
mOptions :: Monad m => AppT m ()
mOptions = methodHTTP HT.methodOptions

-- | HTTP method Patch
mPatch :: Monad m => AppT m ()
mPatch = methodHTTP HT.methodPatch
