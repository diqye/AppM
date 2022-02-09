{-# LANGUAGE OverloadedStrings,TypeSynonymInstances,FlexibleInstances #-}
-- | URL Query Body 到类型
-- 常记溪亭日暮，沉醉不知归路。兴尽晚回舟，误入藕花深处。
-- 争渡，争渡，惊起一滩鸥鹭。
module Web.Core.ToValue
  ( consumV
  , queryV
  , bodyQueryV
  , bodyJSONV
  , URLToValue
  ) where
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Maybe(listToMaybe)
import Web.Core.Core
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad
import Control.Monad.IO.Class
import Network.Wai as W
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.ByteString.Lazy as BL
import Data.String.Conversions
import Data.Aeson
import Network.HTTP.Types.URI
import Data.String.Conversions




-- | url 或 querystring 解析到为指定类型
-- url: /123123/123
-- a <- consumA :: Int -- a=123123
class Read a => URLToValue a where
  toValue :: T.Text -> Maybe a
  toValue str = listToMaybe list
    where list = map fst $ filter ok' $ readsT $ str
          ok' (_,"") = True
          ok' (_,_) = False

readsT :: Read a => T.Text -> [(a,String)]
readsT = reads . T.unpack

instance URLToValue a => URLToValue (Maybe a) where
  toValue "" = Just $ Nothing
  toValue a = Just $ toValue a
instance URLToValue String where
  toValue = Just . cs
instance URLToValue LazyText where
  toValue = Just . cs
instance URLToValue StrictText where
  toValue = Just . cs
instance URLToValue LazyByteString where
  toValue = Just .cs
instance URLToValue StrictByteString where
  toValue = Just .cs

instance URLToValue Int
instance URLToValue Integer
instance URLToValue Float
instance URLToValue Double

{-
instance URLToValue String where
  toValue = Just . T.unpack

instance URLToValue T.Text where
  toValue = Just . id

instance URLToValue TL.Text where
  toValue = Just . TL.fromStrict
instance URLToValue BL.ByteString where
  toValue = Just . LE.encodeUtf8 . TL.fromStrict
-}

-- |a <- consumA :: Int -- a=123123
consumV :: (URLToValue a,Monad m) => AppT m a
consumV = do
  request <- getRequest
  let paths = W.pathInfo request
  guard $ length paths /= 0
  let dir = paths !! 0
  a <- MaybeT $ pure $ toValue dir
  putRequest $ request {W.pathInfo = tail paths}
  pure a
  
-- | querystring 处理方案
queryV :: (URLToValue a,Monad m) => String -> AppT m a
queryV key = do
  req <- getRequest
  let query = W.queryString req
  maybeVal <- MaybeT $ pure $ lookup (cs key) query
  let val = maybe "" cs maybeVal
  MaybeT $ pure $ toValue val

-- | body中 querystring格式处理
bodyQueryV :: (URLToValue a,MonadIO m) => String -> AppT m a
bodyQueryV key = do
  req <- getRequest
  bsquery <- liftIO $ W.strictRequestBody req
  let query = parseQuery $ cs bsquery
  maybeVal <- MaybeT $ pure $ lookup (cs key) query
  let val = maybe "" cs maybeVal
  MaybeT $ pure $ toValue val

-- | body 中 json格式处理
bodyJSONV :: (FromJSON a,MonadIO m) => AppT m a
bodyJSONV = do
  req <- getRequest
  bsjson <- liftIO $ W.lazyRequestBody req
  MaybeT $ pure $ decode bsjson
  
  
