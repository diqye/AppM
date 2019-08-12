{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | URL Query Body 到类型
module Web.Core.ToValue
  ( WrapQuery(..)
  , consumV
  ) where
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Maybe(listToMaybe)
import Web.Core.Core
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad
import Network.Wai as W
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.ByteString.Lazy as BL



-- | Querystring的便民类型
-- 写Query的时候再思考要不要去掉
newtype WrapQuery = WrapQuery [(String,String)]



-- | url 或 querystring 解析到为指定类型
-- url: /123123/123
-- a <- consumA :: Int -- a=123123
-- (WrapQuery query) <- mQuery :: WrapQuery [(String,String)] 
class Read a => URLToValue a where
  toValue :: T.Text -> Maybe a
  toValue str = listToMaybe list
    where list = map fst $ filter ok' $ readsT $ str
          ok' (_,"") = True
          ok' (_,_) = False

readsT :: Read a => T.Text -> [(a,String)]
readsT = reads . T.unpack

instance URLToValue Int
instance URLToValue Integer
instance URLToValue Float
instance URLToValue Double


instance URLToValue String where
  toValue = Just . T.unpack

instance URLToValue T.Text where
  toValue = Just . id

instance URLToValue TL.Text where
  toValue = Just . TL.fromStrict
instance URLToValue BL.ByteString where
  toValue = Just . LE.encodeUtf8 . TL.fromStrict

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
  
