{-# LANGUAGE OverloadedStrings #-}

-- | 所有和HTTP Header相关的东西
module Web.Core.Header where

import qualified Data.Text.Lazy as TL
import Web.Core.Core



putHtmlHeader :: Monad m => AppT m ()
putHtmlHeader = putHeader "Content-Type" "text/html; charset=utf-8"