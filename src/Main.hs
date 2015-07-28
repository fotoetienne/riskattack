{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import qualified Data.ByteString.Char8 as BS
import           Risk
import           Data.Maybe

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (serveDirectory "./static/") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          , ("attack", riskHandler)] <|>
    dir "static" (serveDirectory "./static/")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

riskHandler :: Snap ()
riskHandler = do
    a <- getParam "a"
    d <- getParam "d"
    let f x = read $ BS.unpack $ fromMaybe "1" x
    writeBS $ BS.pack $ show $ exactSuccessProb $ Battlefield (f a) (f d)

