{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.HTTP.Types
import GHC.Generics
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Network.Wai.Parse hiding (File)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import System.Directory
import Control.Applicative

main = scotty 8080 $ do
  post "/results/post/" $ do
      fs <- files :: ActionM [File]
      let fs' = [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName, fi) <- fs ]
      liftIO $ sequence_ [ B.writeFile ("static/" ++ fn) fc | (_,fn,fc) <- fs' ]
      status ok200
  get "/results/query/:name" $ do
      name <- param "name"
      let startsWith pattern txt = take (length pattern) txt == pattern
      bla <- liftIO (filter (startsWith name) <$> listDirectory "static/")
      json bla
  get "/results/get/:name" $ do
      name <- param "name"
      file $ "static/" ++ name

