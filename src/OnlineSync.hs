{-# LANGUAGE DeriveGeneric #-}
module OnlineSync where

import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Types

import Control.Applicative
import System.IO
import Control.DeepSeq
import Data.Binary (Binary, encode, decode)
import Data.Maybe
import Data.Text (pack, unpack)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BI

data Connection = Connection { server :: String, port :: Integer } deriving (Generic)

instance Show Connection where
    show (Connection s p) = "http://" ++ s ++ ":" ++ show p

seemless :: Binary a => Connection -> String -> a -> IO a
seemless con name computation  = tryResult con name >>= maybeCompute con name computation

tryResult :: Binary a => Connection -> String -> IO (Maybe a)
tryResult con name = (pure decode <*>) <$> getResults con name

maybeCompute :: Binary a => Connection -> String -> a -> Maybe a ->IO a
maybeCompute _   name _           (Just x) = return x
maybeCompute con name computation Nothing  = postResults con name (encode computation) >> return computation

postResults :: Connection -> String -> B.ByteString -> IO ()
postResults con name object = do
    let fn = "/tmp/" ++ name
    B.writeFile fn object
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest $ show con ++ "/results/post"
    let request = initialRequest { method = BI.pack "POST" }
    mfd <- formDataBody [ partFileSource (pack name) fn ] request
    print mfd
    response <- httpLbs mfd manager
    return()

getResults :: Connection -> String -> IO (Maybe B.ByteString)
getResults con name = do
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest $ show con ++ "/results/get/" ++ name
    let request = initialRequest { method = BI.pack "GET" }
    response <- httpLbs request manager
    print response
    return $ if responseStatus response == ok200 then Just (responseBody response) else Nothing
