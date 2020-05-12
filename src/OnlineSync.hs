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

seemless :: Binary a => String -> String -> a -> IO a
seemless url name computation  = tryResult url name >>= maybeCompute url name computation

tryResult :: Binary a => String -> String -> IO (Maybe a)
tryResult url name = (pure decode <*>) <$> getResults url name

maybeCompute :: Binary a => String -> String -> a -> Maybe a ->IO a
maybeCompute _   name _           (Just x) = return x
maybeCompute url name computation Nothing  = postResults url name (encode computation) >> return computation

postResults :: String -> String -> B.ByteString -> IO ()
postResults url name object = do
    let fn = "/tmp/" ++ name
    B.writeFile fn object
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest $ url ++ "/results/post" 
    let request = initialRequest { method = BI.pack "POST" }
    mfd <- formDataBody [ partFileSource (pack name) fn ] request 
    print mfd
    response <- httpLbs mfd manager
    return()

getResults :: String -> String -> IO (Maybe B.ByteString)
getResults url name = do
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest $ url ++ "/results/get/" ++ name
    let request = initialRequest { method = BI.pack "GET" }
    response <- httpLbs request manager
    print response
    return $ if responseStatus response == ok200 then Just (responseBody response) else Nothing
