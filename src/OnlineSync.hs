{-# LANGUAGE DeriveGeneric #-}
module OnlineSync (
    Connection (..),
    seemless,
    tryResult,
    postUncompressedFile,
    postResults,
    getResults,
    getChunk) where

import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Types

import GHC.Generics
import Text.Printf
import Control.Applicative
import System.IO
import Control.DeepSeq
import Control.Monad (zipWithM_)
import Control.Monad.Trans.Maybe
import Data.Binary (Binary, encode, decode)
import Data.Maybe
import Data.Text (pack, unpack)
import Data.List (sort)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as Strict
import qualified Data.ByteString.Lazy.Char8 as Lazy

import qualified Codec.Compression.Zlib as Z (compress, decompress)

data Connection = Connection { server :: String, port :: Integer } deriving (Generic)
instance A.FromJSON Connection
instance A.ToJSON Connection

instance Show Connection where
    show (Connection s p) = "http://" ++ s ++ ":" ++ show p

seemless :: Binary a => Connection -> String -> a -> IO a
seemless con name computation  = tryResult con name >>= maybeCompute con name computation

tryResult :: Binary a => Connection -> String -> IO (Maybe a)
tryResult con name = (pure (decode . Z.decompress) <*>) <$> getResults con name

maybeCompute :: Binary a => Connection -> String -> a -> Maybe a ->IO a
maybeCompute _   name _           (Just x) = return x
maybeCompute con name computation Nothing  = postResults con name (encode computation) >> return computation

postUncompressedFile :: Connection -> String -> String -> IO ()
postUncompressedFile con fn name = Lazy.readFile fn >>= postResults con name

suffix :: Int -> String
suffix j
   | j == 0 = ""
   | otherwise = printf ".%02d" j

postChunk :: Manager -> Request -> String -> Strict.ByteString -> Int -> IO ()
postChunk manager request name chunk i = do
    let fn = "/tmp/" ++ name ++ suffix i
    Strict.writeFile fn chunk
    mfd <- formDataBody [ partFileSource (pack name) fn ] request
    putStr $ " - " ++ name ++ "(part " ++ show i ++ ")"
    response <- httpLbs mfd manager
    putStrLn $ ": " ++ (show . statusCode . responseStatus $ response)
    return ()

postResults :: Connection -> String -> Lazy.ByteString -> IO ()
postResults con name object = do
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest $ show con ++ "/results/post"
    let request = initialRequest { method = Strict.pack "POST" }
    putStrLn $ "Posting object to: " ++ show con
    zipWithM_ (postChunk manager request name) (rechunk $ Z.compress object) [0,1..]

getChunk :: Manager -> Connection -> String -> IO (Maybe Lazy.ByteString)
getChunk manager con name = do
    irGet <- parseRequest $ show con ++ "/results/get/" ++ name
    let request = irGet { method = Strict.pack "GET" }
    response <- httpLbs request manager
    putStrLn $ " - " ++ name ++ ": " ++ (show . statusCode . responseStatus $ response)
    return $ if responseStatus response == ok200 -- all/and
             then Just (responseBody response)
             else Nothing

getResults :: Connection -> String -> IO (Maybe Lazy.ByteString)
getResults con name = do
    manager <- newManager defaultManagerSettings
    irQuery <- parseRequest $ show con ++ "/results/query/" ++ name
    let request = irQuery { method = Strict.pack "GET" }
    response <- httpLbs request manager
    let names = fromMaybe [] (sort <$> A.decode (responseBody response) :: Maybe [String]) -- from parse
    putStrLn $ "Request to " ++ show con
               ++ " to get: " ++ name ++ ": "
    maybeChunkList <- mapM (getChunk manager con) names -- maybe from getchunk
    return (Lazy.concat <$> (emptyToNothing . sequence) maybeChunkList)

emptyToNothing :: Maybe [a] -> Maybe [a]
emptyToNothing Nothing = Nothing
emptyToNothing (Just []) = Nothing
emptyToNothing (Just x) = Just x

rechunk :: Lazy.ByteString -> [Strict.ByteString]
rechunk s
    | Lazy.null s = []
    | otherwise = let (pref, suff) = Lazy.splitAt chunkSize s
                  in repack pref : rechunk suff
        where
            chunkSize = 20 * 1024 * 1024 -- 20 MB
            repack = Strict.concat . Lazy.toChunks
