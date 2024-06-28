{-# LANGUAGE OverloadedStrings, OverloadedLabels, DeriveGeneric, DeriveAnyClass #-}

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
import Database.SQLite.Simple as SL
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict,pack)
import Data.Maybe
import Database.PostgreSQL.Simple as P
import Database.PostgreSQL.Simple.FromRow as PFR
import Database.PostgreSQL.Simple.ToRow as PTR
import qualified Data.Text.Lazy as TL
import Debug.Trace

-- SQLite3
data Pair = Pair { key :: T.Text, val :: T.Text } deriving (Eq, Read, Show)
instance SL.FromRow Pair where
    fromRow = Pair <$> SL.field <*> SL.field
instance SL.ToRow Pair where
    toRow (Pair k v) = SL.toRow (k, v)

-- PostgreSQL
newtype PValue = PValue { fieldValue :: String }
data PPair = PPair { k :: !String, v :: !String } deriving (Eq, Read, Show)
data PKFT = PKFT { k4 :: !String, from :: !Integer, to :: !Integer } deriving (Eq, Read, Show)
data PTriple = PTriple { k3 :: !String, v3 :: !String, v3' :: !String } deriving (Eq, Read, Show)
data PDateVal = PDateVal { date :: !Integer, val' :: !String } deriving (Eq, Read, Show)
--instance P.FromRow PPair where
--    fromRow = PPair <$> field <*> field
--instance P.ToRow PPair where
--    toRow (PPair k v) = P.toRow (k, v)


instance PFR.FromRow PValue where
  fromRow = PValue <$> PFR.field
instance PFR.FromRow PPair where
  fromRow = PPair <$> PFR.field <*> PFR.field
instance PFR.FromRow PDateVal where
  fromRow = PDateVal <$> PFR.field <*> PFR.field

instance PTR.ToRow PValue where
    toRow (PValue v) = PTR.toRow (Only v)
instance PTR.ToRow PPair where
    toRow (PPair k v) = PTR.toRow (k, v)
instance PTR.ToRow PTriple where
    toRow (PTriple k v v') = PTR.toRow (k, v, v')
instance PTR.ToRow PKFT where
    toRow (PKFT k v v') = PTR.toRow (k, v, v')
instance PTR.ToRow PDateVal where
    toRow (PDateVal d v) = PTR.toRow (d, v)

instance ToJSON PValue where
  toJSON (PValue value) =
    object ["value" .= value]

fetchData :: P.Connection -> String -> IO [PPair]
fetchData conn k = P.query conn "SELECT * FROM keyvalue WHERE key = ?" (Only k)

allLogs :: P.Connection -> String -> IO [PValue]
allLogs conn k = P.query conn "SELECT val FROM kvlog WHERE key = ? ORDER BY change" (Only k)

logsFromTo :: P.Connection -> String -> Integer -> Integer -> IO [PDateVal]
logsFromTo conn k f t = P.query conn "SELECT change, val FROM kvlog WHERE key = ? AND change BETWEEN ? AND ? ORDER BY change" (PKFT k f t)

logsFromTo' :: P.Connection -> String -> Integer -> Integer -> IO [PDateVal]
logsFromTo' conn k f t = 
    let query = "SELECT change, val FROM kvlog WHERE key = ? AND change BETWEEN ? AND ? ORDER BY change"
        params = PKFT k f t
        tracedQuery = traceShow (query, params) query
    in P.query conn tracedQuery params

main =
    do
    connp <- connect defaultConnectInfo { connectDatabase = "ds", connectUser = "stefan", connectPassword = "podersdorf" }
    scotty 8080 $ do
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
        get "/keyvalue/sqlite/set/:db/:key/:val/" $ do
          dbpar <- param "db"
          keypar <- param "key"
          valpar <- param "val"
          res <- liftIO $ do
              conn <- SL.open ("static/" ++ dbpar ++ ".db")
              SL.execute_ conn "CREATE TABLE IF NOT EXISTS keyvalue (key TEXT PRIMARY KEY, val TEXT)"
              SL.execute conn "REPLACE INTO keyvalue(key, val) VALUES(?, ?)" (Pair keypar valpar)
              SL.execute_ conn "CREATE TABLE IF NOT EXISTS kvlog (change INT, key TEXT, val TEXT)"
              SL.execute conn "INSERT INTO kvlog(change, key, val) VALUES(strftime('%s','now'), ?, ?)" (Pair keypar valpar)
              SL.close conn
          addHeader "Access-Control-Allow-Origin" "*"
          addHeader "Access-Control-Allow-Methods" "*"
          status ok200
        get "/keyvalue/set/:db/:key/:val/" $ do
          keypar <- param "key"
          valpar <- param "val"
          let inq = "INSERT INTO keyvalue (key, val) VALUES (?, ?) ON CONFLICT (key) DO UPDATE SET val = ?"
          let logq = "INSERT INTO kvlog (change, key, val) VALUES (EXTRACT(epoch FROM now()), ?, ?)"
          res <- liftIO $ do
              P.execute connp inq (PTriple keypar valpar valpar) 
              P.execute connp logq (PPair keypar valpar)
          addHeader "Access-Control-Allow-Origin" "*"
          addHeader "Access-Control-Allow-Methods" "*"
          status ok200
        get "/keyvalue/sqlite/get/:db/:key/" $ do
          dbpar <- param "db"
          keypar <- param "key"
          rval <- liftIO $ do
              conn <- SL.open ("static/" ++ dbpar ++ ".db")
              res <- (SL.query conn "SELECT key, val FROM keyvalue WHERE key = ?" (Only (keypar :: String))) :: IO [Pair]
              SL.close conn
              return $ (fmap val . listToMaybe) res
          addHeader "Access-Control-Allow-Origin" "*"
          addHeader "Access-Control-Allow-Methods" "*"
          case rval of
            Nothing -> status notFound404
            Just txt -> text (fromStrict txt)
        get "/keyvalue/get/:db/:key/" $ do
          key <- param "key"
          rval <- liftIO $ do
              res <- fetchData connp key
              return $ (fmap v . listToMaybe) res
          addHeader "Access-Control-Allow-Origin" "*"
          addHeader "Access-Control-Allow-Methods" "*"
          case rval of
            Nothing -> status notFound404
            Just txt -> Web.Scotty.text (Data.Text.Lazy.pack txt)
        get "/keyvalue/sqlite/logs/:db/:key/" $ do
          dbpar <- param "db"
          keypar <- param "key"
          rval <- liftIO $ do
              conn <- SL.open ("static/" ++ dbpar ++ ".db")
              res <- (SL.query conn "SELECT key, val FROM kvlog WHERE key = ?" (Only (keypar :: String))) :: IO [Pair]
              SL.close conn
              return $ fmap val res
          addHeader "Access-Control-Allow-Origin" "*"
          addHeader "Access-Control-Allow-Methods" "*"
          case rval of
            [] -> status notFound404
            xs -> text (fromStrict $ T.intercalate "|" xs)

        get "/keyvalue/logs/:db/:key/" $ do
          keypar <- param "key"
          rval <- liftIO $ do
              res <- P.query connp "SELECT key, val FROM kvlog WHERE key = ?" (Only (keypar :: String))
              return $ fmap v res
          addHeader "Access-Control-Allow-Origin" "*"
          addHeader "Access-Control-Allow-Methods" "*"
          case rval of
            [] -> status notFound404
            xs -> text (fromStrict $ T.intercalate "|" $ map T.pack xs)
        get "/keyvalue/alllogs/:db/:key/" $ do
          key <- param "key"
          rval <- liftIO $ do
              res <- allLogs connp key
              return $ fmap fieldValue res
          addHeader "Access-Control-Allow-Origin" "*"
          addHeader "Access-Control-Allow-Methods" "*"
          json rval
        get "/keyvalue/logsrange/:key/:from/:to/" $ do
          key <- param "key"
          from <- param "from"
          to <- param "to"
          rval <- liftIO $ do
              res <- logsFromTo connp key from to
              return $ fmap (\(PDateVal c_ v_) -> "[" ++ (show c_) ++ ", " ++ v_ ++ "]") res
          addHeader "Access-Control-Allow-Origin" "*"
          addHeader "Access-Control-Allow-Methods" "*"
          json rval


