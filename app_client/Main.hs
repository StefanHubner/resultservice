{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Binary (decode, encode)
import qualified Data.ByteString.Lazy as B
import OnlineSync

main :: IO()
main = do
    let computation = [1..20] :: [Integer]
    let name = "testChunked"
    let con = Connection "stefan.hubner.info" 8080
    res <- seemless con name computation :: IO [Integer]
    print res
    complex <- tryResult con "liss.private.mip.het" :: IO (Maybe [Result])
    print complex
    alH <- tryResult con "smHmipTrue" :: IO (Maybe ([(Int, Int, Double)], Int))
    print $ (head . fst) <$> alH
    --convertOnce


-- one off, commit all local files to server (should not be here)
type Result = (([String], [Double]), (Double, [[Int]]))
type OldResult = ([String], (Double, [[Int]]))

parseResult :: OldResult -> Result
parseResult (ys, r) = ((ys, []), r)

convertOnce :: IO ()
convertOnce = do
    let con = Connection "stefan.hubner.info" 8080
    let fsnsn = [ ("results.liss.ec.private/results.300.het.50avg.bin",    "liss.ec.private.nomip.het.50avg")
                , ("results.liss.ec.private/results.300.het.bin",          "liss.ec.private.nomip.het")
                , ("results.liss.ec.private/results.300.het.nologs.bin",   "liss.ec.private.nomip.het.nologs")
                , ("results.liss.ec.private/results.300.het.withlogs.bin", "liss.ec.private.nomip.het.withlogs")
                , ("results.liss.ec.public/results.300.het.bin",           "liss.ec.public.nomip.het")
                , ("results.liss.ec.public/results.300.het.50avg.bin",     "liss.ec.public.nomip.het.50avg")
                , ("results.liss.ec.public/results.300.het.avg100.bin",    "liss.ec.public.nomip.het.100avg")
                , ("results.liss.private/results.300.het.nomip.bin",       "liss.private.nomip.het")
                , ("results.liss.public/results.300.het.bin",              "liss.public.nomip.het")
                , ("results.private/results.300.het.bin",                  "russian.private.nomip.het")
                , ("results.public/results.300.het.bin",                   "russian.public.nomip.het")]
    let fsnso = [ ("results.liss.nohet.private/rolling.300.bin",           "liss.private.nomip.nohet")
                , ("results.liss.nohet.public/rolling.300.bin",            "liss.public.nomip.nohet")
                , ("results.nohet.private/rolling.300.bin",                "russian.private.nomip.nohet")
                , ("results.nohet.public/rolling.300.bin",                 "russian.public.nomip.nohet")
                , ("results.spanish.private/rolling.300.bin",              "spanish.private.nomip.nohet")
                , ("results.spanish.public/rolling.300.bin",               "spanish.public.nomip.nohet")]
    mapM_ (uncurry $ postOnce con) fsnsn


postOnce :: Connection -> String -> String -> IO()
postOnce con fn name = do
    let p = "/home/hubner/Dropbox/Academia/projects/scarp/scarp/utilities/"
    encoded <- B.readFile (p++fn)
    let decoded = decode encoded :: [Result] -- Result vs OldResult
    postResults con name (encode . map id $ decoded) -- id vs parseResult
    test <- tryResult con name :: IO (Maybe [Result])
    print test
    return ()
