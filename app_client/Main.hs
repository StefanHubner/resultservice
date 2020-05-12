{-# LANGUAGE OverloadedStrings #-}

module Main where

import OnlineSync (seemless)

main = do
    let computation = [1..5]
    let name = "simpleList"
    res <- seemless "http://localhost:8080" name computation :: IO [Int]
    print res 

