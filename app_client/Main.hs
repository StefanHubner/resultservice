{-# LANGUAGE OverloadedStrings #-}

module Main where

import OnlineSync

main = do
    let computation = [1..10]
    let name = "simpleListCompressed"
    res <- seemless (Connection "stefan.hubner.info" 8080) name computation :: IO [Int]
    print res 

