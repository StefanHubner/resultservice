{-# LANGUAGE OverloadedStrings #-}

module Main where

import OnlineSync

main = do
    let computation = [1..5]
    let name = "simpleList"
    res <- seemless (Connection "stefan.hubner.info" 8080) name computation :: IO [Int]
    print res 

