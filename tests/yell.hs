module Main where

import Data.Char (toUpper)

main = yell "yell.hs"

yell file = do
    s <- readFile file
    let loud = map toUpper s
    putStr loud
