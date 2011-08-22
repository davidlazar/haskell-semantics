module Main where

import Data.List

main = print (foo "hello world")

foo str = rest
    where (top, rest) = splitAt 3 str
