-- Solution to Project Euler Problem #16
module Main where

import Data.Char (digitToInt)

main = print (sumOfDigits (2^1000))

sumOfDigits = sum . map digitToInt . show
