module Main where

main :: IO ()
main = print . take 10 . map fac $ [0..]

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)
