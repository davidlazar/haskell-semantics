module Main where

main :: IO ()
main = print (collatz 27)

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n
    | even n    = n : collatz (n `div` 2)
    | otherwise = n : collatz (3 * n + 1)
