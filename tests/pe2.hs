-- Solution to Project Euler Problem #2
module Main where

main = print . sum . takeWhile (< 4000000) . filter even $ fibs

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
