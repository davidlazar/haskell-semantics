-- Solution to Project Euler Problem #1
module Main where

main = print $ sum [ x | x <- [1..999], gcd x 15 > 1 ]
