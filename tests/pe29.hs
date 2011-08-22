-- Solution to Project Euler Problem #29
module Main where

import Data.List (group, sort)

main = print . length . group . sort $ [a^b | a <- [2..100], b <- [2..100]]
