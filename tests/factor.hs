module Main where

main = mapM_ (print . factors) [1, 2, 2^17, 182839, 8059771]

-- The code below was probably copied from:
-- http://www.polyomino.f2s.com/david/haskell/codeindex.html

divides d n = rem n d == 0

ld n = ldf 2 n

ldf k n
    | divides k n = k
    | k ^ 2 > n   = n
    | otherwise   = ldf (k+1) n

factors n   
    | n <  1 = []
    | n == 1 = []
    | otherwise = p : factors (div n p) where p = ld n
