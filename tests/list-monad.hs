module Main where

import Control.Monad

main :: IO ()
main = mapM_ (print . ($ str)) [permutations, powerset, inits, tails]
    where str = "abcde"

-- The beauty of nondeterminism via the list monad.
permutations = sortByM (\_ _ -> [False, True])
powerset = filterM (const [False, True])
inits = takeWhileM (const [False, True])
tails = dropWhileM (const [False, True])


-- We define some additional monadic varieties which are not included
-- in the standard libraries.

dropWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
dropWhileM _ [] = return []
dropWhileM p xs@(x:xs') = do
    flg <- p x
    let ys = dropWhileM p xs'
    if flg then ys else return xs

takeWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
takeWhileM _ [] = return []
takeWhileM p xs@(x:xs') = do
    flg <- p x
    let ys = takeWhileM p xs'
    if flg then liftM (x:) ys else return []

-- Reduced from Ordering to Bool. cmp is "less than"
insertByM :: (Monad m) => (a -> a -> m Bool) -> a -> [a] -> m [a]
insertByM _ x [] = return [x]
insertByM cmp x ys@(y:ys') = do
    flg <- cmp x y
    let yss = insertByM cmp x ys'
    if flg then return (x:ys) else liftM (y:) yss

sortByM :: (Monad m) => (a -> a -> m Bool) -> [a] -> m [a]
sortByM cmp = foldM (flip $ insertByM cmp) []
