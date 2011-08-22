module Main where

main :: IO ()
main = mapM_ (print . uncurry (clunky somemap)) pairs
    where somemap = zip [1..10] [100, 99..]
          pairs = [(1,10), (4,5), (10,10), (3,20), (0,2)]

-- | @clunky env var1 var2@ returns the sum of the values mapped to @var1@
-- and @var2@ in @env@. If @var1@ or @var2@ is not mapped in @env@, returns
-- @var1 + var2@.
--
-- >>> clunky [(1,4),(2,8),(3,15),(4,16)] 1 4
-- 20
-- >>> clunky [(1,4),(2,8),(3,15),(4,16)] 1 5
-- 6
--
clunky :: [(Int, Int)] -> Int -> Int -> Int
clunky env var1 var2 = clunky' (lookup var1 env) (lookup var2 env)
    where clunky' (Just val1) (Just val2) = val1 + val2
          clunky' _ _ = var1 + var2
