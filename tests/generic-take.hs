module Main where

main = print $ genericTake 5 [100, 99..]

-- genericTake from Data.List

genericTake             :: (Integral a) => a -> [b] -> [b]
genericTake _ []        =  []
genericTake 0 _         =  []
genericTake n (x:xs) 
   | n > 0              =  x : genericTake (n-1) xs
   | otherwise          =  error "List.genericTake: negative argument"
