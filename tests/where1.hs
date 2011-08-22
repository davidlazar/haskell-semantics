module Main where

main = print $ foo 42 [(4,8), (15,16), (23,42), (3,14), (2,71), (11,8)]
  where
    foo n ps = product [j, n, x + y, bar] + sum rest' 
        where (a : b : rest) = ps
              (j, y) = b
              (x, _) = a
              bar = (qux n)^2
                  where tmp = 3
                        qux t = tmp^t
              rest' = drop 3 fsts
                  where fsts = map fst rest
