main = putStr (quine q)
quine s = s ++ show s
q = "main = putStr (quine q)\nquine s = s ++ show s\nq = "