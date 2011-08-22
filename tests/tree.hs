module Main where

main = print tests

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving Eq

leafTrav :: Tree a -> Tree Integer
leafTrav tr = fst (trav tr 0)

trav :: Tree a -> Integer -> (Tree Integer, Integer)
trav Empty n = (Empty, n)
trav (Node _ Empty Empty) n = (Node n Empty Empty, n + 1)
trav (Node _ lt rt) n =
    let (lr, m) = trav lt n; (rr, m') = trav rt m in
        (Node (-1) lr rr, m')

testTree1 = Node 0 (Node 0 Empty Empty) 
                   (Node 0 Empty Empty)

testTree1' = Node (-1) (Node 0 Empty Empty)
                       (Node 1 Empty Empty)

testTree2 =
    Node 0 (Node 0 (Node 0 Empty Empty)
                    Empty)
           (Node 0 (Node 0 Empty Empty)
                   (Node 0 Empty Empty))

testTree2' =
    Node (-1) (Node (-1) (Node 0 Empty Empty)
                          Empty)
              (Node (-1) (Node 1 Empty Empty)
                         (Node 2 Empty Empty))

tests = [ leafTrav testTree1  == testTree1'
        , leafTrav testTree1' == testTree1'
        , leafTrav testTree2  == testTree2'
        , leafTrav testTree2' == testTree2'
        ]
