module Main where

main = print (foo "hello world");

foo
 = \ x0 ->
    case x0 of
    { str -> rest
        where
        { (top, rest) = splitAt (fromInteger 3) str}}
