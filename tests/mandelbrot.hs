module Main where

import Data.Complex
import Data.Char

orbit :: (Num a) => a -> [a]
orbit c = iterate (\z -> z^2 + c) 0

mandelbrot :: (RealFloat a) => Int -> Complex a -> Int
mandelbrot iters = length . takeWhile ((<= 2) . magnitude) . take iters . orbit

mandelbrotCanvas :: (RealFloat a, Enum a) => Int -> a -> a -> a -> [String]
mandelbrotCanvas iters x y r =
    [[ if mandelbrot iters (x :+ y) == iters then '*' else ' '
        | x <- [-r, -r + 2 * r / x .. r]]
        | y <- [-r, -r + 2 * r / y .. r]]

printMandelbrot :: (RealFloat a, Enum a) => Int -> a -> a -> a -> IO ()
printMandelbrot iters x y r =
    mapM_ putStrLn . filter (not . all isSpace) $ mandelbrotCanvas iters x y r

main :: IO ()
main = printMandelbrot 256 100 64 2
