module Temp where
import Data.List

main :: IO ()
main = do
    n <- readLn :: IO Int
    ts <- getLine
    putStr (show (minimumBy compareTemp (getTemps ts)))

getTemps ::  String -> [Int]
getTemps ts | length ts == 0 = [0]
            | True = [read x | x <- xs]
           where xs = words ts

compareTemp :: Int -> Int -> Ordering
compareTemp a b | abs a > abs b = GT
                | abs a < abs b = LT
                | abs a == abs b && a > b = LT
                | True = GT
