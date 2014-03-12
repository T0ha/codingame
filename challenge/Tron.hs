module Tron where 
import System.IO
import Data.Array
import Data.List
import Prelude hiding ( Either(Left, Right))

data Direction = Left | Right | Up | Down deriving Show
type Position = (Point, Point)
data Point  = Point Int Int deriving Show
type Turn = (Direction, Point)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    -- Read init information from standard input, if any
    loop (array ((0, 0), (29, 19)) [ ((x, y), -1)  | x <- [0..29], y <- [0..19]])

loop :: Array (Int, Int) Int -> IO ()
loop state = do
    -- Read information from standard input
    line <- getLine
    let 
        [ n, m ] = readWords line :: [Int]
    pos <- getPositions n
    let [_, _, x, y] = pos !! m
    -- hPutStrLn stderr (show  pos)
    makeDesision (maximumBy compareTurns (filterTurns (generateTurns (Point x y)) state))
    -- decide n m pos state

    -- Compute logic here
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write action to standard output
    
    loop (newState pos state)

getPositions :: Int ->  IO [[Int]]
getPositions n = sequence [getLine>>= \s ->  return (readWords s) | _ <- [ 1..n ]]
                    
readWords :: (Read a) => String -> [a]
readWords ln = [read i | i <- lns]
        where lns = words ln

newState :: [[Int]] -> Array (Int, Int) Int -> Array (Int, Int) Int
newState  pos state = state // [((x, y), i) | (i, [_, _, x, y]) <- pos', x /= -1, y /= -1] 
        where pos' = zip [0..(length pos) - 1] pos

generateTurns :: Point -> [Turn]
generateTurns (Point x y) = [(Left, Point ( x - 1 ) y),
                          (Right, Point ( x + 1 ) y),
                          (Up, Point x ( y - 1 )),
                          (Down, Point x ( y + 1 ))]

filterTurns :: [Turn] -> Array (Int, Int) Int -> [Turn]
filterTurns [] _ = []
filterTurns (t:ts) state | (x < 0 || y < 0 || x >= 29 || y >= 19)  = filterTurns ts state
                         | ( state ! (x, y) /= -1 ) = filterTurns ts state
                         | True = t:(filterTurns ts state)
                        where (_, Point x y) = t
          
compareTurns :: Turn -> Turn -> Ordering
compareTurns (_, (Point x1 y1)) (_, (Point x2 y2)) 
    | abs (15 - x1) > abs (15 - x2) &&  abs (10 - y1) > abs (10 - y2) = LT
    | abs (15 - x1) < abs (15 - x2) &&  abs (10 - y1) < abs (10 - y2) = GT
    | abs (15 - x1) > abs (15 - x2) &&  abs (10 - y1) <= abs (10 - y2) = LT
    | abs (15 - x1) <= abs (15 - x2) &&  abs (10 - y1) > abs (10 - y2) = GT
    | True = GT
makeDesision :: Turn -> IO ()
makeDesision (Left, _)  = putStrLn "LEFT"
makeDesision (Right, _)  = putStrLn "RIGHT"
makeDesision (Up, _)  = putStrLn "UP"
makeDesision (Down, _)  = putStrLn "DOWN"
