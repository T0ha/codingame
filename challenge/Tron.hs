import System.IO
import Data.Array
import Data.List
import Prelude hiding ( Either(Left, Right))

data Direction = Left | Right | Up | Down deriving Show
type Position = (Point, Point)
data Point  = Point Int Int deriving Show
type Turn = (Direction, Point)
type State = Array (Int, Int) Int

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    -- Read init information from standard input, if any
    loop (array ((0, 0), (29, 19)) [ ((x, y), -1)  | x <- [0..29], y <- [0..19]])

loop :: State -> IO ()
loop state = do
    -- Read information from standard input
    line <- getLine
    let 
        [ n, m ] = readWords line :: [Int]
    pos <- getPositions n
    let my@[_, _, x, y] = pos !! m
    let state' = newState pos state
    -- hPutStrLn stderr (show  pos)
    makeDesision (maximumBy (compareTurns state') (legalTurns state' (Point x y)))
    -- decide n m pos state

    -- Compute logic here
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write action to standard output
    
    loop state'

getPositions :: Int ->  IO [[Int]]
getPositions n = sequence [getLine>>= \s ->  return (readWords s) | _ <- [ 1..n ]]
                    
readWords :: (Read a) => String -> [a]
readWords ln = [read i | i <- lns]
        where lns = words ln

legalTurns :: State -> Point -> [Turn]
legalTurns state pt = filterTurns (generateTurns pt) state

newState :: [[Int]] -> State -> State
newState  pos state = state' // [((x,y), -1) | (i, [_, _, x, _]) <- pos', x == -1, ((x,y), i') <- (assocs state'), i == i']
        where pos' = zip [0..(length pos) - 1] pos
              state' = state // [((x, y), i) | (i, [_, _, x, y]) <- pos', x /= -1, y /= -1]

generateTurns :: Point -> [Turn]
generateTurns (Point x y) = [(Left, Point ( x - 1 ) y),
                          (Right, Point ( x + 1 ) y),
                          (Up, Point x ( y - 1 )),
                          (Down, Point x ( y + 1 ))]

filterTurns :: [Turn] -> State -> [Turn]
filterTurns [] _ = []
filterTurns (t:ts) state | (x < 0 || y < 0 || x >= 30 || y >= 20)  = filterTurns ts state
                         | ( state ! (x, y) /= -1 ) = filterTurns ts state
                         | True = t:(filterTurns ts state)
                        where (_, Point x y) = t
          
compareTurns :: State -> Turn -> Turn -> Ordering
compareTurns state (_, pa@(Point x1 y1)) (_, pb@(Point x2 y2)) = compare da db
    where da = recLT state' pa 1
          db = recLT state' pb 1
          state' = newState [[0,0,x1,y1], [0,0,x2,y2]] state

recLT :: State -> Point -> Int -> Int
recLT state pt@(Point x y) i = foldr (countTurns state') (i + length turns) turns
          where state' = newState [[0,0,x,y]] state
                turns = legalTurns state' pt

countTurns :: State -> Turn -> Int -> Int
countTurns state (_, pt) i | i > 500 = 501 
                           | True = recLT state pt i

makeDesision :: Turn -> IO ()
makeDesision (Left, _)  = putStrLn "LEFT"
makeDesision (Right, _)  = putStrLn "RIGHT"
makeDesision (Up, _)  = putStrLn "UP"
makeDesision (Down, _)  = putStrLn "DOWN"
module Tron where 
