import System.IO
import Data.List
import Prelude hiding (readList)

data Direction = N | NE| E | SE | S | SW | W | NW deriving Show
type State = [Int]
type Turn = (Direction, State)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    -- Read init information from standard input, if any
    s <- getLine
    loop (readList s)

loop :: State -> IO ()
loop state@[lx, ly, tx, ty] = do
    -- Read information from standard input
    power <- readLn :: IO Int
    
    -- Compute logic here
    let (diretion, nstate) = makeTurn state
    putStrLn (show diretion)
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write action to standard output
    
    loop nstate

readList :: (Read a) => String -> [a]
readList s = [read x| x <- xs]
        where xs = words s

makeTurn :: State -> Turn
makeTurn state = filterTurns (createTurns state)

createTurns :: State -> [ Turn ]
createTurns [lx, ly, tx, ty] = [(N, [lx, ly, tx, ty -1]),
                                (NE, [lx, ly, tx + 1, ty - 1]),
                                (E, [lx, ly, tx + 1, ty]),
                                (SE, [lx, ly, tx + 1, ty + 1]),
                                (S, [lx, ly, tx, ty + 1]),
                                (SW, [lx, ly, tx - 1, ty + 1]),
                                (W, [lx, ly, tx - 1, ty]),
                                (NW, [lx, ly, tx - 1, ty - 1])]
filterTurns :: [Turn] -> Turn
filterTurns turns = minimumBy compareTurns (filter goodTurn turns)

compareTurns :: Turn -> Turn -> Ordering
compareTurns (_, [lx, ly, txa, tya]) (_, [_, _, txb, tyb]) | a > b = GT 
                                                           | a < b = LT
                                                           | True = EQ
                                                        where a = sqrt (fromIntegral ((txa  - lx) ^2 + (tya - ly) ^ 2))
                                                              b = sqrt (fromIntegral ((txb - lx)^2 + (tyb - ly)^2))

goodTurn :: Turn -> Bool
goodTurn (_, [_, _, tx, ty]) | (tx < 0 || tx >=40) && (ty < 0 || ty >=18) = False
                             | True = True

