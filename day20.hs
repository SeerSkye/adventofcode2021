module Day20 where
import Data.Array
import Control.Arrow((&&&))

lightToBool :: Char -> Bool
lightToBool '#' = True
lightToBool  _  = False

readInput :: IO (Array Int Bool, Array (Int,Int) Bool)
readInput = (makeRules . head &&& makeBoard . drop 2) . lines <$> readFile "input/day20.txt"

makeRules :: String -> Array Int Bool
makeRules = listArray (0,511) . fmap lightToBool

-- Note that y is our first index! this is because doing ranges of int pairs gives them
-- in column major ordering, but we want them row-major for doing the bitstring conversion
makeBoard :: [String] -> Array (Int,Int) Bool
makeBoard lst = array ((0,0), (length (head lst) - 1, length lst - 1)) $
    concatMap (\(row, y) -> (\(val, x) -> ((y,x), lightToBool val)) <$> zip row [0..] ) $ zip lst [0..]

-- Been getting a lot of use out of this so far this year, huh
bitstringToInt :: [Bool] -> Int
bitstringToInt = foldr (\(a, x) acc -> acc + fromEnum x * 2^a) 0 . zip [0..] . reverse 

getNextLight
    :: Array Int Bool -- the rules of the cellular automata
    -> Array (Int, Int) Bool -- the current board state
    -> Bool -- The state for out-of-bounds cells
    -> (Int, Int) -- The cell we're checking
    -> Bool -- whether the cell is on for the next cycle
getNextLight rules board defaultCell (y,x) = 
    rules ! bitstringToInt (getCell <$> range ((y-1,x-1),(y+1,x+1)))
  where
    getCell p = if inRange (bounds board) p then board!p else defaultCell

expandBounds :: ((Int, Int),(Int, Int)) -> ((Int, Int),(Int,Int))
expandBounds ((ymin, xmin),(ymax, xmax)) = ((ymin-1, xmin-1),(ymax+1,xmax+1))

stepCells
    :: Array Int Bool -- the rules of the automata
    -> Array (Int, Int) Bool -- the initial boardstate
    -> Bool -- the state for out-of-bounds cells
    -> (Bool, Array (Int,Int) Bool) -- the new out of bounds cell state and board
stepCells rules board defaultCell = (nextDefaultCell, nextBoard)
  where
    nextDefaultCell = if defaultCell then rules!511 else rules!0
    newBounds = expandBounds $ bounds board
    nextBoard = listArray newBounds $ getNextLight rules board defaultCell <$> range newBounds

countCells :: Array (Int,Int) Bool -> Int
countCells = foldr (\x acc -> if x then acc+1 else acc) 0

runGenerations
    :: Int -- the number of generation to run
    -> Array Int Bool -- the rules
    -> Array (Int, Int) Bool -- the initial board
    -> Bool -- the out of bounds cell state
    -> (Bool, Array (Int,Int) Bool) -- the ending state of the game
runGenerations n rules board defaultCell 
    | n <= 0    = (defaultCell, board)
    | otherwise = let (newDefaultCell, newBoard) = stepCells rules board defaultCell in
        runGenerations (n-1) rules newBoard newDefaultCell

part1 :: (Array Int Bool, Array (Int,Int) Bool) -> Int
part1 (rules, board) = countCells $ snd $ runGenerations 2 rules board False

part2 :: (Array Int Bool, Array (Int,Int) Bool) -> Int
part2 (rules, board) = countCells $ snd $ runGenerations 50 rules board False
