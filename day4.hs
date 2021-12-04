module Day4 where
import System.IO
    ( hIsEOF, hGetLine, withFile, Handle, IOMode(ReadMode) )
import Control.Monad ( replicateM )
import Data.List ( (\\), inits, transpose, find )
import Data.Maybe ( mapMaybe )

-- splitOn is taken from stack overflow, cause I didn't feel like importing 
-- something for it
splitOn :: Char -> String -> [String]
splitOn c s = 
    case break (==c) s of
        (a, _:r) -> a : splitOn c r
        (a, "") -> [a]

newtype Board = Board [[Int]]
    deriving (Show, Eq)

readInput :: IO ([Int], [Board])
readInput = withFile "input/day4.txt" ReadMode $ \h -> do
    numbers <- fmap read . splitOn ','<$> hGetLine h :: IO [Int]
    boards <- readBoards h
    return (numbers, boards)

readBoards :: Handle -> IO [Board]
readBoards h = readBoardsGo h [] 
  where
    readBoardsGo :: Handle -> [Board] -> IO [Board]
    readBoardsGo h boards = do
        isEOF <- hIsEOF h
        if isEOF then return boards
            else do
                hGetLine h -- empty line
                nextBoard <- Board <$> replicateM 5 (fmap read . words <$> hGetLine h)
                readBoardsGo h (nextBoard : boards)

checkBoard :: [Int] -> Board -> Bool 
checkBoard calledNums (Board board) = 
    any (all (`elem` calledNums)) board
    || any (all (`elem` calledNums)) (transpose board)

calculateScore :: [Int] -> Board -> Int
calculateScore calledNums (Board board) =
    sum (concat board \\ calledNums) * last calledNums

part1 :: ([Int], [Board]) -> Int
part1 (numOrder, boards) =
    head $ mapMaybe (\nums -> calculateScore nums <$> find (checkBoard nums) boards) (inits numOrder)

-- This whole part 2 is very inefficient, but was quick to write
part2step :: ([Int], [Board]) -> ([Int], [Board])
part2step (numOrder, boards) = 
    (numOrder, boards \\ take 1 (mapMaybe (\nums -> find (checkBoard nums) boards) (inits numOrder)))

part2 :: ([Int], [Board]) -> Int
part2 input = let finalState = until (\(_, bs) -> length bs == 1) part2step input in
    part1 finalState