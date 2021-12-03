module Day2 where
import Data.List(foldl')

-- Read the input file, split it, and then parse it into commands
readInput :: IO [Command]
readInput = fmap (parseCommand . words) . lines <$> readFile "input/day2.txt"

-- A simple enum for our Commands after being parsed
-- I much prefer transforming the input into data types like these
-- over operating on strings directly.
data Command = Forward Int | Down Int | Up Int 
    deriving Show

-- Parsing is very simple through pattern matching, erroring on any invalid input
parseCommand :: [String] -> Command
parseCommand ["forward", x] = Forward (read x)
parseCommand ["down", x] = Down (read x)
parseCommand ["up", x] = Up (read x)
parseCommand e = error $ "Invalid command: " ++ show e

-- Move takes a Command and a Position as a tuple, and then
-- returns a new position according to the requirements of part 1
move :: Command -> (Int, Int) -> (Int, Int)
move (Forward d) (x, y) = (x+d, y)
move (Down d) (x, y) = (x, y+d)
move (Up d) (x, y) = (x, y-d)

-- Since the `move` function for part 1 is associative, it doesn't matter
-- whether we use `foldr` or `foldl`
part1 :: [Command] -> Int
part1 cmds = let (x, y) = foldr move (0,0) cmds in
    x*y

-- The addition of the 'aim' parameter means we no longer have an associative
-- function. As such the parameters have been rearranged to nicely match the 
-- required function type for foldl
move2 :: (Int, Int, Int) -> Command -> (Int, Int, Int)
move2 (x, y, a) (Forward d) = (x+d, y+d*a, a)
move2 (x, y, a) (Down d) = (x, y, a+d)
move2 (x, y, a) (Up d) = (x, y, a-d)

part2 :: [Command] -> Int
part2 cmds = let (x, y, _) = foldl' move2 (0,0,0) cmds in
    x*y

day2 :: IO ()
day2 = do
    input <- readInput
    putStrLn $ "Part 1 solution: " ++ show (part1 input)
    putStrLn $ "Part 2 solution: " ++ show (part2 input)