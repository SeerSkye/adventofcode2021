module Day1 where

-- Read the input, split into lines, and parse each line into an Int
readInput :: IO [Int]
readInput = fmap read . lines <$> readFile "input/day1.txt"

-- A nice way to analyze a list by adjacent pairs is to 
-- simply zip the list with the tail of the list. 
-- `zip depths (tail depths)` can be written point-free as
-- `zip <*> tail`, but I have avoided doing so for clarity 
part1 :: [Int] -> Int 
part1 depths = length $ filter (uncurry (<)) $ zip depths (tail depths)

-- The same zipping approach as above works for larger windows too. Since
-- All we are doing is summing the three elements using zipWith is a nice
-- convenience
get3WindowSums :: [Int] -> [Int]
get3WindowSums lst = zipWith3 (\a b c -> a + b + c) lst (tail lst) (tail $ tail lst)

-- After creating the list of 3-wide window sums the rest of the problem is
-- identical to part 1
part2 :: [Int] -> Int
part2 = part1 . get3WindowSums

day1 :: IO ()
day1 = do
    input <- readInput
    putStrLn $ "Part 1 solution: " ++ show (part1 input)
    putStrLn $ "Part 2 solution: " ++ show (part2 input)

testInput :: [Int]
testInput = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
