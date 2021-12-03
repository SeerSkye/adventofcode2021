module Day3 where

-- Input reading code
readInput :: IO [[Int]]
readInput = fmap toBinaryList . lines <$> readFile "input/day3.txt"

toBinaryList :: String -> [Int]
toBinaryList = fmap (read . return)

-- Get the gamma rate by summing up our 'bit'-strings and then testing whether
-- the number is greater than half the number of codes. This takes advantage
-- of using a [Int] representation for our input
gammaRate :: [[Int]] -> [Int]
gammaRate codes = normalize $ foldr (zipWith (+)) (replicate codeLength 0) codes
  where
    numCodes = length codes
    codeLength = length $ head codes
    -- I know there's a way to do this comparison with solely integer math but
    -- converting to doubles is just so convenient
    normalize = fmap (\x -> if fromIntegral x >= fromIntegral numCodes / 2 then 1 else 0)

-- The epsilon rate is always just bitwise inverted from the gamma rate
-- (1-) is just a cheeky way of turning 0 to 1 and vice versa
epsilonFromGamma :: [Int] -> [Int]
epsilonFromGamma = fmap (1-)

-- fairly straight-forward conversion
bitstringToInt :: [Int] -> Int
bitstringToInt = foldr (\(a, x) acc -> acc + x*2^a) 0 . zip [0..] . reverse 

part1 :: [[Int]] -> Int 
part1 codes = bitstringToInt gamma * bitstringToInt (epsilonFromGamma gamma)
  where
    gamma = gammaRate codes

-- This will crash if `bit` is too high, as we attempt to index the lists out of bounds!
-- Fortunately, because the solution exists we will avoid reaching that point.
o2RatingStep :: (Int, [[Int]]) -> (Int, [[Int]])
o2RatingStep (bit, codes) = (bit+1, newCodes)
  where
    newCodes = filter (\c -> (c !! bit) == (gamma !! bit)) codes
    gamma = gammaRate codes

o2Rating :: [[Int]] -> Int
o2Rating = bitstringToInt 
    . head 
    . snd 
    . until (\(_, cs) -> length cs == 1) o2RatingStep
    . (,) 0

-- The CO2 Rating is the same as above, but with an `epsilonFromGamma` call thrown in
co2RatingStep :: (Int, [[Int]]) -> (Int, [[Int]])
co2RatingStep (bit, codes) = (bit+1, newCodes)
  where
    newCodes = filter (\c -> c !! bit == epsilon !! bit) codes
    epsilon = epsilonFromGamma $ gammaRate codes

co2Rating :: [[Int]] -> Int
co2Rating = bitstringToInt 
    . head 
    . snd 
    . until (\(_, cs) -> length cs == 1) co2RatingStep
    . (,) 0

part2 :: [[Int]] -> Int 
part2 codes = o2Rating codes * co2Rating codes

day3 :: IO ()
day3 = do
    input <- readInput
    putStrLn $ "Part 1 solution: " ++ show (part1 input)
    putStrLn $ "Part 2 solution: " ++ show (part2 input)

-- The example from the problem description
testInput :: [[Int]]
testInput = 
    [ [0,0,1,0,0]
    , [1,1,1,1,0]
    , [1,0,1,1,0]
    , [1,0,1,1,1]
    , [1,0,1,0,1]
    , [0,1,1,1,1]
    , [0,0,1,1,1]
    , [1,1,1,0,0]
    , [1,0,0,0,0]
    , [1,1,0,0,1]
    , [0,0,0,1,0]
    , [0,1,0,1,0]
    ]
