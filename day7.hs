module Day7 where
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

testInput :: [Int]
testInput = [16,1,2,0,4,2,7,1,2,14]

readInput :: IO [Int]
readInput = fmap (read . T.unpack) . T.split (==',') <$> TIO.readFile "input/day7.txt"

-- Okay, here's my thoughts. The fuel used is a big sum of terms of the form |x-c_a|
-- where x is the spot we're moving all the crabs too and c_a is the starting position of crab
-- a. If x is very small, then all the terms will have x < c_a and so the terms will be negative
-- before the abs. As x increases some terms will move from having x < c_a to x > c_a, until 
-- eventually all terms have x > c_a and are all positive before the abs.
--
-- Consider an individual term |x-c_a|. As x increases by 1, |x-c_a| will at first decrease by 1 
-- until x == c_a, at which point it will begin to increase by 1 instead. This function:
-- F(x) = | -1 if x < c_a
--        | 1  if x >= c_a
-- Is an increasing function equal to the first differences of our calculation, and in an imprecise
-- sense is the derivative of our function. Because our fuel calculation is a sum of these terms, the
-- first differences of our fuel calculation as a whole will also be increasing, which will lead to
-- our function looking like it has positive curvature. This means we can reasonably be sure that
-- there's a single minimum, and that we can find it by looking for the point where our calculation
-- stops decreasing and starts increasing instead.

-- This is all perhaps a bit overcomplicated for the problem, but I like to feel justified
-- when I make assumptions like "we can just look for the first time the calculation increases
-- instead of decreases"

fuelUsed :: [Int] -> Int -> Int
fuelUsed lst x = sum $ fmap (abs . (x-)) lst

part1 :: [Int] -> Int
part1 input = minimum
  where
    fuel = fmap (fuelUsed input) [0..] 
    minimum = fst $ head $ dropWhile (uncurry (>)) $ zip fuel (drop 1 fuel)

-- Part 2 changes our distance into triangular numbers, I believe the same logic holds
-- to be sure our function has a single minimum. 
-- Specifically: Now our terms are in the form of nthTriangular(|x-c_a|). Since nthTriangular is
-- increasing over positive numbers, we know that while the |x-c_a| is decreasing the size of the
-- decrease will become smaller, and while |x-c_a| is increasing the size of the increase will
-- become larger. This means that the first differences of our new term calculation should also be
-- increasing.
nthTriangular :: Int -> Int
nthTriangular n = n * (n+1) `div` 2 

fuelUsed' :: [Int] -> Int -> Int
fuelUsed' lst x = sum $ fmap (nthTriangular . abs . (x-)) lst

part2 :: [Int] -> Int
part2 input = minimum
  where
    fuel = fmap (fuelUsed' input) [0..] 
    minimum = fst $ head $ dropWhile (uncurry (>)) $ zip fuel (drop 1 fuel)

