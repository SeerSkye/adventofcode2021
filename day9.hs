module Day9 where
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict((!))
import qualified Data.HashSet as HS
import Data.Maybe (mapMaybe)
import Data.List(sort)

readInput :: IO (H.HashMap (Int, Int) Int)
readInput = toHashMap . fmap (fmap (read . return)) . lines <$> readFile "input/day9.txt"

-- Transform our 2-d list into a hashmap with coordinates as keys.
-- Haskell lists are singly linked lists, which are bad for this sort of problem
-- and while haskell has arrays, they can be a bit of a pain to work with
-- compared to just using a hash map
toHashMap :: [[Int]] -> H.HashMap (Int, Int) Int
toHashMap lst = H.fromList $
    concatMap (\(row, y) -> 
        (\(val, x) -> ((x,y), val)) <$> zip row [0..] ) $ zip lst [0..]

-- Create a list of adjacent locations
getAdjacentKeys :: (Int, Int) -> [(Int, Int)]
getAdjacentKeys (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

-- Get the list of the values of all adjacent locations in the map
getAdjacents :: H.HashMap (Int, Int) Int -> (Int, Int) -> [Int]
getAdjacents map k = mapMaybe (`H.lookup` map) (getAdjacentKeys k)

-- Simply fold over the map, accumulating the risk value whenever all the adjacent values in
-- the map are less than the current one
part1 :: H.HashMap (Int, Int) Int -> Int 
part1 map = H.foldrWithKey (\k v a -> if all (>v) $ getAdjacents map k then a+v+1 else a) 0 map

-- helper function to call our loop with initial parameters
basinSize :: H.HashMap (Int, Int) Int -> (Int, Int) -> Int
basinSize map start = basinSize' map HS.empty HS.empty [start]

-- Our loop for part 2. We take a map, a set of points we've confirmed are in the basin,
-- a set of points we've checked, and a stack for the points we still need to check
basinSize' :: H.HashMap (Int, Int) Int 
          -> HS.HashSet (Int, Int) 
          -> HS.HashSet (Int, Int) 
          -> [(Int, Int)]
          -> Int
-- if we have no more points to check we're done: simply return the size of the
-- basin point set
basinSize' _ found _ [] = HS.size found
-- else we check the top of the stack
basinSize' map found seen (k:ks) = basinSize' map nextFound (HS.insert k seen) nextKeys
  where
    -- At first I did something far more complicated, but it turns out that the problem is
    -- actually just looking for connected areas with no 9s. findWithDefault here means that
    -- currKeyInBasin will also be false if our key is ourside the map
    currKeyInBasin = H.findWithDefault 9 k map /= 9
    -- if the key we're checking is in the basin, add it to the `found` set
    nextFound = if currKeyInBasin then HS.insert k found else found
    -- if the key we're checking is in the basin, then add any adjacent keys that we haven't
    -- already checked to the stack
    nextKeys = if currKeyInBasin 
        then filter (not . (`HS.member` seen)) (getAdjacentKeys k) ++ ks 
            else ks

-- Similar to part 1's solution, except instead of accumulating the values into an integer,
-- we accumulate the keys into a list
makeBasinList :: H.HashMap (Int, Int) Int -> [(Int, Int)] 
makeBasinList map = H.foldrWithKey (\k v a -> if all (>v) $ getAdjacents map k then k:a else a) [] map

part2 :: H.HashMap (Int, Int) Int -> Int 
part2 map = product $ take 3 $ reverse $ sort $ basinSize map <$> makeBasinList map
