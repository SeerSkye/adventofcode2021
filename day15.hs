module Day15 where

import Data.Bifunctor (bimap)
import Data.Ix
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as OrdPSQ

-- Oh boy it's the annual "implement A*" advent of code problem

readInput :: IO (Map (Int, Int) Int)
readInput = toMap . fmap (fmap (read . return)) . lines <$> readFile "input/day15.txt"

toMap :: [[Int]] -> Map (Int, Int) Int
toMap lst =
  Map.fromList $
    concatMap (\(row, y) -> (\(val, x) -> ((x, y), val)) <$> zip row [0 ..]) $ zip lst [0 ..]

-- we'll use simple manhatten distance as our heuristic
manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

getAdjacentKeys :: (Int, Int) -> [(Int, Int)]
getAdjacentKeys (x, y) = [(x + 1, y), (x -1, y), (x, y + 1), (x, y -1)]

aStar :: Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Int
aStar grid start end = case Map.lookup start grid of
  Just _ -> aStar' grid end (Map.singleton start 0) $ OrdPSQ.singleton start (manhattanDistance start end) 0
  Nothing -> error "start node not in grid?"

-- Ugh I took forever getting this right and it isn't even particularly fast
-- but whatever, it works
aStar' ::
  Map (Int, Int) Int -> -- The grid we're pathfinding on
  (Int, Int) -> -- The end coordinate
  Map (Int, Int) Int -> -- A grid of our best routes towards any given location
  OrdPSQ (Int, Int) Int Int -> -- A priority queue of locations to check next
  Int -- Our total pathing cost
aStar' grid end currBests toCheck =
  let (loc, _, cost, queue) = fromMaybe (error "ran out of nodes to check?") (OrdPSQ.minView toCheck)
      neighbors = map (\k -> (k, grid ! k + cost)) $ filter (`Map.member` grid) $ getAdjacentKeys loc
      insertIfBest (loc, cost) bests =
        if cost < Map.findWithDefault maxBound loc currBests
          then Map.insert loc cost bests
          else bests
      newBests = foldr insertIfBest currBests neighbors
      queueIfBest (loc, cost) queue =
        if cost < Map.findWithDefault maxBound loc currBests
          then OrdPSQ.insert loc (cost + manhattanDistance loc end) cost queue
          else queue
      newQueue = foldr queueIfBest queue neighbors
   in if loc == end then cost else aStar' grid end newBests newQueue

part1 :: Map (Int, Int) Int -> Int
part1 grid = aStar grid (fst $ Map.findMin grid) (fst $ Map.findMax grid)

-- Just make our grid bigger for part 2.
makePart2Grid :: Map (Int, Int) Int -> Map (Int, Int) Int
makePart2Grid grid = Map.unions $ fmap duplicateGrid (range ((0, 0), (4, 4)))
  where
    ((maxX, maxY), _) = Map.findMax grid
    duplicateGrid :: (Int, Int) -> Map (Int, Int) Int
    duplicateGrid (x, y) =
      Map.mapKeys (\(origX, origY) -> ((maxX + 1) * x + origX, (maxY + 1) * y + origY)) $
        Map.map (\v -> (v + x + y -1) `mod` 9 + 1) grid

part2 :: Map (Int, Int) Int -> Int
part2 grid =
  let p2grid = makePart2Grid grid
   in aStar p2grid (fst $ Map.findMin p2grid) (fst $ Map.findMax p2grid)