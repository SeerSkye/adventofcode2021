{-# LANGUAGE OverloadedStrings #-}
module Day6 where
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.IntMap.Strict as M

testInput :: [Int]
testInput = [3,4,3,1,2]

readInput :: IO [Int]
readInput = fmap (read . T.unpack) . T.split (==',') <$> TIO.readFile "input/day6.txt"

makeFishMap :: [Int] -> M.IntMap Integer
makeFishMap lst = M.fromListWith (+) $ zip lst (repeat 1)

fishStep :: M.IntMap Integer -> M.IntMap Integer
fishStep map = 
    M.delete (-1) $ M.mapKeys pred $ M.insertWith (+) 7 numSpawn $ M.insertWith (+) 9 numSpawn map
  where
    numSpawn = M.findWithDefault 0 0 map

-- Apply a function n times. Copied from GCH.Utils.Misc
nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f

countFish :: M.IntMap Integer -> Integer
countFish = M.foldr (+) 0 

part1 :: [Int] -> Integer 
part1 = countFish . nTimes 80 fishStep . makeFishMap

part2 :: [Int] -> Integer 
part2 = countFish . nTimes 256 fishStep . makeFishMap