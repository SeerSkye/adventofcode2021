{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Day5 where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text
import Data.Either(fromRight)
import Control.Applicative ((<|>))
import qualified Data.Map.Strict as M

-- Today is a really good day to use a Map, and unfortunately `base` doesn't provide
-- a standard Map type, so it will have to be grabbed from the `containers` library.
-- Since I'm already bringing in additional libraries I have also decided to bring in
-- an actual parsing library and better text data type as well.

data LineSegment = LineSegment {
    startPoint :: (Int, Int),
    endPoint :: (Int, Int)
} deriving Show

-- Lets actually do proper parsing this time around now that we've imported Attoparsec
pair :: Parser (Int, Int)
pair = (,) <$> decimal <*> (char ',' *> decimal)

lineSegment :: Parser LineSegment
lineSegment = 
    LineSegment <$> pair <*> (" -> " *> pair <* (endOfLine <|> endOfInput))

readInput :: IO [LineSegment]
readInput = fromRight (error "parse failure") . parseOnly (many1 lineSegment) <$> T.readFile "input/day5.txt"

getPoints :: LineSegment -> [(Int, Int)]
getPoints (LineSegment (x1, y1) (x2, y2))
    | x1 == x2 = fmap (x1,) [min y1 y2..max y1 y2]
    | y1 == y2 = fmap (,y1) [min x1 x2..max x1 x2]
    | otherwise = [] -- not described in part 1

makeMap :: [LineSegment] -> M.Map (Int, Int) Int
makeMap lines = M.fromListWith (+) $ zip (concatMap getPoints lines) (repeat 1)

countIntersections :: M.Map (Int, Int) Int -> Int
countIntersections = M.size . M.filter (/= 1)

part1 :: [LineSegment] -> Int 
part1 = countIntersections . makeMap

getPoints' :: LineSegment -> [(Int, Int)]
getPoints' (LineSegment (x1, y1) (x2, y2))
    | x1 == x2 = fmap (x1,) [min y1 y2..max y1 y2]
    | y1 == y2 = fmap (,y1) [min x1 x2..max x1 x2]
    | x1 < x2 && y1 < y2 = zip [x1..x2] [y1..y2]
    | x1 > x2 && y1 > y2 = zip [x2..x1] [y2..y1]
    | x1 < x2 && y1 > y2 = zip [x1..x2] (reverse [y2..y1])
    | x1 > x2 && y1 < y2 = zip (reverse [x2..x1]) [y1..y2]
    | otherwise = error "This pattern should be exhaustive, so this error shouldn't happen"

makeMap' :: [LineSegment] -> M.Map (Int, Int) Int
makeMap' lines = M.fromListWith (+) $ zip (concatMap getPoints' lines) (repeat 1) 

part2 :: [LineSegment] -> Int
part2 = countIntersections . makeMap'