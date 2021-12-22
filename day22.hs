{-# Language OverloadedStrings #-}
module Day22 where
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Attoparsec.Text
import Control.Applicative((<|>))
import Data.Either (fromRight)
import Data.List(foldl')

pair :: Parser (Int, Int)
pair = (,) <$> (letter *> "=" *> signed decimal) <*> (".." *> signed decimal)

data Command = On (Int, Int) (Int, Int) (Int, Int)
    | Off (Int, Int) (Int, Int) (Int, Int)
    deriving Show

onCmd :: Parser Command
onCmd = On <$> ("on " *> pair) <*> (char ',' *> pair) <*> (char ',' *> pair) <* (endOfLine <|> endOfInput)

offCmd :: Parser Command
offCmd = Off <$> ("off " *> pair) <*> (char ',' *> pair) <*> (char ',' *> pair) <* (endOfLine <|> endOfInput)

command :: Parser Command
command = onCmd <|> offCmd

readInput :: IO [Command]
readInput = fromRight (error "parse error") . parseOnly (many' command) <$> Text.IO.readFile "input/day22.txt"

data Cuboid = Cuboid (Int, Int, Int) (Int, Int, Int)
    deriving Show

hasVolume :: Cuboid -> Bool
hasVolume (Cuboid (minX, minY, minZ) (maxX, maxY, maxZ)) =
    minX <= maxX && minY <= maxY && minZ <= maxZ

-- two cuboids do not intersect if there is an axis that completely separates them
intersect :: Cuboid -> Cuboid -> Bool
intersect (Cuboid (minX1, minY1, minZ1) (maxX1, maxY1, maxZ1))
          (Cuboid (minX2, minY2, minZ2) (maxX2, maxY2, maxZ2)) =
    not $ minX1 > maxX2 || minX2 > maxX1 || minY1 > maxY2 || minY2 > maxY1 || minZ1 > maxZ2 || minZ2 > maxZ1

-- Remove the second cuboid from the first, decomposing the result into a list of cuboids
-- The basic idea here is that we can use the same algorithm for every type of intersection
-- if we handle the most complex case correctly (and other cases will just fall out as edge
-- cases)
-- The most complex case is the one where the cuboid being subtracted fits entirely within
-- the original cuboid, in which case we decompose the new outer shell into 6 components:
-- two large components on opposite sides along one axis, then two long components along a second
-- and finally 2 small ones along the third.
-- in partial overlap cases we need to make sure that our new cuboids don't have any points
-- outside the original cuboid, hence the `max min1 min2` and `min max1 max2` bits.
subCuboid :: Cuboid -> Cuboid -> [Cuboid]
subCuboid c1@(Cuboid (minX1, minY1, minZ1) (maxX1, maxY1, maxZ1))
          c2@(Cuboid (minX2, minY2, minZ2) (maxX2, maxY2, maxZ2))
    | c1 `intersect` c2 = filter hasVolume
        [ Cuboid (minX1, minY1, minZ1) (minX2-1, maxY1, maxZ1)
        , Cuboid (maxX2+1, minY1, minZ1) (maxX1, maxY1, maxZ1)
        , Cuboid (max minX1 minX2, minY1, minZ1) (min maxX1 maxX2, minY2-1, maxZ1)
        , Cuboid (max minX1 minX2, maxY2+1, minZ1) (min maxX1 maxX2, maxY1, maxZ1)
        , Cuboid (max minX1 minX2, max minY1 minY2, minZ1) (min maxX1 maxX2, min maxY1 maxY2, minZ2-1)
        , Cuboid (max minX1 minX2, max minY1 minY2, maxZ2+1) (min maxX1 maxX2, min maxY1 maxY2, maxZ1)
        ]
    | otherwise = [c1]

-- add a cuboid to a set of cuboids, being sure to not create overlapping cuboids
-- we do this by detecting if our cuboid would collide with any of the cuboids in the
-- list, and if we detect a collision we subtract the existing cuboid from the one to be
-- added, splitting it up into multiple cuboids we continue to add
addCuboid :: [Cuboid] -> Cuboid -> [Cuboid]
addCuboid [] c = [c]
addCuboid (x:xs) c
    | c `intersect` x = x : foldl' addCuboid xs (c `subCuboid` x)
    | otherwise = x:addCuboid xs c

volume :: Cuboid -> Int
volume c@(Cuboid (minX, minY, minZ) (maxX, maxY, maxZ)) = 
    if hasVolume c then (maxX - minX + 1) * (maxY - minY + 1) * (maxZ - minZ + 1) else 0

sumVolumes :: [Cuboid] -> Int
sumVolumes = sum . fmap volume

doCommand :: [Cuboid] -> Command -> [Cuboid]
doCommand cs (On (minX, maxX) (minY, maxY) (minZ, maxZ)) =
    addCuboid cs (Cuboid (minX, minY, minZ) (maxX, maxY, maxZ))
doCommand cs (Off (minX, maxX) (minY, maxY) (minZ, maxZ)) = 
    concatMap (`subCuboid` Cuboid (minX, minY, minZ) (maxX, maxY, maxZ)) cs

-- for part 1 simply truncate the input file to only the first few small commands
solve :: [Command] -> Int
solve = sumVolumes . foldl' doCommand []