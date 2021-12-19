{-# Language OverloadedStrings #-}
module Day19 where
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Attoparsec.Text hiding (take)
import Control.Applicative((<|>))
import Data.Either(fromRight)
import Control.Monad(guard)
import Data.List(sort, groupBy, group)
import Data.Containers.ListUtils(nubOrd)

type Point = (Int, Int, Int)

type ScannerData = [Point]

point :: Parser Point
point = (,,) <$> signed decimal <* char ',' <*> signed decimal <* char ',' <*> signed decimal <* many' endOfLine

scannerData :: Parser ScannerData
scannerData = "--- scanner " *> decimal *> " ---" *> endOfLine 
    *> many' point

readInput :: IO [ScannerData]
readInput = fromRight (error "parse error") . parseOnly (many' scannerData) <$> Text.IO.readFile "input/day19.txt"

distance :: Point -> Point -> Double
distance (x1, y1, z1) (x2, y2, z2) = sqrt 
    $ (fromIntegral x1 - fromIntegral x2)^2 
    + (fromIntegral y1 - fromIntegral y2)^2 
    + (fromIntegral z1 - fromIntegral z2)^2

subVec :: Point -> Point -> Point
subVec (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

addVec :: Point -> Point -> Point
addVec (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

-- never do exact floating point comparison. I know you're supposed to do this based on
-- the relative size of the floating point numbers but I know that none of the numbers are
-- gonna get terribly big or small here
(~=) :: Double -> Double -> Bool
a ~= b = abs(a-b) < 0.01

calcDistance :: ScannerData -> [(Point, [Double])]
calcDistance scan = fmap (\p -> (p, take 12 $ sort $ fmap (distance p) scan)) scan

findSharedSet :: ScannerData -> ScannerData -> [(Point, Point)]
findSharedSet scan1 scan2 = do
    b1 <- scan1
    let b1distances = sort $ distance b1 <$> scan1
    b2 <- scan2
    let b2distances = sort $ distance b2 <$> scan2
    guard $ inSharedSpace b1distances b2distances
    return (b1, b2)
  where
    inSharedSpace xs ys = length (filter ((==2).length) $ groupBy (~=) $ merge xs ys) >= 12
    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys) 
        | x < y = x : merge xs (y:ys)
        | otherwise = y : merge (x:xs) ys

-- all 4 rotations that keep z constant
turns :: [Point -> Point]
turns =
    [ id 
    , \(x,y,z) -> (-y,  x, z)
    , \(x,y,z) -> (-x, -y, z)
    , \(x,y,z) -> ( y, -x, z)
    ]

-- 6 rotations each with a different z value
facings :: [Point -> Point]
facings =
    [ id 
    , \(x,y,z) -> (-x,  y, -z)
    , \(x,y,z) -> (-z,  y,  x)
    , \(x,y,z) -> ( z,  y, -x)
    , \(x,y,z) -> ( x,  z, -y)
    , \(x,y,z) -> ( x, -z,  y)
    ]

allRotations :: [Point -> Point]
allRotations = [a . b | a <- turns, b <- facings]

sameRotation :: [(Point, Point)] -> Bool
sameRotation sharedSet = length (group $ fmap (uncurry subVec) sharedSet) == 1

-- If the two input scanners have overlapping range, combine them into a single set
-- in the coordinate system of the first parameter, along with returning the second
-- scanner's position relative to the first
combineScans :: ScannerData -> ScannerData -> Maybe (Point, ScannerData)
combineScans scan1 scan2 = case sharedSet of
    [] -> Nothing
    (p1, p2):_ -> case findTransform allRotations of
        Nothing -> Nothing
        Just rot -> let scannerPos = subVec (rot p2) p1 in
            Just (scannerPos, nubOrd $ scan1 ++ fmap ((`subVec` scannerPos) . rot) scan2)
  where
    sharedSet = findSharedSet scan1 scan2
    findTransform [] = Nothing
    findTransform (f:fs) = if sameRotation (fmap (fmap f) sharedSet) then Just f else findTransform fs

allBeacons :: (ScannerData, [Point]) -> [ScannerData] -> (ScannerData, [Point])
allBeacons root [] = root
allBeacons (root, scanners) (x:xs) = case combineScans root x of
    Nothing -> allBeacons (root, scanners) (xs ++ [x])
    Just (newScanner, newRoot) -> allBeacons (newRoot, newScanner:scanners) xs

-- this runs real slow lol. My algorithm for this is not good.
part1 :: [ScannerData] -> Int
part1 scans = length $ fst $ allBeacons (head scans, [(0,0,0)]) (tail scans)

manhattenDistance :: Point -> Point -> Int
manhattenDistance (x1, y1, z1) (x2, y2, z2) = abs(x1 - x2) + abs(y1 - y2) + abs(z1 - z2)

part2 :: [ScannerData] -> Int
part2 scans = maximum $ [manhattenDistance x y | x <- scanners, y <- scanners]
  where
    scanners = snd $ allBeacons (head scans, [(0,0,0)]) (tail scans)
