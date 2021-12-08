{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
module Day8 where
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S
import Data.Set((\\))
import Data.List (find)
import Data.Maybe (fromJust)

data SegmentInput = SegmentInput [S.Set Char] [S.Set Char]
    deriving Show

testSegment :: SegmentInput
testSegment = parseSegmentInput "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

readInput :: IO [SegmentInput]
readInput = fmap parseSegmentInput . T.lines <$> TIO.readFile "input/day8.txt"

parseSegmentInput :: T.Text -> SegmentInput
parseSegmentInput t = let (patterns, digits) = T.break (=='|') t in
    SegmentInput (S.fromList . T.unpack <$> T.words patterns) 
        (S.fromList . T.unpack <$> T.words (T.drop 1 digits))

part1 :: [SegmentInput] -> Int
part1 = sum . fmap countP1Digits
  where
    countP1Digits (SegmentInput _ digits) = 
        length $ filter (\x -> S.size x == 2 || S.size x == 3 || S.size x == 4 || S.size x == 7) digits

-- Okay, so, let's think about this.
-- Our goal is to figure out which letter corresponds to which 7-segment digit
-- knowing that all 10 digits are specified in the first part of the input.
-- As mentioned in part 1 there's four digits we can identify completely based on length:
-- 1 uses 2 segments, 7 uses 3 segments, 4 uses 4 segments, and 8 uses 7 segments
-- 8 gives no information to us
-- the two segments in 1 must form the right of the display
-- the segment that is in 7 but not 1 must be the top of the display
-- the two segments in 4 but not 1 must be the middle and top-left
-- The 6 segment digit which does not contain both the middle and top-left must be 0
-- which identifies which segment is the middle of the display
-- at which point we can determine the top right and bottom left from the other 6-segment digits
-- and then there's only one segment left we don't know, so it's trivial to find the bottom segment

-- This is a mess lmao. Probably would have been cleaner to use do notation in the maybe monad
-- instead of unwrapping all the `find`s with `fromJust`
identifyDigits :: SegmentInput -> [Int]
identifyDigits (SegmentInput signals digits) = let
    oneSegments = fromJust $ find ((==2) . S.size) signals
    sevenSegments = fromJust $ find ((==3) . S.size) signals
    fourSegments = fromJust $ find ((==4) . S.size) signals
    eightSegments = fromJust $ find ((==7) . S.size) signals
    fourMiddle = fourSegments \\ oneSegments
    zeroSegments = fromJust $ find (\s -> not (fourMiddle `S.isSubsetOf` s) && S.size s == 6) signals
    nineSegments = fromJust $ find (\s -> (oneSegments `S.isSubsetOf` s) && s /= zeroSegments && S.size s == 6) signals
    sixSegments = fromJust $ find (\s -> s /= zeroSegments && s /= nineSegments && S.size s == 6) signals
    fiveSegments = fromJust $ find (\s -> (fourMiddle `S.isSubsetOf` s) && S.size s == 5) signals
    threeSegments = fromJust $ find (\s -> (oneSegments `S.isSubsetOf` s) && S.size s == 5) signals
    twoSegments = fromJust $ find (\s -> s /= fiveSegments && s /= threeSegments && S.size s == 5) signals
    getDigit s
        | s == zeroSegments = 0
        | s == oneSegments = 1
        | s == twoSegments = 2
        | s == threeSegments = 3
        | s == fourSegments = 4
        | s == fiveSegments = 5
        | s == sixSegments = 6
        | s == sevenSegments = 7
        | s == eightSegments = 8
        | s == nineSegments = 9
        | otherwise = error "unrecognized digit"
  in
    fmap getDigit digits

digitListToInt :: [Int] -> Int 
digitListToInt = foldr (\(a, x) acc -> acc + x*10^a) 0 . zip [0..] . reverse 

part2 :: [SegmentInput] -> Int 
part2 = sum . fmap (digitListToInt . identifyDigits)

-- Actually you know what let's do that rewrite into the maybe monad mentioned above.
-- If you are familiar with rust, this is the equivalent of going from
-- code that's filled with `unwrap()` calls that will crash the program
-- to code that instead returns an Option and uses the `?` operator everywhere
identifyDigits' :: SegmentInput -> Maybe [Int]
identifyDigits' (SegmentInput signals digits) = do
    oneSegments <- find ((==2) . S.size) signals
    sevenSegments <- find ((==3) . S.size) signals
    fourSegments <- find ((==4) . S.size) signals
    eightSegments <- find ((==7) . S.size) signals
    let fourMiddle = fourSegments \\ oneSegments
    zeroSegments <- find (\s -> not (fourMiddle `S.isSubsetOf` s) && S.size s == 6) signals
    nineSegments <- find (\s -> (oneSegments `S.isSubsetOf` s) && s /= zeroSegments && S.size s == 6) signals
    sixSegments <- find (\s -> s /= zeroSegments && s /= nineSegments && S.size s == 6) signals
    fiveSegments <- find (\s -> (fourMiddle `S.isSubsetOf` s) && S.size s == 5) signals
    threeSegments <- find (\s -> (oneSegments `S.isSubsetOf` s) && S.size s == 5) signals
    twoSegments <- find (\s -> s /= fiveSegments && s /= threeSegments && S.size s == 5) signals
    let getDigit = (\s -> if
            | s == zeroSegments -> Just 0
            | s == oneSegments -> Just 1
            | s == twoSegments -> Just 2
            | s == threeSegments -> Just 3
            | s == fourSegments -> Just 4
            | s == fiveSegments -> Just 5
            | s == sixSegments -> Just 6
            | s == sevenSegments -> Just 7
            | s == eightSegments -> Just 8
            | s == nineSegments -> Just 9
            | otherwise -> Nothing)
    mapM getDigit digits

part2' :: [SegmentInput] -> Maybe Int
part2' = fmap (sum . fmap digitListToInt) . mapM identifyDigits'
