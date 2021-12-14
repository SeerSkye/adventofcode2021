{-# LANGUAGE OverloadedStrings #-}
module Day14 where
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Attoparsec.Text
import Data.List (sort, group)
import Control.Arrow((&&&))
import Control.Applicative((<|>))
import Data.Either(fromRight)

-- Okay, so, a naive approach to this solution would involve
-- storing a list that exponentially increases in size. I like
-- my ram and would prefer to avoid that if possible. One thing 
-- I note about the problem is that the order doesn't actually seem
-- to matter like one would think it should. Consider the Example
-- given involving NNCB, if we consider the pairs NN, NC, and CB
-- each of these turns into two new pairs: NN -> NC, CN; NC -> NB, BC;
-- and CB -> CH, HB which are exactly the pairs we need for the next step!
-- This means that we don't need to keep track of the order to get the next
-- pairs, and as such we can just store pairs in buckets! Well, almost
-- anyways, there is one wrinkle: this representation double-counts every character
-- except for the first and last character in the string. Fortunately this
-- is very easy to work around as the first and last character of the string
-- cannot change, so we just remember those two characters before we
-- bucket the input and we're good.

data ProblemData = ProblemData
    { pairs :: Map (Char, Char) Int
    , firstChar :: Char 
    , lastChar :: Char 
    , insertionRules :: Map (Char, Char) Char 
    } deriving Show

startingString :: Parser String
startingString = many' letter <* endOfLine

rule :: Parser ((Char, Char), Char)
rule = (,) <$> rulePair <*> (string " -> " *> letter <* (endOfLine <|> endOfInput))
  where
    rulePair = (,) <$> letter <*> letter

makeData :: String -> [((Char, Char), Char)] -> ProblemData
makeData startString rules = ProblemData
    { insertionRules = Map.fromList rules
    , firstChar = head startString
    , lastChar = last startString
    , pairs = Map.fromList 
        $ map (head &&& length) 
        $ group 
        $ sort 
        $ zip startString (drop 1 startString)
    }

parseInput :: Parser ProblemData
parseInput = makeData <$> startingString <*> (endOfLine *> many' rule) 

readInput :: IO ProblemData
readInput = fromRight (error "parse failure") . parseOnly parseInput 
    <$> Text.IO.readFile "input/day14.txt"

expandPolymer :: ProblemData -> ProblemData
expandPolymer problemData = problemData{pairs = newPairs}
  where
    expandPair pair amount map = case Map.lookup pair (insertionRules problemData) of
        Nothing -> Map.insertWith (+) pair amount map
        Just c -> Map.insertWith (+) (fst pair, c) amount 
                $ Map.insertWith (+) (c, snd pair) amount map
    newPairs = Map.foldrWithKey expandPair Map.empty (pairs problemData)

getCounts :: ProblemData -> (Int, Int)
getCounts problemData = minimum &&& maximum $ fmap snd $ Map.toList countMap
  where
    countPair (c1, c2) amount map = Map.insertWith (+) c1 amount 
        $ Map.insertWith (+) c2 amount map
    edgeChars = [(firstChar problemData, 1), (lastChar problemData, 1)]
    countMap = Map.map(`div` 2) 
        $ Map.foldrWithKey countPair (Map.fromList edgeChars) (pairs problemData)

-- Copied from GCH.Utils.Misc
nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f

part1 :: ProblemData -> Int
part1 problemData = max - min
  where
    (min, max) = getCounts $ nTimes 10 expandPolymer problemData

part2 :: ProblemData -> Int
part2 problemData = max - min
  where
    (min, max) = getCounts $ nTimes 40 expandPolymer problemData
