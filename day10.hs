module Day10 where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (foldl', sort)
import Data.Maybe

readInput :: IO [T.Text]
readInput = T.lines <$> TIO.readFile "input/day10.txt"

data ParseResult = Incomplete [Char] | Corrupted Char
    deriving Show

isOpenBracket :: Char -> Bool
isOpenBracket c = c == '(' || c == '[' || c == '{' || c == '<'

matchingBracket :: Char -> Char
matchingBracket '(' = ')'
matchingBracket '{' = '}'
matchingBracket '[' = ']'
matchingBracket '<' = '>'
matchingBracket a = error $ "asked for the matching bracket of the invalid character " ++ show a

parse' :: ParseResult -> Char -> ParseResult
parse' (Corrupted c) _ = Corrupted c
parse' (Incomplete []) c = if isOpenBracket c then Incomplete [matchingBracket c]
    else error $ "Expected an open bracket, found " ++ show c
parse' (Incomplete (b:bs)) c
    | isOpenBracket c = Incomplete (matchingBracket c:b:bs)
    | c == b = Incomplete bs
    | otherwise = Corrupted c

parse :: T.Text -> ParseResult
parse = T.foldl' parse' (Incomplete [])

p1Score :: ParseResult -> Int
p1Score (Incomplete _) = 0
p1Score (Corrupted c)
    | c == ')' = 3
    | c == ']' = 57
    | c == '}' = 1197
    | c == '>' = 25137
    | otherwise = 0

part1 :: [T.Text] -> Int
part1 = sum . fmap (p1Score . parse)

p2BracketPoints :: Char -> Int
p2BracketPoints c
    | c == ')' = 1
    | c == ']' = 2
    | c == '}' = 3
    | c == '>' = 4
    | otherwise = 0

p2Score :: ParseResult -> Maybe Int
p2Score (Corrupted _) = Nothing
p2Score (Incomplete cs) = 
    Just $ foldl' (\acc c -> 5 * acc + p2BracketPoints c) 0 cs

part2 :: [T.Text] -> Int
part2 = (\l -> l!!(length l `div` 2)) . sort . mapMaybe (p2Score . parse)




