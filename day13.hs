{-# LANGUAGE OverloadedStrings #-}
module Day13 where
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Attoparsec.Text
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Applicative((<|>))
import Data.Either(fromRight)
import Data.List(foldl')

pair :: Parser (Int, Int)
pair = (,) <$> decimal <*> (char ',' *> decimal) <* endOfLine 

dotGrid :: Parser (Set (Int, Int))
dotGrid = Set.fromList <$> many' pair

data Fold = Horizontal Int | Vertical Int
    deriving (Show, Eq)

fold :: Parser Fold
fold = makeFold 
    <$> (string "fold along " *> (char 'x' <|> char 'y'))
    <*> (char '=' *> decimal)
    <* (endOfLine <|> endOfInput )
  where
    makeFold dir loc
        | dir == 'x' = Vertical loc
        | dir == 'y' = Horizontal loc
        | otherwise = error "parse got improper character?"

parseInput :: Parser (Set (Int,Int), [Fold])
parseInput = (,) <$> dotGrid <*> (endOfLine *> many' fold)

readInput :: IO (Set (Int,Int), [Fold])
readInput = fromRight (error "parse failure") . parseOnly parseInput <$> Text.IO.readFile "input/day13.txt"

foldGrid :: Set (Int, Int) -> Fold -> Set (Int, Int)
foldGrid s (Horizontal foldY) = Set.map(\(x,y) -> (x, abs(y - foldY) - 1)) s
foldGrid s (Vertical foldX) = Set.map(\(x,y) -> (abs(x - foldX) - 1, y)) s

part1 :: (Set (Int,Int), [Fold]) -> Int
part1 (grid, folds) = Set.size $ foldGrid grid (head folds)

part2 :: IO ()
part2 = do
    (grid, folds) <- readInput
    printGrid $ foldl' foldGrid grid folds

-- Only print a 40x6 slice, cause that's what our input sits in
printGrid :: Set(Int, Int) -> IO ()
printGrid grid = mapM_ (\y -> do
    mapM_ (\x -> if (x, y) `Set.member` grid then putChar '#' else putChar '.') [39,38..0]
    putStrLn "") [5,4..0]
