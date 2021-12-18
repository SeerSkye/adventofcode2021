{-# Language OverloadedStrings, TupleSections #-}
module Day18 where
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Attoparsec.Text
import Control.Applicative((<|>))
import Data.Either(fromRight)
import Data.List(foldl1')
import Control.Monad(guard)

-- Okay, so, at first this problem seems like a tree structure would fit, but the
-- transformations to reduce snailfish numbers kind of seem like they'd suck
-- when walking a tree. Instead it seems like it would be much easier to just
-- track each element and its nesting level in order, then we could just
-- do some much more convenient list walking instead. 

testNum :: Text
testNum = "[[1,2],3]"

snailfishVal :: Int -> Parser [(Int, Int)]
snailfishVal nestLevel = pure . (, nestLevel) . read . pure <$> digit

snailfishPair :: Int -> Parser [(Int, Int)]
snailfishPair n = 
    (++) <$> (char '[' *> (snailfishVal n <|> snailfishPair (n+1)) <* char ',')
    <*> (snailfishVal n <|> snailfishPair (n+1)) <* char ']'

-- We're gonna use one of these these bad-boys in order to do our reduction algorithm
data Zipper a = Zipper [a] [a]
    deriving (Show, Eq, Ord)

parseInput :: Text -> [(Int, Int)]
parseInput text = fromRight (error "parse error") $ parseOnly (snailfishPair 0) text

readInput :: IO [[(Int, Int)]]
readInput = fmap parseInput . Text.lines <$> Text.IO.readFile "input/day18.txt" 

reduce :: [(Int, Int)] -> [(Int, Int)]
reduce = explode . Zipper []

-- This is kind of inefficient and does a bunch of unneccesary walking.
-- I originally had a single function that both exploded and split, but that
-- didn't actually solve the problem as stated: all explosions must be done before
-- any splits. It would probably be possible to create an explode and split that works
-- after an initial pass that just explodes, but it would be fair bit more complicated
explode :: Zipper (Int, Int) -> [(Int, Int)]
explode (Zipper [] ((x, 4):(y,4):(r, rl):rights)) = -- exploding with no value to the left
    explode $ Zipper [] ((0, 3):(y+r, rl):rights)
explode (Zipper ((l,ll):lefts) [(x, 4),(y, 4)]) = -- explode with no value to the right
    explode $ Zipper lefts [(l+x, ll), (0,3)]
explode (Zipper ((l,ll):lefts) ((x, 4):(y,4):(r, rl):rights)) = -- general explosion
    explode $ Zipper lefts ((l+x, ll):(0,3):(y+r, rl):rights)
explode (Zipper lefts ((x, xl):rights)) = -- no explosion, simply move right
    explode $ Zipper ((x,xl):lefts) rights 
explode (Zipper lefts []) = split $ Zipper [] $ reverse lefts 

split :: Zipper (Int, Int) -> [(Int, Int)]
split (Zipper lefts ((x, xl):rights))
    | x > 9 = explode 
        (Zipper lefts 
            ((floor $   fromIntegral x / 2, xl+1)
            :(ceiling $ fromIntegral x / 2, xl+1)
            :rights))
    | otherwise = split $ Zipper ((x,xl):lefts) rights
split (Zipper lefts []) = reverse lefts

addSnailfish :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
addSnailfish l r = reduce $ fmap (\(x, l) -> (x, l+1)) (l++r)

-- oh no my number representation makes this more complicated
snailfishMagnitude :: [(Int, Int)] -> Int
snailfishMagnitude = go . Zipper []
  where
    go (Zipper [] ((x,xl):(y,yl):rights)) -- special case for if it's the start of the list
        | xl == yl = go $ Zipper [] ((3*x + 2*y, xl-1):rights) --if there's a pair, reduce it
        | otherwise = go $ Zipper [(x,xl)] ((y,yl):rights) -- otherwise walk right
    go (Zipper (left:lefts) ((x,xl):(y,yl):rights))
        -- we want to step left after reducing if possible, to allow us to be able to reduce
        -- leftward
        | xl == yl = go $ Zipper lefts (left:(3*x + 2*y, xl-1):rights)
        | otherwise = go $ Zipper ((x,xl):left:lefts) ((y,yl):rights) --no pair, keep moving forward
    go (Zipper _ [(final, _)]) = final
    go (Zipper _ []) = 0 

part1 :: [[(Int, Int)]] -> Int
part1 = snailfishMagnitude . foldl1' addSnailfish

-- I'm just gonna calculate all of them for part 2, dunno if there's even another option
part2 :: [[(Int, Int)]] -> Int
part2 lst = maximum $ do
    num1 <- lst
    num2 <- lst
    guard $ num1 /= num2
    [ snailfishMagnitude $ addSnailfish num1 num2, snailfishMagnitude $ addSnailfish num2 num1]
