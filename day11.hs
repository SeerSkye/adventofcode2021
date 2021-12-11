module Day11 where
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as S
import Data.Ix ( Ix(range) )
import Control.Monad ( void, replicateM )
import Control.Monad.Trans.State ( State, evalState, get, modify )
import Control.Monad.Loops ( untilM )

-- Same input reading code as day9
readInput :: IO (H.HashMap (Int, Int) Int)
readInput = toHashMap . fmap (fmap (read . return)) . lines <$> readFile "input/day11.txt"

toHashMap :: [[Int]] -> H.HashMap (Int, Int) Int
toHashMap lst = H.fromList $
    concatMap (\(row, y) -> 
        (\(val, x) -> ((x,y), val)) <$> zip row [0..] ) $ zip lst [0..]

type OctoState = H.HashMap (Int, Int) Int 

-- A single cycle of the octopular automata
octoCycle :: State OctoState Int
octoCycle = do 
    modify (fmap succ) --increment every value in the hashmap
    let loop octosFlashed = do -- our loop for iteratively making the octopi flash
        highEnergyOctos <- S.fromMap . void . H.filter (>9) <$> get
        let octosToFlash = S.difference highEnergyOctos octosFlashed
        if S.size octosToFlash == 0 
            then return octosFlashed
            else mapM_ flash octosToFlash >> loop (octosToFlash `S.union` octosFlashed)
    octosFlashed <- loop S.empty -- run the loop, getting the set of all octopi that flashed
    modify (\m -> foldr (`H.insert` 0) m octosFlashed) -- set all the flashed octopi to 0
    return $ S.size octosFlashed -- return the number of octopi that flashed

-- increment every cell in a 3x3 neighborhood around a point
flash :: (Int, Int) -> State OctoState ()
flash (x,y) = mapM_ (modify . H.adjust succ) (range ((x-1, y-1),(x+1,y+1)))

-- simply sum up the return values of 100 cycles of `octoCycle`
part1 :: H.HashMap (Int, Int) Int -> Int 
part1 = sum . evalState (replicateM 100 octoCycle)

-- run until the entire board after a cycle is 0 (which means all the octopi flashed in that cycle)
-- then count how many cycles that took
part2 :: H.HashMap (Int, Int) Int -> Int 
part2 = evalState (fmap length $ octoCycle `untilM` (all (==0) <$> get))

