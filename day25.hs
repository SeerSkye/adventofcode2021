module Day25 where
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

data Seafloor = Seafloor
    { grid :: Map (Int, Int) Char 
    , width :: Int
    , height :: Int
    } deriving (Show, Eq)

toMap :: [String] -> Seafloor
toMap lst = Seafloor
    { grid = Map.filter (\c -> c == '>' || c == 'v') $ Map.fromList $ 
        concatMap (\(row, y) -> (\(val, x) -> ((x, y), val)) <$> zip row [0 ..]) $ zip lst [0 ..]
    , width = length $ head lst
    , height = length lst
    }

readInput :: IO Seafloor
readInput = toMap . lines <$> readFile "input/day25.txt"

moveRight :: Seafloor -> Seafloor
moveRight seafloor = seafloor{grid = Map.foldMapWithKey stepRight (grid seafloor)}
  where
    stepRight loc 'v' = Map.singleton loc 'v'
    stepRight (x,y) '>' = 
        if ((x+1) `mod` width seafloor, y) `Map.member` grid seafloor -- there's something in the space to move to
        then Map.singleton (x,y) '>'
        else Map.singleton ((x+1) `mod` width seafloor, y) '>'
    stepRight _ _ = error "unrecognized character in map"

moveDown :: Seafloor -> Seafloor
moveDown seafloor = seafloor{grid = Map.foldMapWithKey stepDown (grid seafloor)}
  where
    stepDown loc '>' = Map.singleton loc '>'
    stepDown (x,y) 'v' = 
        if (x, (y+1) `mod` height seafloor) `Map.member` grid seafloor -- there's something in the space to move to
        then Map.singleton (x,y) 'v'
        else Map.singleton (x, (y+1) `mod` height seafloor) 'v'
    stepDown _ _ = error "unrecognized character in map"

moveSeaCucumbers :: Seafloor -> Seafloor
moveSeaCucumbers = moveDown . moveRight

part1 :: Seafloor -> Int
part1 = go 0
  where
    go n seafloor = let movedSeafloor = moveSeaCucumbers seafloor in
        if movedSeafloor == seafloor 
        then n+1
        else go (n+1) movedSeafloor

printSeafloor :: Seafloor -> IO ()
printSeafloor seafloor = mapM_ (\y -> do
    mapM_ (\x -> putChar $ Map.findWithDefault '.' (x,y) (grid seafloor)) [0..width seafloor-1]
    putStrLn "") [0..height seafloor-1]
