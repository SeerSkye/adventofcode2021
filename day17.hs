module Day17 where
import Data.Ix

-- part 1 was solved by inspection, part 2 we're just gonna brute force
targetMaxX = 193
targetMinX = 150
targetMaxY = -86
targetMinY = -136

inTarget :: (Int, Int) -> Bool
inTarget = inRange ((targetMinX, targetMinY), (targetMaxX, targetMaxY))

hitsTarget :: (Int, Int) -> Bool
hitsTarget = go (0,0)
  where 
    go p@(x, y) (vx, vy)
        | y < targetMinY = False -- no hitting the target after this point
        | inTarget p = True
        | otherwise = go (x+vx, y+vy) (max 0 (vx - 1), vy - 1)

part2 = length $ filter hitsTarget $ range ((0, -136),(193, 135))