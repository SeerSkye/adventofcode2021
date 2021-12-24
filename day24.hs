{-# Language OverloadedStrings, BangPatterns #-}
module Day24 where
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Attoparsec.Text
import Control.Applicative((<|>))
import Data.Either(fromRight)
import Data.List(foldl', unfoldr)
import Control.Monad (replicateM, guard)

data RegName = X | Y | Z | W
    deriving Show
newtype OutParam = OutParam RegName
    deriving Show
data InParam = InReg RegName | InVal Int
    deriving Show
data Instruction =
      Inp OutParam
    | Mul OutParam InParam
    | Add OutParam InParam
    | Div OutParam InParam
    | Mod OutParam InParam
    | Eql OutParam InParam
    deriving Show

regName :: Parser RegName
regName = 
        W <$ "w"
    <|> X <$ "x"
    <|> Y <$ "y"
    <|> Z <$ "z"

outParam :: Parser OutParam
outParam = OutParam <$> regName

inParam :: Parser InParam
inParam = InReg <$> regName <|> InVal <$> signed decimal

instruction :: Parser Instruction
instruction = Inp <$> ("inp " *> outParam)
    <|> Mul <$> ("mul " *> outParam <* " ") <*> inParam
    <|> Add <$> ("add " *> outParam <* " ") <*> inParam
    <|> Div <$> ("div " *> outParam <* " ") <*> inParam
    <|> Mod <$> ("mod " *> outParam <* " ") <*> inParam
    <|> Eql <$> ("eql " *> outParam <* " ") <*> inParam

readInput :: IO [Instruction]
readInput = fromRight (error "parse error") . parseOnly (instruction `sepBy` endOfLine) <$> Text.IO.readFile "input/day24.txt"

data Registers = Registers 
    { wReg :: Int
    , xReg :: Int
    , yReg :: Int
    , zReg :: Int
    }

modifyRegister :: OutParam -> (Int -> Int) -> Registers -> Registers
modifyRegister (OutParam W) f regs = regs{wReg = f $ wReg regs}
modifyRegister (OutParam X) f regs = regs{xReg = f $ xReg regs}
modifyRegister (OutParam Y) f regs = regs{yReg = f $ yReg regs}
modifyRegister (OutParam Z) f regs = regs{zReg = f $ zReg regs}

inVal :: Registers -> InParam -> Int
inVal _ (InVal x) = x
inVal r (InReg W) = wReg r
inVal r (InReg X) = xReg r
inVal r (InReg Y) = yReg r
inVal r (InReg Z) = zReg r

doInstruction :: [Int] -> Registers -> Instruction -> ([Int],Registers)
doInstruction (x:xs) regs (Inp o) =(xs, modifyRegister o (const x) regs)
doInstruction [] _ (Inp _) = error "ran out of input"
doInstruction inp regs (Mul o i) = (inp, modifyRegister o (* inVal regs i) regs)
doInstruction inp regs (Add o i) = (inp, modifyRegister o (+ inVal regs i) regs)
doInstruction inp regs (Div o i) = (inp, modifyRegister o (`div` inVal regs i) regs)
doInstruction inp regs (Mod o i) = (inp, modifyRegister o (`mod` inVal regs i) regs)
doInstruction inp regs (Eql o i) = (inp, modifyRegister o (\x -> if x == inVal regs i then 1 else 0) regs)

monadCheck :: [Int] -> [Instruction] -> Bool
monadCheck input program = 
    0 == zReg (snd $ foldl' (uncurry doInstruction) (input, Registers 0 0 0 0) program)

intToDigits :: Int -> [Int]
intToDigits = reverse . unfoldr (\n -> if n==0 then Nothing else Just(n `mod` 10, n `div` 10))

nums :: [[Int]]
nums = filter (notElem 0) $ fmap intToDigits [99999999999999, 99999999999998..11111111111111]

part1 :: [Instruction] -> [Int]
part1 program = go nums
  where
    go [] = []
    go (x:xs) =
        if monadCheck x program then x else go xs

-- This code does not actually get an answer in any reasonable time.
-- after starting this program, I ended up calculating the solution by hand
-- by reverse engineering the input.
-- The fact that day 23 and 24 are both no code days is bizarre to me.

-- for posterity, my actual solution:
{-
for each digit:
set x to z%26
divide z by either 1 or 26
add a constant to x
if x == digit then {
	leave z unchanged
} else {
	multiply z by 26
	add digit and another constant to z
}


after first digit:
z = d1 + 7

after second digit:
z = 26 *(d1+7)+d2+8

after 3rd digit:
z = 26*(26*(d1+7)+d2+8)+d3+16

after 4th digit:
z=26*(26*(26*(d1+7)+d2+8)+d3+16)+d4+8

after 5th digit:
if z%26 - 8 == d5 then z=26*(26*(d1+7)+d2+8)+d3+16
-- d4 == d5

after 6th digit:
z=26*(26*(26*(d1+7)+d2+8)+d3+16)+d6+12

after 7th digit:
if z%26 - 11 = d7 then z=26*(26*(d1+7)+d2+8)+d3+16
-- d6+1 = d7

after 8th digit:
z=26*(26*(26*(d1+7)+d2+8)+d3+16)+d8+8

after 9th digit:
if z%26 - 6 == d9 then z=26*(26*(d1+7)+d2+8)+d3+16
-- d8+2 == d9

after 10th digit:
if z%26 - 9 == d10 then z=26*(d1+7)+d2+8
-- d3+7 == d10

after 11th digit:
z=26*(26*(d1+7)+d2+8)+d11+4

after 12th digit:
if z%26 - 5 == d12 then z=26*(d1+7)+d2+8
-- d11-1=d12

after 13th digit:
if z%26 - 4 == d13 then z=d1+7
-- d2+4 == d13

after 14th digit:
if z%26 - 9 == d14 then z=0
-- d1-2 == d14

-- d1-2 == d14
-- d2+4 == d13
-- d3+7 == d10
-- d4 == d5
-- d6+1 = d7
-- d8+2 == d9
-- d11-1=d12

d1 = 9 -> d14 = 7
d2 = 5 -> d13 = 9
d3 = 2 -> d10 = 9
d4 = 9 -> d5 = 9
d6 = 8 -> d7 = 9
d8 = 7 -> d9 = 9
d11 = 9 -> d12 = 8
95299897999897

d1 = 3 -> d14 = 1
d2 = 1 -> d13 = 5
d3 = 1 -> d10 = 8
d4 = 1 -> d5 = 1
d6 = 1 -> d7 = 2
d8 = 1 -> d9 = 3
d11 = 2 -> d12 = 1
31111121382151
-}