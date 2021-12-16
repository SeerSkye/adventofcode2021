{-# Language OverloadedStrings #-}
module Day16 where
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Attoparsec.Text as Attoparsec
import Control.Applicative((<|>))
import Control.Monad(replicateM)
import Data.Either(fromRight)

examplePacket :: [Char]
examplePacket = "EE00D40C823060"

expandHex :: Char -> String
expandHex '0' = "0000"
expandHex '1' = "0001"
expandHex '2' = "0010"
expandHex '3' = "0011"
expandHex '4' = "0100"
expandHex '5' = "0101"
expandHex '6' = "0110"
expandHex '7' = "0111"
expandHex '8' = "1000"
expandHex '9' = "1001"
expandHex 'A' = "1010"
expandHex 'B' = "1011"
expandHex 'C' = "1100"
expandHex 'D' = "1101"
expandHex 'E' = "1110"
expandHex 'F' = "1111"
expandHex _ = error "I can't be bothered doing proper checking, just only pass hex pls"

toBits :: String -> Text
toBits = Text.pack . concatMap expandHex

data TypeID = Sum
    | Product
    | Minimum
    | Maximum
    | GreaterThan
    | LessThan
    | Equal
    deriving Show

typeId :: Parser TypeID
typeId = do
    typeId <- bits 3
    case typeId of
        0 -> return Sum
        1 -> return Product
        2 -> return Minimum
        3 -> return Maximum
        5 -> return GreaterThan
        6 -> return LessThan
        7 -> return Equal
        _ -> fail "unrecognized type ID"
        

data Packet = Literal Int Int
    | Operator Int TypeID [Packet]
    deriving Show

bit :: Parser Bool
bit = toEnum . read . pure <$> (char '0' <|> char '1')

bitstringToInt :: [Bool] -> Int
bitstringToInt = foldr (\(a, x) acc -> acc + fromEnum x * 2^a) 0 . zip [0..] . reverse 

bits :: Int -> Parser Int
bits n = bitstringToInt <$> count n bit

literalBits :: Parser [Bool]
literalBits = do
    lastGroup <- not <$> bit
    if lastGroup
        then count 4 bit
        else (++) <$> count 4 bit <*> literalBits 

literalVal :: Parser Int
literalVal = bitstringToInt <$> literalBits

packet :: Parser Packet
packet = literalPacket <|> opPacket

literalPacket :: Parser Packet
literalPacket = Literal <$> bits 3 <* "100" <*> literalVal

opPacket :: Parser Packet
opPacket = do
    versionCode <- bits 3
    op <- typeId
    lengthType <- bits 1
    if lengthType == 1 
        then do 
            numPackets <- bits 11
            packets <- count numPackets packet
            return $ Operator versionCode op packets
        else do
            numBits <- bits 15
            packetData <- Attoparsec.take numBits
            case parseOnly (many' packet <* endOfInput) packetData of
                Left s -> fail $ "parse error when getting packets by bits: " ++ s
                Right packets -> return $ Operator versionCode op packets

parseInput :: String -> Packet
parseInput s = fromRight (error "parse error") $ parseOnly packet (toBits s)

readInput :: IO Packet
readInput = parseInput <$> readFile "input/day16.txt"

sumVersionCodes :: Packet -> Int
sumVersionCodes (Literal version _) = version
sumVersionCodes (Operator version _ packets) =
    version + sum (sumVersionCodes <$> packets)

part1 = sumVersionCodes

evalPacket :: Packet -> Int
evalPacket (Literal _ v) = v
evalPacket (Operator _ op packets) = case op of
    Sum         -> sum $ evalPacket <$> packets
    Product     -> product $ evalPacket <$> packets
    Minimum     -> minimum $ evalPacket <$> packets
    Maximum     -> maximum $ evalPacket <$> packets
    GreaterThan -> fromEnum $ evalPacket (packets!!0) > evalPacket (packets!!1)
    LessThan    -> fromEnum $ evalPacket (packets!!0) < evalPacket (packets!!1)
    Equal       -> fromEnum $ evalPacket (packets!!0) == evalPacket(packets!!1)

part2 = evalPacket