module Day21 where
import Control.Monad.Trans.State (State, get, put, modify, evalState)
import Control.Monad(replicateM)
import Control.Monad.Loops(untilM)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

p1start :: Int
p1start = 9

p2start :: Int
p2start = 3

-- We're gonna use the state monad for our game
data NextPlayer = PlayerOne | PlayerTwo
    deriving Show

data GameState = GameState
    { p1Pos :: Int
    , p1Score :: Int
    , p2Pos :: Int 
    , p2Score :: Int
    , nextPlayer :: NextPlayer
    , nextRoll :: Int
    , timesRolled :: Int
    } deriving Show

type Game a = State GameState a

gameStart :: GameState
gameStart = GameState
    { p1Pos = p1start
    , p1Score = 0
    , p2Pos = p2start
    , p2Score = 0
    , nextPlayer = PlayerOne
    , nextRoll = 1
    , timesRolled = 0
    } 

rollDeterministic :: Game Int
rollDeterministic = do
    roll <- nextRoll <$> get
    let next = if roll == 100 then 1 else roll+1
    modify $ \s -> s{nextRoll = next, timesRolled = timesRolled s + 1}
    return roll

doTurn :: Game ()
doTurn = do
    player <- nextPlayer <$> get
    case player of
        PlayerOne -> do
            roll <- sum <$> replicateM 3 rollDeterministic
            currPos <- p1Pos <$> get
            currScore <- p1Score <$> get
            let nextPos = ((currPos - 1 + roll) `mod` 10) + 1
            let nextScore = currScore + nextPos
            modify $ \s -> s{p1Pos = nextPos, p1Score = nextScore, nextPlayer = PlayerTwo}
        PlayerTwo -> do
            roll <- sum <$> replicateM 3 rollDeterministic
            currPos <- p2Pos <$> get
            currScore <- p2Score <$> get
            let nextPos = ((currPos - 1 + roll) `mod` 10) + 1
            let nextScore = currScore + nextPos
            modify $ \s -> s{p2Pos = nextPos, p2Score = nextScore, nextPlayer = PlayerOne}

lastPlayerWon :: Game Bool
lastPlayerWon = do
    player <- nextPlayer <$> get
    case player of
        PlayerOne -> do
            score <- p2Score <$> get -- if player 1 is the next player, player 2 just went
            if score >= 1000 then return True else return False
        PlayerTwo -> do
            score <- p1Score <$> get
            if score >= 1000 then return True else return False

--calculate the score assuming the last player to play won
calculateScore :: Game Int
calculateScore = do
    player <- nextPlayer <$> get
    case player of
        PlayerOne -> do
            score <- p1Score <$> get
            count <- timesRolled <$> get
            return $ score * count 
        PlayerTwo -> do
            score <- p2Score <$> get
            count <- timesRolled <$> get
            return $ score * count 

runGame :: Game Int
runGame = (doTurn `untilM` lastPlayerWon) >> calculateScore

part1 :: Int
part1 = evalState runGame gameStart

-- given a player start location and turn, return a map of how many universes that player wins after
-- taking a particular number of moves
winningStates :: Int -> IntMap Int
winningStates loc = go 0 loc 0
  where
    move loc roll = ((loc - 1 + roll) `mod` 10) + 1
    go score loc turns 
        | score >= 21 = IntMap.singleton turns 1 
        | otherwise = IntMap.unionsWith (+) 
            [go (score + move loc x) (move loc x) (turns+1)
            | x <- threeRolls]

threeRolls :: [Int]
threeRolls = [x+y+z | x <- [1,2,3], y <- [1,2,3], z <- [1,2,3]]

-- after n turns, how many possible move sequences could have been taken where the player
-- didn't win given the count of winning universes in the map
countHasntWon :: Int -> IntMap Int -> Int
countHasntWon turn wins = let (possibleWins, _) = IntMap.split (turn+1) wins in
    (27^turn) - IntMap.foldrWithKey (\key count acc -> acc + count * 27^(turn-key)) 0 possibleWins

-- calculate the number of universes where one player beats the other starting on a given square
calcNumWins :: Int -> Int -> (Int, Int)
calcNumWins p1 p2 = let
    p1WinStates = winningStates p1
    p2WinStates = winningStates p2
    -- player one wins in every universe where player one hasn't won after taking 1 less turn
    p1Wins = IntMap.foldrWithKey 
        (\turn count acc -> acc + count * countHasntWon (turn-1) p2WinStates)
        0
        p1WinStates
    -- player two wins in every universe where player one hasn't won after taking the same number of turns
    p2Wins = IntMap.foldrWithKey 
        (\turn count acc -> acc + count * countHasntWon turn p1WinStates)
        0
        p2WinStates
  in
    (p1Wins, p2Wins)

part2 :: Int
part2 = uncurry max $ calcNumWins p1start p2start

