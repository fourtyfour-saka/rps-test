import System.Random
import Text.Printf

data Move = Rock | Paper | Scissors deriving (Show, Enum, Bounded)
data Result = P1Win | P2Win | Draw deriving (Show, Eq)

instance Random Move where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

type Wins = Int
type Strategy = [Move]

-------------
-- Strategies
-------------

randomStrat :: RandomGen g => g -> Strategy
randomStrat g = take 100 $ (randoms g :: [Move])

scissorsStrat :: Strategy
scissorsStrat = replicate 100 Scissors

rockStrat :: Strategy
rockStrat = replicate 100 Rock

paperStrat :: Strategy
paperStrat = replicate 100 Paper


-------------
-- Game Logic
-------------

playMove :: Move -> Move -> Result
playMove p1 p2 = case (p1, p2) of
    (Rock, Paper)     -> P2Win
    (Rock, Scissors)  -> P1Win
    (Paper, Rock)     -> P1Win
    (Paper, Scissors) -> P2Win
    (Scissors, Rock)  -> P2Win
    (Scissors, Paper) -> P1Win
    (_, _)            -> Draw

----------
-- Run it!
----------

main = do
    g <- newStdGen
    g' <- newStdGen
    let player1 = randomStrat g
    let player2 = randomStrat g'
    -- print $ player1
    -- print $ player2
    let results = filter (\x -> x /= Draw) $ zipWith playMove player1 player2
    let p1wins = 100 * (fromIntegral $ length $ filter (\x -> x == P1Win) results) / (fromIntegral $ length results)
    let p2wins = 100 * (fromIntegral $ length $ filter (\x -> x == P2Win) results) / (fromIntegral $ length results)
    printf "P1: %.2f%% | P2: %.2f%%\n" (p1wins :: Float) (p2wins :: Float)
