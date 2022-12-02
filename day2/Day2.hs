{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.List
import Data.Maybe

data RPS = Rock | Paper | Scissors
data XYZ = X | Y | Z

parseLeft :: Text -> RPS
parseLeft "A" = Rock
parseLeft "B" = Paper
parseLeft "C" = Scissors

parseRight :: Text -> XYZ
parseRight "X" = X
parseRight "Y" = Y
parseRight "Z" = Z

data RoundResult = Win | Lose | Draw
    deriving Eq

resolve :: RPS -> RPS -> RoundResult
resolve Rock Rock = Draw
resolve Rock Paper = Win
resolve Rock Scissors = Lose
resolve Paper Rock = Lose
resolve Paper Paper = Draw
resolve Paper Scissors = Win
resolve Scissors Rock = Win
resolve Scissors Paper = Lose
resolve Scissors Scissors = Draw

resultPoints :: RoundResult -> Int
resultPoints Win = 6
resultPoints Lose = 0
resultPoints Draw = 3

rpsPoints :: RPS -> Int
rpsPoints Rock = 1
rpsPoints Paper = 2
rpsPoints Scissors = 3

parseRound :: Text -> (RPS, XYZ)
parseRound t =
    let [a, b] = T.words t in
    (parseLeft a, parseRight b)

parse :: Text -> [(RPS, XYZ)]
parse = map parseRound . T.lines

roundPoints :: (RPS, RPS) -> Int
roundPoints (opp, us) = rpsPoints us + resultPoints (resolve opp us)

part1 :: [(RPS, XYZ)] -> Int
part1 = sum . map round
    where
    round (opp, s) = roundPoints (opp, strat s)

    strat X = Rock
    strat Y = Paper
    strat Z = Scissors

part2 :: [(RPS, XYZ)] -> Int
part2 = sum . map round
    where
    round (opp, s) = roundPoints (opp, strat opp s)

    rpsForResult opp res = fromJust $ find (\us -> resolve opp us == res) opts
    opts = [Rock, Paper, Scissors]

    wantedResult X = Lose
    wantedResult Y = Draw
    wantedResult Z = Win

    strat opp s = rpsForResult opp (wantedResult s)
testcase :: FilePath -> IO ()
testcase fname = do
    putStrLn fname
    input <- parse <$> T.readFile fname
    let p1 = part1 input
    putStrLn ("Part 1: " ++ show p1)
    let p2 = part2 input
    putStrLn ("Part 2: " ++ show p2)

main = do
    testcase "example.in"
    testcase "puzzle.in"
