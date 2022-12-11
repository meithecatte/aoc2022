{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List
import Data.Maybe
import Data.Char

tread :: Read a => Text -> a
tread = read . T.unpack

data Instr
    = Noop
    | Addx Int
    deriving (Show)

parseInstr instr = doParse w1
    where
    (w1:words) = T.words instr
    doParse "noop" = Noop
    doParse "addx" = Addx $ tread (head words)

parse t = parseInstr <$> T.lines t

execInstrs :: [Instr] -> Int -> [Int]
execInstrs [] x = [x]
execInstrs (Noop:prog) x = x:execInstrs prog x
execInstrs ((Addx v):prog) x = x:x:execInstrs prog (x+v)

part1 instrs = sum $ strengthAt <$> indices
    where
    trace = execInstrs instrs 1
    indices = takeWhile (<= length trace) [20,60..]
    strengthAt t = t * trace !! (t - 1)

spriteBounds x = [x - 1..x + 1]

part2 instrs = unlines $ line <$> [0..5]
    where
    trace = execInstrs instrs 1
    checkCoords x y = x `elem` spriteBounds (trace !! (40 * y + x))
    pixel True = '#'
    pixel False = '.'
    line y = [pixel (checkCoords x y) | x <- [0..39]]

testcase :: FilePath -> IO ()
testcase fname = do
    putStrLn fname
    input <- parse <$> T.readFile fname
    let p1 = part1 input
    putStrLn ("Part 1: " ++ show p1)
    let p2 = part2 input
    putStrLn ("Part 2:\n" ++ p2)

main = do
    --testcase "example.in"
    testcase "example2.in"
    testcase "puzzle.in"
