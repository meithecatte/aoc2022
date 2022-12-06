{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Maybe
import Data.Char
import Control.Lens.Operators
import Control.Lens.At

isSep :: Int -> String -> Bool
isSep len = (== len) . length . nub
lastN len s = drop (length s - len) s

part1 = length . fromJust . find (isSep 4 . lastN 4) . inits
part2 = length . fromJust . find (isSep 14 . lastN 14) . inits

testcase :: String -> IO ()
testcase input = do
    putStrLn (take 30 input)
    let p1 = part1 input
    putStrLn ("Part 1: " ++ show p1)
    let p2 = part2 input
    putStrLn ("Part 2: " ++ show p2)

main = do
    testcase "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    testcase "bvwbjplbgvbhsrlpgdmjqwftvncz"
    testcase "nppdvjthqldpwncqszvftbrmjlhg"
    testcase "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    testcase "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    input <- readFile "puzzle.in"
    testcase input
