{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace

parseDigit '=' = -2
parseDigit '-' = -1
parseDigit '0' = 0
parseDigit '1' = 1
parseDigit '2' = 2

unparseDigit (-2) = '='
unparseDigit (-1) = '-'
unparseDigit 0 = '0'
unparseDigit 1 = '1'
unparseDigit 2 = '2'

parseNum s = go $ reverse s
    where
    go "" = 0
    go (x:xs) = parseDigit x + 5 * go xs

unparseNum n = reverse $ go n
    where
    go 0 = ""
    go n = unparseDigit digit:go ((n - digit) `div` 5)
        where
        digit = [0, 1, 2, -2, -1] !! (n `mod` 5)

parse s = parseNum <$> lines s

part1 = unparseNum . sum

testcase :: FilePath -> IO ()
testcase fname = do
    putStrLn fname
    input <- parse <$> readFile fname
    let p1 = part1 input
    putStrLn ("Part 1: " ++ show p1)
    --let p2 = part2 input
    --putStrLn ("Part 2: " ++ show p2)

main = do
    testcase "example.in"
    testcase "puzzle.in"
