{-# LANGUAGE TupleSections #-}
import Data.List
import Data.Maybe
import Data.Char
import Control.Lens.Operators
import Control.Lens.At

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p [] = []
takeUntil p (x:xs) | p x = [x]
takeUntil p (x:xs) | otherwise = x:takeUntil p xs

part1 input = length $ filter visible allXY
    where
    height = length input
    width = length (head input)

    at (x, y) = input !! y !! x
    leftOf (x, y) = (, y) <$> [0..x - 1]
    rightOf (x, y) = (, y) <$> [x+1..width-1]
    above (x, y) = (x, ) <$> [0..y - 1]
    below (x, y) = (x, ) <$> [y + 1..height-1]

    visibleFrom xy f = all (< at xy) $ at <$> f xy
    visible xy = any (visibleFrom xy) $ [leftOf, rightOf, above, below]

    allXY = [(x, y) | x <- [0..width-1], y <- [0..height-1]]

part2 input = maximum $ score <$> allXY
    where
    height = length input
    width = length (head input)

    at (x, y) = input !! y !! x
    leftOf (x, y) = reverse $ (, y) <$> [0..x - 1]
    rightOf (x, y) = (, y) <$> [x+1..width-1]
    above (x, y) = reverse $ (x, ) <$> [0..y - 1]
    below (x, y) = (x, ) <$> [y + 1..height-1]

    visibility xy f = length $ takeUntil (>= at xy) $ at <$> f xy
    score xy = product $ visibility xy <$> [leftOf, rightOf, above, below]

    allXY = [(x, y) | x <- [0..width-1], y <- [0..height-1]]

testcase :: String -> IO ()
testcase fname = do
    putStrLn fname
    input <- lines <$> readFile fname
    let p1 = part1 input
    putStrLn ("Part 1: " ++ show p1)
    let p2 = part2 input
    putStrLn ("Part 2: " ++ show p2)

main = do
    testcase "example.in"
    testcase "puzzle.in"
