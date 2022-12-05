{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.List
import Data.Maybe
import Data.Char

halves :: Text -> (Text, Text)
halves s = T.splitAt (T.length s `div` 2) s

parse :: Text -> [(Text, Text)]
parse = map halves . T.lines

common :: (Text, Text) -> [Char]
common (a, b) = intersect (T.unpack a) (T.unpack b)

priority :: Char -> Int
priority c | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
priority c | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 27

part1 = sum . map doLine
    where
    doLine = head . map priority . common

chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = (take n xs):(chunkList n (drop n xs))

doGroup [a, b, c] = head $ intersect a (intersect b c)

part2 = sum . map priority . map doGroup . chunkList 3 . map T.unpack . T.lines

testcase :: FilePath -> IO ()
testcase fname = do
    putStrLn fname
    input <- T.readFile fname
    let p1 = part1 (parse input)
    putStrLn ("Part 1: " ++ show p1)
    let p2 = part2 input
    putStrLn ("Part 2: " ++ show p2)

main = do
    testcase "example.in"
    testcase "puzzle.in"
