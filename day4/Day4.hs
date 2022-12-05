{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.List
import Data.Maybe
import Data.Char

tread :: Read a => Text -> a
tread = read . T.unpack

type Range = (Int, Int)

parseRange :: Text -> Range
parseRange t =
    let [a, b] = tread <$> T.splitOn "-" t in
    (a, b)

parseLine :: Text -> (Range, Range)
parseLine t =
    let [a, b] = parseRange <$> T.splitOn "," t in
    (a, b)

parse :: Text -> [(Range, Range)]
parse = map parseLine . T.lines

contains :: Range -> Range -> Bool
contains (a, b) (c, d) = a <= c && d <= b

part1 = length . filter bad
    where
    bad (r, s) = r `contains` s || s `contains` r

overlaps :: Range -> Range -> Bool
overlaps (a, b) (c, d) = c <= b && b <= d

part2 = length . filter bad
    where
    bad (r, s) = r `overlaps` s || s `overlaps` r

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
