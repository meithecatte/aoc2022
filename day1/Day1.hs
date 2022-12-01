{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.List

tread :: Read a => Text -> a
tread = read . T.unpack

parseGroup :: Text -> [Int]
parseGroup = map tread . T.splitOn "\n"

parse :: Text -> [[Int]]
parse = map parseGroup . T.splitOn "\n\n" . T.strip

part1 :: [[Int]] -> Int
part1 = maximum . map sum

part2 :: [[Int]] -> Int
part2 = sum . take 3 . reverse . sort . map sum

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
