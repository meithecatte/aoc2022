{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.List
import Data.Maybe
import Data.Char
import Control.Lens.Operators
import Control.Lens.At

tread :: Read a => Text -> a
tread = read . T.unpack

data Move = Move
    { count :: Int
    , from :: Int
    , to :: Int
    }
    deriving (Show)

parseState :: Text -> [[Char]]
parseState t =
    let lines = T.lines t in
    let stacks = init lines in
    let labels = last lines in
    let stackCount = length $ T.words labels in
    parseColumns stackCount stacks

    where
    takeColumn = map (T.take 4)
    dropColumn = map (T.drop 4)

    uncrate :: String -> Maybe Char
    uncrate "" = Nothing
    uncrate ('[':c:"]") = Just c

    parseColumn :: [Text] -> [Char]
    parseColumn ts = catMaybes $ uncrate <$> T.unpack <$> T.strip <$> ts

    parseColumns :: Int -> [Text] -> [[Char]]
    parseColumns 0 _ = []
    parseColumns n stacks =
        let col = parseColumn $ takeColumn stacks in
        col:parseColumns (n - 1) (dropColumn stacks)

parseMove :: Text -> Move
parseMove t =
    let ["move", count, "from", from, "to", to] = T.words t in
    Move
    { count = tread count
    , from = tread from
    , to = tread to
    }

parse t =
    let [state, moves] = T.splitOn "\n\n" t in
    (parseState state, parseMove <$> T.lines moves)

moveCrate a b st =
    let crate = head $ st !! a in
    st & ix a %~ tail & ix b %~ (crate:)

moveCrates n a b st =
    let crates = take n $ st !! a in
    st & ix a %~ drop n & ix b %~ (crates ++)

part1 (state, moves) = head <$> foldl doMove state moves
    where
    doMove state Move { count = count, from = from, to = to } =
        iterate (moveCrate (from - 1) (to - 1)) state !! count

part2 (state, moves) = head <$> foldl doMove state moves
    where
    doMove state Move { count = count, from = from, to = to } =
        moveCrates count (from - 1) (to - 1) state

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
