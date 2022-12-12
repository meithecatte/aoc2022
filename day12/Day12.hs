{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List
import Data.Maybe
import Data.Char
import Control.Lens.Operators
import Control.Lens.At
import Control.Lens.Tuple
import Data.Array
import Control.Monad.ST
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Sequence (Seq(..), (><))

tread :: Read a => Text -> a
tread = read . T.unpack

type Pos = (Int, Int)

parse t = array ((0,0), (width-1,height-1)) listGrid
    where
    lns = lines t
    listGrid = concatMap parseLine $ zip [0..] $ lns
    width = length (head lns)
    height = length lns
    parseLine (y, t) = [((x, y), c) | (x, c) <- zip [0..] t]

neighbours (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

infixl 9 !?
(!?) :: Ix i => Array i e -> i -> Maybe e
grid !? i = if inRange (bounds grid) i then
                Just $ grid ! i
            else
                Nothing

elevation 'S' = 'a'
elevation 'E' = 'z'
elevation c = c

posOf c grid = fst $ fromJust $ find ((== c) . snd) $ assocs grid
possOf c grid = fst <$> filter ((== c) . snd) (assocs grid)

canMove grid from to = fromMaybe False $ canMove' <$> grid !? from <*> grid !? to
    where
    canMove' c c' = elevation c' <= succ (elevation c)

pathLength grid startp end = go (Seq.singleton (end, 0)) (Set.singleton end)
    where
    go :: Seq (Pos, Int) -> Set Pos -> Maybe Int
    go Empty v = Nothing
    go ((pos, dist) :<| q) v | startp pos = Just dist
    go ((pos, dist) :<| q) v = go q' v'
        where
        moves = filter (\a -> canMove grid a pos) $ neighbours pos
        newMoves = filter (flip Set.notMember v) moves
        q' = q >< Seq.fromList ((, dist + 1) <$> newMoves)
        v' = foldl' (flip Set.insert) v newMoves

part1 grid = pathLength grid (== posOf 'S' grid) (posOf 'E' grid)
part2 grid = pathLength grid (\pos -> grid ! pos == 'a') (posOf 'E' grid)

testcase :: FilePath -> IO ()
testcase fname = do
    putStrLn fname
    input <- parse <$> readFile fname
    let p1 = part1 input
    putStrLn ("Part 1: " ++ show p1)
    let p2 = part2 input
    putStrLn ("Part 2: " ++ show p2)

main = do
    testcase "example.in"
    testcase "puzzle.in"
