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
import Data.Array.ST
import Control.Monad.ST
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Sequence (Seq(..), (><))
import Debug.Trace
import Text.Regex.TDFA

tread :: Read a => Text -> a
tread = read . T.unpack

type Pos = (Int, Int)

data Sensor = Sensor
    { sensor :: Pos
    , beacon :: Pos
    }
    deriving Show

parse t = parseLine <$> lines t
    where
    parseLine :: String -> Sensor
    parseLine t =
        let [sx, sy, bx, by] = mrSubList $ t =~ "Sensor at x=([-0-9]+), y=([-0-9]+): closest beacon is at x=([-0-9]+), y=([-0-9]+)" in
        Sensor
        { sensor = (read sx, read sy)
        , beacon = (read bx, read by)
        }

radius :: Sensor -> Int
radius Sensor { sensor = (x1, y1), beacon = (x2, y2) } =
    abs (x1-x2) + abs (y1-y2)

rangeAt :: Int -> Sensor -> Maybe (Int, Int)
rangeAt targetY s =
    let (x, y) = sensor s in
    let r = radius s in
    let dy = abs (targetY - y) in
    if dy <= r then
        let r' = r - dy in
        Just (x - r', x + r')
    else
        Nothing

merge = mergeSorted . sort

mergeSorted [] = []
mergeSorted ((l, r):xs) = go l r xs
    where
    go l r [] = [(l, r)]
    go l r ((l', r'):xs) | l' <= r + 1 = go l (max r r') xs
    go l r ((l', r'):xs) | otherwise   = (l, r):go l' r' xs

size (a, b) = b - a + 1

nub' = map head . group . sort

beaconsAt y sensors = nub' $ filter ((== y) . snd) $ map beacon sensors

scannedAt y sensors = merge $ mapMaybe (rangeAt y) sensors

part1 y sensors = scanned - found
    where
    scanned = sum $ map size $ scannedAt y sensors
    found = length $ beaconsAt y sensors

findAt sensors y =
    case scannedAt y sensors of
        [x] -> Nothing
        [(_, a), (b, _)] | a + 2 == b -> Just (a+1, y)

encode (x, y) = 4000000 * x + y

part2 maxY sensors = encode <$> mapMaybe (findAt sensors) [0..maxY]

testcase :: Int -> Int -> FilePath -> IO ()
testcase targetY maxY fname = do
    putStrLn fname
    input <- parse <$> readFile fname
    let p1 = part1 targetY input
    putStrLn ("Part 1: " ++ show p1)
    let p2 = part2 maxY input
    putStrLn ("Part 2: " ++ show p2)

main = do
    testcase 10 20 "example.in"
    testcase 2000000 4000000 "puzzle.in"
