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
import Data.Foldable
import Control.Monad

tread :: Read a => Text -> a
tread = read . T.unpack

type Pos = (Int, Int)
type Grid s = STUArray s Pos Bool

rocks = [
        ((,0) <$> [0..3]),
        [(1,0), (0,1), (1,1), (2,1), (1,2)],
        [(0,0), (1,0), (2,0), (2,1), (2,2)],
        ((0,) <$> [0..3]),
        [(0,0), (0,1), (1,0), (1,1)]
    ]

(x1, y1) +:+ (x2, y2) = (x1+x2, y1+y2)

shift s = map (s +:+)

fits :: Grid s -> [Pos] -> ST s Bool
fits g [] = return True
fits g (p:ps) = do
    bounds <- getBounds g
    if inRange bounds p then do
        taken <- readArray g p
        if taken then
            return False
        else
            fits g ps
    else
        return False

tryMove :: Grid s -> Pos -> Pos -> [Pos] -> ST s (Bool, Pos)
tryMove grid pos delta shape = do
    let pos' = pos +:+ delta
    ok <- fits grid (shift pos' shape)
    if ok then
        return (True, pos')
    else
        return (False, pos)

place :: Grid s -> [Pos] -> Int -> ST s Int
place grid shape usedRows = do
    mapM_ (\p -> writeArray grid p True) shape
    return $ max usedRows $ maximum $ map ((+1) . snd) shape

jetDelta '<' = (-1, 0)
jetDelta '>' = (1, 0)
down = (0, -1)

type SimState = (Int, [Char])

simulate :: Grid s -> SimState -> [Pos] -> ST s (SimState, Int)
simulate grid (usedRows, jet) shape = go (2, usedRows + 3) jet 0
    where
    go pos (m:jet) usedJet = do
        pos' <- snd <$> tryMove grid pos (jetDelta m) shape
        (fell, pos'') <- tryMove grid pos' down shape
        if fell then
            go pos'' jet (usedJet + 1)
        else do
            usedRows' <- place grid (shift pos'' shape) usedRows
            return ((usedRows', jet), usedJet + 1)

parse = T.unpack . T.strip

part1 jet numRocks = runST $ do
    grid <- newArray ((0,0), (6,numRocks*3)) False
    let rocks' = take numRocks $ cycle rocks
    (usedRows, jet') <- foldlM (\s r -> fst <$> simulate grid s r) (0, cycle jet) rocks'
    return usedRows

type LogState = (SimState, [Int])
simWithLogs :: Grid s -> LogState -> [Pos] -> ST s LogState
simWithLogs grid (s, !log) shape = do
    (s', used) <- simulate grid s shape
    return (s', used:log)

chunksOf n [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h:chunksOf n t

isRepeating n xs = let a:b:c:_ = chunksOf n xs in a == b && b == c

part2 jet numRocks = runST $ do
    grid <- newArray ((0,0), (6,20000)) False
    let rocks' = take 10000 $ cycle rocks
    (s, jetSeq) <- foldlM (simWithLogs grid) ((0, cycle jet), []) rocks'
    let chunked = reverse $ sum <$> chunksOf 5 jetSeq
    let partialSums = scanl (+) 0 chunked
    let reducedPartials = map (`mod` length jet) partialSums
    let inv = reverse reducedPartials
    let repeatLength = fromJust $ find (flip isRepeating inv) [1..]
    let repeatStart = fromJust $ findIndex (isRepeating repeatLength) $ tails reducedPartials
    let base = repeatStart * 5
    let dist = repeatLength * 5
    let fbase = part1 jet base
    let fdist = part1 jet (base + dist) - fbase
    let checked = take 10 $ part1 jet <$> [base, base+dist..]
    let expected = take 10 $ [fbase, fbase+fdist..]
    unless (checked == expected) $ error "bad"
    let (repeats, beyond) = (numRocks - base) `divMod` dist
    return $ part1 jet (base + beyond) + repeats * fdist

testcase :: FilePath -> IO ()
testcase fname = do
    putStrLn fname
    input <- parse <$> T.readFile fname
    let p1 = part1 input 2022
    putStrLn ("Part 1: " ++ show p1)
    let p2 = part2 input 1000000000000
    putStrLn ("Part 2: " ++ show p2)

main = do
    testcase "example.in"
    testcase "puzzle.in"
