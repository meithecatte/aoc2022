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
import Data.Array.ST
import Control.Monad.ST
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Sequence (Seq(..), (><))
import Debug.Trace

tread :: Read a => Text -> a
tread = read . T.unpack

type Pos = (Int, Int)
type Line = [Pos]
type Grid s = STUArray s Pos Bool

parse :: Text -> [Line]
parse t = parseLine <$> T.lines t
    where
    parseLine t = parsePos <$> T.splitOn " -> " t
    parsePos t = let [x, y] = T.splitOn "," t in
                    (tread x, tread y)

dims lines = ((minX-1, 0), (maxX+1, maxY))
    where
    points = concat lines
    minX = minimum $ map fst points
    maxX = maximum $ map fst points
    maxY = maximum $ map snd points

betw a b | a <= b = [a..b]
betw a b | otherwise = [b..a]

seg :: Pos -> Pos -> [Pos]
seg (x1, y1) (x2, y2) | x1 == x2 = (x1,) <$> betw y1 y2
seg (x1, y1) (x2, y2) | y1 == y2 = (,y1) <$> betw x1 x2

drawSegment :: Grid s -> Pos -> Pos -> ST s ()
drawSegment grid a b = mapM_ (\p -> writeArray grid p True) (seg a b)

drawLine :: Grid s -> [Pos] -> ST s ()
drawLine grid (a:b:ps) = do
    drawSegment grid a b
    drawLine grid (b:ps)
drawLine grid _ = return ()

drawInput :: [Line] -> ST s (Grid s, Int)
drawInput lines = do
    let dim = dims lines
    grid <- newArray dim False
    mapM_ (drawLine grid) lines
    return (grid, snd $ snd dim)

nextPoints (x, y) = [(x, y+1), (x-1, y+1), (x+1, y+1)]

findEmpty :: Grid s -> [Pos] -> ST s (Maybe Pos)
findEmpty grid [] = return Nothing
findEmpty grid (p:ps) = do
    taken <- readArray grid p
    if taken then
        findEmpty grid ps
    else
        return $ Just p

path :: Grid s -> Int -> [Pos] -> ST s (Maybe [Pos])
path grid maxY ps@((x, y):_) | y >= maxY = return Nothing
path grid maxY ps@(p:_) = do
    falling <- findEmpty grid (nextPoints p)
    case falling of
        Just p' -> path grid maxY (p':ps)
        Nothing -> return $ Just ps

dropSand :: Grid s -> Int -> [Pos] -> ST s (Maybe [Pos])
dropSand grid maxY ps = do
    ps' <- path grid maxY ps
    case ps' of
        Just (p:ps'') -> do
            writeArray grid p True
            return $ Just ps''
        Nothing -> return Nothing

dropSands :: Grid s -> Int -> Int -> [Pos] -> ST s Int
dropSands grid maxY !count ps = do
    ps' <- dropSand grid maxY ps
    case ps' of
        Just ps'' -> dropSands grid maxY (count + 1) ps''
        Nothing -> return count

part1 lines = do
    (grid, maxY) <- drawInput lines
    dropSands grid maxY 0 [(500,0)]

testcase :: FilePath -> IO ()
testcase fname = do
    putStrLn fname
    input <- parse <$> T.readFile fname
    let p1 = runST $ part1 input
    putStrLn ("Part 1: " ++ show p1)

main = do
    testcase "example.in"
    testcase "puzzle.in"
