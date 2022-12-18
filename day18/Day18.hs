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
import Text.Regex.TDFA
import Data.Foldable
import Control.Monad

tread :: Read a => Text -> a
tread = read . T.unpack

type Pos = (Int, Int, Int)
type Grid s = STUArray s Pos Bool

neighbours :: Pos -> [Pos]
neighbours (x, y, z) = [
                            (x+1, y, z),
                            (x-1, y, z),
                            (x, y+1, z),
                            (x, y-1, z),
                            (x, y, z+1),
                            (x, y, z-1)
                        ]

parse :: Text -> [Pos]
parse t = parseLine <$> T.lines t
    where
    parseLine t = let [x, y, z] = tread <$> T.splitOn "," t in (x, y, z)

getX (x, y, z) = x
getY (x, y, z) = y
getZ (x, y, z) = z

boundingBox :: [Pos] -> (Pos, Pos)
boundingBox cubes = ((minimum (getX <$> cubes) - 1,
                      minimum (getY <$> cubes) - 1,
                      minimum (getZ <$> cubes) - 1),
                     (maximum (getX <$> cubes) + 1,
                      maximum (getY <$> cubes) + 1,
                      maximum (getZ <$> cubes) + 1))

exterior :: [Pos] -> ST s (Grid s)
exterior cubes = do
    grid <- newArray bbox False
    writeArray grid (fst bbox) True
    go grid [fst bbox]
    return grid

    where
    bbox = boundingBox cubes
    shape = Set.fromList cubes

    newExterior :: Grid s -> Pos -> ST s Bool
    newExterior grid p =
        if inRange bbox p && Set.notMember p shape then
            not <$> readArray grid p
        else
            return False

    go :: Grid s -> [Pos] -> ST s ()
    go grid [] = return ()
    go grid (p:ps) = do
        newPoints <- filterM (newExterior grid) $ neighbours p
        mapM_ (\p -> writeArray grid p True) newPoints
        go grid (newPoints ++ ps)

part1 cubes = sum $ map surfaceOf cubes
    where
    shape = Set.fromList cubes
    surfaceOf cube = length $ filter (flip Set.notMember shape) $ neighbours cube

part2 cubes = runST $ do
    ext <- exterior cubes
    walls <- filterM (readArray ext) $ concatMap neighbours cubes
    return $ length walls

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
