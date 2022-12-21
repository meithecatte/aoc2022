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
import Data.Array.ST
import Control.Monad.ST
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Sequence (Seq(..), (><))
import Debug.Trace
import Text.Regex.TDFA
import Data.Foldable
import Control.Monad
import Data.Function.Memoize

tread :: Read a => Text -> a
tread = read . T.unpack

parse :: Text -> [Int]
parse t = tread <$> T.lines t

shift :: Seq (Int, Int) -> (Int, Int) -> Seq (Int, Int)
shift xs x@(_, s) = Seq.insertAt pos' x xs'
    where
    pos = fromJust $ Seq.elemIndexL x xs
    xs' = Seq.deleteAt pos xs
    pos' = (pos + s) `mod` Seq.length xs'

withIndices = Seq.mapWithIndex (,)
withoutIndices = fmap snd

mix order xs = foldl shift xs order

part1 xs = sum [get 1000, get 2000, get 3000]
    where
    xs' = withIndices $ Seq.fromList xs
    xs'' = withoutIndices $ mix xs' xs'
    pos0 = fromJust $ Seq.elemIndexL 0 xs''
    get delta = Seq.index xs'' $ (pos0 + delta) `mod` length xs''

part2 xs = sum [get 1000, get 2000, get 3000]
    where
    xs' = withIndices $ Seq.fromList $ (* 811589153) <$> xs
    xs'' = withoutIndices $ iterate (mix xs') xs' !! 10
    pos0 = fromJust $ Seq.elemIndexL 0 xs''
    get delta = Seq.index xs'' $ (pos0 + delta) `mod` length xs''

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
