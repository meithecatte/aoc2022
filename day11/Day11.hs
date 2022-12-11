{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Array.IArray (Array, elems)
import Control.Monad.ST

tread :: Read a => Text -> a
tread = read . T.unpack

data Monkey = Monkey
    { monkeyNum :: Int
    , items :: [Integer]
    , operation :: Integer -> Integer
    , test :: Integer
    , ifTrue :: Int
    , ifFalse :: Int
    }

parseOp :: Text -> Integer -> Integer -> Integer
parseOp "+" = (+)
parseOp "*" = (*)

parseArg :: Text -> Integer -> Integer
parseArg "old" = id
parseArg n = const (tread n)

parseMonkey :: Text -> Monkey
parseMonkey t =
    let [
            (T.stripPrefix "Monkey " -> Just monkeyNum),
            (T.stripPrefix "  Starting items: " -> Just items),
            (T.stripPrefix "  Operation: new = " -> Just operation),
            (T.stripPrefix "  Test: divisible by " -> Just test),
            (T.stripPrefix "    If true: throw to monkey " -> Just ifTrue),
            (T.stripPrefix "    If false: throw to monkey " -> Just ifFalse)
            ] = T.lines t in
    let [lhs, op, rhs] = T.words operation in
    Monkey
        { monkeyNum = tread $ fromJust $ T.stripSuffix ":" monkeyNum
        , items = tread <$> T.splitOn ", " items
        , operation = \old -> parseOp op (parseArg lhs old) (parseArg rhs old)
        , test = tread test
        , ifTrue = tread ifTrue
        , ifFalse = tread ifFalse
        }

reducer monkeys =
    let m = foldl' lcm 1 $ test <$> monkeys in
    \v -> v `mod` m

type Items s = STArray s Int [Integer]
type Activity s = STUArray s Int Int
type St s = (Items s, Activity s, Integer -> Integer)

modifyArray :: MArray a e m => Ix i => a i e -> i -> (e -> e) -> m ()
modifyArray arr i f = do
    v <- readArray arr i
    writeArray arr i $! f v

doItem :: St s -> Monkey -> Integer -> ST s ()
doItem (items, activity, red) m worry = do
    let n = monkeyNum m
    let !worry' = red $ operation m worry
    let next = if worry' `mod` test m == 0 then ifTrue m else ifFalse m
    modifyArray items next (worry':)
    modifyArray activity n (+1)

doMonkey :: St s -> Monkey -> ST s ()
doMonkey s@(items, activity, red) m = do
    let n = monkeyNum m
    xs <- readArray items n
    writeArray items n []
    mapM_ (doItem s m) (reverse xs)

doRound :: St s -> [Monkey] -> ST s ()
doRound s ms = mapM_ (doMonkey s) ms

doRounds :: (Integer -> Integer) -> [Monkey] -> Int -> ST s (Activity s)
doRounds red ms n = do
    let lastN = length ms - 1
    monkeyItems <- newListArray (0, lastN) ((reverse . items) <$> ms)
    activity <- newArray (0, lastN) 0
    mapM_ (const $ doRound (monkeyItems, activity, red) ms) [1..n]
    return activity

parse :: Text -> [Monkey]
parse t = parseMonkey <$> T.splitOn "\n\n" t

part1 monkeys = a0 * a1
    where
    activity = elems $ runSTUArray (doRounds (`div` 3) monkeys 20)
    a0:a1:_ = reverse $ sort activity

part2 monkeys = a0 * a1
    where
    activity = elems $ runSTUArray (doRounds (reducer monkeys) monkeys 10000)
    a0:a1:_ = reverse $ sort activity

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
    --testcase "example2.in"
    testcase "puzzle.in"
