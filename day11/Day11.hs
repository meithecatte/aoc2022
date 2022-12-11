{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.Trans.State.Strict
import Control.Lens.Operators
import Control.Lens.At
import Control.Lens.Tuple

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

type Items = [[Integer]]
type Activity = [Int]
type St = (Items, Activity)

doItem :: Monkey -> Integer -> State St ()
doItem m worry = do
    let n = monkeyNum m
    let worry' = operation m worry `div` 3
    let next = if worry' `mod` test m == 0 then ifTrue m else ifFalse m
    modify (_1 . ix next %~ (++ [worry']))
    modify (_2 . ix n %~ (+1))

doMonkey :: Monkey -> State St ()
doMonkey m = do
    let n = monkeyNum m
    items <- gets $ (!! n) . fst
    modify (_1 . ix n .~ [])
    mapM_ (doItem m) items

doRound :: [Monkey] -> State St ()
doRound ms = mapM_ doMonkey ms

parse :: Text -> [Monkey]
parse t = parseMonkey <$> T.splitOn "\n\n" t

part1 monkeys = a0 * a1
    where
    s0 :: St
    s0 = (items <$> monkeys, const 0 <$> monkeys)

    doRounds :: State St ()
    doRounds = mapM_ (const $ doRound monkeys) [1..20]

    (s', activity) = execState doRounds s0
    a0:a1:_ = reverse $ sort activity

testcase :: FilePath -> IO ()
testcase fname = do
    putStrLn fname
    input <- parse <$> T.readFile fname
    let p1 = part1 input
    putStrLn ("Part 1: " ++ show p1)
    --let p2 = part2 input
    --putStrLn ("Part 2:\n" ++ p2)

main = do
    testcase "example.in"
    --testcase "example2.in"
    testcase "puzzle.in"
