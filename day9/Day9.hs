{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List
import Data.Maybe
import Data.Char

tread :: Read a => Text -> a
tread = read . T.unpack

delta :: Text -> (Int, Int)
delta "R" = (1, 0)
delta "L" = (-1, 0)
delta "U" = (0, 1)
delta "D" = (0, -1)

parse t = foldMap parseLine $ T.lines t
parseLine line =
    let [dir, count] = T.words line in
    replicate (tread count) dir

near (hx, hy) (tx, ty) = hx - 1 <= tx && tx <= hx + 1 && hy - 1 <= ty && ty <= hy + 1
follow h t | near h t = t
follow h@(hx, hy) t@(tx, ty) = (x (), y ())
    where
    x () | hx == tx = tx
    x () | hx < tx = tx - 1
    x () | hx > tx = tx + 1

    y () | hy == ty = ty
    y () | hy < ty = ty - 1
    y () | hy > ty = ty + 1

type Pos = (Int, Int)
type State = (Pos, Pos, Set Pos)

part1 input =
    let (h, t, ts) = foldl' doStep ((0,0), (0,0), Set.fromList [(0,0)]) input in
    Set.size ts
    where
    doStep :: State -> Text -> State
    doStep ((hx, hy), t, ts) step =
        let (dx, dy) = delta step in
        let h = (hx + dx, hy + dy) in
        let t' = follow h t in
        (h, t', Set.insert t' ts)

simulate [] = []
simulate [x] = [x]
simulate (h:t:xs) =
    let t' = follow h t in
    h:simulate (t':xs)

type State' = ([Pos], Set Pos)
part2 input =
    let (rope, ts) = foldl' doStep (replicate 10 (0, 0), Set.fromList [(0,0)]) input in
    Set.size ts
    where
    doStep :: State' -> Text -> State'
    doStep ((hx, hy):rope, ts) step =
        let (dx, dy) = delta step in
        let h' = (hx + dx, hy + dy) in
        let rope' = simulate (h':rope) in
        (rope', Set.insert (last rope') ts)

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
    testcase "example2.in"
    testcase "puzzle.in"
