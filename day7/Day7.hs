{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.List
import Data.Maybe
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad

tread :: Read a => Text -> a
tread = read . T.unpack

type Name = Text
type Path = [Name]

data FS
    = File Integer
    | UnkDir
    | Dir (Map Name FS)
    deriving Show

-- I tried to use lenses for this and lost
-- an hour of my life I'll never get back
modifyPath :: (FS -> FS) -> Path -> FS -> FS
modifyPath f [] fs = f fs
modifyPath f (x:xs) (Dir m) =
    if Map.member x m
        then Dir $ Map.adjust (modifyPath f xs) x m
        else error ("can't find: " ++ show x)
modifyPath f (x:xs) fs = fs

applyContents p fs contents = modifyPath setContents p fs
    where
    setContents UnkDir = Dir (Map.fromList entries)
    entries = parseEntry <$> contents

    parseEntry ent =
        let [desc, name] = T.words ent in
        let val = case desc of
                    "dir" -> UnkDir
                    s -> File $ tread s
        in (name, val)

parseFS :: Path -> FS -> [Text] -> FS
parseFS _ fs [] = fs
parseFS _ fs ("$ cd /":rest) = parseFS [] fs rest
parseFS p fs ("$ cd ..":rest) = parseFS (init p) fs rest
parseFS p fs ((T.stripPrefix "$ cd " -> Just dir):rest) =
    parseFS (p ++ [dir]) fs rest
parseFS p fs ("$ ls":rest) =
    let (contents, rest') = break (T.isPrefixOf "$ ") rest in
    let fs' = applyContents p fs contents in
    parseFS p fs' rest'

traversal fs@(Dir m) = fs:(foldMap traversal m)
traversal fs = [fs]

size (File s) = s
size (Dir m) = foldl' (+) 0 (size <$> m)

isDir (Dir _) = True
isDir _ = False

printIndent n = putStr (replicate n ' ')
printFS name indent (File s) = do
    printIndent indent
    putStrLn $ show name ++ " (file " ++ show s ++ ")"
printFS name indent fs@(Dir m) = do
    printIndent indent
    putStrLn $ show name ++ " (dir " ++ show (size fs) ++ ")"
    mapM_ printDir $ Map.toList m
    where
    printDir (name, fs) = printFS name (indent + 2) fs

directories fs = filter isDir $ traversal fs

part1 fs = sum $ filter (<= 100000) $ size <$> directories fs

part2 fs = minimum $ filter (>= neededSize) $ size <$> directories fs
    where
    totalSpace = 70000000
    spaceNeeded = 30000000
    spaceTaken = size fs
    spaceFree = totalSpace - spaceTaken
    neededSize = spaceNeeded - spaceFree

testcase :: FilePath -> IO ()
testcase fname = do
    putStrLn fname
    input <- T.readFile fname
    let fs = parseFS [] UnkDir (T.lines input)
    let p1 = part1 fs
    putStrLn ("Part 1: " ++ show p1)
    let p2 = part2 fs
    putStrLn ("Part 2: " ++ show p2)

main = do
    testcase "example.in"
    testcase "puzzle.in"
