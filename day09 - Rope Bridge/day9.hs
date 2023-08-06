#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package split-0.2.3.5 --package linear-1.21.10
-------------------------------
-------------------------------
----  Day 9:  Rope Bridge  ----
-------------------------------
-------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 --package linear-1.21.10 -- '.\day9.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day9part1
-- 6081

-- *Main> day9part2
-- 2487


-------------
-- Imports --
-------------
import Linear.Vector
import Linear.V2
import Data.List (sort, group)
import Data.List.Split (splitOn)
import Debug.Trace (trace)


-------------
-- Program --
-------------
main = day9part2

day9part1 = do
    contents <- readFile "day9 (data).csv"
    let headHistory = scanl (+) zero . concat . map parse . lines $ contents
    let tailHistory = reverse . foldr progress zero . reverse $ headHistory
    print . length . group . sort $ tailHistory

day9part2 = do
    contents <- readFile "day9 (data).csv"
    let headHistory = scanl (+) zero . concat . map parse . lines $ contents
    let nextTailHistory = reverse . foldr progress zero . reverse
    print . length . group . sort . (foldl (.) id $ replicate 9 nextTailHistory) $ headHistory

type Pos = V2 Int
type TailHistory = [V2 Int]

progress :: Pos -> TailHistory -> TailHistory
progress headPos [] = [headPos]
progress headPos (tailPos@(V2 x y):tailHistory) = newTailPos:tailPos:tailHistory
  where V2 dx dy = headPos - tailPos
        newTailPos = if dx == 0 && dy == 0 then
                       tailPos -- head and tail are overlapping, so no change
                     else if abs dx <= 1 && abs dy <= 1 then
                       tailPos -- head and tail are adjacent, so no change
                     else if dx == 0 then
                       V2 (x) (y + signum dy) -- move tail along same column
                     else if dy == 0 then
                       V2 (x + signum dx) (y) -- move tail along same row
                     else
                       V2 (x + signum dx) (y + signum dy) -- move tail diagonally

parse :: String -> [V2 Int]
parse = (\[dir, amount] -> toV2 dir (read amount)) . splitOn " "

toV2 :: String -> Int -> [V2 Int]
toV2 "U" amount = replicate amount $ V2   0   1
toV2 "D" amount = replicate amount $ V2   0 (-1)
toV2 "R" amount = replicate amount $ V2   1   0
toV2 "L" amount = replicate amount $ V2 (-1)  0