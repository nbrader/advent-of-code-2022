#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package split-0.2.3.5
-------------------------------------------
-------------------------------------------
----  Day 3:  Rucksack Reorganization  ----
-------------------------------------------
-------------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 -- '.\day3.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day3part1
-- 7581

-- *Main> day3part2
-- 2525


-------------
-- Imports --
-------------
import Data.List.Split (splitOn, chunksOf)
import Data.Char (ord,toUpper)


-------------
-- Program --
-------------
main = day3part2

day3part1 = do
      contents <- readFile "day3 (data).csv"
      let totalScore = sum . map priority . map (\(xs,ys) -> head $ filter (`elem` ys) xs) . map (\xs -> let len = length xs in (take (len `div` 2) xs, drop (len `div` 2) xs)) . lines $ contents
      print totalScore

day3part2 = do
      contents <- readFile "day3 (data).csv"
      let totalScore = sum . map priority . map (\[xs,ys,zs] -> head . filter (`elem` zs) . filter (`elem` ys) $ xs) . chunksOf 3 . lines $ contents
      print totalScore

priority c | n > 96    = n-96
           | otherwise = n-65+27
  where n = ord c