#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package split-0.2.3.5

------------------------------------
------------------------------------
----  Day 1:  Calorie Counting  ----
------------------------------------
------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 -- '.\day1.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day1part1
-- 71471

-- *Main> day1part2
-- 211189


-------------
-- Imports --
-------------
import Data.List (sort)
import Data.List.Split (splitOn)


-------------
-- Program --
-------------
main = day1part2

day1part1 = do
  contents <- readFile "day1 (data).csv"
  let totals = map sum . map (map read) . splitOn [""] . lines $ contents
  print $ maximum totals
  
  -- mapM_ print . sort $ totals  -- Sanity Check

day1part2 = do
  contents <- readFile "day1 (data).csv"
  let totals = map sum . map (map read) . splitOn [""] . lines $ contents
  let top3Total = sum . take 3 . reverse . sort $ totals
  print top3Total
  
  -- print . take 3 . reverse . sort $ totals  -- Sanity Check