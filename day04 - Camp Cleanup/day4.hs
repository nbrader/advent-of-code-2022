#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package split-0.2.3.5
--------------------------------
--------------------------------
----  Day 4:  Camp Cleanup  ----
--------------------------------
--------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 -- '.\day4.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day4part1
-- 462

-- *Main> day4part2
-- 835


-------------
-- Imports --
-------------
import Data.List.Split (splitOn, chunksOf)
import Data.Char (ord,toUpper)


-------------
-- Program --
-------------
main = day4part2

day4part1 = do
      contents <- readFile "day4 (data).csv"
      let totalScore = length . filter (id) . map ((\[intvl1, intvl2] -> contains intvl1 intvl2 || contains intvl2 intvl1) . map (map (read :: String -> Int))) . map (map (splitOn "-") . splitOn ",") . lines $ contents
      print totalScore

day4part2 = do
      contents <- readFile "day4 (data).csv"
      let totalScore = length . filter (id) . map ((\[intvl1, intvl2] -> overlap intvl1 intvl2 || overlap intvl2 intvl1) . map (map (read :: String -> Int))) . map (map (splitOn "-") . splitOn ",") . lines $ contents
      print totalScore

contains [x0,x1] [y0,y1] = x0 <= y0 && x1 >= y1
overlap [x0,x1] [y0,y1] = (x0 <= y1 && x0 >= y0) || (x1 <= y1 && x1 >= y0)