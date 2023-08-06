#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci
----------------------------------
----------------------------------
----  Day 6:  Tuning Trouble  ----
----------------------------------
----------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 -- '.\day6.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day6part1
-- 1578

-- *Main> day6part2
-- 2178


-------------
-- Imports --
-------------
import Data.List
import Data.Maybe (fromJust)


-------------
-- Program --
-------------
main = day6part2

day6part1 = do
      contents <- readFile "day6 (data).csv"
      print . parseN 4 $ contents

day6part2 = do
      contents <- readFile "day6 (data).csv"
      print . parseN 14 $ contents

parseN n = (+n) . fromJust . findIndex ((==n) . length . group . sort . take n) . tails