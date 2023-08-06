#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package split-0.2.3.5 --package text-2.0.1
---------------------------------
---------------------------------
----  Day 5:  Supply Stacks  ----
---------------------------------
---------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 --package text-2.0.1 -- '.\day5.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day5part1
-- SPFMVDTZT

-- *Main> day5part2
-- ZFSJBPRFP


-------------
-- Imports --
-------------
import Data.List (transpose)
import Data.List.Split (splitOn, chunksOf)
import Data.Char (ord,toUpper)
import Data.Text hiding (take, drop, foldr, replicate, words, map, reverse, lines, concat, head, splitOn, chunksOf, last, init, read, transpose, dropWhile)
import Debug.Trace (trace)


-------------
-- Program --
-------------
main = day5part2

day5part1 = do
      contents <- readFile "day5 (data).csv"
      let (initStacks, instructionRows) = parseData contents
      let finalStacks = concat . map (take 1) . ($ initStacks) . foldr (.) id . reverse . map parseLine $ instructionRows
      putStrLn . concat . map show $ finalStacks
      
day5part2 = do
      contents <- readFile "day5 (data).csv"
      let (initStacks, instructionRows) = parseData contents
      let finalStacks = concat . map (take 1) . ($ initStacks) . foldr (.) id . reverse . map parseLineKeepOrder $ instructionRows
      putStrLn . concat . map show $ finalStacks

replace' old new input = unpack $ replace (pack old) (pack new) (pack input)

data Crate = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z deriving (Show, Read)

-- initStacks = [[W,T,H,P,J,C,F],[H,B,J,Z,F,V,R,G],[R,T,P,H],[T,H,P,N,S,Z],[D,C,J,H,Z,F,V,N],[Z,D,W,F,G,M,P],[P,D,J,S,W,Z,V,M],[S,D,N],[M,F,S,Z,D]]

replaceInList i new stacks = take i stacks ++ [new] ++ drop (i+1) stacks

move i j stacks = let movedAndOldPlace = stacks !! i
                   in case movedAndOldPlace of
                        (moved:oldPlace) -> let newPlace = moved:(stacks !! j) in replaceInList j newPlace . replaceInList i oldPlace $ stacks
moveN n i j = foldr (.) id $ replicate n (move i j)
moveNKeepOrder n i j stacks = let oldPlace = stacks !! i
                                  newPlace = stacks !! j
                                in replaceInList j (take n oldPlace ++ newPlace) . replaceInList i (drop n $ oldPlace) $ stacks

parseData :: String -> ([[Crate]],[String])
parseData = (\[stackRows,instructionRows] -> (getStacks stackRows, instructionRows)) . splitOn [[]] . lines
  where getStacks :: [String] -> [[Crate]]
        getStacks inRows = map (map (read . (:[])) . dropWhile (== ' ')) . map head . chunksOf 2 . map last . chunksOf 2 . transpose . init $ inRows

parseLine :: String -> [[Crate]] -> [[Crate]]
parseLine = (\[n,i,j] -> moveN n (i-1) (j-1)) . map (read :: String -> Int) . words . replace' "to " "" . replace' "from " "" . drop 5
parseLineKeepOrder = (\[n,i,j] -> moveNKeepOrder n (i-1) (j-1)) . map (read :: String -> Int) . words . replace' "to " "" . replace' "from " "" . drop 5