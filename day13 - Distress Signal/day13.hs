#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package split-0.2.3.5
-- use the following command to build: stack ghc -- "filename.hs" -O2
------------------------------------
------------------------------------
----  Day 13:  Distress Signal  ----
------------------------------------
------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 -- '.\day13.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day13part1
-- 5938

-- *Main> day13part2
-- 29025


-------------
-- Imports --
-------------
import Data.List (intercalate, reverse, span, sort, intersperse, foldl', findIndex, map, delete, null, concatMap, minimumBy)
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (fromJust, isJust)
import Debug.Trace (trace)


-------------
-- Program --
-------------
main = day13part2

day13part1 = do
    contents <- readFile "day13 (data).csv"
    print . sum . map fst . filter ((\[t1,t2] -> t1 < t2) . snd) . zip [1..] . map (map readTree) . map (take 2) . chunksOf 3 . lines $ contents

day13part2 = do
    contents <- readFile "day13 (data).csv"
    let divider1 = readTree "[[2]]"
    let divider2 = readTree "[[6]]"
    
    let trees = sort . (divider1:) . (divider2:) . map readTree . filter (not . null) . lines $ contents
    let divider1Index = fromJust . findIndex (==divider1) $ trees
    let divider2Index = fromJust . findIndex (==divider2) $ trees
    
    print ((divider1Index+1)*(divider2Index+1))

data IntTree = Leaf Int | Branch [IntTree] deriving (Show, Eq)

takeInners :: String -> [String]
takeInners = reverse . map reverse . snd . foldl' parseChar (0,[[]]) . tail . init
  where parseChar :: (Int,[String]) -> Char -> (Int,[String])
        parseChar (depth, accumStr:accum) c@'['              = ((depth+1), (         (c:accumStr) : accum))
        parseChar (depth, accumStr:accum) c@']'              = ((depth-1), (         (c:accumStr) : accum))
        parseChar (depth, accumStr:accum) c@',' | depth == 0 = ( depth   , ([] :        accumStr  : accum))
        parseChar (depth, accumStr:accum) c                  = ( depth   , (         (c:accumStr) : accum))

readTree :: String -> IntTree
readTree ("[]")   = Branch []
readTree ('[':xs) = Branch (map readTree (takeInners ('[':xs)))
readTree num = Leaf (read num :: Int)

showLeavesAsNestedList :: IntTree -> String
showLeavesAsNestedList (Branch []) = "[]"
showLeavesAsNestedList (Branch cs) = "[" ++ (intercalate "," . map showLeavesAsNestedList $ cs) ++ "]"
showLeavesAsNestedList (Leaf x)    = show x

instance Ord IntTree where
  compare    (Leaf n)      (Leaf m)    = compare n m
  compare    (Branch xs)   (Branch ys) = compare xs ys
  compare t1@(Leaf n)   t2@(Branch _)  = compare (Branch [t1]) t2
  compare t1@(Branch _) t2@(Leaf m)    = compare t1 (Branch [t2])