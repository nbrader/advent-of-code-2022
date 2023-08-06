#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package split-0.2.3.5 --package containers-0.6.5.1
-- use the following command to build: stack ghc -- "filename.hs" -O2
---------------------------------------
---------------------------------------
----  Day 14:  Regolith Reservoir  ----
---------------------------------------
---------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 --package containers-0.6.5.1 -- '.\day14.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day14part1
-- 779

-- *Main> day14part2
-- 27426


-------------
-- Imports --
-------------
import Data.Set (Set)
import Data.Set as S hiding (map, foldl')
import Data.List (foldl')
import Data.List.Split (splitOn)


-------------
-- Program --
-------------
main = day14part2

day14part1 = do
    contents <- readFile "day14 (data).csv"
    let paths = map (map ((\[x,y] -> (read x, read y) :: (Int,Int)) . splitOn ",")) . map (splitOn " -> ") . lines $ contents
        maxDepth = maximum . map snd . concat $ paths
        
        allPoints :: WorldPoints
        allPoints = unions $ map pointsFromPath paths
        
        source = (500,0)
        
        simulateUnitFall :: (WorldPoints,Path) -> (Path,Bool)
        simulateUnitFall (initPoints,prevPath) = (\(_,_,finalPath,fellOff) -> (finalPath,fellOff)) $ until (\(_,finishedUnitFall,_,fellOff) -> finishedUnitFall) run (initPoints,False,prevPath,False)
          where run :: (WorldPoints,Bool,Path,Bool) -> (WorldPoints,Bool,Path,Bool)
                run (points,finishedUnitFall,finalPos:path,_)
                      = let below     = finalPos `addV2` ( 0,1)
                            leftDiag  = finalPos `addV2` (-1,1)
                            rightDiag = finalPos `addV2` ( 1,1)
                            leftleft  = finalPos `addV2` (-1,0)
                            left      = finalPos `addV2` (-1,0)
                            right     = finalPos `addV2` ( 1,0)
                            rightright     = finalPos `addV2` ( 1,0)
                            nextPos | not (below     `elem` points) = below
                                    | not (leftDiag  `elem` points) = leftDiag
                                    | not (rightDiag `elem` points) = rightDiag
                                    | otherwise = finalPos
                            fellOff = snd nextPos > maxDepth
                            finishedUnitFall = fellOff || nextPos == finalPos
                            buriedPoints = (   [p | p <- [leftDiag], finishedUnitFall, leftleft   `elem` points, left       `elem` points]
                                            ++ [p | p <- [below]   , finishedUnitFall, left       `elem` points, right      `elem` points]
                                            ++ [p | p <- [below]   , finishedUnitFall, rightright `elem` points, rightright `elem` points])
                            newPoints = foldl' (flip S.delete) points buriedPoints --stop checking buried sand
                            nextPath = if finishedUnitFall then finalPos:path else nextPos:finalPos:path
                        in (newPoints,finishedUnitFall,nextPath,fellOff)

        simulateAllSandFall :: WorldPoints -> (WorldPoints,Bool,Int,Path)
        simulateAllSandFall initPoints = until (\(_,finishedAllSandFall,_,_) -> finishedAllSandFall) run (initPoints,False,0,[source])
          where run (points,finishedAllSandFall,iterations,prevTail)
                    = let (finalPos:nextPath,fellOff) = simulateUnitFall (points,prevTail)
                      in (insert finalPos points,fellOff,iterations+1,nextPath)
                      -- in trace (show finalPos) $ (insert finalPos points,fellOff,iterations+1,nextPath)

        numRestingAfterSim :: WorldPoints -> Int
        numRestingAfterSim initPoints = (\(_,_,iterations,_) -> iterations-1) $ simulateAllSandFall initPoints
        
        numResting = numRestingAfterSim allPoints
    
    print $ numResting

day14part2 = do
    contents <- readFile "day14 (data).csv"
    let paths = map (map ((\[x,y] -> (read x, read y) :: (Int,Int)) . splitOn ",")) . map (splitOn " -> ") . lines $ contents
        maxDepth = maximum . map snd . concat $ paths
        
        allPoints :: WorldPoints
        allPoints = unions $ map pointsFromPath paths
        
        source = (500,0)
        
        simulateUnitFall :: (WorldPoints,Path) -> (Path,Bool)
        simulateUnitFall (initPoints,prevPath) = (\(_,_,finalPath,blockedSource) -> (finalPath,blockedSource)) $ until (\(_,finishedUnitFall,_,blockedSource) -> finishedUnitFall) run (initPoints,False,prevPath,False)
          where run :: (WorldPoints,Bool,Path,Bool) -> (WorldPoints,Bool,Path,Bool)
                run (points,finishedUnitFall,finalPos:path,_)
                      = let below     = finalPos `addV2` ( 0,1)
                            leftDiag  = finalPos `addV2` (-1,1)
                            rightDiag = finalPos `addV2` ( 1,1)
                            leftleft  = finalPos `addV2` (-1,0)
                            left      = finalPos `addV2` (-1,0)
                            right     = finalPos `addV2` ( 1,0)
                            rightright     = finalPos `addV2` ( 1,0)
                            nextPos | snd finalPos == (maxDepth+1)  = finalPos -- hit floor
                                    | not (below     `elem` points) = below
                                    | not (leftDiag  `elem` points) = leftDiag
                                    | not (rightDiag `elem` points) = rightDiag
                                    | otherwise = finalPos
                            blockedSource = nextPos == source
                            finishedUnitFall = blockedSource || nextPos == finalPos
                            nextPath = if finishedUnitFall then finalPos:path else nextPos:finalPos:path
                            buriedPoints = (   [p | p <- [leftDiag], finishedUnitFall, leftleft   `elem` points, left       `elem` points]
                                            ++ [p | p <- [below]   , finishedUnitFall, left       `elem` points, right      `elem` points]
                                            ++ [p | p <- [below]   , finishedUnitFall, rightright `elem` points, rightright `elem` points])
                            newPoints = foldl' (flip S.delete) points buriedPoints --stop checking buried sand
                        in (newPoints,finishedUnitFall,nextPath,blockedSource)

        simulateAllSandFall :: WorldPoints -> (WorldPoints,Bool,Int,Path)
        simulateAllSandFall initPoints = until (\(_,finishSim,_,_) -> finishSim) run (initPoints,False,0,[source])
          where run (points,finishSim,iterations,prevTail)
                    = let (finalPos:nextPath,blockedSource) = simulateUnitFall (points,prevTail)
                      in (insert finalPos points,blockedSource,iterations+1,nextPath)

        numRestingAfterSim :: WorldPoints -> Int
        numRestingAfterSim initPoints = (\(_,_,iterations,_) -> iterations) $ simulateAllSandFall initPoints
        
        numResting = numRestingAfterSim allPoints
    
    print $ numResting

addV2 (x1,y1) (x2,y2) = (x1+x2, y1+y2)

type Point = (Int, Int)
type WorldPoints = S.Set Point
type Path = [Point]

pointsFromEnds :: Point -> Point -> [Point]
pointsFromEnds (x1,y1) (x2,y2) | dx == 0   = let step = signum dy in [(x1, y1 + i*step) | i <- [1..(abs dy)]]
                               | otherwise = let step = signum dx in [(x1 + i*step, y1) | i <- [1..(abs dx)]]
  where dx = x2-x1
        dy = y2-y1

pointsFromPath :: [Point] -> WorldPoints
pointsFromPath (x:xs) = snd $ foldl' processVert (x, singleton x) xs 
  where processVert :: ((Int, Int),S.Set (Int, Int)) -> (Int, Int) -> ((Int, Int),S.Set (Int, Int))
        processVert (v1, set) v2 = (v2, fromList (pointsFromEnds v1 v2) `union` set)