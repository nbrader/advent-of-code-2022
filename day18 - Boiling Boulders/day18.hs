#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package strict-0.4.0.1 --package split-0.2.3.4 --package containers-0.6.5.1 --package fgl-5.7.0.3
-------------------------------------
-------------------------------------
----  Day 18:  Boiling Boulders  ----
-------------------------------------
-------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-18.22 ghc --package strict-0.4.0.1 --package split-0.2.3.4 --package containers-0.6.5.1 --package fgl-5.7.0.3 -- '.\day18.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day18part1
-- 3412

-- *Main> day18part2
-- 2018


-------------
-- Imports --
-------------
import Data.List hiding (union)
import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.Tree
import Data.Set as S hiding (map, foldl', null, filter)
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Maybe (maybeToList, fromJust)
import Control.Monad (guard)


-------------
-- Program --
-------------
main = day18part2

day18part1 = do
    contents <- readFile "day18 (data).csv"
    let allCubes :: [Cube]
        allCubes = map (map (read :: String -> Int)) . map (splitOn ",") . lines $ contents
    let labelledCubes :: [(Int,Cube)]
        labelledCubes = zip [0..] allCubes
    let cubeIDMap = Map.fromList (zip allCubes [0..])
    let labelledEdges = do
            cube <- allCubes
            dir <- allDirs
            let adj = add cube dir
            id1 <- maybeToList $ Map.lookup cube cubeIDMap
            id2 <- maybeToList $ Map.lookup adj cubeIDMap
            return (id1,id2,())
    let g :: Gr Int ()
        g = (\g -> gmap (\(from, i, _, to) -> (from, i, 6 - outdeg g i, to)) g) $ mkGraph labelledCubes labelledEdges
        ts = map (fmap (fromJust . lab g)) $ dff' g
    
    print . sum $ map (foldTree (\exposedFaces accums -> exposedFaces + sum accums)) ts
    -- print . sum $ map (foldTree (\exposedFaces accums -> 1 + sum accums)) ts

day18part2 = do
    contents <- readFile "day18 (data).csv"
    
    -- First find the set of all cubes within the bounding cuboid of all lava cubes, that connect to a non-lava cube that's known to be "outside"
    let allLavaCubes = map (map (read :: String -> Int)) . map (splitOn ",") . lines $ contents
        
        initialCubeForBounds = head allLavaCubes
        [initMinX,initMinY,initMinZ] = initialCubeForBounds
        [initMaxX,initMaxY,initMaxZ] = initialCubeForBounds
        (minX,minY,minZ,maxX,maxY,maxZ)
            = foldl' (\(minX,minY,minZ,maxX,maxY,maxZ) [x,y,z] ->
                            (min x minX, min y minY, min z minZ, max x maxX, max y maxY, max z maxZ))
                     (initMinX,initMinY,initMinZ,initMaxX,initMaxY,initMaxZ)
                     (tail allLavaCubes)
        
        allUniverseCubes :: [Cube]
        allUniverseCubes = [[x,y,z] | x <- [(minX-1)..(maxX+1)], y <- [(minY-1)..(maxY+1)], z <- [(minZ-1)..(maxZ+1)]]
        labelledUniverseCubes = zip [0..] allUniverseCubes
        universeCubeIDMap = Map.fromList (zip allUniverseCubes [0..])
        universeCubePosMap = Map.fromList (zip [0..] allUniverseCubes)
        
        labelledUniverseEdges = do
            cube <- allUniverseCubes
            dir <- allDirs
            let adj = add cube dir
            id1 <- maybeToList $ Map.lookup cube universeCubeIDMap
            id2 <- maybeToList $ Map.lookup adj universeCubeIDMap
            guard $ not (cube `elem` allLavaCubes)
            guard $ not ((universeCubePosMap Map.! id2) `elem` allLavaCubes)
            return (id1,id2,())
        
        universeGraph :: Gr Cube ()
        universeGraph = mkGraph labelledUniverseCubes labelledUniverseEdges
        exteriorTrees = map (fmap (fromJust . lab universeGraph)) $ dff [0] universeGraph
        exteriorCubes = S.unions $ map (foldTree (\pos accums -> S.insert pos (S.unions accums))) exteriorTrees
    
    -- Then find the all cubes not in this set
    let allNonExteriorCubes = filter (not . (`S.member` exteriorCubes)) allUniverseCubes
    
    let dummyLabelledCubes :: [(Int,Cube)]
        dummyLabelledCubes = zip [0..] allNonExteriorCubes
    
    -- Then do as in part 1
    let cubeIDMap = Map.fromList (zip allNonExteriorCubes [0..])
    let labelledEdges = do
            cube <- allNonExteriorCubes
            dir <- allDirs
            let adj = add cube dir
            id1 <- maybeToList $ Map.lookup cube cubeIDMap
            id2 <- maybeToList $ Map.lookup adj cubeIDMap
            return (id1,id2,())
    let g :: Gr Int ()
        g = (\g -> gmap (\(from, i, _, to) -> (from, i, 6 - outdeg g i, to)) g) $ mkGraph dummyLabelledCubes labelledEdges
        ts = map (fmap (fromJust . lab g)) $ dff' g
    
    print . sum $ map (foldTree (\exposedFaces accums -> exposedFaces + sum accums)) ts
    -- print . sum $ map (foldTree (\exposedFaces accums -> 1 + sum accums)) ts

type Cube = [Int]

allDirs = [[x,y,z] | let range = [-1,0,1], x <- range, y <- range, z <- range, abs x + abs y + abs z == 1]
add [x,y,z] [dx,dy,dz] = [x+dx, y+dy, z+dz]