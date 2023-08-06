#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package containers-0.6.5.1 --package split-0.2.3.5 --package array-0.5.4.0 --package astar-0.3.0.0 --package unordered-containers-0.2.19.1
-- use the following command to build: stack ghc -- "filename.hs" -O2
-------------------------------------
-------------------------------------
----  Day 10:  Cathode-Ray Tube  ----
-------------------------------------
-------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package containers-0.6.5.1 --package split-0.2.3.5 --package array-0.5.4.0 --package astar-0.3.0.0 --package unordered-containers-0.2.19.1 -- '.\day12.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day12part1
-- 391

-- *Main> day12part2
-- 386


-------------
-- Imports --
-------------
import Data.HashSet as H
import Data.Graph.AStar
import Data.List as L (sort, intersperse, foldl', findIndex, map, delete, null, concatMap, minimumBy)
import Data.Maybe (fromJust, isJust)
import Data.Array as A (Array, (!), listArray)
import Data.Char (ord)
import Control.Monad (guard)


-------------
-- Program --
-------------
main = day12part2

day12part1 = do
    contents <- readFile "day12 (data).csv"
    let lines = Prelude.lines $ contents
    let width  = length (head lines)
    let height = length lines
    let offWorldEdge (y,x) = x < 0 || x > width-1 || y < 0 || y > height-1
        
    let (_,Just start, Just end,_) = until (\(_,s,e,_) -> isJust s && isJust e) findInLine (0, Nothing, Nothing, lines)
          where findInLine (i, Nothing, Nothing, line:lines') = (i+1, fmap ((,) i) $ findIndex (=='S') line, fmap ((,) i) $ findIndex (=='E') line, lines')
                findInLine (i, s,       Nothing, line:lines') = (i+1, s,                                          fmap ((,) i) $ findIndex (=='E') line, lines')
                findInLine (i, Nothing, e,       line:lines') = (i+1, fmap ((,) i) $ findIndex (=='S') line, e,                                          lines')
                findInLine x = error $ show x
        
    let grid :: Array (Int,Int) Int
        grid = listArray ((0,0),(height-1,width-1)) . L.map charToHeight $ concat lines
    
    let graphFromGrid (y,x) = H.fromList $ do
                let oldHeight = grid A.! (y,x) :: Int
                (dx,dy) <- [(-1,0),(1,0),(0,-1),(0,1)]
                
                let newPos = (y+dy, x+dx)
                guard $ not (offWorldEdge newPos)
                
                let newHeight = grid A.! newPos :: Int
                guard $ newHeight <= oldHeight+1
                -- guard $ trace (show (dx,dy) ++ "\t" ++ show (grid A.! newPos) ++ "\t" ++ show newHeight ++ "\t" ++ show oldHeight) $ newHeight <= oldHeight+1
                
                return newPos
    let costs v1 v2 = 1
    let heuristic (x,y) = let (x',y') = end in abs (x'-x) + abs (y-y')
    let goal v = v == end
    
    let (Just minPath) = aStar graphFromGrid costs heuristic goal start
    
    -- print (minPath)
    print (length minPath)
    
    -- let showPath :: [(Int,Int)] -> String
        -- showPath path = unlines $ [[if (y,x) `elem` path then '#' else c | (x,c) <- zip [0..] line] | (y,line) <- zip [0..] lines]
    
    -- putStrLn . showPath $ minPath

day12part2 = do
    contents <- readFile "day12 (data).csv"
    let lines = Prelude.lines $ contents
    let width  = length (head lines)
    let height = length lines
    let offWorldEdge (y,x) = x < 0 || x > width-1 || y < 0 || y > height-1
        
    let (_,Just start, Just end,_) = until (\(_,s,e,_) -> isJust s && isJust e) findInLine (0, Nothing, Nothing, lines)
          where findInLine (i, Nothing, Nothing, line:lines') = (i+1, fmap ((,) i) $ findIndex (=='S') line, fmap ((,) i) $ findIndex (=='E') line, lines')
                findInLine (i, s,       Nothing, line:lines') = (i+1, s,                                          fmap ((,) i) $ findIndex (=='E') line, lines')
                findInLine (i, Nothing, e,       line:lines') = (i+1, fmap ((,) i) $ findIndex (=='S') line, e,                                          lines')
                findInLine x = error $ show x
        
    let grid :: Array (Int,Int) Int
        grid = listArray ((0,0),(height-1,width-1)) . L.map charToHeight $ concat lines
    
    let graphFromGrid (y,x) = H.fromList $ do
                let oldHeight = grid A.! (y,x) :: Int
                (dx,dy) <- [(-1,0),(1,0),(0,-1),(0,1)]
                
                let newPos = (y+dy, x+dx)
                guard $ not (offWorldEdge newPos)
                
                let newHeight = grid A.! newPos :: Int
                guard $ newHeight >= oldHeight-1
                -- guard $ trace (show (dx,dy) ++ "\t" ++ show (grid A.! newPos) ++ "\t" ++ show newHeight ++ "\t" ++ show oldHeight) $ newHeight <= oldHeight+1
                
                return newPos
    let costs v1 v2 = 1
    let heuristic (y,x) = x -- relies on the solution being on the left side of the grid
    let goal v = grid A.! v == 1
    
    let (Just minPath) = aStar graphFromGrid costs heuristic goal end
    
    -- print (minPath)
    print (length minPath)
    
    -- let showPath :: [(Int,Int)] -> String
        -- showPath path = unlines $ [[if (y,x) `elem` path then '#' else c | (x,c) <- zip [0..] line] | (y,line) <- zip [0..] lines]
    
    -- putStrLn . showPath $ minPath

charToHeight :: Char -> Int
charToHeight 'S'  = charToHeight 'a'
charToHeight 'E'  = charToHeight 'z'
charToHeight char = ord char - 96