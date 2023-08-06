#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci
--------------------------------------
--------------------------------------
----  Day 8:  Treetop Tree House  ----
--------------------------------------
--------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc -- '.\day8.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day8part1
-- 1801

-- *Main> day8part2
-- 209880


-------------
-- Imports --
-------------
import Data.Array as A (Array, (!), listArray)
import Data.Char (digitToInt)
import Control.Monad (guard)


-------------
-- Program --
-------------
main = day8part2

day8part1 = do
    contents <- readFile "day8 (data).csv"
    
    let lines = Prelude.lines $ contents
        width  = length (head lines)
        height = length lines
        offWorldEdge (x,y) = x < 0 || x > width-1 || y < 0 || y > height-1
        grid = listArray ((0,0),(width-1,height-1)) $ map digitToInt $ concat lines
        
        visible = do x <- [0..width-1]
                     y <- [0..height-1]
                     
                     let ourHeight = grid A.! (x,y) :: Int
                         
                         viewLimit (dx,dy) = until viewOffWorldOrBlocked doStep start
                           where viewOffWorldOrBlocked visiblePos
                                    = let treeHeight = grid A.! visiblePos :: Int
                                      in offWorldEdge visiblePos || treeHeight >= ourHeight
                                 doStep (x',y') = (x'+dx, y'+dy)
                                 start = doStep (x,y)
                     
                     guard $ any (\dir -> offWorldEdge (viewLimit dir)) [(-1,0),(1,0),(0,-1),(0,1)]
                     
                     return ()
    
    print (length visible)

day8part2 = do
    contents <- readFile "day8 (data).csv"
    
    let lines = Prelude.lines $ contents
        width  = length (head lines)
        height = length lines
        isEdge (x,y) = x == 0 || x == width-1 || y == 0 || y == height-1
        grid = listArray ((0,0),(width-1,height-1)) $ map digitToInt $ concat lines
        
        scores = do x <- [1..width-2]
                    y <- [1..height-2]
                     
                    let ourHeight = grid A.! (x,y) :: Int
                        
                        viewDist (dx,dy) = until viewOffWorldOrBlocked doStep start
                           where viewOffWorldOrBlocked i
                                    = let visiblePos = (x + i*dx, y + i*dy)
                                          treeHeight = grid A.! visiblePos :: Int
                                      in isEdge visiblePos || treeHeight >= ourHeight
                                 doStep = (+1)
                                 start  = doStep 0
                    
                    return (product [viewDist dir | dir <- [(-1,0),(1,0),(0,-1),(0,1)]])
    
    print (maximum scores)