#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package astar-0.3.0.0 --package unordered-containers-0.2.19.1 --package strict-0.4.0.1
---------------------------------------
---------------------------------------
----  Day 24:  Blizzard Basin  ----
-----------------------------------
-----------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package astar-0.3.0.0 --package unordered-containers-0.2.19.1 --package strict-0.4.0.1 -- '.\day24.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day24part1
-- 249

-- *Main> day24part2
-- 735


-------------
-- Imports --
-------------
import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import Debug.Trace (trace)
import qualified Data.HashSet as H
import Data.Graph.AStar
import Data.List as L (sort, intersperse, foldl', findIndex, map, delete, null, concatMap, minimumBy, transpose)
import Data.Maybe (fromJust, isJust)
import Data.Char (ord)
import Control.Monad (guard)
import Numeric (readBin)
import Data.Bits


-------------
-- Program --
-------------
main = day24part2

day24part1 = do
    contents <- readFile "day24 (data).csv"
    let fileRows = lines contents
    let world = parseWorld fileRows
    let start = worldStart world
    let end = worldEnd world
    
    let next (Nothing,time) = H.fromList $ do
                                    wait <- [True, False]
                                    if wait
                                     then return (Nothing,time+1)
                                     else do 
                                        let newPos = start
                                        guard $ not (world `hasBlizzardAtSpaceTime` (newPos,time))
                                        return (Just newPos,time+1)

        next (Just (x,y),time) = H.fromList $ do
                                    (dx,dy) <- [(-1,0),(1,0),(0,-1),(0,1),(0,0)]
                                    let newPos = (x+dx, y+dy)
                                    guard $ (newPos `inWorld` world) && not (world `hasBlizzardAtSpaceTime` (newPos,time))
                                    return (Just newPos,time+1)
    
    let costs v1 v2 = 1
    let heuristic (Nothing,_)    = let {(x,y) = start; (x',y') = end} in abs (x'-x) + abs (y-y') + 1
        heuristic (Just (x,y),_) = let                 (x',y') = end  in abs (x'-x) + abs (y-y')
    let goal (Nothing,_) = False
        goal (Just v,_)  = v == end
    
    let (Just minPath) = aStar next costs heuristic goal (Nothing,0)
    
    print (length minPath) -- the length of the minPath is one more than the number of steps but that's ok because we need another step onto the exit on the edge
    
    -- mapM_ putStrLn $ map (unlines . (\(time, (ourPos, _)) -> showWorldWithOurPos time world ourPos)) (zip [0..] minPath)

day24part2 = do
    contents <- readFile "day24 (data).csv"
    let fileRows = lines contents
    let world = parseWorld fileRows
    let start = worldStart world
    let end = worldEnd world
    
    let toEndNext (Nothing,time) = H.fromList $ do
                                        wait <- [True, False]
                                        if wait
                                         then return (Nothing,time+1)
                                         else do 
                                            let newPos = start
                                            guard $ not (world `hasBlizzardAtSpaceTime` (newPos,time))
                                            return (Just newPos,time+1)
        toEndNext (Just (x,y),time) = H.fromList $ do
                                        (dx,dy) <- [(-1,0),(1,0),(0,-1),(0,1),(0,0)]
                                        let newPos = (x+dx, y+dy)
                                        guard $ (newPos `inWorld` world) && not (world `hasBlizzardAtSpaceTime` (newPos,time))
                                        return (Just newPos,time+1)
    
    let toStartNext (Nothing,time) = H.fromList $ do
                                        wait <- [True, False]
                                        if wait
                                         then return (Nothing,time+1)
                                         else do 
                                            let newPos = end
                                            guard $ not (world `hasBlizzardAtSpaceTime` (newPos,time))
                                            return (Just newPos,time+1)
        toStartNext (Just (x,y),time) = H.fromList $ do
                                        (dx,dy) <- [(-1,0),(1,0),(0,-1),(0,1),(0,0)]
                                        let newPos = (x+dx, y+dy)
                                        guard $ (newPos `inWorld` world) && not (world `hasBlizzardAtSpaceTime` (newPos,time))
                                        return (Just newPos,time+1)
    
    let costs v1 v2 = 1
    
    let toEndHeuristic (Nothing,_)    = let {(x,y) = start; (x',y') = end} in abs (x'-x) + abs (y-y') + 1
        toEndHeuristic (Just (x,y),_) = let                 (x',y') = end  in abs (x'-x) + abs (y-y')
    let toStartHeuristic (Nothing,_)    = let {(x,y) = end; (x',y') = start} in abs (x'-x) + abs (y-y') + 1
        toStartHeuristic (Just (x,y),_) = let                 (x',y') = start  in abs (x'-x) + abs (y-y')
        
    let toEndGoal (Nothing,_) = False
        toEndGoal (Just v,_)  = v == end
    let toStartGoal (Nothing,_) = False
        toStartGoal (Just v,_)  = v == start
    
    let start1 = 0
    let (Just minPath1) = aStar toEndNext costs toEndHeuristic toEndGoal (Nothing,start1)
    let pathLength1 = length minPath1
    
    let start2 = pathLength1
    let (Just minPath2) = aStar toStartNext costs toStartHeuristic toStartGoal (Nothing,start2)
    let pathLength2 = length minPath2
    
    let start3 = pathLength1 + pathLength2
    let (Just minPath3) = aStar toEndNext costs toEndHeuristic toEndGoal (Nothing,start3)
    let pathLength3 = length minPath3
    
    print (sum [pathLength1, pathLength2, pathLength3]) -- the length of the minPath is one more than the number of steps but that's ok because we need another step onto the exit on the edge
    
    -- let output = unlines $ (   map (unlines . (\(time, (ourPos, _)) -> showWorldWithOurPos time world ourPos)) (zip [start1..] minPath1) ++ [""]
                            -- ++ map (unlines . (\(time, (ourPos, _)) -> showWorldWithOurPos time world ourPos)) (zip [start2..] minPath2) ++ [""]
                            -- ++ map (unlines . (\(time, (ourPos, _)) -> showWorldWithOurPos time world ourPos)) (zip [start3..] minPath3) )
    
    -- writeFile "output.txt" output

-- Vectors
type V2 = (Int, Int)
type SpaceTime = (V2,Int)
addV2 (x1,y1) (x2,y2) = (x1+x2, y1+y2)

-- World
-- lefts, rights (resp. ups and downs) are the blizzards stored as comving bitstrings of length 2*w (resp. 2*h) and where
--       - a 1 in lefts (resp. rights) is a left-moving (resp. right-moving) blizzard at that position with the most sig. bit representing 0 at the very left-most occupiable position
--       - a 1 in ups (resp. downs) is a up moving-moving (resp. down-moving) blizzard at that position with the most sig. bit representing 0 at the very top-most occupiable position
--   Storing these as bitstrings allows for a very efficient calculation of world state at any point in time as mere bitshifting.
--   The bitstrings are twice the width (resp. height) of the world because they are 2 copies of the row (resp. col) so that wrap-around behaviour is modelled simply by taking an arbitrary slice of size width (resp. height) within the two copies.
data World = World {worldDims :: V2, worldStart :: V2, worldEnd :: V2, lefts :: [CoMoving], rights :: [CoMoving], ups :: [CoMoving], downs :: [CoMoving]} deriving (Show)
parseWorld :: [String] -> World
parseWorld rows = World {worldDims = (width,height), worldStart = (0,0), worldEnd = (width-1,height-1), lefts = ls, rights = rs, ups = us, downs = ds}
  where rows' = map (init . tail) . (init . tail) $ rows -- Ignore edges to avoid having to consider walls explicitly:
        cols' = transpose rows'                          --  Getting to the end from bottom right is guaranteed in 1 extra minute
        height = length rows'                            --  and we'll handle when to step on to top left separately
        width  = length $ head rows'
        
        ls = [parseCoMoving '<' row | row <- rows']
        rs = [parseCoMoving '>' row | row <- rows']
        us = [parseCoMoving '^' col | col <- cols'] -- I could ignore left and right columns in the provided data (since they're both empty for ups and downs)
        ds = [parseCoMoving 'v' col | col <- cols'] -- but I don't see the need to make the solution less general here

data CoMoving = CoMoving {fromCoMoving :: Integer, coMovingSize :: Int} deriving (Show)

parseCoMoving :: Char -> String -> CoMoving
parseCoMoving dir row = CoMoving {fromCoMoving = wrapped, coMovingSize = size}
  where size = length row
        unwrapped = fst . head $ readBin [if c == dir then '1' else '0' | c <- row]
        wrapped = (unwrapped `shiftL` size) .|. unwrapped

forwardBlizzardAtPosTime,
 backwardBlizzardAtPosTime :: CoMoving -> Int -> Int -> Bool
forwardBlizzardAtPosTime  (CoMoving cm size) pos time = (cm `shiftR` (  time  `mod` size)) `testBit` (size-1 - pos)
backwardBlizzardAtPosTime (CoMoving cm size) pos time = (cm `shiftR` ((-time) `mod` size)) `testBit` (size-1 - pos)

showWorld :: Int -> World -> [String]
showWorld time world = [firstRow] ++ midRows ++ [lastRow]
  where (w,h) = worldDims world
        firstRow = "#." ++ replicate w '#'
        lastRow  = replicate w '#' ++ ".#"
        midRows  = ["#" ++ row ++ "#" | row <- rows]
        
        -- rows = replicate w '.'
        rows = [[showPos x y | x <- [0..(w-1)]] | y <- [0..(h-1)]]
        
        -- showPos x y = last (show (x+y))
        showPos x y | num == 0 = '.'
                    | num > 1  = head (show num)
                    | l = '<'
                    | r = '>'
                    | u = '^'
                    | d = 'v'
          where l = backwardBlizzardAtPosTime (lefts  world !! y) x time
                r = forwardBlizzardAtPosTime  (rights world !! y) x time
                u = backwardBlizzardAtPosTime (ups    world !! x) y time
                d = forwardBlizzardAtPosTime  (downs  world !! x) y time
                num = length . filter id $ [l,r,u,d]

showWorldWithOurPos :: Int -> World -> Maybe V2 -> [String]
showWorldWithOurPos time world Nothing = [show time] ++ showWorld time world
showWorldWithOurPos time world (Just ourPos) = [show time] ++ [firstRow] ++ midRows ++ [lastRow]
  where (w,h) = worldDims world
        firstRow = "#." ++ replicate w '#'
        lastRow  = replicate w '#' ++ ".#"
        midRows  = ["#" ++ row ++ "#" | row <- rows]
        
        -- rows = replicate w '.'
        rows = [[showPos x y | x <- [0..(w-1)]] | y <- [0..(h-1)]]
        
        -- showPos x y = last (show (x+y))
        showPos x y | (x,y) == ourPos = 'E'
                    | num == 0 = '.'
                    | num > 1  = head (show num)
                    | l = '<'
                    | r = '>'
                    | u = '^'
                    | d = 'v'
          where l = backwardBlizzardAtPosTime (lefts  world !! y) x time
                r = forwardBlizzardAtPosTime  (rights world !! y) x time
                u = backwardBlizzardAtPosTime (ups    world !! x) y time
                d = forwardBlizzardAtPosTime  (downs  world !! x) y time
                num = length . filter id $ [l,r,u,d]

hasBlizzardAtSpaceTime :: World -> SpaceTime -> Bool
hasBlizzardAtSpaceTime world ((x,y), time) = num > 0
  where l = backwardBlizzardAtPosTime (lefts  world !! y) x time
        r = forwardBlizzardAtPosTime  (rights world !! y) x time
        u = backwardBlizzardAtPosTime (ups    world !! x) y time
        d = forwardBlizzardAtPosTime  (downs  world !! x) y time
        num = length . filter id $ [l,r,u,d]

inWorld :: V2 -> World -> Bool
inWorld (x,y) world = 0 <= x && x < w && 0 <= y && y < h
  where (w,h) = worldDims world