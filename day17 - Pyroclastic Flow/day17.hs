#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package strict-0.4.0.1 --package containers-0.6.5.1 --package hashable-1.3.0.0
-------------------------------------
-------------------------------------
----  Day 17:  Pyroclastic Flow  ----
-------------------------------------
-------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-18.22 ghc --package strict-0.4.0.1 --package containers-0.6.5.1 --package hashable-1.3.0.0 -- '.\day17.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day17part1
-- 3227

-- *Main> day17part2
-- 1597714267


-------------
-- Imports --
-------------
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
import Data.List as L (reverse)
import Data.List hiding (union)
import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import Debug.Trace (trace)
import Data.Bits
import qualified Data.Map.Strict as Map 
import Data.Map.Strict hiding (filter, map, foldl', union)
import Data.Map.Strict (Map, (!))
import GHC.Generics (Generic)


-------------
-- Program --
-------------
main = day17part2

day17part1 = do
    contents <- readFile "day17 (data).csv"
    let numOfRocksToStop = 2022
    let jetMoves = map (\(i,x) -> let moveAmount = case x of {'>' -> 1; '<' -> -1} in (i, moveAmount)) $ zip [0..] contents
    let ((World worldNonCulledTiles _ rockPos cullingHeight heightAboveCull rocksStopped _):_) = dropWhile (\(World worldNonCulledTiles nextRocks@(nextRock:laterRocks) fallingRock cullingHeight heightAboveCull rocksStopped _) -> rocksStopped < numOfRocksToStop) $ scanl (step numOfRocksToStop) (World 0 (tail initRocks) (startRock 0 $ head initRocks) 0 0 0 Map.empty) (cycle jetMoves)
    
    print (cullingHeight + heightAboveCull)

day17part2 = do
    contents <- readFile "day17 (data).csv"
    let numOfRocksToStop = 1000000000000
    let jetMoves = map (\(i,x) -> let moveAmount = case x of {'>' -> 1; '<' -> -1} in (i, moveAmount)) $ zip [0..] contents
    let ((World worldNonCulledTiles _ rockPos cullingHeight heightAboveCull rocksStopped _):_) = dropWhile (\(World worldNonCulledTiles nextRocks@(nextRock:laterRocks) fallingRock cullingHeight heightAboveCull rocksStopped _) -> rocksStopped < numOfRocksToStop) $ scanl (step numOfRocksToStop) (World 0 (tail initRocks) (startRock 0 $ head initRocks) 0 0 0 Map.empty) (cycle jetMoves)
    
    print (cullingHeight + heightAboveCull)

-- Each rock has a shape of Tiles (rock-units) is encoded as Integer.
type Tiles = Integer

data Rock = Rock {
    rockTiles :: !Tiles,
    rockRect   :: !Rect } deriving (Show, Eq, Ord, Generic)

data Rect = Rect {
    rectLeft   :: !Int,
    rectBottom :: !Int,
    rectWidth  :: !Int,
    rectHeight :: !Int } deriving (Show, Eq, Ord, Generic)

rectRight r = rectLeft   r + rectWidth r
rectTop   r = rectBottom r + rectHeight r

type WorldStart = (Tiles, Rock, JetMove)

data World = World {
    worldNonCulledTiles    :: !Tiles,
    worldNextRocks         :: [Rock],
    worldFallingRock       :: !Rock,
    worldCullingHeight     :: !Int,
    worldHeightAboveCull   :: !Int,
    worldRocksStopped      :: !Int,
    worldPrevWorldsRocksStoppedAndCullingHeight :: Map WorldStart (Int,Int)} deriving (Show)

-- Every 8 bits of the integer represents a horizontal cross-section of the rock in the world as follows:
-- 
--       -- Least significant bit
--      |
--      V 
--      00111000 00001000 00001000
--
--  represents
--  
--      Wall    Wall   <-- Every bit position that's a multiple of 8 is where the walls of the world are (the right wall is just the left wall on the previous line)
--      |       |           The first bit (from least significant) is the wall and the next 7 bits are the points occupied by the falling rock within that row of possible places
--      |       |
--      V       V
--      |       | <-- The remaining rows are all zeroes as impled by the Integer type
--      |   #   | <-- The next  row up (from left) is the third  8 bits
--      |   #   | <-- The next  row up (from left) is the second 8 bits
--      | ###   | <-- The first row up (from left) is the first  8 bits


-- isOverlapping n n
-- time:  O(n) (approx.) [0 < n < 10000000000, 0 < secs < 7.97]
-- space: O(n) (approx.) [0 < n < 10000000000, 0 < bytes < 30000141704]
-- https://nux-pc/svn/Nux-SVN/My Programming/Scratchings/Advent of Code 2022/day17 (bits).hs/?r=1569
isOverlapping :: Tiles -> Tiles -> Bool
isOverlapping ps1 ps2 = (ps1 .&. ps2) /= zeroBits

isOverlappingWorld :: Rock -> World -> Bool
isOverlappingWorld r w = rockRect r `isOverlappingHeight` (worldHeightAboveCull w) && rockTiles r `isOverlapping` worldNonCulledTiles w

isOverlappingHeight :: Rect -> Int -> Bool
isOverlappingHeight (Rect _ b _ _) h = b <= h

union :: Tiles -> Tiles -> Tiles
union ps1 ps2 = ps1 .|. ps2

intersection :: Tiles -> Tiles -> Tiles
intersection ps1 ps2 = ps1 .&. ps2

inWall :: Rock -> Bool
inWall (Rock _ r) = rectLeft r < 1 || rectRight r > 7

onFloor :: Rock -> Bool
onFloor (Rock _ (Rect l b w h)) = b == 0

floorTiles = foldl' (.|.) zeroBits $ map point [(x,0) | x <- [0..7]]

move :: (Int,Int) -> Tiles -> Tiles
move (x,y) points = points `shiftL` (x + 8*y)

moveRect :: (Int,Int) -> Rect -> Rect
moveRect (x,y) (Rect l b w h) = Rect (l+x) (b+y) w h

moveRock :: (Int,Int) -> Rock -> Rock
moveRock (x,y) rock = Rock (move (x,y) (rockTiles rock)) (moveRect (x,y) (rockRect rock))

-- point (0,n)
-- time:  O(n) [0 < n < 50000000000, 0 < secs < 12.36]
-- space: O(n) [0 < n < 50000000000, 0 < bytes < 50000139672]
-- https://nux-pc/svn/Nux-SVN/My Programming/Scratchings/Advent of Code 2022/day17 (bits).hs/?r=1569
point :: (Int,Int) -> Tiles
point (x,y) = move (x,y) 1

-- startRock: positions 2 away from left wall and 3 up from top of world in nonculled world space. Note that (8*x,y) corresponds to within the wall which includes (0,0).
startRock :: Int -> Rock -> Rock
startRock worldHeightAboveCull (Rock pts rect)
    = let rect' = rect {
                    rectLeft   = 3,
                    rectBottom = worldHeightAboveCull + 3 }
      in  Rock {
            rockTiles = (move (3, worldHeightAboveCull + 3)) pts,
            rockRect   = rect' }

rock1, rock2, rock3, rock4, rock5 :: Rock
rock1 = (\pts -> Rock pts (Rect 0 0 3 0)) $ foldl' (.|.) zeroBits $ map point [(0,0), (1,0), (2,0), (3,0)]
rock2 = (\pts -> Rock pts (Rect 0 0 2 2)) $ foldl' (.|.) zeroBits $ map point [(1,0), (0,1), (1,1), (2,1), (1,2)]
rock3 = (\pts -> Rock pts (Rect 0 0 2 2)) $ foldl' (.|.) zeroBits $ map point [(0,0), (1,0), (2,0), (2,1), (2,2)]
rock4 = (\pts -> Rock pts (Rect 0 0 0 3)) $ foldl' (.|.) zeroBits $ map point [(0,0), (0,1), (0,2), (0,3)]
rock5 = (\pts -> Rock pts (Rect 0 0 1 1)) $ foldl' (.|.) zeroBits $ map point [(0,0), (1,0), (0,1), (1,1)]

initRocks = cycle [rock1, rock2, rock3, rock4, rock5]

showRocks :: Int -> Tiles -> Tiles -> String
showRocks rows worldNonCulledTiles fallingRock = intercalate "\n" . reverse $ [[if x `mod` 8 == 0 then '|' else (if pt `isOverlapping` worldNonCulledTiles then '1' else (if pt `isOverlapping` fallingRock then '2' else '0'))| x <-[0..8], let pt = point (x,y)] | y <-[0..(rows-1)]]


-- FourRowsSlice (tiles,n) where 'tiles' is the set of tiles encoded as an integer as usual and 'n' is the number of rows above the first line of the non-culled world
newtype FourRowsSlice = FourRowsSlice (Tiles,Int)
fromFourRowsSlice (FourRowsSlice (x,y)) = x
allColsNonEmpty :: FourRowsSlice -> Bool
allColsNonEmpty (FourRowsSlice (tiles,_))
    =  ( tiles `isOverlapping` col1
      && tiles `isOverlapping` col2
      && tiles `isOverlapping` col3
      && tiles `isOverlapping` col4
      && tiles `isOverlapping` col5
      && tiles `isOverlapping` col6
      && tiles `isOverlapping` col7 )
  where -- col x = foldl' (.|.) zeroBits $ map point [(x,y)| y <- [0..3]]
        col1 = 33686018
        col2 = 67372036
        col3 = 134744072
        col4 = 269488144
        col5 = 538976288
        col6 = 1077952576
        col7 = 2155905152

fourRowSlicesWithRock :: Rock -> Tiles -> Int -> [FourRowsSlice]
fourRowSlicesWithRock (Rock _ (Rect _ rockBottom _ rockHeight)) tiles heightAboveCull
    | otherwise      = [FourRowsSlice (let result = botFour .&. move (0,-bottomRow) tiles in {-trace (showRocks 4 (move (0,-bottomRow) tiles) result ++ "\n") $-} result, bottomRow) | i <- [0..(rockHeight+3)], let topRow = rockBottom+rockHeight+3-i, let bottomRow = topRow-3, bottomRow >= 0]
  where -- botFour = foldl' (.|.) zeroBits $ map point [(x,y)| x <- [0..7], y <- [0..3]]
        botFour = 4294967295

cullWorld bottomRow (cullingHeight, heightAboveCull, worldTiles, fallingRock, prevWorldsRocksStoppedAndCullingHeight, rocksStopped)
    = ( cullingHeight   + bottomRow,
        heightAboveCull - bottomRow,
        move (0,-bottomRow) worldTiles,
        moveRock (0,-bottomRow) fallingRock,
        prevWorldsRocksStoppedAndCullingHeight,
        rocksStopped )

type JetMove = Int
step :: Int -> World -> (Int, JetMove) -> World
step numOfRocksToStop w (jetMoveID, jetMove)
    = let result = World newWorldTiles newNextRocks newFallingRock newCullingHeight newHeightAboveCull newRocksStopped newPrevWorldHeights
      in if False -- (worldRocksStopped w) `mod` 1 == 0 --fallBlocked && newCullingHeight /= 0 -- && newRocksStopped `mod` 10 == 0
            then trace (show (worldRocksStopped w, worldCullingHeight w, worldHeightAboveCull w)) result
            -- then trace (   show (worldNonCulledTiles w, worldCullingHeight w, worldHeightAboveCull w, worldRocksStopped w, worldFallingRock w, jetMove) ++ "\n"
                        -- ++ showRocks 10 (worldNonCulledTiles w) (rockTiles $ worldFallingRock w) ++ "\n") result
            else result
  where jettedCandidate = moveRock (jetMove,0) (worldFallingRock w)
        jetMoveBlocked = inWall jettedCandidate || jettedCandidate `isOverlappingWorld` w
        jettedRock
            | jetMoveBlocked = worldFallingRock w
            | otherwise = jettedCandidate
        fallCandidate = moveRock (0,-1) jettedRock
        fallBlocked = onFloor jettedCandidate || fallCandidate `isOverlappingWorld` w
        fallenRock
            | fallBlocked = jettedRock
            | otherwise = fallCandidate
        (newCullingHeight, newHeightAboveCull, newWorldTiles, newFallingRock, newPrevWorldHeights, newRocksStopped)
            | fallBlocked = let rocksStopped = worldRocksStopped w + 1
                                heightAboveCull' = max (worldHeightAboveCull w) ((rectTop $ rockRect fallenRock) + 1)
                                newWorld = ( worldCullingHeight w,
                                             heightAboveCull',
                                             rockTiles fallenRock `union` worldNonCulledTiles w,
                                             startRock heightAboveCull' (head $ worldNextRocks w),
                                             worldPrevWorldsRocksStoppedAndCullingHeight w,
                                             rocksStopped )
                            in case find allColsNonEmpty (fourRowSlicesWithRock fallenRock (rockTiles fallenRock `union` worldNonCulledTiles w) (worldHeightAboveCull w)) of
                                    Just (FourRowsSlice (_,bottomRow)) -> let result@(cullingHeight, heightAboveCull, worldTiles, fallingRock, prevWorldsRocksStoppedAndCullingHeight, _) = cullWorld bottomRow newWorld
                                                                              worldStart = (worldTiles, fallingRock, jetMoveID)
                                                                          in if worldStart `Map.member` prevWorldsRocksStoppedAndCullingHeight
                                                                              then let (rocksStoppedAtStartOfRecurrence, cullingHeightAtStartOfRecurrence) = prevWorldsRocksStoppedAndCullingHeight Map.! worldStart
                                                                                       rocksStoppedEachRecurrence = worldRocksStopped w - rocksStoppedAtStartOfRecurrence
                                                                                       cullingHeightGainEachRecurrence = cullingHeight - cullingHeightAtStartOfRecurrence
                                                                                       recurrencesRemaining = (numOfRocksToStop - rocksStoppedAtStartOfRecurrence) `div` rocksStoppedEachRecurrence
                                                                                       rockStoppedAfterAllRecurrences   = rocksStoppedAtStartOfRecurrence  + recurrencesRemaining*rocksStoppedEachRecurrence
                                                                                       cullingHeightAfterAllRecurrences = cullingHeightAtStartOfRecurrence + recurrencesRemaining*cullingHeightGainEachRecurrence
                                                                                    in (cullingHeightAfterAllRecurrences, heightAboveCull, worldTiles, fallingRock, prevWorldsRocksStoppedAndCullingHeight, rockStoppedAfterAllRecurrences+1)
                                                                              else (cullingHeight, heightAboveCull, worldTiles, fallingRock, Map.insert worldStart (worldRocksStopped w, cullingHeight) prevWorldsRocksStoppedAndCullingHeight, rocksStopped)
                                    Nothing -> newWorld
            | otherwise = (worldCullingHeight w, worldHeightAboveCull w, worldNonCulledTiles w, fallenRock, worldPrevWorldsRocksStoppedAndCullingHeight w, worldRocksStopped w)
        newNextRocks
            | fallBlocked = tail (worldNextRocks w)
            | otherwise = worldNextRocks w