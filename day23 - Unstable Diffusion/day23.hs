#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package strict-0.4.0.1 --package containers-0.6.5.1
---------------------------------------
---------------------------------------
----  Day 23:  Unstable Diffusion  ----
---------------------------------------
---------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package strict-0.4.0.1 --package containers-0.6.5.1 -- '.\day23.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day23part1
-- 3966

-- *Main> day23part2
-- 933


-------------
-- Imports --
-------------
import Data.List
import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import Data.Set (Set)
import Data.Set as S hiding (map, foldl')
import Debug.Trace (trace)


-------------
-- Program --
-------------
main = day23part2
-- idea 1: implement naively with lists of 2D points
-- idea 2: implement like with "day17 - Pyroclastic Flow" but with an arbitrary number of these vertical columns making up the width of the world, with new columns being added when needed.
--          Checking for neighbours can then be done for all elves in parallel with xor operations on a shifted version of all elves

day23part1 = do
    contents <- readFile "day23 (data).csv"
    let fileRows = lines contents
    let initElfPositions = readElfPositions fileRows
    let initElves :: Elves
        initElves = Elves {elfPositions = initElfPositions, elfPosSet = S.fromList initElfPositions, elfMoves = initElfMoves}
    let finalElves = foldl' (flip ($!)) initElves (replicate 10 doElfRound)
    
    let rectSize = aabbSize . aabbFromPoints $ elfPositions finalElves
    
    print (rectSize - length (elfPositions initElves))

day23part2 = do
    contents <- readFile "day23 (data).csv"
    let fileRows = lines contents
    let initElfPositions = readElfPositions fileRows
    let initElves :: Elves
        initElves = Elves {elfPositions = initElfPositions, elfPosSet = S.fromList initElfPositions, elfMoves = initElfMoves}
    let elfHistory = scanl' (flip ($!)) initElves (repeat doElfRound)
    
    let roundStopped = fst . head $ dropWhile (\(_, (elves1, elves2)) -> elfPosSet elves1 /= elfPosSet elves2) $ zip [1..] (zip elfHistory (tail elfHistory))
    
    print roundStopped

data Elves = Elves {elfPositions :: [V2], elfPosSet :: Set V2, elfMoves :: [ElfMove]} deriving (Show, Eq)
type ElfMove = (V2,(V2,V2))

doElfRound :: Elves -> Elves
doElfRound prev = Elves {elfPositions = afterFullPositions, elfPosSet = afterFullPosSet, elfMoves = tail prevMoves}
-- doElfRound prev = Elves {elfPositions = trace (showWorld afterFullPositions) afterFullPositions, elfPosSet = afterFullPosSet, elfMoves = tail prevMoves}
  where prevPositions = elfPositions prev
        prevPosSet = elfPosSet prev
        prevMoves = elfMoves prev
        moves = Prelude.take 4 prevMoves
        afterHalf = do
            prevPos@(x,y) <- elfPositions prev
            let viablePositions = [prevPos `addV2` move | (move,(sideMove1,sideMove2)) <- moves, not (any (`S.member` prevPosSet) [prevPos `addV2` move' | move' <- [move, sideMove1, sideMove2]])]
            let viable = head viablePositions
            let noOfViable = length viablePositions
            
            if noOfViable == 4 || noOfViable == 0
             then return prevPos
             else return viable
        (afterFullPositions, posChanges) = foldl' (\(prevFull, prevChanged) (prevPos, afterPos) -> let overlapping = [() | afterPos' <- afterHalf, afterPos' == afterPos]
                                                                                                    in ((if (afterPos == prevPos) then prevPos else if length overlapping == 1 then afterPos else prevPos) : prevFull, if (afterPos /= prevPos && length overlapping == 1) then ((prevPos,afterPos) : prevChanged) else prevChanged)) ([],[]) (zip prevPositions afterHalf)
        -- afterFullPositions = [if (after == prev) then prev else if length overlapping == 1 then after else prev | (prev,after) <- zip prevPositions afterHalf, let overlapping = [() | after' <- afterHalf, after' == after]]
        afterFullPosSet = foldl' (\prevSet (prevPos, afterPos) -> S.insert afterPos . S.delete prevPos $ prevSet) prevPosSet posChanges

data AABB = AABB {aabbLeft :: Int, aabbTop :: Int, aabbRight :: Int, aabbBottom :: Int}
aabbFromPoints :: [V2] -> AABB
aabbFromPoints ((x0,y0):vs) = foldl' step (AABB x0 y0 x0 y0) vs
  where step(AABB left top right bottom) (x,y) = AABB {aabbLeft = min left x, aabbTop = min top y, aabbRight = max right x, aabbBottom = max bottom y}

aabbSize (AABB left top right bottom) = (right+1-left)*(bottom+1-top)

addV2 (x1,y1) (x2,y2) = (x1+x2, y1+y2)

type V2 = (Int, Int)

readElfPositions :: [String] -> [V2]
readElfPositions rows = [(colNum, rowNum) | (rowNum, row) <- zip [1..] rows, (colNum,c) <- zip [1..] row, case c of {'#' -> True; _ -> False}]

noMove = (0, 0)
vecN = (0,-1)
vecS = (0, 1)
vecE = (-1,0)
vecW = ( 1,0)
vecNE = (-1,-1)
vecNW = ( 1,-1)
vecSE = (-1, 1)
vecSW = ( 1, 1)

initElfMoves :: [ElfMove]
initElfMoves = cycle [
    (vecN,(vecNE,vecNW)),
    (vecS,(vecSE,vecSW)),
    (vecE,(vecNE,vecSE)),
    (vecW,(vecNW,vecSW))
    ]

showWorld elfPositions = intercalate "\n" . reverse $ [[if (x,y) `elem` elfPositions then '#' else '.' | x <- [(-2)..11]] | y <- reverse [(-2)..11]]