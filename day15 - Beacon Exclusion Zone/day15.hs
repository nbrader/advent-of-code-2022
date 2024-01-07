#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package split-0.2.3.5 --package strict-0.4.0.1
-- use the following command to build: stack ghc -- "filename.hs" -O2
------------------------------------------
------------------------------------------
----  Day 15:  Beacon Exclusion Zone  ----
------------------------------------------
------------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 --package strict-0.4.0.1 -- '.\day15.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day15part1
-- 5142231

-- *Main> day15part2
-- 10884459367718


-------------
-- Imports --
-------------
{-# LANGUAGE BangPatterns #-}
import Control.Monad (guard)
import Data.List (intercalate, reverse, span, sort, intersperse, foldl', findIndex, map, delete, null, concatMap, minimumBy, groupBy)
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (fromJust, isJust)
import Debug.Trace (trace)
import Data.Char (ord,toUpper)
import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import Data.Function (on)


-------------
-- Program --
-------------
main = day15part2

day15part1 = do
    contents <- readFile "day15 (data).csv"
    -- let row = 10 -- example
    let sensors = map readSensor . lines $ contents
    let isExample = length sensors == 14
    let row | isExample = 10
            | otherwise = 2000000
    let intervalsAtRow = foldl' combine (Intervals []) . map fromJust . filter isJust . map (sensorToIntervalOnRow row) $ sensors
    print $ numCovered intervalsAtRow

day15part2 = do
    contents <- readFile "day15 (data).csv"
    let sensors = map readSensor . lines $ contents
    let isExample = length sensors == 14
    let minRow = 0
    let minCol = 0
    let maxRow | isExample = 20
               | otherwise = 4000000
    let maxCol | isExample = 20
               | otherwise = 4000000
    let rects = map sensorToInfNormBounds sensors
    let outsideLines = concat $ map rectToOutsideLines rects
    let horizLines = map fst $ filter (\(p,isHoriz) -> isHoriz) outsideLines
    let verticalLines = map fst $ filter (\(p,isHoriz) -> not isHoriz) outsideLines
    
    let intersections = [(x,y) | horizLine <- horizLines, verticalLine <- verticalLines, let mP = intersect horizLine verticalLine, isJust mP, let Just (x,y) = mP] 
    let quadTree = foldl' (flip addRectBodge) (emptyBodge (minCol, minRow, maxCol, maxRow)) rects
    
    print . head . map (\(x,y) -> x*4000000 + y) . filter (\(x,y) -> minCol <= x && x <= maxCol && minRow <= y && y <= maxRow) $ map infNormtoManhattanPoint $ filter (\(x,y) -> let result = (not . (flip inQuadTree) quadTree $ (x,y)) in if x-y `mod` 10000 == 0 then trace (show (x,y,result)) result else result) intersections

type Point = (Int, Int)
type Sensor = (Point,Point)

newtype Interval  = Interval (Int, Int) deriving (Show, Eq, Ord)
newtype Intervals = Intervals [Int] deriving (Show)

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

onAndOff = 1:(-1):onAndOff

combine :: Intervals -> Intervals -> Intervals
combine (Intervals xs) (Intervals ys) = Intervals . map fst . filter ((/= 0) . snd) . getDeltaEachPos . (\xs@((pos,num):_) -> (pos,num-1):xs) . reverse . map (fmap (\num -> if num >= 1 then 1 else 0)) $ foldl' getNumAtPos [] deltas'
  where deltas = merge (zip xs onAndOff)
                       (zip ys onAndOff)
        
        deltas' = filter ((/=0) . snd) $ map (\grp -> let pos = fst $ head grp in (pos, sum $ map snd grp)) $ groupBy ((==) `on` fst) deltas
        
        getNumAtPos :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
        getNumAtPos [] (pos,delta) = [(pos,delta)]
        getNumAtPos acc@((_,numAtPos):xs) (pos,delta) = ((pos,numAtPos+delta):acc)
        
        getDeltaEachPos :: [(Int,Int)] -> [(Int,Int)]
        getDeltaEachPos xs = map (\((p,curr),(_,prev)) -> (p,curr-prev)) $ zip (tail xs) xs

inspectedRow = 10

readSensor :: String -> Sensor
readSensor inStr = ((sX,sY),(bX,bY))
  where (sensorXStr,afterSensorXStr) = break (==',') (drop (length "Sensor at x="             ) $ inStr)
        (sensorYStr,afterSensorYStr) = break (==':') (drop (length ", y="                     ) $ afterSensorXStr)
        (beaconXStr,afterBeaconXStr) = break (==',') (drop (length ": closest beacon is at x=") $ afterSensorYStr)
        beaconYStr = drop (length ", y=") afterBeaconXStr
        sX = read sensorXStr
        sY = read sensorYStr
        bX = read beaconXStr
        bY = read beaconYStr

sensorToIntervalOnRow :: Int -> Sensor -> Maybe Intervals
sensorToIntervalOnRow row ((sX,sY),(bX,bY)) | rad > 0   = Just $ Intervals [sX-rad,sX+rad]
                                            | otherwise = Nothing
  where dist = abs (bX-sX) + abs (bY-sY) -- manhattan distance
        rad = dist - abs (row-sY)

numCovered :: Intervals -> Int
numCovered (Intervals xs) = sum . map (\[x1,x2] -> x2-x1) . chunksOf 2 $ xs

-- contains :: Intervals -> Int -> Bool
-- (Intervals xs) `contains` x = any (\[x1,x2] -> x1 <= x && x <= x2) (chunksOf 2 xs)

-- showRow :: Int -> Int -> Intervals -> String
-- showRow min max intervals = [if intervals `contains` x then '.' else 'X'| x <- [min..max]]

infNormfromManhattanPoint (x,y) = (x-y,x+y)
infNormtoManhattanPoint   (x,y) = ((y+x)`div`2, (y-x)`div`2)

type MinX = Int
type MinY = Int
type MaxX = Int
type MaxY = Int
type Rect = (MinX,MinY,MaxX,MaxY)

sensorToInfNormBounds :: Sensor -> Rect
sensorToInfNormBounds ((sX,sY),(bX,bY)) = (infNormX-radius, infNormY-radius, infNormX+radius, infNormY+radius)
  where radius = abs (bX-sX) + abs (bY-sY) -- manhattan distance
        (infNormX,infNormY) = infNormfromManhattanPoint (sX,sY)

type PivotX = Int
type PivotY = Int
type TopLeft = QuadTree
type TopRight = QuadTree
type BottomLeft = QuadTree
type BottomRight = QuadTree
data QuadTree = Empty Rect | Full | Branch PivotX PivotY TopLeft TopRight BottomLeft BottomRight deriving (Show)

addRect :: Rect -> QuadTree -> QuadTree
addRect r@(x0,y0,x1,y1) (Empty (x0',y0',x1',y1')) = Branch x0 y0 (Empty (x0',y0',x0,y0)) (Empty (x0,y0',x1',y0)) (Empty (x0',y0,x0,y1')) (Branch x1 y1 Full (Empty (x1,y0,x1',y1)) (Empty (x0,y1,x1,y1')) (Empty (x1',y1,x1',y1')))
addRect r@(x0,y0,x1,y1) Full  = Full
addRect r@(x0,y0,x1,y1) (Branch xP yP tl tr bl br)
    | x1 < xP && y1 < yP = (Branch xP yP (addRect r tl) tr bl br) -- in top left
    | x1 < xP && y0 < yP = (Branch xP yP (addRect (x0,y0,x1,yP) tl) tr (addRect (x0,yP,x1,y1) bl) br) -- across left
    | x1 < xP            = (Branch xP yP tl tr (addRect r bl) br) -- in bottom left
    | x0 < xP && y1 < yP = (Branch xP yP (addRect (x0,y0,xP,y1) tl) (addRect (xP,y0,x1,y1) tr) bl br) -- across top 2
    | x0 < xP && y0 < yP = (Branch xP yP (addRect (x0,y0,xP,yP) tl) (addRect (xP,y0,x1,yP) tr) (addRect (x0,yP,xP,y1) bl) (addRect (xP,yP,x1,y1) br)) -- across all 4
    | x0 < xP            = (Branch xP yP tl tr (addRect (x0,y0,xP,y1) bl) (addRect (xP,y0,x1,y1) br)) -- across bottom 2
    |            y1 < yP = (Branch xP yP tl (addRect r tr) bl br) -- in top right
    |            y0 < yP = (Branch xP yP tl (addRect (x0,y0,x1,yP) tr) bl (addRect (x0,yP,x1,y1) br)) -- across right
    | otherwise          = (Branch xP yP tl tr bl (addRect r br)) -- in bottom right

addRectBodge (x0,y0,x1,y1) = addRect (x0-1,y0-1,x1,y1)
emptyBodge (x0,y0,x1,y1) = Empty (x0-1,y0-1,x1,y1)

inQuadTree :: Point -> QuadTree -> Bool
inQuadTree (x,y) (Empty (x0,y0,x1,y1)) = False
inQuadTree (x,y) Full  = True
inQuadTree (x,y) (Branch xP yP tl tr bl br)
    | x <= xP && y <= yP = inQuadTree (x,y) tl
    | x <= xP            = inQuadTree (x,y) bl
    |            y <= yP = inQuadTree (x,y) tr
    | otherwise          = inQuadTree (x,y) br

showQuadTree :: Int -> Int -> Int -> Int -> QuadTree -> [String]
showQuadTree xMin yMin xMax yMax quadTree = [[if (x,y) `inQuadTree` quadTree then '0' else '1'| x <- [xMin..xMax]] | y <- [yMin..yMax]]

rectToOutsideLines :: Rect -> [(((Int,Int),(Int,Int)),Bool)]
rectToOutsideLines (x0',y0',x1',y1') = [(((x0,y0),(x0,y1)), True),(((x1,y0),(x1,y1)), True),(((x0,y0),((x1,y0))), False),(((x0,y1),(x1,y1)), False)]
  where (x0,y0,x1,y1) = (x0'-1,y0'-1,x1'+1,y1'+1)

intersect ((x0,y0),(_,y1)) ((x0',y0'),(x1',_)) | (y0 <= y0' && y0' <= y1 && x0' <= x0 && x0 <= x1') = Just (x0,y0')
                                               | otherwise = Nothing