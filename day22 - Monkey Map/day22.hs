#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package strict-0.4.0.1 --package unordered-containers-0.2.19.1
-------------------------------
-------------------------------
----  Day 22:  Monkey Map  ----
-------------------------------
-------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package strict-0.4.0.1 --package unordered-containers-0.2.19.1 -- '.\day22.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day22part1
-- 106094

-- *Main> day22part2
-- 162038


-------------
-- Imports --
-------------
import Data.List
import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import qualified Data.HashMap.Strict as Map
import Data.HashSet (HashSet)
import Data.HashSet as H hiding (map, foldl')
import Data.Either (lefts)


-------------
-- Program --
-------------
main = day22part2

day22part1 = do
    contents <- readFile "day22 (data).csv"
    let fileRows  = lines contents
    let worldRows = init fileRows
    let pathRow   = last fileRows
    let world :: World
        world = readWorld worldRows
    
    let pathMoves :: [Move]
        pathMoves = readPath pathRow
    
    let pathPoints = map head . group $ walkPath pathMoves world
    let ((finalCol, finalRow), finalDir) = last $ pathPoints
    let answer = 1000*finalRow + 4*finalCol + dirToNum finalDir
    
    -- print ((finalCol, finalRow), finalDir)
    print answer
    
    -- putStrLn $ showWorld (16,12) world pathPoints   -- example world size
    -- putStrLn $ showWorld (100,200) world pathPoints -- actual puzzle input data world size


day22part2 = do
    contents <- readFile "day22 (data).csv"
    let fileRows  = lines contents
    let worldRows = init fileRows
    let pathRow   = last fileRows
    let world :: World
        world = readWorld worldRows
    
    let pathMoves :: [Move]
        pathMoves = readPath pathRow
    
    let isBigCube = length (head worldRows) > 16
    
    let pathPoints = map head . group $ walkPath2 isBigCube pathMoves world
    let ((finalCol, finalRow), finalDir) = last $ pathPoints
    let answer = 1000*finalRow + 4*finalCol + dirToNum finalDir
    
    -- print ((finalCol, finalRow), finalDir)
    print answer
    
    -- putStrLn $ showWorld (16,12) world pathPoints   -- example world size
    -- putStrLn $ showWorld (100,200) world pathPoints -- actual puzzle input data world size

addV2 (x1,y1) (x2,y2) = (x1+x2, y1+y2)
subV2 (x1,y1) (x2,y2) = (x1-x2, y1-y2)

type Point = (Integer, Integer)
data World = World {
    start  :: Point,
    floors :: H.HashSet Point,
    walls  :: H.HashSet Point } deriving (Show)
data Move = Walk | RotL90 | RotR90 deriving (Show)

type Floor = Point
type Wall  = Point
readWorld :: [String] -> World
readWorld rows = foldl' insertFloorOrWall emptyWorldWithStart floorAndWalls
  where floorAndWalls :: [Either Floor Wall]
        floorAndWalls = toFloorsAndWalls rows
        
        emptyWorldWithStart :: World
        emptyWorldWithStart = World {
            start  = head $ lefts floorAndWalls,
            floors = H.fromList [],
            walls  = H.fromList []
            }
        
        toFloorsAndWalls :: [String] -> [Either Floor Wall]
        toFloorsAndWalls rows = [readChar c (colNum, rowNum) | (rowNum, row) <- zip [1..] rows, (colNum,c) <- zip [1..] row, case c of {' ' -> False; _ -> True}]
          where readChar '.' pos = Left  pos
                readChar '#' pos = Right pos
        
        insertFloorOrWall :: World -> Either Floor Wall -> World
        insertFloorOrWall world (Left  f) = world {floors = H.insert f $ floors world}
        insertFloorOrWall world (Right w) = world {walls  = H.insert w $ walls  world}

data Token = TokenNum String | TokenChar Char
readPath :: String -> [Move]
readPath = parser . lexer []
  where lexer :: String -> String -> [Token]
        lexer []     [] = []
        lexer numStr [] = TokenNum (reverse numStr) : []
        lexer []     ('L':remainder) =                             TokenChar 'L' : lexer [] remainder
        lexer numStr ('L':remainder) = TokenNum (reverse numStr) : TokenChar 'L' : lexer [] remainder
        lexer []     ('R':remainder) =                             TokenChar 'R' : lexer [] remainder
        lexer numStr ('R':remainder) = TokenNum (reverse numStr) : TokenChar 'R' : lexer [] remainder
        lexer numStr (c:remainder)   = lexer (c:numStr) remainder
        
        parser :: [Token] -> [Move]
        parser [] = []
        parser (TokenNum numStr:ts) = replicate (read numStr :: Int) Walk ++ parser ts
        parser (TokenChar 'L'  :ts) = RotL90 : parser ts
        parser (TokenChar 'R'  :ts) = RotR90 : parser ts

data Dir = R | U | L | D deriving (Show, Eq)
rotL90, rotR90 :: Dir -> Dir

rotL90 R = U
rotL90 U = L
rotL90 L = D
rotL90 D = R

rotR90 R = D
rotR90 U = R
rotR90 L = U
rotR90 D = L

opposite R = L
opposite U = D
opposite L = R
opposite D = U

fromDir R = ( 1, 0)
fromDir U = ( 0,-1)
fromDir L = (-1, 0)
fromDir D = ( 0, 1)

inWorld :: Point -> World -> Bool
inWorld pos world = (   pos `H.member` walls  world
                     || pos `H.member` floors world )

walkPath :: [Move] -> World -> [(Point,Dir)]
walkPath moves world = scanl' doMove (start world, R) moves
  where doMove (pos,dir) Walk   = let newPosIgnoringOuter = pos `addV2` fromDir dir
                                      stepBack = addV2 (fromDir (opposite dir))
                                      newPosIgnoringWalls | newPosIgnoringOuter `inWorld` world = newPosIgnoringOuter
                                                          | otherwise = until (\pos' -> not ( stepBack pos' `inWorld` world )) stepBack pos
                                      pos' | newPosIgnoringWalls `H.member` walls world = pos
                                           | otherwise = newPosIgnoringWalls
                                  in (pos',dir)
        doMove (pos,dir) RotL90 = (pos, rotL90 dir)
        doMove (pos,dir) RotR90 = (pos, rotR90 dir)


type Edge = (Point,Point)
type EdgeID = Int
cubeEdges :: Bool -> Map.HashMap EdgeID Edge
cubeEdges isBigCube = if isBigCube then cubeEdgesBig else cubeEdgesSmall

cubeEdgesBig :: Map.HashMap EdgeID Edge
cubeEdgesBig = Map.fromList $ zip [0..] [
    ((51,1),(100,1)),
    ((1,151),(1,200)),
    
    ((101,1),(150,1)),
    ((1,200),(50,200)),
    
    ((51,1),(51,50)),
    ((1,150),(1,101)),
    
    ((51,51),(51,100)),
    ((1,101),(50,101)),
    
    ((101,50),(150,50)),
    ((100,51),(100,100)),
    
    ((150,1),(150,50)),
    ((100,150),(100,101)),
    
    ((51,150),(100,150)),
    ((50,151),(50,200))
    ]

cubeEdgesSmall :: Map.HashMap EdgeID Edge
cubeEdgesSmall = Map.fromList $ zip [0..] [
    ((9,1),(12,1)),
    ((12,1),(12,4)),
    ((12,5),(12,8)),
    ((16,9),(13,9)),
    ((16,12),(16,9)),
    ((16,12),(13,12)),
    ((12,12),(9,12)),
    ((9,12),(9,9)),
    ((5,8),(8,8)),
    ((1,8),(4,8)),
    ((1,5),(1,8)),
    ((4,5),(1,5)),
    ((5,5),(8,5)),
    ((9,1),(9,4))
    ]

linkedCubeEdges isBigCube = if isBigCube then linkedCubeEdgesBig else linkedCubeEdgesSmall

linkedCubeEdgesBig = Map.fromList [
    (0,1),
    (1,0),
    (2,3),
    (3,2),
    (4,5),
    (5,4),
    (6,7),
    (7,6),
    (8,9),
    (9,8),
    (10,11),
    (11,10),
    (12,13),
    (13,12)
    ]

linkedCubeEdgesSmall = Map.fromList [
    (0,11),
    (11,0),
    
    (1,4),
    (4,1),
    
    (2,3),
    (3,2),
    
    (5,10),
    (10,5),
    
    (6,9),
    (9,6),
    
    (7,8),
    (8,7),
    
    (12,13),
    (13,12)
    ]

getTopLeftOfFace :: Bool -> Point -> Point
getTopLeftOfFace isBigCube = getTopLeftOfFaceWithFaceSize faceSize
  where faceSize | isBigCube = 50
                 | otherwise = 4

getTopLeftOfFaceWithFaceSize :: Integer -> Point -> Point
getTopLeftOfFaceWithFaceSize faceSize (x,y) = (faceSize*i+1,faceSize*j+1)
  where (i,j) = ((x-1)`div`faceSize,(y-1)`div`faceSize)
        
topLeftToFromEdgeID :: Bool -> (Point,Dir) -> EdgeID
topLeftToFromEdgeID isBigCube = if isBigCube then topLeftToFromEdgeIDBig else topLeftToFromEdgeIDSmall

topLeftToFromEdgeIDBig :: (Point,Dir) -> EdgeID
topLeftToFromEdgeIDBig (topLeft@( 51,  1),R) = undefined
topLeftToFromEdgeIDBig (topLeft@( 51,  1),U) = 0
topLeftToFromEdgeIDBig (topLeft@( 51,  1),L) = 4
topLeftToFromEdgeIDBig (topLeft@( 51,  1),D) = undefined
topLeftToFromEdgeIDBig (topLeft@(101,  1),R) = 10
topLeftToFromEdgeIDBig (topLeft@(101,  1),U) = 2
topLeftToFromEdgeIDBig (topLeft@(101,  1),L) = undefined
topLeftToFromEdgeIDBig (topLeft@(101,  1),D) = 8
topLeftToFromEdgeIDBig (topLeft@( 51, 51),R) = 9
topLeftToFromEdgeIDBig (topLeft@( 51, 51),U) = undefined
topLeftToFromEdgeIDBig (topLeft@( 51, 51),L) = 6
topLeftToFromEdgeIDBig (topLeft@( 51, 51),D) = undefined
topLeftToFromEdgeIDBig (topLeft@(  1,101),R) = undefined
topLeftToFromEdgeIDBig (topLeft@(  1,101),U) = 7
topLeftToFromEdgeIDBig (topLeft@(  1,101),L) = 5
topLeftToFromEdgeIDBig (topLeft@(  1,101),D) = undefined
topLeftToFromEdgeIDBig (topLeft@( 51,101),R) = 11
topLeftToFromEdgeIDBig (topLeft@( 51,101),U) = undefined
topLeftToFromEdgeIDBig (topLeft@( 51,101),L) = undefined
topLeftToFromEdgeIDBig (topLeft@( 51,101),D) = 12
topLeftToFromEdgeIDBig (topLeft@(  1,151),R) = 13
topLeftToFromEdgeIDBig (topLeft@(  1,151),U) = undefined
topLeftToFromEdgeIDBig (topLeft@(  1,151),L) = 1
topLeftToFromEdgeIDBig (topLeft@(  1,151),D) = 3

topLeftToFromEdgeIDSmall :: (Point,Dir) -> EdgeID
topLeftToFromEdgeIDSmall (topLeft@( 9,1),R) = 1
topLeftToFromEdgeIDSmall (topLeft@( 9,1),U) = 0
topLeftToFromEdgeIDSmall (topLeft@( 9,1),L) = 13
topLeftToFromEdgeIDSmall (topLeft@( 9,1),D) = undefined
topLeftToFromEdgeIDSmall (topLeft@( 1,5),R) = undefined
topLeftToFromEdgeIDSmall (topLeft@( 1,5),U) = 11
topLeftToFromEdgeIDSmall (topLeft@( 1,5),L) = 10
topLeftToFromEdgeIDSmall (topLeft@( 1,5),D) = 9
topLeftToFromEdgeIDSmall (topLeft@( 5,5),R) = undefined
topLeftToFromEdgeIDSmall (topLeft@( 5,5),U) = 12
topLeftToFromEdgeIDSmall (topLeft@( 5,5),L) = undefined
topLeftToFromEdgeIDSmall (topLeft@( 5,5),D) = 8
topLeftToFromEdgeIDSmall (topLeft@( 9,5),R) = 2
topLeftToFromEdgeIDSmall (topLeft@( 9,5),U) = undefined
topLeftToFromEdgeIDSmall (topLeft@( 9,5),L) = undefined
topLeftToFromEdgeIDSmall (topLeft@( 9,5),D) = undefined
topLeftToFromEdgeIDSmall (topLeft@( 9,9),R) = undefined
topLeftToFromEdgeIDSmall (topLeft@( 9,9),U) = undefined
topLeftToFromEdgeIDSmall (topLeft@( 9,9),L) = 7
topLeftToFromEdgeIDSmall (topLeft@( 9,9),D) = 6
topLeftToFromEdgeIDSmall (topLeft@(13,9),R) = 4
topLeftToFromEdgeIDSmall (topLeft@(13,9),U) = 3
topLeftToFromEdgeIDSmall (topLeft@(13,9),L) = undefined
topLeftToFromEdgeIDSmall (topLeft@(13,9),D) = 5
        
toEdgeDir :: Bool ->  EdgeID -> Dir
toEdgeDir isBigCube = if isBigCube then toEdgeDirBig else toEdgeDirSmall

toEdgeDirBig :: EdgeID -> Dir
toEdgeDirBig 0  = D
toEdgeDirBig 1  = R
toEdgeDirBig 2  = D
toEdgeDirBig 3  = U
toEdgeDirBig 4  = R
toEdgeDirBig 5  = R
toEdgeDirBig 6  = R
toEdgeDirBig 7  = D
toEdgeDirBig 8  = U
toEdgeDirBig 9  = L
toEdgeDirBig 10 = L
toEdgeDirBig 11 = L
toEdgeDirBig 12 = U
toEdgeDirBig 13 = L

toEdgeDirSmall :: EdgeID -> Dir
toEdgeDirSmall 0  = D
toEdgeDirSmall 1  = L
toEdgeDirSmall 2  = L
toEdgeDirSmall 3  = D
toEdgeDirSmall 4  = L
toEdgeDirSmall 5  = U
toEdgeDirSmall 6  = U
toEdgeDirSmall 7  = R
toEdgeDirSmall 8  = U
toEdgeDirSmall 9  = U
toEdgeDirSmall 10 = R
toEdgeDirSmall 11 = D
toEdgeDirSmall 12 = D
toEdgeDirSmall 13 = R

invLerpSteps :: Bool -> Point -> Edge -> Integer
invLerpSteps isBigCube = invLerpStepsWithFaceSize faceSize
  where faceSize | isBigCube = 50
                 | otherwise = 4

invLerpStepsWithFaceSize :: Integer -> Point -> Edge -> Integer
invLerpStepsWithFaceSize faceSize (x,y) (from@(x0,y0),to)
    = let delta@(dx,dy) = to `subV2` from
      in case delta of
          (0,_) -> (y-y0)*(signum dy) `mod` faceSize
          (_,0) -> (x-x0)*(signum dx) `mod` faceSize

lerpSteps :: Integer -> Edge -> Point
lerpSteps steps (from@(x0,y0),to)
    = let (dx,dy) = to `subV2` from
      in (x0 + steps*(signum dx), y0 + steps*(signum dy))

linkedPos :: Bool -> (Point, Dir) -> (Point, Dir)
linkedPos isBigCube (pos, dir) = (lerpSteps steps toEdge, toEdgeDir isBigCube toEdgeID)
  where fromEdgeID = topLeftToFromEdgeID isBigCube (getTopLeftOfFace isBigCube pos, dir)
        fromEdge = cubeEdges isBigCube Map.! fromEdgeID
        
        toEdgeID = linkedCubeEdges isBigCube Map.! fromEdgeID
        toEdge = cubeEdges isBigCube Map.! toEdgeID
        
        steps = invLerpSteps isBigCube pos fromEdge

walkPath2 :: Bool -> [Move] -> World -> [(Point,Dir)]
walkPath2 isBigCube moves world = scanl' doMove (start world, R) moves
  where doMove (pos,dir) Walk   = let newPosIgnoringOuter = pos `addV2` fromDir dir
                                      stepBack = addV2 (fromDir (opposite dir))
                                      (newPosIgnoringWalls, newDirIgnoringWalls) | newPosIgnoringOuter `inWorld` world = (newPosIgnoringOuter, dir)
                                                                                 | otherwise = linkedPos isBigCube (pos, dir)
                                      posAndDir' | newPosIgnoringWalls `H.member` walls world = (pos,dir)
                                                 | otherwise = (newPosIgnoringWalls, newDirIgnoringWalls)
                                  in posAndDir'
        doMove (pos,dir) RotL90 = (pos, rotL90 dir)
        doMove (pos,dir) RotR90 = (pos, rotR90 dir)

dirToNum R = 0
dirToNum U = 3
dirToNum L = 2
dirToNum D = 1

showWorld (dimX,dimY) world pathPoints = intercalate "\n" . reverse $ [[case find ((==(x,y)) . fst) (reverse pathPoints) of
                                                                            Just (_,R) -> '>'
                                                                            Just (_,U) -> '^'
                                                                            Just (_,L) -> '<'
                                                                            Just (_,D) -> 'V'
                                                                            Nothing -> (if (x,y) `H.member` floors world then '.' else (if (x,y) `H.member` walls world then '#' else ' ')) | x <- [0..dimX]] | y <- reverse [0..dimY]]