#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package containers-0.6.5.1 --package split-0.2.3.5
-------------------------------------
-------------------------------------
----  Day 10:  Cathode-Ray Tube  ----
-------------------------------------
-------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package containers-0.6.5.1 --package split-0.2.3.5 -- '.\day10.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day10part1
-- 14160

-- *Main> day10part2
-- RJERPEFC


-------------
-- Imports --
-------------
import Data.List (transpose)
import Data.List.Split (splitOn, chunksOf)
import Data.Map as M (lookup, fromList)
import Data.Maybe (fromJust)
import Debug.Trace (trace)


-------------
-- Program --
-------------
main = day10part2

day10part1 = do
    contents <- readFile "day10 (data).csv"
    let registerHistory = opsToRegisterHistory . map parseLn . lines $ contents
    -- mapM_ print registerHistory
    -- putStrLn ""
    print . sum $ do
                    i <- [20, 60.. (length registerHistory - 1)]
                    let regVal = fromJust $ M.lookup i (M.fromList $ zip [1..] (registerHistory))
                    
                    return (i*regVal)

day10part2 = do
    contents <- readFile "day10 (data).csv"
    
    let (width,height) = (40, 6)
        (xMin,xMax) = (0, width-1)
        (yMin,yMax) = (0, height-1)
        
        spritePosHistory = opsToRegisterHistory . map parseLn . lines $ contents
        cursorHistory = concat . repeat $ [x | _ <- [yMin..yMax], x <- [xMin..xMax]]
        cursorSpritePairs = zip cursorHistory spritePosHistory
        pixelChars = [if abs (cursorX-spriteX) <= 1 then '#' else '.' | (cursorX, spriteX) <- cursorSpritePairs]
    
    let asciiArt = chunksOf width $ pixelChars
    let maybeSimple = asciiArt2String asciiArt
    case maybeSimple of
        Just simple -> putStrLn simple
        Nothing     -> putStrLn $ unlines asciiArt

data Op = NoOp | AddX Int deriving (Show)

parseLn cs = case (take 4 cs) of
               "noop" -> NoOp
               "addx" -> AddX (read (drop 5 cs) :: Int)

opsToRegisterHistory = init . scanl (+) 1 . concat . map opToRegisterChanges
  where opToRegisterChanges NoOp = [0]
        opToRegisterChanges (AddX x) = [0,x]

-- AsciiArt to String

asciiArt2String :: [String] -> Maybe String
asciiArt2String rows = sequence $ map (flip M.lookup asciiArt2Char) $ transpose chunked
  where chunked = map (chunksOf 5) rows

-- asciiArt2Char :: M.Internal.Map [String] Char
asciiArt2Char = M.fromList [
        ([
            ".##..",
            "#..#.",
            "#..#.",
            "####.",
            "#..#.",
            "#..#."
        ],'A'),

        ([
            ".##..",
            "#..#.",
            "#....",
            "#....",
            "#..#.",
            ".##.."
        ],'C'),

        ([
            "####.",
            "#....",
            "###..",
            "#....",
            "#....",
            "####."
        ],'E'),

        ([
            "####.",
            "#....",
            "###..",
            "#....",
            "#....",
            "#...."
        ],'F'),

        ([
            ".##..",
            "#..#.",
            "#....",
            "#.##.",
            "#..#.",
            ".###."
        ],'G'),

        ([
            "#..#.",
            "#..#.",
            "####.",
            "#..#.",
            "#..#.",
            "#..#."
        ],'H'),

        ([
            "..##.",
            "...#.",
            "...#.",
            "...#.",
            "#..#.",
            ".##.."
        ],'J'),

        ([
            "#..#.",
            "#.#..",
            "##...",
            "#.#..",
            "#.#..",
            "#..#."
        ],'K'),

        ([
            "#....",
            "#....",
            "#....",
            "#....",
            "#....",
            "####."
        ],'L'),

        ([
            "###..",
            "#..#.",
            "#..#.",
            "###..",
            "#....",
            "#...."
        ],'P'),

        ([
            "###..",
            "#..#.",
            "#..#.",
            "###..",
            "#.#..",
            "#..#."
        ],'R'),
        ([
            "####.",
            "...#.",
            "..#..",
            ".#...",
            "#....",
            "####."
        ],'Z')
    ]