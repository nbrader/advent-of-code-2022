#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package split-0.2.3.5

---------------------------------------
---------------------------------------
----  Day 2:  Rock Paper Scissors  ----
---------------------------------------
---------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 -- '.\day2.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day2part1
-- 11449

-- *Main> day2part2
-- 13187


-------------
-- Imports --
-------------
import Data.List (sort)
import Data.List.Split (splitOn)


-------------
-- Program --
-------------
main = day2part2

day2part1 = do
      contents <- readFile "day2 (data).csv"
      let totalScore = sum . map myScoreForGame
                           . decodePart1 $ contents
      print totalScore

day2part2 = do
      contents <- readFile "day2 (data).csv"
      let totalScore = sum . map myScoreForGame
                           . map (\(theirMove, targetOutcome) -> (theirMove, responseForOutcome theirMove targetOutcome))
                           . decodePart2 $ contents
      print totalScore

data Move = Rock | Paper | Scissors
data Outcome = MyLoss | Draw | MyWin deriving (Eq)

data TheirMove = TheirMove Move
data MyMove    = MyMove    Move

allMoves :: [Move]
allMoves = [Rock,Paper,Scissors]

rockPaperScissors :: TheirMove -> MyMove -> Outcome
rockPaperScissors (TheirMove Rock)     (MyMove Rock)     = Draw
rockPaperScissors (TheirMove Paper)    (MyMove Paper)    = Draw
rockPaperScissors (TheirMove Scissors) (MyMove Scissors) = Draw
rockPaperScissors (TheirMove Rock)     (MyMove Paper)    = MyWin
rockPaperScissors (TheirMove Paper)    (MyMove Scissors) = MyWin
rockPaperScissors (TheirMove Scissors) (MyMove Rock)     = MyWin
rockPaperScissors (TheirMove Rock)     (MyMove Scissors) = MyLoss
rockPaperScissors (TheirMove Paper)    (MyMove Rock)     = MyLoss
rockPaperScissors (TheirMove Scissors) (MyMove Paper)    = MyLoss


-------------
-- Scoring --
-------------
myPointsForMove (MyMove Rock)     = 1
myPointsForMove (MyMove Paper)    = 2
myPointsForMove (MyMove Scissors) = 3

myPointsForOutcome MyLoss = 0
myPointsForOutcome Draw   = 3
myPointsForOutcome MyWin  = 6

myScoreForGame (theirMove, myMove)
    = myPointsForMove myMove + myPointsForOutcome (rockPaperScissors theirMove myMove)


--------------------------
-- Decoding Cheat Sheet --
--------------------------
decodePart1 = map (\[x,y] -> (readAsTheirMove x, readAsMyMove  y)) . map (splitOn " ") . lines
decodePart2 = map (\[x,y] -> (readAsTheirMove x, readAsOutcome y)) . map (splitOn " ") . lines

readAsTheirMove :: String -> TheirMove
readAsTheirMove "A" = TheirMove Rock
readAsTheirMove "B" = TheirMove Paper
readAsTheirMove "C" = TheirMove Scissors

readAsMyMove :: String -> MyMove
readAsMyMove "X" = MyMove Rock
readAsMyMove "Y" = MyMove Paper
readAsMyMove "Z" = MyMove Scissors

readAsOutcome :: String -> Outcome
readAsOutcome "X" = MyLoss
readAsOutcome "Y" = Draw
readAsOutcome "Z" = MyWin

responseForOutcome :: TheirMove -> Outcome -> MyMove
responseForOutcome theirMove targetOutcome
    = head . filter ((==targetOutcome)
           . rockPaperScissors theirMove) $ map MyMove allMoves