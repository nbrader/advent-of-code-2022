#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package containers-0.6.5.1 --package split-0.2.3.5 --package deque-0.4.4
-----------------------------------------
-----------------------------------------
----  Day 11:  Monkey in the Middle  ----
-----------------------------------------
-----------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package containers-0.6.5.1 --package split-0.2.3.5 --package deque-0.4.4 -- '.\day11 (part 2).hs' -O2
-}

------------
-- Output --
------------
-- *Main> day11part2
-- 18170818354


-------------
-- Imports --
-------------
{-# LANGUAGE BangPatterns #-}
import Data.List (sort, intersperse, foldl')
import Data.List.Split (splitOn, chunksOf)
import Data.Map as M hiding (map, foldl, foldr, foldl')
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Deque.Lazy as D hiding (head, tail)
import GHC.Exts as E (fromList, toList)


-------------
-- Program --
-------------
main = day11part2

day11part2 = do
    contents <- readFile "day11 (data).csv"
    let initMonkeys = parse contents
        finalMonkeys = foldl' (flip ($!)) initMonkeys (replicate 10000 doMonkeyRound)
        monkeyHistory = (scanl (flip ($!)) initMonkeys (replicate 10000 doMonkeyRound))
    print . product . Prelude.take 2 . Prelude.reverse . sort . map inspections . map snd . E.toList $ finalMonkeys
    return ()

data Monkey = Monkey {items :: Deque Integer, updtInteger :: Integer -> Integer, test :: Integer -> Bool, ifTrue :: Int,  ifFalse :: Int, inspections :: Integer}

instance Show Monkey where
  show Monkey {items = xs, updtInteger = _, test = _, ifTrue = t,  ifFalse = f, inspections = count} = "Monkey {items = " ++ show xs ++ ", updtInteger = _, test = _, ifTrue = " ++ show t ++ ",  ifFalse = " ++ show f ++ ", inspections = " ++ show count ++ "}"

unsafeLookup k mp = case M.lookup k mp of
                     Nothing -> error $ show k ++ " " ++ show mp
                     Just x -> x

parse :: String -> M.Map Int Monkey
parse = M.fromList . zip [0..] . map readMonkey . splitOn [""] . lines

readFunc :: String -> String -> Integer -> Integer
readFunc varStr funcStr
    | '+' `elem` funcStr = processPlus . map (\x -> if x == "old" then Nothing else Just x) . splitOn "+" . Prelude.filter (/= ' ') $ funcStr
    | '*' `elem` funcStr = processMult . map (\x -> if x == "old" then Nothing else Just x) . splitOn "*" . Prelude.filter (/= ' ') $ funcStr
    | otherwise = error $ "readFunc varStr funcStr: " ++ show varStr ++ " " ++ show funcStr
  where processMult [Nothing,Nothing] = \x -> x*x
        processMult [Nothing,Just c2] = \x -> x*(read c2)
        processMult [Just c1,Nothing] = \x -> (read c1)*x
        processMult [Just c1,Just c2] = \x -> (read c1)*(read c2)
        processMult xs = error (show xs)
        
        processPlus [Nothing,Nothing] = \x -> x+x
        processPlus [Nothing,Just c2] = \x -> x+(read c2)
        processPlus [Just c1,Nothing] = \x -> (read c1)+x
        processPlus [Just c1,Just c2] = \x -> (read c1)+(read c2)
        processPlus xs = error (show xs)

readMonkey :: [String] -> Monkey
readMonkey inputStr
    = Monkey {
        items = E.fromList . read . (\listStr -> "[" ++ listStr ++ "]") . Prelude.drop 18 $ inputStr !! 1,
        updtInteger = readFunc "old" . Prelude.drop 19 $ inputStr !! 2,
        test = (\d x -> x `mod` d == 0) . read . Prelude.drop 21 $ inputStr !! 3,
        ifTrue = read . Prelude.drop 29 $ inputStr !! 4,
        ifFalse = read . Prelude.drop 30 $ inputStr !! 5,
        inspections = 0}

updateNextInteger :: Int -> M.Map Int Monkey -> M.Map Int Monkey
updateNextInteger monkeyID oldMonkeys = M.adjust updateMonkey monkeyID oldMonkeys
  where monkey = unsafeLookup monkeyID oldMonkeys
        isExample = (length $ M.keys oldMonkeys) < 8
        monkeyLCM
            | isExample = 13*17*19*23
            | otherwise = 2*3*5*7*11*13*17*19
        updateItem = (`mod` monkeyLCM) . updtInteger monkey
        updateMonkey m = m {items = E.fromList $ (updateItem $ head (E.toList $ items m)) : tail (E.toList $ items m)}

transferItem :: Int -> M.Map Int Monkey -> M.Map Int Monkey
transferItem fromID oldMonkeys = M.insert toID toMonkey' . M.insert fromID fromMonkey' $ oldMonkeys
  where fromMonkey = unsafeLookup fromID oldMonkeys
        
        Just (item, fromMonkeyNewItems) = uncons (items fromMonkey)
        
        toID = if test fromMonkey item then ifTrue fromMonkey else ifFalse fromMonkey
        toMonkey   = unsafeLookup toID   oldMonkeys
        
        toMonkeyNewItems                = snoc   item (items toMonkey)
        
        fromMonkey' = fromMonkey {items = fromMonkeyNewItems, inspections = inspections fromMonkey + 1}
        toMonkey'   = toMonkey {items = toMonkeyNewItems}

doMonkeyTurn :: Int -> M.Map Int Monkey -> M.Map Int Monkey
doMonkeyTurn monkeyID oldMonkeys = foldl' (flip ($!)) oldMonkeys (replicate (length . E.toList $ items currMonkey) monkeyThrow)
  where currMonkey = unsafeLookup monkeyID oldMonkeys
        monkeyThrow :: M.Map Int Monkey -> M.Map Int Monkey
        monkeyThrow = transferItem monkeyID . updateNextInteger monkeyID

doMonkeyRound :: M.Map Int Monkey -> M.Map Int Monkey
doMonkeyRound prev = foldl' (flip ($!)) prev [doMonkeyTurn i | i <- [0..(size prev-1)]]