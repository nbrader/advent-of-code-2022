#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package strict-0.4.0.1 --package containers-0.6.5.1 --package mtl-2.2.2 --package vector-0.12.3.1
---------------------------------------------
---------------------------------------------
----  Day 20:  Grove Positioning System  ----
---------------------------------------------
---------------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-18.22 ghc --package strict-0.4.0.1 --package containers-0.6.5.1 --package mtl-2.2.2 --package vector-0.12.3.1 -- '.\day20.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day20part1
-- 4224

-- *Main> day20part2
-- 861907680486


-------------
-- Imports --
-------------
import Data.List
import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import Data.Maybe (fromJust)
import Control.Monad.Reader
import Data.Vector.Unboxed as V (Vector(..), fromList, (!), (//), replicate, length)
import qualified Data.Sequence as SQ
import Data.Sequence hiding (fromList, zipWith, length, drop, take)
import qualified Data.Foldable as F hiding (length)


-------------
-- Program --
-------------
main = day20part2

day20part1 = do
    contents <- readFile "day20 (data).csv"
    let origValList = map (read :: String -> Int) . lines $ contents
        
        -- find zero and make it first
        zeroIndex = fromJust $ elemIndex 0 origValList
        rotatedList = rotateList zeroIndex origValList
        
        -- make zero implicit, this is our value list
        valueList = drop 1 rotatedList
        valueArr  = fromList valueList
        
        size = Prelude.length valueList
        
        initNumCircle :: NumCircle
        initNumCircle = let xs = rotateList (-(zeroIndex-1)) $ take size [0..] in return (fromList xs, SQ.fromList xs)
        
        finalNumCircle = mix initNumCircle
        
        temp = runReader finalNumCircle (zeroIndex,size,valueArr)
        
        [final1000th, final2000th, final3000th] = (\(posArr, idArr) -> [valueArr ! (idArr `SQ.index` ((i-1) `mod` (size+1))) | i <- [1000,2000,3000]]) $ temp
    
    print $ sum [final1000th, final2000th, final3000th]

day20part2 = do
    contents <- readFile "day20 (data).csv"
    let decryptionKey = 811589153    
        
        origValList = map ((*decryptionKey) . (read :: String -> Int)) . lines $ contents
        
        -- find zero and make it first
        zeroIndex = fromJust $ elemIndex 0 origValList
        rotatedList = rotateList zeroIndex origValList
        
        -- make zero implicit, this is our value list
        valueList = drop 1 rotatedList
        valueArr  = fromList valueList
        
        size = Prelude.length valueList
        
        initNumCircle :: NumCircle
        initNumCircle = let xs = rotateList (-(zeroIndex-1)) $ take size [0..] in return (fromList xs, SQ.fromList xs)
        
        finalNumCircle = foldl' (flip ($)) initNumCircle (Prelude.replicate 10 mix)
        
        temp = runReader finalNumCircle (zeroIndex,size,valueArr)
        
        [final1000th, final2000th, final3000th] = (\(posArr, idArr) -> [valueArr ! (idArr `SQ.index` ((i-1) `mod` (size+1))) | i <- [1000,2000,3000]]) $ temp
    
    print $ sum [final1000th, final2000th, final3000th]

rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs = zipWith const (drop n (cycle xs)) xs

type ZeroIndex = Int
type Size = Int
type ValueArr = Vector Int
type PosArr   = Vector Int
type IDArr    = SQ.Seq Int
type NumCircle = Reader (ZeroIndex,Size,ValueArr) (PosArr,IDArr)

-- make sure not to move zeroth position zero amount as anything landing on the zeroth place will be sent to the right.
moveRightBySKIPZERO :: Int -> Int -> Seq a -> Seq a
moveRightBySKIPZERO pos shift seq = 
  let (left, rest) = SQ.splitAt pos seq
      (element :< right) = viewl rest
      newSeq = left <> right
      naiveNewIndex = pos + shift
      newIndex | naiveNewIndex <= 0             = (naiveNewIndex - 1) `mod` SQ.length seq
               | naiveNewIndex >= SQ.length seq = (naiveNewIndex + 1) `mod` SQ.length seq
               | otherwise                      = naiveNewIndex
  in SQ.insertAt newIndex element newSeq

-- So far as it applies to the problem at hand, this version assumes an implicit zero at the start such that we can just move the start to the end and this is interpretted as having moved the start to the other size of zero. 
moveRightBy :: Int -> Int -> Seq a -> Seq a
moveRightBy pos shift seq = 
  let (left, rest) = SQ.splitAt pos seq
      (element :< right) = viewl rest
      newSeq = left <> right
      newIndex = (pos + shift) `mod` SQ.length seq
  in SQ.insertAt newIndex element newSeq

moveByID :: NumCircle -> Int -> NumCircle
moveByID prev numID = do
    (posArr,idArr) <- prev
    (_,size,valueArr) <- ask
    
    let pos        = posArr   ! numID
    let naiveShift = valueArr ! numID
        
    let newPos = (pos + naiveShift) `mod` size
    let shift  = newPos - pos
    
    let step = signum shift
    
    let newPosArr = posArr // ((numID, (pos + shift) `mod` size) : [(jumpedID, ((posArr!jumpedID) - step) `mod` size) | i <- take (abs shift) [1..], let jumpedID = idArr `SQ.index` ((pos + i*step) `mod` size)])
    let newIDArr  = (pos `moveRightBy` shift) idArr
    -- trace (showNumCircle valueArr $ return (newPosArr,newIDArr)) $ return (newPosArr,newIDArr)
    return (newPosArr,newIDArr)

validNumCircle :: ValueArr -> NumCircle -> Bool
validNumCircle valueArr numCircle  = (\(posArr, idArr) -> (interpretPosArr valueArr posArr) == (interpretIDArr valueArr idArr)) $ runReader numCircle (undefined, size, valueArr)
  where size = V.length valueArr

showNumCircle :: ValueArr -> NumCircle -> String
showNumCircle valueArr numCircle  = (\(posArr, idArr) -> show (interpretPosArr valueArr posArr) ++ "\t" ++ show (interpretIDArr valueArr idArr)) $ runReader numCircle (undefined, size, valueArr)
  where size = V.length valueArr

printNumCircle :: ValueArr -> NumCircle -> IO ()
printNumCircle valueArr numCircle  = putStrLn $ showNumCircle valueArr numCircle

fromMixIDs :: [Int] -> Int -> NumCircle
fromMixIDs numIDs size = foldl' moveByID initNumCircle (take size numIDs)
  where initNumCircle :: NumCircle
        initNumCircle = let xs = take size [0..] in return (fromList xs, SQ.fromList xs)

printMixIDs :: [Int] -> ValueArr -> IO ()
printMixIDs numIDs valueArr = printNumCircle valueArr $ fromMixIDs numIDs size
  where size = V.length valueArr
        initNumCircle :: NumCircle
        initNumCircle = let xs = take size [0..] in return (fromList xs, SQ.fromList xs)

interpretPosArr :: ValueArr -> PosArr -> [Int]
interpretPosArr valueArr posArr = [valueArr ! numID | pos <- take (V.length valueArr) [0..], let numID = idArr ! pos]
  where idArr = V.replicate (V.length valueArr) 0 // [(pos,numID) | numID <- take (V.length valueArr) [0..], let pos = posArr ! numID]
 
interpretIDArr :: ValueArr -> IDArr -> [Int]
interpretIDArr valueArr idArr = [valueArr ! numID | numID <- F.toList idArr]

mix :: NumCircle -> NumCircle
mix prev = do
    (zeroIndex,size,_) <- ask
    foldl' moveByID prev (take size [(i - zeroIndex) `mod` size | i <- [0..]])