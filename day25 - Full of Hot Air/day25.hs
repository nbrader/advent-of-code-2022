#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package containers-0.6.5.1
------------------------------------
------------------------------------
----  Day 25:  Full of Hot Air  ----
------------------------------------
------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package containers-0.6.5.1 -- '.\day25.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day25part1
-- 2---1010-0=1220-=010


-------------
-- Imports --
-------------
import Data.List
import Debug.Trace (trace)


-------------
-- Program --
-------------
main = day25part1

day25part1 = do
    contents <- readFile "day25 (data).csv"
    let snafuList :: [SNAFU]
        snafuList = map readSNAFU . lines $ contents
    
    putStrLn . showSNAFU . intToSNAFU . sum . map snafuToInt $ snafuList

-- SNAFU is just the puzzle's silly name for "Balanced Quinary" (i.e. like balanced ternary which is base 3 with digits {1,0,-1} only it's base 5)
data SNAFUDigit = Digit2 | Digit1 | Digit0 | DigitMinus | DigitDoubleMinus deriving (Show)
newtype SNAFU = SNAFU {snafuDigits :: [SNAFUDigit]} deriving (Show)
newtype Base5 = Base5 {base5Digits :: [Int]} deriving (Show)

evalSNAFUDigit :: SNAFUDigit -> Int
evalSNAFUDigit Digit2 = 2
evalSNAFUDigit Digit1 = 1
evalSNAFUDigit Digit0 = 0
evalSNAFUDigit DigitMinus = -1
evalSNAFUDigit DigitDoubleMinus = -2

encodeSNAFUDigit :: Int -> SNAFUDigit
encodeSNAFUDigit   2  = Digit2
encodeSNAFUDigit   1  = Digit1
encodeSNAFUDigit   0  = Digit0
encodeSNAFUDigit (-1) = DigitMinus 
encodeSNAFUDigit (-2) = DigitDoubleMinus

snafuToInt x = sum [(evalSNAFUDigit d)*5^i | (i,d) <- zip [0..] (reverse $ snafuDigits x)]
base5ToInt x = sum [d*5^i | (i,d) <- zip [0..] (reverse $ base5Digits x)]

intToSNAFU :: Int -> SNAFU
intToSNAFU n = SNAFU . map encodeSNAFUDigit . (\(sDigits,carry) -> if carry == 0 then sDigits else carry:sDigits) $ foldr step ([],0) b5digits
  where b5digits = base5Digits . intToBase5 $ n
        step :: Int -> ([Int],Int) -> ([Int],Int)
        step b5Digit (sDigits,carry)
            | b5Digit+carry < 3 = (b5Digit+carry  :sDigits,0)
            | otherwise         = (b5Digit+carry-5:sDigits,1)

intToBase5 :: Int -> Base5
intToBase5 n = Base5 $ unfoldr step (n,mostSigDigit n)
  where step :: (Int,Int) -> Maybe (Int,(Int,Int))
        step (n,i) | i < 0 = Nothing
                   | otherwise =  let (d,n') = n `divMod` (5^i)
                                  in Just (d, (n',i-1))

mostSigDigit :: Int -> Int
mostSigDigit n = floor (log (fromInt n) / log 5)

fromInt = fromInteger . toInteger 

readSNAFU :: String -> SNAFU
readSNAFU = SNAFU . map readCharAsDigit

showSNAFU :: SNAFU -> String
showSNAFU = concat . map showDigit . snafuDigits

showDigit :: SNAFUDigit -> String
showDigit Digit2 = "2"
showDigit Digit1 = "1"
showDigit Digit0 = "0"
showDigit DigitMinus = "-"
showDigit DigitDoubleMinus = "="

readCharAsDigit :: Char -> SNAFUDigit
readCharAsDigit '2' = Digit2
readCharAsDigit '1' = Digit1
readCharAsDigit '0' = Digit0
readCharAsDigit '-' = DigitMinus
readCharAsDigit '=' = DigitDoubleMinus