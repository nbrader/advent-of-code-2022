#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package containers-0.6.5.1
--------------------------------
--------------------------------
----  Day 21:  Monkey Math  ----
--------------------------------
--------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package containers-0.6.5.1 -- '.\day21.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day21part1
-- 84244467642604 % 1

-- *Main> day21part2
-- 3759569926192 % 1


-------------
-- Imports --
-------------
import Data.Map.Lazy hiding (drop, take, map)


-------------
-- Program --
-------------
main = day21part2

day21part1 = do
    contents <- readFile "day21 (data).csv"
    let monkeyList :: [(String,Monkey)]
        monkeyList = map readMonkey . lines $ contents
    
    let monkeyMap :: Map String Monkey
        monkeyMap = fromList monkeyList
    
    print $ floor $ evalMonkeyMap monkeyMap

day21part2 = do
    contents <- readFile "day21 (data).csv"
    let monkeyList :: [(String,Monkey)]
        monkeyList = map readMonkey . lines $ contents
    
    let monkeyMap :: Map String Monkey
        monkeyMap = fromList monkeyList
    
    -- print . fromJust $ find (\i -> evalMonkeyMap2 i monkeyMap) (concat $ zipWith (\x y -> [x,y]) [0..] [-1,-2..])
    print $ floor $ evalMonkeyMap3 monkeyMap

data Monkey = Num Rational
            | Add  String String
            | Sub  String String
            | Mult String String
            | Div  String String deriving (Show, Eq)

data ExprList = ExprVar
              | ExprAdd    Rational ExprList
              | ExprMult   Rational ExprList
              | ExprRecip           ExprList deriving (Show, Eq)

readMonkey :: String -> (String, Monkey)
readMonkey xs
    | '+' `elem` xs = let name   = take 4 xs
                          child1 = take 4 . drop 6 $ xs
                          child2 = take 4 . drop 13 $ xs
                      in (name, Add child1 child2)
    | '-' `elem` xs = let name   = take 4 xs
                          child1 = take 4 . drop 6 $ xs
                          child2 = take 4 . drop 13 $ xs
                      in (name, Sub child1 child2)
    | '*' `elem` xs = let name   = take 4 xs
                          child1 = take 4 . drop 6 $ xs
                          child2 = take 4 . drop 13 $ xs
                      in (name, Mult child1 child2)
    | '/' `elem` xs = let name   = take 4 xs
                          child1 = take 4 . drop 6 $ xs
                          child2 = take 4 . drop 13 $ xs
                      in (name, Div child1 child2)
    | otherwise     = let name  = take 4 xs
                          child = drop 6 $ xs
                      in (name, Num (fromInteger (read child :: Integer)))

-- evaluate
evalMonkeyMap :: Map String Monkey -> Rational
evalMonkeyMap m = go (m ! "root")
  where go (Num x) = x
        go (Add x y) = go (m!x) + go (m!y)
        go (Sub x y) = go (m!x) - go (m!y)
        go (Mult x y) = go (m!x) * go (m!y)
        go (Div x y) = go (m!x) / go (m!y)

-- dumb search
evalMonkeyMap2 :: Integer -> Map String Monkey -> Bool
evalMonkeyMap2 i m = case m ! "root" of (Add x y) -> (go (get x) == go (get y))
  where go :: Monkey -> Rational
        go (Num x) = x
        go (Add x y) = go (get x) + go (get y)
        go (Sub x y) = go (get x) - go (get y)
        go (Mult x y) = go (get x) * go (get y)
        go (Div x y) = go (get x) / go (get y)
        
        get :: String -> Monkey
        get "humn" = Num (fromInteger i)
        get x = m!x

-- simplify and rearrange
-- evalMonkeyMap3 :: Map String Monkey -> ()
evalMonkeyMap3 m = rearrange $ case m ! "root" of (Add x y) -> (simplify (get x), simplify (get y))
  where simplify :: Either ExprList Monkey -> Either ExprList Rational
        simplify (Left ExprVar)  = Left ExprVar
        simplify (Right (Num x)) = Right x
        simplify (Right (Add x y)) = let arg1 = simplify (get x)
                                         arg2 = simplify (get y)
                                     in case (arg1,arg2) of
                                          (Right arg1', Right arg2') -> Right $ arg1' + arg2'
                                          (Left  arg1', Right arg2') -> Left $ ExprAdd arg2' arg1'
                                          (Right arg1', Left  arg2') -> Left $ ExprAdd arg1' arg2'
                                          (Left  arg1', Left  arg2') -> error "I won't bother making this work because it's not in my puzzle input"
        simplify (Right (Sub x y)) = let arg1 = simplify (get x)
                                         arg2 = simplify (get y)
                                     in case (arg1,arg2) of
                                          (Right arg1', Right arg2') -> Right $ arg1' - arg2'
                                          (Left  arg1', Right arg2') -> Left $ ExprAdd (-arg2') arg1'
                                          (Right arg1', Left  arg2') -> Left $ ExprAdd arg1' (ExprMult (-1) arg2')
                                          (Left  arg1', Left  arg2') -> error "I won't bother making this work because it's not in my puzzle input"
        simplify (Right (Mult x y)) = let arg1 = simplify (get x)
                                          arg2 = simplify (get y)
                                      in case (arg1,arg2) of
                                           (Right arg1', Right arg2') -> Right $ arg1' * arg2'
                                           (Left  arg1', Right arg2') -> Left $ ExprMult arg2' arg1'
                                           (Right arg1', Left  arg2') -> Left $ ExprMult arg1' arg2'
                                           (Left  arg1', Left  arg2') -> error "I won't bother making this work because it's not in my puzzle input"
        simplify (Right (Div x y)) = let arg1 = simplify (get x)
                                         arg2 = simplify (get y)
                                     in case (arg1,arg2) of
                                          (Right arg1', Right arg2') -> Right $ arg1' / arg2'
                                          (Left  arg1', Right arg2') -> Left $ ExprMult (1/arg2') arg1'
                                          (Right arg1', Left  arg2') -> Left $ ExprMult arg1' (ExprRecip arg2')
                                          (Left  arg1', Left  arg2') -> error "I won't bother making this work because it's not in my puzzle input"
        
        get :: String -> Either ExprList Monkey
        get "humn" = Left ExprVar
        get x      = Right $ m!x
        
        rearrange :: (Either ExprList Rational, Either ExprList Rational) -> Rational
        rearrange (Left e, Right x) = rearrange' e x
        rearrange (Right x, Left e) = rearrange' e x
        rearrange _ = error "I won't bother making this work because it's not in my puzzle input"
        
        rearrange' :: ExprList -> Rational -> Rational
        rearrange'  ExprVar        x = x
        rearrange' (ExprAdd  y  e) x = rearrange' e (x - y)
        rearrange' (ExprMult y  e) x = rearrange' e (x / y)
        rearrange' (ExprRecip   e) x = rearrange' e (1 / x)