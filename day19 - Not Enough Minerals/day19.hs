#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package strict-0.4.0.1 --package containers-0.6.5.1 --package mtl-2.2.2 --package partial-order-0.2.0.0 --package parallel-3.2.2.0
-- -- stack --resolver lts-18.22 ghci --package strict-0.4.0.1 --package split-0.2.3.4 --package containers-0.6.5.1 --package mtl-2.2.2 --package fgl-5.7.0.3 --package data-ordlist-0.4.7.0 --package search-algorithms-0.3.2
----------------------------------------
----------------------------------------
----  Day 19:  Not Enough Minerals  ----
----------------------------------------
----------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-18.22 ghc --package strict-0.4.0.1 --package containers-0.6.5.1 --package mtl-2.2.2 --package partial-order-0.2.0.0 -- '.\day19.hs' -O2 -threaded
    To run on parallel cores, run the following shell command after building:
        '.\day19.exe' +RTS -N
-}

------------
-- Output --
------------
-- *Main> day19part1
-- 1550

-- *Main> day19part2
-- 18630


-------------
-- Imports --
-------------
import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import Data.Tree
import Data.Function (on)
import Data.Maybe (fromMaybe, fromJust, isJust, maybeToList)
-- import Debug.Trace (trace)
import Control.Monad.RWS.Strict

import Data.List as L (sort, intersperse, foldl', findIndex, map, delete, null, concatMap, minimumBy, transpose)
import Control.Monad (guard)
import qualified Data.PartialOrd as PO

import Control.Parallel.Strategies

import qualified Data.Map.Strict as M
-- import Data.Tree
-- import Data.Graph.Inductive.Query.DFS
-- import Data.Graph.Inductive.Graph
-- import Data.Graph.Inductive.PatriciaTree


-------------
-- Program --
-------------
main = day19part2

day19part1 = do
    contents <- readFile "day19 (data).csv"
    let blueprints :: [Blueprint]
        blueprints = map readBlueprint . lines $ contents
    
    print $ sum $ (map (getQualityLevel 24) (zip [1..] blueprints) `using` parList rdeepseq)
-- bpID: 1 maxGeodes: 9
-- bpID: 2 maxGeodes: 1
-- bpID: 3 maxGeodes: 0
-- bpID: 4 maxGeodes: 0
-- bpID: 5 maxGeodes: 2
-- bpID: 6 maxGeodes: 0
-- bpID: 7 maxGeodes: 10
-- bpID: 8 maxGeodes: 2
-- bpID: 9 maxGeodes: 13
-- bpID: 10 maxGeodes: 0
-- bpID: 11 maxGeodes: 0
-- bpID: 12 maxGeodes: 5
-- bpID: 13 maxGeodes: 2
-- bpID: 14 maxGeodes: 0
-- bpID: 15 maxGeodes: 12
-- bpID: 16 maxGeodes: 5
-- bpID: 17 maxGeodes: 3
-- bpID: 18 maxGeodes: 0
-- bpID: 19 maxGeodes: 0
-- bpID: 20 maxGeodes: 1
-- bpID: 21 maxGeodes: 7
-- bpID: 22 maxGeodes: 1
-- bpID: 23 maxGeodes: 1
-- bpID: 24 maxGeodes: 7
-- bpID: 25 maxGeodes: 0
-- bpID: 26 maxGeodes: 1
-- bpID: 27 maxGeodes: 5
-- bpID: 28 maxGeodes: 0
-- bpID: 29 maxGeodes: 2
-- bpID: 30 maxGeodes: 11

day19part2 = do
    contents <- readFile "day19 (data).csv"
    let blueprints :: [Blueprint]
        blueprints = map readBlueprint . take 3 . lines $ contents
    
    let maxGeodesList = map (getMaxGeodes 32) blueprints `using` parList rdeepseq
    -- mapM_ print maxGeodesList
    print $ product maxGeodesList
-- 54
-- 23
-- 15

-- conjectures & theorems
-- The aim is to find what strategy gives the most geodes after 24 minutes for each blueprint
-- A strategy is a sequences of consecutive choices, one per minute, of whether to build one of the 4 bots or to wait (i.e. build nothing).
-- To find a solution, I will reason about strategies I will refer to as "sprint strategies".
-- The first class of sprint strategy is "Class 1" and consists of any pure sequence of building geodebots.
    -- Using the class 1 sprint strategy for the remainder of the 24 minutes is always optimal if possible as any other strategy would have fewer geodebots at some point
    -- in time and having fewer geode bots before the last minute will given a strictly smaller return of geodes and having fewer geode bots only on the last minute gives
    -- the same return of geodes.
    
    -- If we currently have g0 geodes and gb0 geodeBots and we've just finished minute t0 then using the class 1 sprint strategy (if possible) for the next t minutes will result in the following geodes after finishing minute t0+t:
        -- geodesAfterSprint(t) := g0 + t*(gb0 + (t-1)/2)
    
    -- Using the class 1 sprint strategy is only possible for the next t minutes (i.e. another t geodebots built) if the current ore, oreBots, obsidian, obsidianBots, geodeBotOreCost and geodeBotObsidianCost satisfies the following:
           -- geodeBotOreCost <= ore +     0*(oreBots - geodeBotOreCost)
        -- && geodeBotOreCost <= ore +     1*(oreBots - geodeBotOreCost)
        -- ...
        -- && geodeBotOreCost <= ore + (t-1)*(oreBots - geodeBotOreCost)
        
        -- &&
           -- geodeBotObsidianCost <= obsidian +     0*(obsidianBots - geodeBotObsidianCost)
        -- && geodeBotObsidianCost <= obsidian +     1*(obsidianBots - geodeBotObsidianCost)
        -- ...
        -- && geodeBotObsidianCost <= obsidian + (t-1)*(obsidianBots - geodeBotObsidianCost)
        
    -- This can be rephrased as
           -- geodeBotOreCost +     0*(geodeBotOreCost - oreBots) <= ore
           -- geodeBotOreCost +     1*(geodeBotOreCost - oreBots) <= ore
        -- ...
        -- && geodeBotOreCost + (t-1)*(geodeBotOreCost - oreBots) <= ore
        
        -- &&
           -- geodeBotObsidianCost +     0*(geodeBotObsidianCost - obsidianBots) <= obsidian
        -- && geodeBotObsidianCost +     1*(geodeBotObsidianCost - obsidianBots) <= obsidian
        -- ...
        -- && geodeBotObsidianCost + (t-1)*(geodeBotObsidianCost - obsidianBots) <= obsidian
        
    -- Which, if geodeBotObsidianCost > obsidianBots then simplifies to
           -- geodeBotOreCost      + (t-1)*(geodeBotOreCost      - oreBots     ) <= ore
        -- && geodeBotObsidianCost + (t-1)*(geodeBotObsidianCost - obsidianBots) <= obsidian
        
    -- or
           -- oreBots      + t*(geodeBotOreCost      - oreBots     ) <= ore
        -- && obsidianBots + t*(geodeBotObsidianCost - obsidianBots) <= obsidian
        



-- conjectures & theorems (old)
-- 1. If a state has oreBots == geodeBotOreCost && obsidianBots == geodeBotObsidianCost then every action thereafter should be MakeGeodeBot
-- 2. The ability to make a bot for a state and it's future is either, currently unavailable, temporarily available or guaranteed thereafter if the resource is (currently less than required), (equal to or greater than required but the bot count is lower) or (the bot count is at least equal) respectively.
-- 3. The ultimate number of geodes is equal to the sum of the durations before endTime that geodebots are made
-- 4. obsidian is only useful for making geodeBots
-- 5. your next geodeBot will be produced at worst as slowly as your last (so long as your producing them whenever you can)
-- 6. i)   you never need to make another ore bot when you have more than the ore cost required for any bot
-- 6. ii)  you never need to make another clay bot when you have more than the clay cost required for any bot
-- 6. iii) you never need to make another obsidian bot when you have more than the obsidian cost required for any bot

-- 7. An upper bound to the geodes you can have after 24 minutes can be determined based on a blueprint as follows:


-- Let geodeMax(t)    be the max geodes    you could have after t minutes.
-- Let geodeBotMax(t) be the max geodeBots you could have after t minutes.
-- Let geodeBotUpper1(t) := t. This is the number of geodeBots you could have after t minutes if you always had enough resources from the start.

-- 7. i)
---     geodeBotMax(t) < geodeBotUpper1(t)
---     geodeBotMax(t) < t
--    This is because you can't build geodeBots faster than once a minute and there's at least one minute (the first) where you don't have the resources to build a geodeBot.


-- Let geodeBotUpper2(t;t0,gb0) := t-t0+gb0. This is the number of geodeBots you could have after t minutes if you had gb0 geodeBots after t0 minutes and always had enough resources to build a geodeBot from t0 minutes onwards.


-- If geodeBotUpper2(t1;t0,gb0) = geodeBotMax(t1) + 1 and t >= t1
--  then geodeBotMax(t) < geodeBotUpper2(t;t0,gb0)
--  but since gb0 = geodeBotMax(t1) + 1 - (t1-t0)
--  and geodeBotUpper2(t;t0,gb0) = t-t0+gb0
--  then geodeBotMax(t) < t-t0+gb0
--       geodeBotMax(t) < t-t0+(geodeBotMax(t1) + 1 - (t1-t0))
--       geodeBotMax(t) < t - t1 + geodeBotMax(t1) + 1
--       geodeBotMax(t) - geodeBotMax(t1) < t - t1 + 1
--  Let geodeBotMaxInc(t;t1) := geodeBotMax(t) - geodeBotMax(t1)   (i.e. the number of geodebots that could be gained from t1 minutes to t minutes)
--  then geodeBotMaxInc(t;t1) < t - t1 + 1
--
-- So,
--  7. ii)
--      If geodeBotUpper2(t1;t0,gb0) = geodeBotMax(t1) + 1 and t >= t1
--       then geodeBotMaxInc(t;t1) < t - t1 + 1
--    This is for the same reason as 7(i) but instead of starting from t=0 we start from an arbitrary point in time


-- Let geodeUpper1(t) := sum{i=1 to i=t-1}( geodeBotUpper1(i) )
--  so geodeUpper1(t) = sum{i=1 to i=t-1}( i )
--  so geodeUpper1(t) = t*(t-1)/2
-- 7. iii)
--      geodeMax(t) < geodeUpper1(t)
--      geodeMax(t) < t*(t-1)/2
--    This is just the statement that we will always acheive fewer geodes than the situation where we could build a geodeBot every minute from the start (such as with 7(i)) and we had an extra geode at start.

data Blueprint = Blueprint {
    getID                   :: Int,
    getOreBotOreCost        :: Int,
    getClayBotOreCost       :: Int,
    getObsidianBotOreCost   :: Int,
    getObsidianBotClayCost  :: Int,
    getGeodeBotOreCost      :: Int,
    getGeodeBotObsidianCost :: Int } deriving Show

exampleBlueprintStr = "Blueprint 1: Each ore robot costs 2 ore. Each clay robot costs 2 ore. Each obsidian robot costs 2 ore and 17 clay. Each geode robot costs 2 ore and 10 obsidian."

readBlueprint :: String -> Blueprint 
readBlueprint inStr = Blueprint { getID                   = read idStr,
                                  getOreBotOreCost        = read oreBotOreCostStr,
                                  getClayBotOreCost       = read clayBotOreCostStr,
                                  getObsidianBotOreCost   = read obsidianBotOreCostStr,
                                  getObsidianBotClayCost  = read obsidianBotClayCostStr,
                                  getGeodeBotOreCost      = read geodeBotOreCostStr,
                                  getGeodeBotObsidianCost = read geodeBotObsidianCostStr }
  where (idStr,                   after1) = break (==':') (drop (length "Blueprint ") $ inStr)
        (oreBotOreCostStr,        after2) = break (==' ') (drop (length ": Each ore robot costs ") $ after1)
        (clayBotOreCostStr,       after3) = break (==' ') (drop (length " ore. Each clay robot costs ") $ after2)
        (obsidianBotOreCostStr,   after4) = break (==' ') (drop (length " ore. Each obsidian robot costs ") $ after3)
        (obsidianBotClayCostStr,  after5) = break (==' ') (drop (length " ore and ") $ after4)
        (geodeBotOreCostStr,      after6) = break (==' ') (drop (length " clay. Each geode robot costs ") $ after5)
        (geodeBotObsidianCostStr, after7) = break (==' ') (drop (length " ore and ") $ after6)

data Choice = Wait | MakeOreBot | MakeClayBot | MakeObsidianBot | MakeGeodeBot deriving (Show)



data SimulationStateTime = SimulationStateTime {
    time  :: Int,
    getState :: SimulationState } deriving Show

data SimulationState = SimulationState {
    stateOre      :: Int,
    stateClay     :: Int,
    stateObsidian :: Int,
    stateGeodes   :: Int,
    stateOreBots      :: Int,
    stateClayBots     :: Int,
    stateObsidianBots :: Int,
    stateGeodeBots   :: Int } deriving Show

ore = stateOre . getState
clay = stateClay . getState
obsidian = stateObsidian . getState
geodes = stateGeodes . getState
oreBots = stateOreBots . getState   
clayBots = stateClayBots . getState
obsidianBots = stateObsidianBots . getState
geodeBots = stateGeodeBots . getState

data MaxPath = MaxPath {
    -- maxpathStates :: [SimulationStateTime],
    -- maxpathLength :: Int,
    maxPathGeodes :: Int }

geodesFromMaybeMaxPath = fromMaybe 0 . fmap maxPathGeodes . maybeMaxPath


-- allDirs = undefined
-- add = undefined
-- getAllCubes :: String -> [Cube]
-- getAllCubes contents = undefined



-- type Cube = ()
-- day18part1 = do
    -- contents <- readFile "day18 (data).csv"
    -- let allCubes = getAllCubes contents
    -- let labelledCubes :: [(Int,Cube)]
        -- labelledCubes = zip [0..] allCubes
    -- let cubeIDMap = Map.fromList (zip allCubes [0..])
    -- let labelledEdges = do
            -- cube <- allCubes
            -- dir <- allDirs
            -- let adj = add cube dir
            -- id1 <- maybeToList $ Map.lookup cube cubeIDMap
            -- id2 <- maybeToList $ Map.lookup adj cubeIDMap
            -- return (id1,id2,())
    -- let g :: Gr Int ()
        -- g = (\g -> gmap (\(from, i, _, to) -> (from, i, 6 - outdeg g i, to)) g) $ mkGraph labelledCubes labelledEdges
        -- ts = map (fmap (fromJust . lab g)) $ dff' g
    
    -- print . sum $ map (foldTree (\exposedFaces accums -> exposedFaces + sum accums)) ts

-- Do a breadth first search, which amounts to repeatedly doing the following:
--  for each active path, progress it in all viable directions and for each of these directions:
--       if the result is outright worse than an active paths at this time value. If it is, discard it.
--           otherwise, it will be added to the active paths. Remove all active paths at this time value that are outright worse than it and then add it to the set.
--
-- More simply, this amounts to adding each new state for that time to a list and calling "maxima" from Data.PartialOrd

getMaxGeodes :: Int -> Blueprint -> Int
getMaxGeodes endTime bp = maxGeodes
  where maxGeodes :: Int
        maxGeodes = geodesFromMaybeMaxPath $ fst $ execRWS (unfoldTreeM step initState) bp (Record Nothing (M.fromList [(t,[]) | t <- [0..endTime]]))
          where initState = SimulationStateTime {
                    time     = 0,
                    getState = SimulationState {
                        stateOre      = 0,
                        stateClay     = 0,
                        stateObsidian = 0,
                        stateGeodes   = 0,
                        stateOreBots      = 1,
                        stateClayBots     = 0,
                        stateObsidianBots = 0,
                        stateGeodeBots    = 0
                        }
                    }
                
                step :: SimulationStateTime -> RWS Blueprint () Record (SimulationStateTime,[SimulationStateTime])
                step prev = do
                    newStates <- getNewStates prev
                    return (prev, if (time prev < endTime) then newStates else [])
                    -- trace (show prev) $ return (prev, if (time prev < endTime) then newStates else [])
                
                getNewStates :: SimulationStateTime -> RWS Blueprint () Record [SimulationStateTime]
                getNewStates prev = do
                    oreBotOreCost        <- asks getOreBotOreCost
                    clayBotOreCost       <- asks getClayBotOreCost
                    obsidianBotOreCost   <- asks getObsidianBotOreCost
                    obsidianBotClayCost  <- asks getObsidianBotClayCost
                    geodeBotOreCost      <- asks getGeodeBotOreCost
                    geodeBotObsidianCost <- asks getGeodeBotObsidianCost
                    
                    -- Currently only looks at whether a sprint of class 1 to the finish could beat the current best geodes count found.
                    -- If a sprint of class 1 could get us there that doesn't mean we can achieve such a sprint (it's only an upper bound, not a least upper bound on the acheivable geodes).
                    -- If the class 1 only check doesn't optimise this enough, I can try to consider classes of sprint which give tighter upper bounds on the acheivable geodes.
                    currentMax <- gets geodesFromMaybeMaxPath
                    
                    let timeBeforeLastMinute = endTime - time prev
                    let class1SprintPossible = let dt = timeBeforeLastMinute
                                               in dt == 0 || (    oreBots prev      + dt*(geodeBotOreCost      - oreBots prev     ) <= ore prev
                                                               && obsidianBots prev + dt*(geodeBotObsidianCost - obsidianBots prev) <= obsidian prev )
                    let class1SprintMaxGeodes = let dt = timeBeforeLastMinute in geodes prev + dt*geodeBots prev + (dt*(dt-1)`div`2)
                    
                    bestMap <- gets bestFoundAtTime
                    let best = bestMap M.! time prev
                    
                    let worseThanFound = any (PO.>= getState prev) best
                    
                    if class1SprintMaxGeodes <= currentMax
                     then return [] -- We wouldn't improve on the previous best even if we could do a class 1 sprint
                     else if worseThanFound
                        then return []
                        else do
                            modify $ (\r -> r {bestFoundAtTime = M.adjust (PO.maxima . (getState prev:)) (time prev) bestMap})
                            
                            if class1SprintPossible
                                then do
                                    -- trace (show class1SprintMaxGeodes) $ return ()
                                    modify $ (\r -> r { maybeMaxPath = Just $ MaxPath { maxPathGeodes = class1SprintMaxGeodes }})
                                    return [] -- We could do a class 1 sprint so we recorded the calculated result of that sprint and ended early
                                else do
                                    let canAffordOreBot      = ore prev >= oreBotOreCost
                                    let canAffordClayBot     = ore prev >= clayBotOreCost
                                    let canAffordObsidianBot = ore prev >= obsidianBotOreCost && clay prev >= obsidianBotClayCost
                                    let canAffordGeodeBot    = ore prev >= geodeBotOreCost && obsidian prev >= geodeBotObsidianCost
                                    
                                    let couldUseMoreOreBots = oreBots prev < maximum [oreBotOreCost, clayBotOreCost, obsidianBotOreCost, geodeBotOreCost]
                                    let couldUseMoreClayBots = clayBots prev < obsidianBotClayCost
                                    let couldUseMoreObsidianBots = obsidianBots prev < geodeBotObsidianCost
                                    
                                    let makeOreBot
                                         | canAffordOreBot && couldUseMoreOreBots = [MakeOreBot]
                                         | otherwise = []
                                    
                                    let makeClayBot
                                         | canAffordClayBot && couldUseMoreClayBots = [MakeClayBot]
                                         | otherwise = []
                                    
                                    let makeObsidianBot
                                         | canAffordObsidianBot && couldUseMoreObsidianBots = [MakeObsidianBot]
                                         | otherwise = []
                                    
                                    let makeGeodeBot
                                         | canAffordGeodeBot = [MakeGeodeBot]
                                         | otherwise = []
                                    
                                    let wait -- only wait if we have something to wait for
                                         | canAffordOreBot && canAffordClayBot && canAffordObsidianBot && canAffordGeodeBot = []
                                         | otherwise = [Wait]
                                    
                                    let choices = makeGeodeBot ++ makeObsidianBot ++ makeClayBot ++ makeOreBot ++ wait
                                    
                                    mapM (\c -> getNewState prev c) choices
                    
                    -- if null makeGeodeBot
                     -- then return $ makeObsidianBot ++ makeClayBot ++ makeOreBot ++ [Wait]
                     -- else return $ makeGeodeBot -- My intution says we should always make a GeodeBot if we can but I'm not sure I can prove it. Tests on smaller endTimes failed to disprove my guess but also show hardily any speed increase.
                
                -- getNewStates :: SimulationStateTime -> [Choice] -> RWS Blueprint () (Maybe MaxPath) [SimulationStateTime]
                -- getNewStates prev choices = go choices (return [])
                  -- where go [] best = best
                        -- go (choice:cs) best = do
                            -- bs <- best
                            -- newState <- getNewState prev choice
                            -- go cs (return $ PO.maxima (newState:bs))
                
                getNewState :: SimulationStateTime -> Choice -> RWS Blueprint () Record SimulationStateTime
                getNewState prev choice = applyChoice (progressTime prev) choice
                
                applyChoice :: SimulationStateTime -> Choice -> RWS Blueprint () Record SimulationStateTime
                applyChoice prev Wait = return prev
                applyChoice prev MakeOreBot = do
                    oreBotOreCost <- asks getOreBotOreCost
                    let oldState = getState prev
                    let newState = oldState {
                            stateOre  = ore prev - oreBotOreCost,
                            stateOreBots = oreBots prev + 1 }
                    return $ prev {getState = newState}
                applyChoice prev MakeClayBot = do
                    clayBotOreCost <- asks getClayBotOreCost
                    let oldState = getState prev
                    let newState = oldState {
                            stateOre  = ore prev - clayBotOreCost,
                            stateClayBots = clayBots prev + 1 }
                    return $ prev {getState = newState}
                applyChoice prev MakeObsidianBot = do
                    obsidianBotOreCost  <- asks getObsidianBotOreCost
                    obsidianBotClayCost <- asks getObsidianBotClayCost
                    let oldState = getState prev
                    let newState = oldState {
                            stateOre  = ore prev  - obsidianBotOreCost,
                            stateClay = clay prev - obsidianBotClayCost,
                            stateObsidianBots = obsidianBots prev + 1 }
                    return $ prev {getState = newState}
                applyChoice prev MakeGeodeBot = do
                    geodeBotOreCost  <- asks getGeodeBotOreCost
                    geodeBotObsidianCost <- asks getGeodeBotObsidianCost
                    let oldState = getState prev
                    let newState = oldState {
                            stateOre      = ore prev      - geodeBotOreCost,
                            stateObsidian = obsidian prev - geodeBotObsidianCost,
                            stateGeodeBots = geodeBots prev + 1 }
                    return $ prev {getState = newState}
                progressTime :: SimulationStateTime -> SimulationStateTime
                progressTime prev = prev { time     = time prev + 1,
                                           getState = newState }
                  where oldState = getState prev
                        newState = oldState {
                            stateOre      = ore      prev + oreBots prev,
                            stateClay     = clay     prev + clayBots prev,
                            stateObsidian = obsidian prev + obsidianBots prev,
                            stateGeodes   = geodes   prev + geodeBots prev }

data Record = Record {
    maybeMaxPath :: Maybe MaxPath,
    bestFoundAtTime :: M.Map Int [SimulationState]
    }

getQualityLevel :: Int -> (Int, Blueprint) -> Int
getQualityLevel endTime (bpID, bp) = bpID * maxGeodes
-- getQualityLevel endTime (bpID, bp) = trace ("bpID: " ++ show bpID ++ " maxGeodes: " ++ show maxGeodes) $ bpID * maxGeodes
  where maxGeodes = getMaxGeodes endTime bp

instance PO.PartialOrd SimulationState where
    s1 <= s2 = and $ funcs <*> pure s1 <*> pure s2
      where funcs
                = [ (PO.<=) `on` stateOre,
                    (PO.<=) `on` stateClay,
                    (PO.<=) `on` stateObsidian,
                    (PO.<=) `on` stateGeodes,
                    (PO.<=) `on` stateOreBots,
                    (PO.<=) `on` stateClayBots,
                    (PO.<=) `on` stateObsidianBots,
                    (PO.<=) `on` stateGeodeBots ]

instance (PO.PartialOrd a, PO.PartialOrd b) => PO.PartialOrd (a,b) where
    (x1,y1) <= (x2,y2) = x1 PO.<= x2 && y1 PO.<= y2