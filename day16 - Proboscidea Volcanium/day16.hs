#!/usr/bin/env stack
-- stack --resolver lts-21.9 ghci --package regex-tdfa --package containers --package split --package mtl --package strict --package search-algorithms
------------------------------------------
------------------------------------------
----  Day 16:  Proboscidea Volcanium  ----
------------------------------------------
------------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.9 ghc --package regex-tdfa --package containers --package split --package mtl --package strict --package search-algorithms -- '.\day16.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day16part1
-- 2320

-- *Main> day16part2
-- 


-------------
-- Imports --
-------------
import qualified Data.Map.Strict as Map
import Data.Map.Strict hiding (filter, map, drop, foldl')
import Data.Map.Strict (Map, (!))
import Data.Set (Set)
import Data.Set as S hiding (map, foldl', null, filter, drop)
import Data.List.Split (splitOn)
import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import Debug.Trace (trace)
import Control.Monad (guard, forM_)
import Data.List
import Data.Maybe
import Control.Monad.RWS.Strict
import Data.Ord
import Data.Function
import Text.Regex.TDFA (getAllTextMatches, (=~), AllTextMatches)


-------------
-- Program --
-------------
main = day16part2

day16part1 = do
    contents <- readFile "day16 (data).csv"
    
    let duration = 30
    
    let lines = Prelude.lines $ contents
    let valves :: [Valve]
        valves = map readValve lines
    let nonBlockedValves = [(name,flow,adjNames) | (name,flow,adjNames) <- valves, flow /= 0]
    let nonBlockedNames = [name | (name,flow,adjNames) <- nonBlockedValves]
    let bigGraph = Map.fromList [(name, S.fromList adjNames) | (name,flow,adjNames) <- valves]
    
    -- first calculate shortest paths between all non-blocked valves and use it to make a smaller graph
    let (shortestLengths, treePrevs) = getShortestPaths bigGraph
    let initial = "AA"
    
    let bestBigGraphPaths = do
            v1 <- "AA" : nonBlockedNames
            v2 <- nonBlockedNames
            guard $ v1 /= v2
            
            let bestPath = getShortestPath v1 v2 treePrevs
            let bestPathLength = fromJust $ fromJust $ lookupNested v1 v2 shortestLengths
            return ((v1, v2), (bestPathLength, bestPath))
    
    let initialEvent = (initial,0)
    
    let smallGraphNodes = nonBlockedNames :: [String]
    let smallGraphEdges = Map.fromList bestBigGraphPaths :: Map.Map (String,String) Path
    let smallGraphTimeCost v1 v2 = fst $ smallGraphEdges Map.! (v1,v2)
    let flows :: Flows
        flows = Map.fromList $ ("END",0):[(name,flow) | (name,flow,adjNames) <- nonBlockedValves]
    let smallGraph = Map.fromList [(v,filter (/= v) smallGraphNodes) | v <- smallGraphNodes]
    
    let edgesFromStartToSmallGraph = let (v0,t0) = initialEvent in ((v0,t0), ("END",duration):[(v1,t1) | v1 <- smallGraphNodes, v0 /= v1, let t1 = t0 + smallGraphTimeCost v0 v1 + 1, t1 <= duration]) -- since we don't start on a node on the small graph when the start node is a blocked valve
    let eventEdges                   = edgesFromStartToSmallGraph:[((v0,t0), ("END",duration):[(v1,t1) | v1 <- smallGraphNodes, v0 /= v1, let t1 = t0 + smallGraphTimeCost v0 v1 + 1, t1 <= duration]) | t0 <- [1..duration], v0 <- smallGraphNodes] -- I could optimise by not generating for nodesAtTime at all times but just the nodes that are reached
    let eventGraph :: EventGraph
        eventGraph = Map.fromList eventEdges 
                                                                    -- Note that this ---->  ("END",duration,False):  <----- is because I want to "complete" event histories which don't do any more moving or opening valves because we don't have time (which can easily still be the best option)
    
    ------------
    -- Search --
    ------------
    let cfg = (Config {cfgDuration = duration, cfgGraph = eventGraph, cfgFlows = flows})
    
    let initGuy = Guy {guyVisited = [(initial,0)], guyLatestEvent = initialEvent}
    let searchInit = SearchState {currentGroupVisited = S.singleton initial, currentGuys = replicate 1 initGuy, currentPressureReleased = 0}
    let bestFoundSearchState = ssBestFoundSearchState $ runSearch cfg $ search searchInit
    
    -----------
    -- Print --
    -----------
    print $ currentPressureReleased bestFoundSearchState
    
    -- printValvesAsTGF valves
    -- putStrLn ""
    
    -- let smallGraphValves = [(v,flow,adj) | v <- smallGraphNodes, let flow = flows Map.! v, let adj = [v2 | v2 <- smallGraphNodes]]
    -- printValvesAsTGF smallGraphValves
    
    -- let smallGraphValvesWithPaths = [(v,flow,adj) | v <- smallGraphNodes, let flow = flows Map.! v, let adj = [(v2,path) | v2 <- smallGraphNodes, v2 /= v, let path = smallGraphEdges Map.! (v,v2)]]
    -- printValvesAsTGFWithDist smallGraphValvesWithPaths
    
    -- let eventGraphValves = [(eventNodeLabel (v,time), flow, adj) | v <- smallGraphNodes, time <- [0..duration], let flow = flows Map.! v, let adj = map eventNodeLabel $ eventGraphNext (v,time)]
    -- printValvesAsTGF eventGraphValves
    -- mapM_ print . map (\((v1,v2),(cost,route)) -> cost) $ bestBigGraphPaths
    -- print bestEventGraphPath
    -- mapM_ putStrLn $ [name ++ " " ++ name ++ show flow | (name,flow,adjNames) <- valves]
    -- putStrLn "#"
    -- mapM_ putStrLn $ [name ++ " " ++ adjName | (name,flow,adjNames) <- valves, adjName <- adjNames]
    
    -- I looked at a picture of the graph
    -- let humanGuessedPathNames = ["DY","HF","CF","RO","XJ","DP","RU","KG"]
    -- let humanGuessedPath = map (\path -> (sum $ map snd path, path)) . Prelude.take 1 $ sortBy (compare `on` (\path -> sum $ map snd path)) $ flip evalStateT S.empty $ attachCosts humanGuessedPathNames initialEvent
    -- print humanGuessedPath

day16part2 = do
    contents <- readFile "day16 (data).csv"
    
    let duration = 26
    
    let lines = Prelude.lines $ contents
    let valves :: [Valve]
        valves = map readValve lines
    let nonBlockedValves = [(name,flow,adjNames) | (name,flow,adjNames) <- valves, flow /= 0]
    let nonBlockedNames = [name | (name,flow,adjNames) <- nonBlockedValves]
    let bigGraph = Map.fromList [(name, S.fromList adjNames) | (name,flow,adjNames) <- valves]
    
    -- first calculate shortest paths between all non-blocked valves and use it to make a smaller graph
    let (shortestLengths, treePrevs) = getShortestPaths bigGraph
    let initial = "AA"
    
    let bestBigGraphPaths = do
            v1 <- "AA" : nonBlockedNames
            v2 <- nonBlockedNames
            guard $ v1 /= v2
            
            let bestPath = getShortestPath v1 v2 treePrevs
            let bestPathLength = fromJust $ fromJust $ lookupNested v1 v2 shortestLengths
            return ((v1, v2), (bestPathLength, bestPath))
    
    let initialEvent = (initial,0)
    
    let smallGraphNodes = nonBlockedNames :: [String]
    let smallGraphEdges = Map.fromList bestBigGraphPaths :: Map.Map (String,String) Path
    let smallGraphTimeCost v1 v2 = fst $ smallGraphEdges Map.! (v1,v2)
    let flows :: Flows
        flows = Map.fromList $ ("END",0):[(name,flow) | (name,flow,adjNames) <- nonBlockedValves]
    let smallGraph = Map.fromList [(v,filter (/= v) smallGraphNodes) | v <- smallGraphNodes]
    
    let edgesFromStartToSmallGraph = let (v0,t0) = initialEvent in ((v0,t0), ("END",duration):[(v1,t1) | v1 <- smallGraphNodes, v0 /= v1, let t1 = t0 + smallGraphTimeCost v0 v1 + 1, t1 <= duration]) -- since we don't start on a node on the small graph when the start node is a blocked valve
    let eventEdges                   = edgesFromStartToSmallGraph:[((v0,t0), ("END",duration):[(v1,t1) | v1 <- smallGraphNodes, v0 /= v1, let t1 = t0 + smallGraphTimeCost v0 v1 + 1, t1 <= duration]) | t0 <- [1..duration], v0 <- smallGraphNodes] -- I could optimise by not generating for nodesAtTime at all times but just the nodes that are reached
    let eventGraph :: EventGraph
        eventGraph = Map.fromList eventEdges 
                                                                    -- Note that this ---->  ("END",duration,False):  <----- is because I want to "complete" event histories which don't do any more moving or opening valves because we don't have time (which can easily still be the best option)
    
    ------------
    -- Search --
    ------------
    let cfg = (Config {cfgDuration = duration, cfgGraph = eventGraph, cfgFlows = flows})
    
    let initGuy = Guy {guyVisited = [(initial,0)], guyLatestEvent = initialEvent}
    let searchInit = SearchState {currentGroupVisited = S.singleton initial, currentGuys = replicate 2 initGuy, currentPressureReleased = 0}
    let bestFoundSearchState = ssBestFoundSearchState $ runSearch cfg $ search searchInit
    
    -----------
    -- Print --
    -----------
    print $ currentPressureReleased bestFoundSearchState


------------------------- --
-- Search Monad + Actions --
----------------------------
-- Apparently the name for this version of "discrete-event simulation" is called "next-event time progression" where we can jump to whatever the next event is rather than processing moments in time between
search :: SearchState -> Search ()
search ss@(SearchState anyoneVisited guys@((Guy visited event@(v,time)):otherGuys) ancestorPressureReleased)
    | all (\(Guy _ (valveName,_)) -> valveName == "END") guys = do
        -- trace ("all (\\(Guy _ (valveName,_)) -> valveName == \"END\") guys") $ return ()
        -- trace (show (map guyVisited $ currentGuys ss) ++ " " ++ show (currentPressureReleased ss)) $ return ()
        bestFoundEndPressure <- gets ssBestFoundEndPressure
        if ancestorPressureReleased > bestFoundEndPressure
         then do
            modify (\so -> so {ssBestFoundSearchState = ss})
         else return ()
    | otherwise = do
        -- trace (show (map guyVisited $ currentGuys ss)) $ return ()
        bestFoundEndPressure <- gets ssBestFoundEndPressure
        graph <- asks cfgGraph
        flows <- asks cfgFlows
        duration <- asks cfgDuration
        cfg <- ask
        
        let nextSearchProgresses :: [SearchState]
            nextSearchProgresses = do
                childEvent@(childName,childTime) <- fromMaybe [] $ Map.lookup (v,time) graph
                
                guard $ childName == "END" || not (childName `elem` anyoneVisited)
                
                let childPressureReleased = pressureReleased flows duration childEvent + ancestorPressureReleased
                let nextVisited = childEvent:visited
                let nextGroupVisited = S.insert childName anyoneVisited
                let mightBeatPrevBest = childPressureReleased + magicallyOpenRemainingPressure nextGroupVisited childTime cfg > bestFoundEndPressure
                
                guard $ mightBeatPrevBest
                
                -- if and $ zipWith (==) (reverse anyoneVisited) (reverse ["END","END","VG","AH","DP","WE","XJ","RU","KG","RC","CF","HF","DY","EF","AA"])
                 -- then do
                    -- trace (show anyoneVisited ++ " " ++ show ancestorPressureReleased) $ return ()
                    -- magicallyOpenRemainingPressureTRACE nextGroupVisited childTime cfg
                    -- trace (show childPressureReleased) $ return ()
                 -- else return ()
                
                -- if (\visited -> and $ zipWith (==) (reverse visited) ((map (\(n,_,t) -> (n,t)) day1Part2AlmostSolutionPath2))) nextVisited
                 -- then do
                    -- trace (show (ss,childEvent,childPressureReleased + magicallyOpenRemainingPressure nextGroupVisited childTime cfg,bestFoundEndPressure)) $ return ()
                    -- -- magicallyOpenRemainingPressureTRACE nextGroupVisited childTime cfg
                    -- -- trace (show childPressureReleased) $ return ()
                 -- else return ()
                let nextGuy = Guy nextVisited childEvent
                let nextGuys = sortBy (comparing guyTime) (nextGuy:otherGuys)
                return (SearchState nextGroupVisited nextGuys childPressureReleased)
        
        mapM_ search nextSearchProgresses

guyTime (Guy _ (_,time)) = time

-- rotations xs = (map (\i -> take n . drop i $ cycle xs) [0..(n-1)])
  -- where n = length xs

pathPressureReleased :: Int -> [(a,Int,Int)] -> Int
pathPressureReleased duration path = sum [(duration-time)*flow | (label,flow,time) <- path]

day1Part1SolutionPath = [("AA",0,0),("DY",23,3),("HF",17,6),("RC",14,9),("XJ",24,12),("DP",18,16),("RU",16,21),("KG",25,25),("EF",22,28),("END",0,30)]

day1Part2SolutionPath1, day1Part2SolutionPath2 :: [(String,Int,Int)]
day1Part2SolutionPath1 = [("AA",0,0),("DY",23,3),("HF",17,6),("CF",20,9),("RC",14,14),("XJ",24,17),("DP",18,21),("VG",3,24),("END",0,26)]
day1Part2SolutionPath2 = [("AA",0,0),("RU",16,5),("KG",25,9),("EF",22,12),("AH",12,15),("XE",11,19),("END",0,26)]

day1Part1Solution = pathPressureReleased 30 day1Part1SolutionPath
day1Part2Solution = pathPressureReleased 26 day1Part2SolutionPath1 + pathPressureReleased 26 day1Part2SolutionPath2

-- ghci> manualPathGreen + manualPathRed
-- 2881
-- ghci> manualPathPurple + manualPathRed
-- 2541

-- manualPathGreen -> 1216

printSearchState (visited, ancestorPressureReleased, event) desc = do
    bestFoundEndPressure <- gets ssBestFoundEndPressure
    let outStr = unlines [
            "----",
            desc,
            intercalate "\t" ["bestFoundEndPressure", show bestFoundEndPressure],
            intercalate "\t" ["visited", show visited],
            intercalate "\t" ["ancestorPressureReleased", show ancestorPressureReleased],
            intercalate "\t" ["event", show event],
            "----"
            ]
    trace outStr $ return ()

-- Use RWS monad to track visited so that we don't revisit an already opened valve node (so we can't open a valve several times to get more pressure to release like was happening in my previous commits...)
data SearchState = SearchState {currentGroupVisited :: S.Set ValveName, currentGuys :: [Guy], currentPressureReleased :: Int} deriving (Show)
data Guy = Guy {guyVisited :: [Event], guyLatestEvent :: Event} deriving (Show)
type Search  = RWS  Config () SearchOutput
type SearchT = RWST Config () SearchOutput
data Config = Config {cfgDuration :: Int, cfgGraph :: EventGraph, cfgFlows :: Flows} deriving (Show)
newtype SearchOutput = SearchOutput {ssBestFoundSearchState :: SearchState} deriving (Show)
runSearch :: Config -> Search () -> SearchOutput
runSearch cfg s = fst $ execRWS s cfg (SearchOutput {ssBestFoundSearchState = SearchState S.empty [] 0})

ssBestFoundEndPressure = currentPressureReleased . ssBestFoundSearchState

magicallyOpenRemainingPressure visited time cfg = (duration-time) * unvisitedTotalFlow
  where duration = cfgDuration cfg
        flows = cfgFlows cfg
        unvisitedTotalFlow = sum [flow | (name,flow) <- Map.toList flows, not (name `elem` visited)]

magicallyOpenRemainingPressureTRACE visited time cfg = trace ("(" ++ (show duration) ++ " - " ++ (show time) ++ ") * " ++ (show unvisitedTotalFlow) ++ " = " ++ (show $ (duration-time) * unvisitedTotalFlow)) $ return ()
  where duration = cfgDuration cfg
        flows = cfgFlows cfg
        unvisitedTotalFlow = sum [flow | (name,flow) <- Map.toList flows, name `elem` visited]

clampNonNegative x | x >= 0    = x
                   | otherwise = 0

pressureReleased :: Flows -> Int -> Event -> Pressure
pressureReleased flows duration (v1,t1)
    = let timeWillBeOpen = clampNonNegative (duration - t1)
          f1 = flows Map.! v1
      in f1*timeWillBeOpen


------------------
-- TGF (Output) --
------------------
printValvesAsTGF :: [Valve] -> IO ()
printValvesAsTGF = putStrLn . showValvesAsTGF

printValvesAsTGFWithDist :: [ValveWithPaths] -> IO ()
printValvesAsTGFWithDist = putStrLn . showValvesAsTGFWithDist

showValvesAsTGF :: [Valve] -> String
showValvesAsTGF valves = unlines . concat $ [
        [name ++ " " ++ name ++ "-" ++ show flow | (name,flow,adjNames) <- valves],
        ["#"],
        [name ++ " " ++ adjName | (name,flow,adjNames) <- valves, adjName <- adjNames]
    ]

showValvesAsTGFWithDist :: [ValveWithPaths] -> String
showValvesAsTGFWithDist valves = unlines . concat $ [
        [name ++ " " ++ name ++ "-" ++ show flow | (name,flow,adj) <- valves],
        ["#"],
        [name ++ " " ++ adjName ++ " " ++ show pathLength | (name,flow,adj) <- valves, (adjName, (pathLength,_)) <- adj]
    ]

writeValvesAsTGF :: String -> [Valve] -> IO ()
writeValvesAsTGF filepath valves = writeFile filepath $ showValvesAsTGF valves

writeValvesAsTGFWithDist :: String -> [ValveWithPaths] -> IO ()
writeValvesAsTGFWithDist filepath valves = writeFile filepath $ showValvesAsTGFWithDist valves


-----------
-- Other --
-----------
type Pressure = Int

type Time = Int

type Flow  = Int
type Flows = Map String Flow

type ValveName = String
type Valve = (String,Flow,[String])
type ValveWithPaths = (String,Flow,[(String,Path)])

type Event = (ValveName,Time)
type EventGraph = Map Event [Event]

type Path = (Time,[String])

readValve :: String -> Valve 
readValve inStr = (name,flow,adjNames)
  where (name,after1)    = break (==' ') (Data.List.drop (length "Valve "         ) $ inStr)
        (flowStr,after2) = break (==';') (Data.List.drop (length " has flow rate=") $ after1)
        adjNamesStr = Data.List.drop 1 $ dropWhile (/=' ') $ dropWhile (/='v') $ after2
        flow = read flowStr
        adjNames = splitOn ", " adjNamesStr

eventNodeLabel (v,time) = v ++ ":" ++ show time

type Graph = Map.Map String (S.Set String)
type ShortestLengths = Map.Map String (Map.Map String (Maybe Int))
type TreePrevs = Map.Map String (Map.Map String (Maybe String))

lookupNested :: String -> String -> Map.Map String (Map.Map String a) -> Maybe a
lookupNested outerKey innerKey m = do
  innerMap <- Map.lookup outerKey m
  Map.lookup innerKey innerMap

getGraphFromFile :: FilePath -> IO Graph
getGraphFromFile inputFilePath = do
    content <- readFile inputFilePath
    let linesInfos = map (getAllTextMatches . (=~ "[A-Z][A-Z]|\\d+")) (lines content)
    return $ Map.fromList [(head x, S.fromList (drop 2 x)) | x <- linesInfos]

-- getShortestPathLengths uses Floyd–Warshall algorithm
getShortestPathLengths :: Graph -> ShortestLengths
getShortestPathLengths graph = foldl' updateLengths initialLengths [(mid, src, dst) | mid <- keys, src <- keys, dst <- keys]
  where
    keys = Map.keys graph
    initialLengths = Map.fromList [(x, Map.fromList [(y, if y `S.member` (fromJust (Map.lookup x graph)) then Just 1 else Nothing) | y <- keys]) | x <- keys]
    updateLengths acc (mid, src, dst) =
      let srcToMid = (acc Map.! src) Map.! mid
          midToDst = (acc Map.! mid) Map.! dst
          srcToDst = (acc Map.! src) Map.! dst
          candidateLength = (+) <$> srcToMid <*> midToDst
      in case candidateLength of
           Just cl -> if maybe True (cl <) srcToDst
                      then Map.adjust (\m -> Map.adjust (const candidateLength) dst m) src acc
                      else acc
           Nothing -> acc

-- getShortestPaths uses Floyd–Warshall algorithm
getShortestPaths :: Graph -> (ShortestLengths, TreePrevs)
getShortestPaths graph = foldl' updatePaths (initialLengths, initialPrevs) [(mid, src, dst) | mid <- keys, src <- keys, dst <- keys]
  where
    keys = Map.keys graph
    initialLengths = Map.fromList [(x, Map.fromList [(y, if y `S.member` (fromJust (Map.lookup x graph)) then Just 1 else Nothing) | y <- keys]) | x <- keys]
    initialPrevs = Map.fromList [(x, Map.fromList [(y, if y `S.member` (fromJust (Map.lookup x graph)) then Just x else Nothing) | y <- keys]) | x <- keys]
    updatePaths (lengths, prevs) (mid, src, dst) =
      let srcToMid = (lengths Map.! src) Map.! mid
          midToDst = (lengths Map.! mid) Map.! dst
          srcToDst = (lengths Map.! src) Map.! dst
          candidateLength = (+) <$> srcToMid <*> midToDst
      in case candidateLength of
           Just cl -> if maybe True (cl <) srcToDst
                      then (Map.adjust (\m -> Map.adjust (const candidateLength) dst m) src lengths,
                            Map.adjust (\m -> Map.adjust (const (prevs Map.! mid Map.! dst)) dst m) src prevs)
                      else (lengths, prevs)
           Nothing -> (lengths, prevs)

getShortestPath :: String -> String -> TreePrevs -> [String]
getShortestPath src dst treePrevs =
  if isNothing (treePrevs Map.! src Map.! dst)
  then []
  else let Just v = treePrevs Map.! src Map.! dst
           go v' = if v' == src then [src] else v' : go (fromJust (treePrevs Map.! src Map.! v'))
       in reverse (go v)

-- main :: IO ()
-- main = do
  -- graph <- getGraphFromFile "day16 (data).csv"
  -- let (shortestLengths, treePrevs) = getShortestPaths graph
  -- forM_ [(src, dst) | src <- Map.keys graph, dst <- Map.keys graph] $ \(src, dst) ->
    -- putStrLn $ "src: " ++ src ++ ", dst: " ++ dst ++ ", path: " ++ show (getShortestPath src dst treePrevs)
