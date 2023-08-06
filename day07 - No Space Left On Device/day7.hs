#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package split-0.2.3.5 --package containers-0.6.6
-------------------------------------------
-------------------------------------------
----  Day 7:  No Space Left On Device  ----
-------------------------------------------
-------------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 --package containers-0.6.6 -- '.\day7.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day7part1
-- 2031851

-- *Main> day7part2
-- 2568781


-------------
-- Imports --
-------------
import Data.List (elemIndex, sort)
import Data.Tree
import Debug.Trace
import Data.Maybe (isJust)


-------------
-- Program --
-------------
main = day7part2

day7part1 = do
      contents <- readFile "day7 (data).csv"
      let out = getTree . updateSystem . map toLog . lines $ contents
      -- putStrLn . drawTree . fmap show $ out
      print . sum . fst . foldTree collectSmallDirSizes $ out

day7part2 = do
      contents <- readFile "day7 (data).csv"
      let out = getTree . updateSystem . map toLog . lines $ contents
          dirSizes = fst . foldTree collectDirSizes $ out
          totalUsed = maximum dirSizes
          dirSizesBigEnoughToHelp = filter (> 30000000 - (70000000 - totalUsed)) dirSizes
      print . minimum $ dirSizesBigEnoughToHelp

data TerminalLog = ChangeDirUp | ChangeDirTop | ChangeDirDown String | ListCurrent | ListOutputDir String | ListOutputFile String Int deriving (Show)

insertAt :: a -> Int -> [a] -> [a]
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as

removeAt i xs = take i xs ++ drop (1 + i) xs

toLog :: String -> TerminalLog
toLog "$ cd .."           = ChangeDirUp
toLog "$ cd /"            = ChangeDirTop
toLog ('$':' ':'c':'d':' ':childName) = ChangeDirDown childName
toLog "$ ls"              = ListCurrent
toLog ('d':'i':'r':' ':dirName) = ListOutputDir dirName
toLog logStr = let [size,fileName] = words logStr
                in ListOutputFile fileName (read size)

indexOfDirInChildren  childName children = Just childName `elemIndex` (map (\(Node dirNode  _) -> case dirNode  of {(Dir  dirName)    -> Just dirName;  _ -> Nothing}) children)
indexOfFileInChildren childName children = Just childName `elemIndex` (map (\(Node fileNode _) -> case fileNode of {(File fileName _) -> Just fileName; _ -> Nothing}) children)

data DirNode = Dir String | File String Int deriving (Eq, Show)
data TreePos = TreePos ((Tree DirNode, Maybe TreePos), Int)

updateFromLog :: TerminalLog -> (Tree DirNode, Maybe TreePos) -> (Tree DirNode, Maybe TreePos)
updateFromLog ChangeDirUp                    (currTree, Nothing)                                                          = error "no parent"
updateFromLog ChangeDirUp                    (currTree, Just (TreePos ((Node parentNode children, maybeParentPos), pos))) = (Node parentNode (insertAt currTree pos children), maybeParentPos)
updateFromLog ChangeDirTop                   treeContext                                                                  = until (\((Node label _),_) -> label == (Dir "/")) (updateFromLog ChangeDirUp) treeContext
updateFromLog (ChangeDirDown childName)      ((Node currNode currChildren), maybePos)
    = let maybeIndex = indexOfDirInChildren childName currChildren
      in case maybeIndex of
        Just i -> let selectedChild = currChildren !! i
                  in (selectedChild, Just (TreePos (((Node currNode (removeAt i currChildren)), maybePos), i)))
        Nothing -> trace "OMG SOMETHING WENT WRONG" $ updateFromLog (ChangeDirDown childName) ((Node currNode ((Node (Dir childName) []):currChildren)), maybePos)
updateFromLog ListCurrent                    (Node currNode currChildren, maybePos) = (Node currNode currChildren, maybePos)
updateFromLog (ListOutputDir dirName)        (Node currNode currChildren, maybePos) | isJust (indexOfDirInChildren dirName currChildren) = (Node currNode currChildren, maybePos)
                                                                                    | otherwise                                          = (Node currNode (currChildren ++ [Node (Dir dirName) []]), maybePos)
updateFromLog (ListOutputFile fileName size) (Node currNode currChildren, maybePos) | isJust (indexOfFileInChildren fileName currChildren) = (Node currNode currChildren, maybePos)
                                                                                    | otherwise                                          = (Node currNode (currChildren ++ [Node (File fileName size) []]), maybePos)

updateSystem :: [TerminalLog] -> (Tree DirNode, Maybe TreePos)
updateSystem = foldr updateFromLog (Node (Dir "/") [], Nothing) . reverse

getTree :: (Tree DirNode, Maybe TreePos) -> Tree DirNode
getTree (Node currNode currChildren, Nothing)  = Node currNode currChildren
getTree (currTree, Just (TreePos ((Node parentNode children, maybeParentPos), pos))) = getTree $ (Node parentNode (insertAt currTree pos children), maybeParentPos)

collectSmallDirSizes :: DirNode -> [([Int],[Int])] -> ([Int],[Int])
collectSmallDirSizes n@(Dir  _) smallDirSizesAndGrandchildSizes = let childSizes = concat . map snd $ smallDirSizesAndGrandchildSizes
                                                                      smallDirSizes = concat . map fst $ smallDirSizesAndGrandchildSizes
                                                                      dirSize = sum childSizes
                                                                  in if dirSize <= 100000
                                                                        then (dirSize:smallDirSizes, childSizes)
                                                                        else (        smallDirSizes, childSizes)
collectSmallDirSizes n@(File _ size) _ = ([], [size])

collectDirSizes :: DirNode -> [([Int],[Int])] -> ([Int],[Int])
collectDirSizes n@(Dir  _) smallDirSizesAndGrandchildSizes = let childSizes = concat . map snd $ smallDirSizesAndGrandchildSizes
                                                                 smallDirSizes = concat . map fst $ smallDirSizesAndGrandchildSizes
                                                                 dirSize = sum childSizes
                                                             in (dirSize:smallDirSizes, childSizes)
collectDirSizes n@(File _ size) _ = ([], [size])