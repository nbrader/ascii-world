#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package unordered-containers --package memoize --package hashable

{-# LANGUAGE DeriveGeneric #-}

-------------------------------------
-------------------------------------
----  Day 11: Plutonian Pebbles  ----
-------------------------------------
-------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package unordered-containers --package memoize --package hashable -- '.\day11.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day11part1
-- 216042

-- *Main> day11part2
-- 255758646442399


-------------
-- Imports --
-------------
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import Data.Function.Memoize
import GHC.Generics (Generic)
import Data.Hashable


-------------
-- Program --
-------------
main = day11part2

readStones :: String -> [Int]
readStones = map read . words

splitDigits :: Int -> (Int, Int)
splitDigits x 
    | x < 10    = (0, x)
    | otherwise = (left, right)
  where
    len = 1 + floor (log (fromIntegral x) / log 10)
    halfLen = len `div` 2
    divisor = floor (10 ** fromIntegral halfLen)
    left = x `div` divisor
    right = x `mod` divisor

numDigits x = floor (log x / log 10)

isEven x = x `mod` 2 == 0

blink :: [Int] -> [Int]
blink stones = concatMap blinkStoneStep stones

-- Memoized blinkStoneStep function
blinkStoneStep :: Int -> [Int]
blinkStoneStep x
    | x == 0
        = [1]
    | isEven (length $ show x)
        = let (l,r) = splitDigits x
          in [l,r]
    | otherwise = [x * 2024]

-- Memoized nthBlink function using memoize
nthBlink :: Int -> Int -> [Int]
nthBlink = memoize2 nthBlinkCore

-- Core implementation of nthBlink
nthBlinkCore :: Int -> Int -> [Int]
nthBlinkCore x 0 = [x]
nthBlinkCore x n = concatMap blinkStoneStep (nthBlink x (n-1))

day11part1 = do
    contents <- readFile "day11 (data).csv"
    
    let initStones = readStones contents
        numOfBlinks = 25
        finalStones = iterate blink initStones !! numOfBlinks
    
    print $ length finalStones

day11part2 = do
    contents <- readFile "day11 (data).csv"
    
    let numOfBlinks = 75
        initStones = readStones contents
        expanded = expand numOfBlinks initStones
        
        initMapOfStoneCountFromInitAndBlinks :: Map.HashMap InitAndBlinks Int
        initMapOfStoneCountFromInitAndBlinks = Map.fromList $ map (\stone -> (InitAndBlinks {initAndBlinks_Init = stone, initAndBlinks_BlinksRemaining = 0}, 1)) (ed_NextStones expanded)
    
    -- printExpandData numOfBlinks initStones
    -- printMapOfStoneCountFromInitAndBlinks initMapOfStoneCountFromInitAndBlinks
    
    let -- lookupStoneCountForcingResultAndCaching can calculate a total but in this case we throw it away because we just want the map
        (countsMapWithRepeatsPrecalc, _) = foldl' (\(countsMap, _) rep -> lookupStoneCountForcingResultAndCaching rep countsMap (ed_StoneChildren expanded)) (initMapOfStoneCountFromInitAndBlinks, 0) (ed_Repeats expanded)
    
    -- mapM_ print $ Map.toList countsMap
    
    let (_, total) = foldl' (\(countsMap, total) initStone -> let (newCountsMap, newSubtotal) = lookupStoneCountForcingResultAndCaching (InitAndBlinks {initAndBlinks_Init = initStone, initAndBlinks_BlinksRemaining = numOfBlinks}) countsMap (ed_StoneChildren expanded)
                                                              in (newCountsMap, total + newSubtotal)) (countsMapWithRepeatsPrecalc, 0) initStones
    
    print total

data InitAndBlinks = InitAndBlinks {
    initAndBlinks_Init :: Int,
    initAndBlinks_BlinksRemaining :: Int
    } deriving (Show, Eq, Generic)

instance Hashable InitAndBlinks

-- lookupStoneCount is currently unused but it could be useful for more efficient program if lookupStoneCountForcingResultAndCaching is overzealous at calculating results it doesn't have in it's map
lookupStoneCount :: InitAndBlinks -> Map.HashMap InitAndBlinks Int -> Maybe Int
lookupStoneCount (InitAndBlinks _ 0) m = Just 1
lookupStoneCount k m = Map.lookup k m

lookupStoneCountForcingResultAndCaching :: InitAndBlinks -> Map.HashMap InitAndBlinks Int -> Map.HashMap Int [Int] -> (Map.HashMap InitAndBlinks Int, Int)
lookupStoneCountForcingResultAndCaching (InitAndBlinks _ 0) countsMap childrenMap
    = (countsMap, 1)
lookupStoneCountForcingResultAndCaching k@(InitAndBlinks initStone blinksRemaining) countsMap childrenMap
    = case Map.lookup k countsMap of
        Nothing -> let children = fromJust $ Map.lookup initStone childrenMap
                       (newCountsMap, newCount)
                            = foldl' (\(countsMap', total) child -> let (newCountsMap', newCount) = lookupStoneCountForcingResultAndCaching (InitAndBlinks child (blinksRemaining-1)) countsMap' childrenMap
                                                                    in (newCountsMap', total + newCount)) (countsMap, 0) children
                   in (Map.insert k newCount newCountsMap, newCount)
        Just count -> (countsMap, count)

printMapOfStoneCountFromInitAndBlinks m = do
    putStrLn "MapOfStoneCountFromInitAndBlinks = "
    mapM_ print $ Map.toList m
    putStrLn ""

printExpandData numOfBlinks initStones = do
    let expanded = expand numOfBlinks initStones
    
    putStrLn "ed_Seen expanded = "
    mapM_ print $ ed_Seen expanded
    putStrLn ""
    
    putStrLn "ed_Repeats expanded = "
    mapM_ print $ ed_Repeats expanded
    putStrLn ""
    
    putStrLn "ed_StoneChildren expanded = "
    mapM_ print $ ed_StoneChildren expanded
    putStrLn ""
    
    putStrLn "ed_NextStones expanded = "
    mapM_ print $ ed_NextStones expanded
    putStrLn ""

data ExpandData = ExpandData {
    ed_Seen :: [Int],
    ed_Repeats :: [InitAndBlinks],
    ed_StoneChildren :: Map.HashMap Int [Int],
    ed_NextStones :: [Int],
    ed_BlinksRemaining :: Int
    } deriving Show

expand :: Int -> [Int] -> ExpandData
expand blinksRemaining stoneNums = until ((==0) . ed_BlinksRemaining) go (ExpandData {ed_Seen = stoneNums, ed_Repeats = [], ed_StoneChildren = Map.empty, ed_NextStones = stoneNums, ed_BlinksRemaining = blinksRemaining})
  where go :: ExpandData -> ExpandData
        go prevData
            = let
                newBlinksRemaining = ed_BlinksRemaining prevData - 1
                newChildren
                    | ed_BlinksRemaining prevData == 0 = []
                    | otherwise = map (\p -> (p, blinkStoneStep p)) (ed_NextStones prevData)
                groupedNewChildren = group $ sort $ concatMap snd $ newChildren
                repeatsRepeatingStonesInThisStep = concat $ map (drop 1) $ groupedNewChildren
                uniqueInThisStep = map head $ filter ((== 1) . length) $ groupedNewChildren
                repeatsRepeatingStonesInPrevSteps = uniqueInThisStep `intersect` ed_Seen prevData
                newRepeats = map (\val -> InitAndBlinks {initAndBlinks_Init = val, initAndBlinks_BlinksRemaining = newBlinksRemaining}) $ (repeatsRepeatingStonesInThisStep `union` repeatsRepeatingStonesInPrevSteps)
                nextStones = filter (not . (`elem` ed_Seen prevData)) (concatMap (take 1) groupedNewChildren)
              in ExpandData {
                    ed_Seen    = ed_Seen prevData ++ nextStones,
                    ed_Repeats = ed_Repeats prevData ++ newRepeats,
                    ed_StoneChildren = ed_StoneChildren prevData `Map.union` Map.fromList newChildren,
                    ed_NextStones = nextStones,
                    ed_BlinksRemaining = newBlinksRemaining
                    }
