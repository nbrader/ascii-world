#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package split-0.2.3.5 --package strict-0.4.0.1 --package unordered-containers-0.2.19.1 --package array-0.5.4.0 --package memoize-1.1.2

-------------------------------------
-------------------------------------
----  Day 11: Plutonian Pebbles  ----
-------------------------------------
-------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 --package strict-0.4.0.1 --package unordered-containers-0.2.19.1 --package array-0.5.4.0 --package memoize-1.1.2 -- '.\day11.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day11part1
-- 216042

-- *Main> day11part2
-- 


-------------
-- Imports --
-------------
import Data.Array as A
import Data.Char (digitToInt)
import Control.Monad (guard)
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import Data.Function.Memoize


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
    
    printExpandData numOfBlinks initStones

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
    ed_Repeats :: [Repeat],
    ed_StoneChildren :: [ChildRelation],
    ed_NextStones :: [Int],
    ed_BlinksRemaining :: Int
    } deriving Show

data Repeat = Repeat {
    repeatValueSeen :: Int,
    repeatBlinksRemainingWhenSeenAgain :: Int
    } deriving Show

data ChildRelation = ChildRelation {
    relationParent :: Int,
    relationChildren :: [Int]
    } deriving Show

expand :: Int -> [Int] -> ExpandData
expand blinksRemaining stoneNums = until ((==0) . ed_BlinksRemaining) go (ExpandData {ed_Seen = stoneNums, ed_Repeats = [], ed_StoneChildren = [], ed_NextStones = stoneNums, ed_BlinksRemaining = blinksRemaining})
  where go :: ExpandData -> ExpandData
        go prevData
            = let
                newBlinksRemaining = ed_BlinksRemaining prevData - 1
                newChildren
                    | ed_BlinksRemaining prevData == 0 = []
                    | otherwise = map (\p -> ChildRelation {relationParent = p, relationChildren = blinkStoneStep p}) (ed_NextStones prevData)
                groupedNewChildren = group $ sort $ concatMap relationChildren $ newChildren
                repeatsRepeatingStonesInThisStep = concat $ map (drop 1) $ groupedNewChildren
                uniqueInThisStep = map head $ filter ((== 1) . length) $ groupedNewChildren
                repeatsRepeatingStonesInPrevSteps = uniqueInThisStep `intersect` ed_Seen prevData
                newRepeats = map (\val -> Repeat {repeatValueSeen = val, repeatBlinksRemainingWhenSeenAgain = newBlinksRemaining}) $ (repeatsRepeatingStonesInThisStep `union` repeatsRepeatingStonesInPrevSteps)
                nextStones = filter (not . (`elem` ed_Seen prevData)) (concatMap (take 1) groupedNewChildren)
              in ExpandData {
                    ed_Seen    = ed_Seen prevData ++ nextStones,
                    ed_Repeats = ed_Repeats prevData ++ newRepeats,
                    ed_StoneChildren = ed_StoneChildren prevData ++ newChildren,
                    ed_NextStones = nextStones,
                    ed_BlinksRemaining = newBlinksRemaining
                    }
