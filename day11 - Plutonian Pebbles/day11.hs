#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package split-0.2.3.5 --package strict-0.4.0.1 --package unordered-containers-0.2.19.1 --package array-0.5.4.0

-------------------------------------
-------------------------------------
----  Day 11: Plutonian Pebbles  ----
-------------------------------------
-------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 --package strict-0.4.0.1 --package unordered-containers-0.2.19.1 --package array-0.5.4.0 -- '.\day11.hs' -O2
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


-------------
-- Program --
-------------
main = day11part2

readStones :: String -> [Int]
readStones = map read . words

splitDigits :: Int -> (Int,Int)
splitDigits x = (read lStr, read rStr)
  where str = show x
        n = length str
        halfN = n `div` 2
        (lStr,rStr) = splitAt halfN str

numDigits x = floor (log x / log 10)

isEven x = x `mod` 2 == 0

blink :: [Int] -> [Int]
blink stones = concatMap changeStone stones
  where changeStone :: Int -> [Int]
        changeStone x
            | x == 0
                = [1]
            | isEven (length $ show x)
                = let (l,r) = splitDigits x
                  in [l,r]
            | otherwise = [x * 2024]

day11part1 = do
    contents <- readFile "day11 (data).csv"
    
    let initStones = readStones contents
        numOfBlinks = 25
        finalStones = iterate blink initStones !! numOfBlinks
    
    print $ length finalStones

day11part2 = do
    contents <- readFile "day11 (data).csv"

    let initStones = readStones contents

        -- Memoized function for blinking stones
        memoizedBlink :: [Int] -> Int -> [Int]
        memoizedBlink stones = (map (iterate blink stones !!) [0..] !!)

        -- Use memoization for stonesAfterBlinksFromStone
        stonesAfterBlinksFromStone :: Int -> Int -> [Int]
        stonesAfterBlinksFromStone i n = memoizedBlink [i] n

        finalStones = concat $ map (\i -> stonesAfterBlinksFromStone i 25) initStones

    print $ length finalStones

memoizedBlink :: [Int] -> Map.HashMap (Int, Int) [Int]
memoizedBlink initStones = 
    let memoMap = Map.fromList [((stone, 0), [stone]) | stone <- initStones]
        blinkedMap = foldl' computeBlinkForAllStones memoMap [1..75]
    in blinkedMap

computeBlinkForAllStones :: Map.HashMap (Int, Int) [Int] -> Int -> Map.HashMap (Int, Int) [Int]
computeBlinkForAllStones memo n =
    foldl' (computeStoneBlinkMemo n) memo (Map.keys memo)

computeStoneBlinkMemo :: Int -> Map.HashMap (Int, Int) [Int] -> (Int, Int) -> Map.HashMap (Int, Int) [Int]
computeStoneBlinkMemo n memo (stone, prevN)
    | prevN == n - 1 = 
        let prevStones = fromMaybe [stone] $ Map.lookup (stone, prevN) memo
            newStones = blink prevStones
        in Map.insert (stone, n) newStones memo
    | otherwise = memo

day11part2' = do
    contents <- readFile "day11 (data).csv"
    let initStones = readStones contents
        memoMap = memoizedBlink initStones
        finalStones = concat [fromMaybe [stone] $ Map.lookup (stone, 35) memoMap | stone <- initStones]
    print $ length finalStones