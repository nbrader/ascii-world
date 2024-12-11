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

splitDigits :: Int -> (Int,Int)
splitDigits x = (read lStr, read rStr)
  where str = show x
        n = length str
        halfN = n `div` 2
        (lStr,rStr) = splitAt halfN str

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
    
    let initStones = readStones contents
        finalStonesLists = map (\n -> concatMap (flip nthBlink n) initStones) [0..75]
    
    mapM_ print . map length $ finalStonesLists