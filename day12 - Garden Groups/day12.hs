#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package split-0.2.3.5 --package strict-0.4.0.1 --package unordered-containers-0.2.19.1 --package array-0.5.4.0 --package memoize-1.1.2 --package hashable-1.3.0.0

{-# LANGUAGE DeriveGeneric #-}

---------------------------------
---------------------------------
----  Day 11: Garden Groups  ----
---------------------------------
---------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 --package strict-0.4.0.1 --package unordered-containers-0.2.19.1 --package array-0.5.4.0 --package memoize-1.1.2 --package hashable-1.3.0.0 -- '.\day12.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day12part1
-- 

-- *Main> day12part2
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
import GHC.Generics (Generic)
import Data.Hashable


-------------
-- Program --
-------------
main = day12part1

-- readStones :: String -> [Int]
-- readStones = map read . words

day12part1 = do
    contents <- readFile "day12 (data).csv"
    
    -- let initStones = readStones contents
        -- numOfBlinks = 25
        -- finalStones = iterate blink initStones !! numOfBlinks
    
    print $ contents