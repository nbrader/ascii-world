#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci

--------------------------------------
--------------------------------------
----  Day 1:  Historian Hysteria  ----
--------------------------------------
--------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc -- '.\day1.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day1part1
-- 

-- *Main> day1part2
-- 


-------------
-- Imports --
-------------
import Data.Char (isDigit)
import Data.List (tails, isPrefixOf, transpose, sort)
import Data.Map as M hiding (map, filter)
import Data.Maybe (fromJust)


-------------
-- Program --
-------------
main = day1part1

day1part1 = do
  contents <- readFile "day1 (data).csv"
  let rowsOfWords = map words $ lines contents
  let sortedColumnsOfWords = map sort . map (map ((read :: String -> Int))) $ transpose rowsOfWords
  let [col1,col2] = sortedColumnsOfWords
  print $ sum . map abs $ zipWith (-) col2 col1

-- day1part2 = do
  -- contents <- readFile "day1 (data).csv"
  -- let total = sum . map lineToNum . lines $ contents
  -- print $ total