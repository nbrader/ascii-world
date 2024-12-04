#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci

-------------------------------------
-------------------------------------
----  Day 2:  Red-Nosed Reports  ----
-------------------------------------
-------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc -- '.\day2.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day2part1
-- 236

-- *Main> day2part2
-- 308


-------------
-- Imports --
-------------


-------------
-- Program --
-------------
main = day2part2

isSafe, isIncreasing, isDecreasing, isCorrectStepSize :: [Int] -> Bool

isSafe xs = (isIncreasing xs || isDecreasing xs) && isCorrectStepSize xs

isIncreasing xs = all (<0) $ zipWith (-) xs (tail xs)

isDecreasing xs = all (>0) $ zipWith (-) xs (tail xs)

isCorrectStepSize xs = all (\x -> x >= 1 && x <= 3) . map abs $ zipWith (-) xs (tail xs)

removeNth :: Int -> [a] -> [a]
removeNth = \n -> \list ->
      case n of 
          0 -> tail list
          otherwise -> head list: removeNth (n-1) (tail list)

getReportVariants xs = xs : [removeNth n xs | n <- [0 .. (length xs - 1)]]

day2part1 = do
  contents <- readFile "day2 (data).csv"
  let reports = map (map ((read :: String -> Int))) . map words . lines $ contents
  let safe = filter isSafe reports
  print $ length safe

day2part2 = do
  contents <- readFile "day2 (data).csv"
  let reports = map (map ((read :: String -> Int))) . map words . lines $ contents
  let reportVariants = map getReportVariants reports
  let safe = filter (any isSafe) reportVariants
  print $ length safe
