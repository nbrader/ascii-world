#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package split-0.2.3.5

-------------------------------
-------------------------------
----  Day 5:  Print Queue  ----
-------------------------------
-------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc -- '.\day5.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day5part1
-- 

-- *Main> day5part2
-- 


-------------
-- Imports --
-------------
import Data.List (sortBy)
import Data.List.Split (splitOn, chunksOf)


-------------
-- Program --
-------------
main = day5part2

readInt = read :: String -> Int

isCorrect :: [(Int,Int)] -> [Int] -> Bool
isCorrect [] _ = True
isCorrect _ [] = True
isCorrect rules (p:pages) = all (not . (`elem` pages)) forbiddenPages && isCorrect newRules pages
  where
    forbiddenPages = map fst $ filter (\r -> snd r == p) rules
    newRules = filter (\r -> fst r `elem` pages) rules

compareFromRules rules x y
    | any (== (x,y)) rules = LT
    | any (== (y,x)) rules = GT
    | otherwise = EQ

getMiddle :: [a] -> a
getMiddle xs = head $ drop (length xs `div` 2) $ xs

day5part1 = do
    contents <- readFile "day5 (data).csv"
    
    let rows = lines contents
    let [ruleRows,updateRows] = splitOn [""] rows
    let rules = [(\[x,y] -> (readInt x, readInt y)) $ splitOn "|" row | row <- ruleRows]
    let updates = [map readInt $ splitOn "," row | row <- updateRows]
    let correctUpdates = filter (isCorrect rules) updates
    let middles = map getMiddle correctUpdates
    let total = sum middles
    
    print total

day5part2 = do
    contents <- readFile "day5 (data).csv"
    
    let rows = lines contents
    let [ruleRows,updateRows] = splitOn [""] rows
    let rules = [(\[x,y] -> (readInt x, readInt y)) $ splitOn "|" row | row <- ruleRows]
    let updates = [map readInt $ splitOn "," row | row <- updateRows]
    let incorrectUpdates = filter (not . isCorrect rules) updates
    let sortedUpdated = map (sortBy (compareFromRules rules)) incorrectUpdates
    let middles = map getMiddle sortedUpdated
    let total = sum middles
    
    print total
