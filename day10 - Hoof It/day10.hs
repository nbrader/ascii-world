#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package split-0.2.3.5 --package strict-0.4.0.1 --package unordered-containers-0.2.19.1 --package array-0.5.4.0

---------------------------
---------------------------
----  Day 10: Hoof It  ----
---------------------------
---------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 --package strict-0.4.0.1 --package unordered-containers-0.2.19.1 --package array-0.5.4.0 -- '.\day10.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day10part1
-- 611

-- *Main> day10part2
-- 1380


-------------
-- Imports --
-------------
import Data.Array as A (Array, (!), listArray)
import Data.Char (digitToInt)
import Control.Monad (guard)
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as Map


-------------
-- Program --
-------------
main = day10part2

day10part1 = do
    contents <- readFile "day10 (data).csv"
    
    let lines = Prelude.lines $ contents
        width  = length (head lines)
        height = length lines
        grid = listArray ((0,0),(width-1,height-1)) $ map digitToInt $ concat lines
        
        (tops:lowerLevelSets) = [[(x,y) | x <- [0..width-1], y <- [0..height-1], grid A.! (x,y) == i] | i <- [9,8..0]]
        finishSets = [foldl (\upper next -> expand upper `intersect` next) [top] lowerLevelSets | top <- tops]
        
        countTrailHeads = foldl' (\mp finishSet -> foldl' (\mp trailHead -> Map.alter (\maybeTotal -> maybe (Just 1) (\x -> Just (x+1)) maybeTotal) trailHead mp) mp finishSet) Map.empty finishSets
    
    -- mapM_ (\k -> print $ fromJust $ Map.lookup k countTrailHeads) (sort $ Map.keys countTrailHeads)
    print $ sum (Map.elems countTrailHeads)

day10part2 = do
    contents <- readFile "day10 (data).csv"
    
    let lines = Prelude.lines $ contents
        width  = length (head lines)
        height = length lines
        grid = listArray ((0,0),(width-1,height-1)) $ map digitToInt $ concat lines
        
        (tops:lowerLevelSets) = [[(x,y) | x <- [0..width-1], y <- [0..height-1], grid A.! (x,y) == i] | i <- [9,8..0]]
        finishSets = do
            top <- tops
            return $ foldl step [[top]] lowerLevelSets
          where step :: [[(Int, Int)]] -> [(Int, Int)] -> [[(Int, Int)]]
                step upperSets next = concatMap step' upperSets
                  where step' :: [(Int, Int)] -> [[(Int, Int)]]
                        step' upper = map step'' (expand upper)
                          where step'' :: (Int, Int) -> [(Int, Int)]
                                step'' x = [x] `intersect` next
    
    print $ length $ concat $ concat $ finishSets

expand :: [(Int,Int)] -> [(Int,Int)]
expand = nub . concatMap (\p -> [p `addV2` v | v <- [(1,0),(0,1),(-1,0),(0,-1)]])

addV2 (x1,y1) (x2,y2) = (x1+x2, y1+y2)