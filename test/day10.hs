#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3

---------------------------------
---------------------------------
----  Day 10: Hoof It  ----------
---------------------------------
---------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3 -- 'day10.hs' -O2
-}

-------------
-- Imports --
-------------
import Data.Maybe (catMaybes, fromMaybe)
import Data.List as L (foldl', nub)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (digitToInt, isDigit)
import Data.Function
import Control.Monad (forM_)

import Util ( lrduDirs )
import Mask ( Point, Mask )

import AsciiWorld (    AsciiWorld(..)
                    , MaskOrPointsIndex(..)
                    , readAsciiWorld
                    , lookupMask
                    , lookupPoints
                    )


-------------
-- Program --
-------------
main = day10part2

-- Read topographic map
-- Heights are 0-9, '.' for non-walkable
readTopoMap :: String -> (Int, Int, M.Map Point Int)
readTopoMap input = (width, height, heightMap)
  where rows = lines input
        height = length rows
        width = if height == 0 then 0 else length (head rows)

        heightMap = M.fromList $ do
            (y', row) <- zip [0..] rows
            (x, char) <- zip [0..] row
            let y = height - 1 - y'  -- Flip Y to match AsciiWorld coordinate system
            if isDigit char
                then return ((x, y), digitToInt char)
                else []

-- Find all positions with a given height
findHeights :: Int -> M.Map Point Int -> [Point]
findHeights h heightMap = [pt | (pt, height) <- M.toList heightMap, height == h]

-- Get neighbors at exactly height + 1
getUpwardNeighbors :: M.Map Point Int -> Point -> [Point]
getUpwardNeighbors heightMap pos =
    case M.lookup pos heightMap of
        Nothing -> []
        Just currentHeight ->
            let targetHeight = currentHeight + 1
                neighbors = [(x + dx, y + dy) | (dx, dy) <- lrduDirs, let (x, y) = pos]
            in [n | n <- neighbors, M.lookup n heightMap == Just targetHeight]

-- Part 1: Find score of a trailhead (number of reachable peaks)
scoreTrailhead :: M.Map Point Int -> Point -> Int
scoreTrailhead heightMap start = S.size $ findReachablePeaks heightMap start
  where
    findReachablePeaks :: M.Map Point Int -> Point -> S.Set Point
    findReachablePeaks hMap pos
        | M.lookup pos hMap == Just 9 = S.singleton pos
        | otherwise = S.unions [findReachablePeaks hMap neighbor | neighbor <- getUpwardNeighbors hMap pos]

-- Part 2: Find rating of a trailhead (number of distinct paths to peaks)
ratingTrailhead :: M.Map Point Int -> Point -> Int
ratingTrailhead heightMap start = countPaths heightMap start
  where
    countPaths :: M.Map Point Int -> Point -> Int
    countPaths hMap pos
        | M.lookup pos hMap == Just 9 = 1
        | otherwise = sum [countPaths hMap neighbor | neighbor <- getUpwardNeighbors hMap pos]

day10part1 :: IO ()
day10part1 = do
    contents <- readFile "day10 (example).csv"
    let (width, height, heightMap) = readTopoMap contents
        trailheads = findHeights 0 heightMap
        scores = map (scoreTrailhead heightMap) trailheads
        totalScore = sum scores

    putStrLn $ "Trailheads: " ++ show (length trailheads)
    putStrLn $ "Individual scores: " ++ show scores
    putStrLn $ "Total score (Part 1): " ++ show totalScore

day10part2 :: IO ()
day10part2 = do
    contents <- readFile "day10 (example).csv"
    let (width, height, heightMap) = readTopoMap contents
        trailheads = findHeights 0 heightMap
        ratings = map (ratingTrailhead heightMap) trailheads
        totalRating = sum ratings

    putStrLn $ "Trailheads: " ++ show (length trailheads)
    putStrLn $ "Individual ratings: " ++ show ratings
    putStrLn $ "Total rating (Part 2): " ++ show totalRating

-- Test with simple example
testSimple :: IO ()
testSimple = do
    contents <- readFile "day10 (simple).csv"
    let (width, height, heightMap) = readTopoMap contents
        trailheads = findHeights 0 heightMap

    putStrLn "=== Simple Test ==="
    putStrLn $ "Grid: " ++ show width ++ "x" ++ show height
    putStrLn $ "Height map: " ++ show heightMap
    putStrLn $ "Trailheads: " ++ show trailheads

    forM_ trailheads $ \th -> do
        let score = scoreTrailhead heightMap th
            rating = ratingTrailhead heightMap th
        putStrLn $ "Trailhead " ++ show th ++ ": score=" ++ show score ++ ", rating=" ++ show rating
