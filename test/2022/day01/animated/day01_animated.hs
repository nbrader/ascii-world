#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
   --package split-0.2.3.5
-}

-- |
-- Animated visualization for AoC 2022 Day 1 ("Calorie Counting").
-- Shows the process of summing calories and finding the top elves.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.List (sort, sortBy)
import Data.List.Split (splitOn)
import Data.Ord (Down(..), comparing)
import qualified Data.Map as M
import System.Console.ANSI
import System.IO (hSetEncoding, stdout, utf8)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let totals = parseCalories contents
        sortedTotals = sortBy (comparing Down) totals
        frames = buildFrames totals sortedTotals
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    setCursorPosition 25 0
    putStrLn "AoC 2022 Day 1 animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "01"
        filename = case inputType of
            "data" -> "day" ++ dayNum ++ " (data).csv"
            "example2" -> "day" ++ dayNum ++ " (example 2).csv"
            "example3" -> "day" ++ dayNum ++ " (example 3).csv"
            _ -> "day" ++ dayNum ++ " (example).csv"
        path = "test/2022/day" ++ dayNum ++ "/standard/" ++ filename
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure $ unlines
            [ "1000", "2000", "3000"
            , ""
            , "4000"
            , ""
            , "5000", "6000"
            , ""
            , "7000", "8000", "9000"
            , ""
            , "10000"
            ]

parseCalories :: String -> [Int]
parseCalories contents = map sum elvesCalories
  where
    elvesCalories = map (map read) . splitOn [""] . lines $ contents

buildFrames :: [Int] -> [Int] -> [(Int, Int, Int, Int)]
buildFrames totals sortedTotals =
    [(i, maxCal, top3, length totals) | (i, maxCal, top3) <- zip3 [1..] maxScans top3Scans]
  where
    maxScans = scanl max 0 totals
    top3Scans = [sum (take 3 (sortBy (comparing Down) (take i totals))) | i <- [0..length totals]]

renderFrame :: (Int, Int, Int, Int) -> IO ()
renderFrame (idx, maxCal, top3Sum, totalElves) = do
    setCursorPosition 0 0
    putStrLn "Calorie Counting - Finding Top Elves"
    putStrLn "Part context: [Part 1] find max; [Part 2] sum of top 3."
    putStrLn ""
    putStrLn $ "Elves processed: " ++ show idx ++ " / " ++ show totalElves
    putStrLn $ "Current max calories: " ++ show maxCal
    putStrLn $ "Current top 3 sum: " ++ show top3Sum
    putStrLn ""

    -- Visualize as a simple progress bar
    let width = 50
        progress = if totalElves > 0 then (idx * width) `div` totalElves else 0
        progressPoints = [(i, 0) | i <- [0..min progress (width-1)]]
        asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList [("Progress", progressPoints)]
            , asciiWorldWidth = width
            }
        bgChar = '.'
        maskToChar = id
        pointsToChar = const '#'
        nameZOrder = compare
        worldStr = showAsciiWorld 1 bgChar maskToChar pointsToChar nameZOrder asciiWorld

    putStr worldStr
    threadDelay 100000
