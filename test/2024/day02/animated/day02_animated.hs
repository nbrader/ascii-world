#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2024 Day 02 ("Red-Nosed Reports").
-- Shows validating reports for safety.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import qualified Data.Map as M
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

type Report = [Int]

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let reports = parseReports contents
        frames = map checkReport reports
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    setCursorPosition 25 0
    putStrLn "Red-Nosed Reports animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "02"
        filename = case inputType of
            "data" -> "day" ++ dayNum ++ " (data).csv"
            "example2" -> "day" ++ dayNum ++ " (example 2).csv"
            "example3" -> "day" ++ dayNum ++ " (example 3).csv"
            _ -> "day" ++ dayNum ++ " (example).csv"
        path = "test/2024/day" ++ dayNum ++ "/standard/" ++ filename
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure $ unlines
            [ "7 6 4 2 1"
            , "1 2 7 8 9"
            , "9 7 6 2 1"
            , "1 3 2 4 5"
            , "8 6 4 4 1"
            , "1 3 6 7 9"
            ]

parseReports :: String -> [Report]
parseReports = map (map read . words) . lines

checkReport :: Report -> (Report, Bool)
checkReport report = (report, isSafe report)

isSafe :: Report -> Bool
isSafe levels = (allIncreasing || allDecreasing) && validDifferences
  where
    diffs = zipWith (-) (tail levels) levels
    allIncreasing = all (> 0) diffs
    allDecreasing = all (< 0) diffs
    validDifferences = all (\d -> abs d >= 1 && abs d <= 3) diffs

renderFrame :: (Report, Bool) -> IO ()
renderFrame (report, safe) = do
    setCursorPosition 0 0
    putStrLn "Red-Nosed Reports - Checking Safety"
    putStrLn "Part context: [Part 1] strictly monotonic; [Part 2] allow one bad level."
    putStrLn ""
    putStrLn $ "Report: " ++ show report
    putStrLn $ "Safe: " ++ if safe then "YES" else "NO"
    putStrLn ""

    -- Visualize levels as points
    let width = length report
        points = [(i, 0) | i <- [0..width-1]]
        asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList [("Levels", points)]
            , asciiWorldWidth = max 10 width
            }
        bgChar = '.'
        maskToChar = id
        pointsToChar = const (if safe then 'O' else 'X')
        nameZOrder = compare
        worldStr = showAsciiWorld 2 bgChar maskToChar pointsToChar nameZOrder asciiWorld

    putStr worldStr
    threadDelay 300000
