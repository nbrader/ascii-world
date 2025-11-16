#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2024 Day 02 ("Red-Nosed Reports").
-- Shows step-by-step validation of safety reports.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, hSetBuffering, stdout, utf8, BufferMode(NoBuffering))

type Report = [Int]

data Frame = Frame
    { frameReport :: Report
    , frameReportIdx :: Int
    , frameTotalReports :: Int
    , frameSafeCount :: Int
    , frameCheckIdx :: Int
    , frameIsSafe :: Bool
    , frameFailReason :: Maybe String
    }

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let reports = parseReports contents
        frames = buildFrames reports
        total = length frames
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ (renderFrame total) (zip [0..] frames)
    setCursorPosition 30 0
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

buildFrames :: [Report] -> [Frame]
buildFrames reports = concat $ zipWith processReport [0..] reports
  where
    totalReports = length reports
    processReport idx report =
        let (safe, reason) = checkSafety report
            prevSafe = length $ filter (fst . checkSafety) (take idx reports)
            newSafe = if safe then prevSafe + 1 else prevSafe
        in [ Frame report idx totalReports prevSafe (length report - 1) safe reason
           , Frame report idx totalReports newSafe (length report) safe reason
           ]

checkSafety :: Report -> (Bool, Maybe String)
checkSafety levels
    | null diffs = (True, Nothing)
    | not (allIncreasing || allDecreasing) = (False, Just "Not monotonic (mixed increasing/decreasing)")
    | not validDifferences = (False, Just "Invalid step size (must be 1-3)")
    | otherwise = (True, Nothing)
  where
    diffs = zipWith (-) (tail levels) levels
    allIncreasing = all (> 0) diffs
    allDecreasing = all (< 0) diffs
    validDifferences = all (\d -> abs d >= 1 && abs d <= 3) diffs

renderFrame :: Int -> (Int, Frame) -> IO ()
renderFrame total (idx, Frame report reportIdx totalReports safeCount checkIdx isSafe failReason) = do
    let diffs = if length report > 1
                then zipWith (-) (tail report) report
                else []

        frameContent = unlines
            [ "Red-Nosed Reports - Checking Safety"
            , "Frame " ++ show (idx + 1) ++ " / " ++ show total
            , "Part context: [Part 1] strictly monotonic; [Part 2] allow one bad level."
            , ""
            , "Report " ++ show (reportIdx + 1) ++ " / " ++ show totalReports
            , "Safe reports so far: " ++ show safeCount
            , ""
            , "Levels: " ++ unwords (map show report)
            , if isSafe
              then "Result: SAFE ✓"
              else "Result: UNSAFE ✗ - " ++ maybe "" id failReason
            , ""
            , "Differences between levels:"
            , "  " ++ unwords [show (report !! i) ++ "->" ++ show (report !! (i+1)) ++ ":" ++ showDiff (diffs !! i)
                             | i <- [0..length diffs - 1]]
            , ""
            , "Rules:"
            , "  • Levels must be strictly increasing OR decreasing"
            , "  • Adjacent levels must differ by 1-3"
            ]

    setCursorPosition 0 0
    putStr frameContent
    threadDelay (if checkIdx >= length report then 250000 else 150000)
  where
    showDiff d
        | d > 0 = "+" ++ show d
        | otherwise = show d
