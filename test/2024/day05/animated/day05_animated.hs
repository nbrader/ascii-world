#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2024 Day 05 ("Print Queue").
-- Shows pages being ordered according to rules.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.List (sortBy)
import qualified Data.Map as M
import qualified Data.Set as S
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

type Rule = (Int, Int)  -- (before, after)
type Update = [Int]

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let (rules, updates) = parseInput contents
        frames = concatMap (buildFrames rules) (take 3 updates)
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    setCursorPosition 25 0
    putStrLn "Print Queue animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "05"
        filename = case inputType of
            "data" -> "day" ++ dayNum ++ " (data).csv"
            "example2" -> "day" ++ dayNum ++ " (example 2).csv"
            "example3" -> "day" ++ dayNum ++ " (example 3).csv"
            _ -> "day" ++ dayNum ++ " (example).csv"
        path = "test/2024/day" ++ dayNum ++ "/standard/" ++ filename
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure defaultInput
  where
    defaultInput = unlines
        [ "47|53", "97|13", "97|61", "97|47", "75|29"
        , "61|13", "75|53", "29|13", "97|29", "53|29"
        , ""
        , "75,47,61,53,29"
        , "97,61,53,29,13"
        , "75,29,13"
        ]

parseInput :: String -> ([Rule], [Update])
parseInput contents = (rules, updates)
  where
    lns = lines contents
    (ruleLns, updateLns) = span (/= "") lns
    rules = [(read a, read b) | line <- ruleLns, let [a,b] = words $ map (\c -> if c == '|' then ' ' else c) line]
    updates = [map read $ words $ map (\c -> if c == ',' then ' ' else c) line | line <- drop 1 updateLns, not (null line)]

buildFrames :: [Rule] -> Update -> [(Update, Bool)]
buildFrames rules update = [(update, isValid rules update)]

isValid :: [Rule] -> Update -> Bool
isValid rules update = all checkRule rules
  where
    positions = M.fromList (zip update [0..])
    checkRule (before, after) =
        case (M.lookup before positions, M.lookup after positions) of
            (Just posBefore, Just posAfter) -> posBefore < posAfter
            _ -> True

renderFrame :: (Update, Bool) -> IO ()
renderFrame (update, valid) = do
    setCursorPosition 0 0
    putStrLn "Print Queue - Checking Update"
    putStrLn "Part context: [Part 1] validate ordering; [Part 2] fix incorrect orders."
    putStrLn ""
    putStrLn $ "Update: " ++ show update
    putStrLn $ "Valid: " ++ if valid then "YES" else "NO"
    putStrLn ""

    -- Visualize pages as points in a line
    let width = length update
        points = [(i, 0) | i <- [0..width-1]]
        asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList [("Pages", points)]
            , asciiWorldWidth = max 10 width
            }
        bgChar = '.'
        maskToChar = id
        pointsToChar = const 'P'
        nameZOrder = compare
        worldStr = showAsciiWorld 2 bgChar maskToChar pointsToChar nameZOrder asciiWorld

    putStr worldStr

    threadDelay 500000
