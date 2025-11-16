#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2024 Day 01 ("Historian Hysteria").
-- Shows sorting and pairing two lists with distance calculation.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.List (sort, transpose)
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, hSetBuffering, stdout, utf8, BufferMode(NoBuffering))

data Frame = Frame
    { frameLeftList :: [Int]
    , frameRightList :: [Int]
    , framePairIdx :: Int
    , frameTotalDist :: Int
    , frameCurrentDist :: Maybe Int
    }

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let (left, right) = parseLists contents
        sortedLeft = sort left
        sortedRight = sort right
        frames = buildFrames sortedLeft sortedRight
        total = length frames
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ (renderFrame total) (zip [0..] frames)
    setCursorPosition 30 0
    putStrLn "Historian Hysteria animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "01"
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
            [ "3   4"
            , "4   3"
            , "2   5"
            , "1   3"
            , "3   9"
            , "3   3"
            ]

parseLists :: String -> ([Int], [Int])
parseLists contents = unzip pairs
  where
    pairs = [(read a, read b) | line <- lines contents, let [a, b] = words line]

buildFrames :: [Int] -> [Int] -> [Frame]
buildFrames left right = go 0 0
  where
    pairs = zip left right
    go idx totalDist
        | idx >= length pairs = []
        | otherwise =
            let (a, b) = pairs !! idx
                dist = abs (a - b)
                newTotal = totalDist + dist
            in Frame left right idx totalDist Nothing :
               Frame left right idx newTotal (Just dist) :
               go (idx + 1) newTotal

renderFrame :: Int -> (Int, Frame) -> IO ()
renderFrame total (idx, Frame left right pairIdx totalDist currentDist) = do
    let displayCount = min 10 (length left)
        pairs = zip left right

        frameContent = unlines (
            [ "Historian Hysteria - Pairing Sorted Lists"
            , "Frame " ++ show (idx + 1) ++ " / " ++ show total
            , "Part context: [Part 1] sum distances; [Part 2] similarity score."
            , ""
            , "Pairs processed: " ++ show pairIdx ++ " / " ++ show (length pairs)
            , "Total distance: " ++ show totalDist
            , case currentDist of
                Just d -> "Current pair distance: " ++ show d ++ " ***"
                Nothing -> ""
            , ""
            , "Current pairs (sorted):"
            , "  Left  | Right | Distance"
            , "  ------|-------|--------"
            ] ++ [formatPair i (pairs !! i) (i == pairIdx) | i <- [0..min (pairIdx) (displayCount - 1)]] ++
            (if pairIdx >= displayCount then ["  ... (" ++ show (pairIdx - displayCount + 1) ++ " more pairs)"] else []) ++
            [ ""
            , "Lists are sorted before pairing"
            , "Distance = |left - right| for each pair"
            ])

    setCursorPosition 0 0
    putStr frameContent
    threadDelay (if currentDist /= Nothing then 200000 else 100000)
  where
    formatPair i (a, b) isCurrent =
        let marker = if isCurrent then ">" else " "
            dist = abs (a - b)
        in marker ++ " " ++ padLeft 5 (show a) ++ " | " ++ padLeft 5 (show b) ++ " | " ++ padLeft 6 (show dist)

    padLeft n s = replicate (n - length s) ' ' ++ s
