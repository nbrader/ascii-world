#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2024 Day 01 ("Historian Hysteria").
-- Shows pairing and sorting two lists.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.List (sort)
import qualified Data.Map as M
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let (left, right) = parseLists contents
        sortedLeft = sort left
        sortedRight = sort right
        pairs = zip sortedLeft sortedRight
        frames = zip [1..] (scanl (+) 0 [abs (a - b) | (a, b) <- pairs])
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ (renderFrame pairs) frames
    setCursorPosition 25 0
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

renderFrame :: [(Int, Int)] -> (Int, Int) -> IO ()
renderFrame pairs (idx, distance) = do
    setCursorPosition 0 0
    putStrLn "Historian Hysteria - Pairing Lists"
    putStrLn "Part context: [Part 1] sum distances; [Part 2] similarity score."
    putStrLn ""
    putStrLn $ "Pairs matched: " ++ show idx ++ " / " ++ show (length pairs)
    putStrLn $ "Total distance: " ++ show distance
    putStrLn ""

    -- Visualize pairs
    let width = min 40 (length pairs)
        leftPoints = [(i, 0) | i <- [0..min (idx-1) (width-1)]]
        rightPoints = [(i, 2) | i <- [0..min (idx-1) (width-1)]]
        asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList [("Left", leftPoints), ("Right", rightPoints)]
            , asciiWorldWidth = width
            }
        bgChar = '.'
        maskToChar = id
        pointsToChar name = case name of
            "Left" -> 'L'
            "Right" -> 'R'
            _ -> '?'
        nameZOrder = compare
        worldStr = showAsciiWorld 3 bgChar maskToChar pointsToChar nameZOrder asciiWorld

    putStr worldStr
    threadDelay 150000
