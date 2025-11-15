#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2023 Day 20 ("Pulse Propagation").
-- Shows pulses propagating through modules.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import qualified Data.Map as M
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.IO (hSetEncoding, stdout, utf8)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

data Pulse = Low | High deriving (Show, Eq)
type PulseEvent = (String, Pulse, String)  -- (from, pulse, to)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    let inputType = if null args then "example" else head args
    let frames = buildFrames
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    setCursorPosition 25 0
    putStrLn "Pulse Propagation animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "20"
        filename = case inputType of
            "data" -> "day" ++ dayNum ++ " (data).csv"
            "example2" -> "day" ++ dayNum ++ " (example 2).csv"
            "example3" -> "day" ++ dayNum ++ " (example 3).csv"
            _ -> "day" ++ dayNum ++ " (example).csv"
        path = "test/2023/day" ++ dayNum ++ "/standard/" ++ filename
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure "No data available"

buildFrames :: [(Int, [PulseEvent], Int, Int)]
loadInput inputType = do
    let dayNum = "20"
        filename = case inputType of
            "data" -> "day" ++ dayNum ++ " (data).csv"
            "example2" -> "day" ++ dayNum ++ " (example 2).csv"
            "example3" -> "day" ++ dayNum ++ " (example 3).csv"
            _ -> "day" ++ dayNum ++ " (example).csv"
        path = "test/2023/day" ++ dayNum ++ "/standard/" ++ filename
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure "No data available"

buildFrames = zip4 [1..10] pulseSequences lowCounts highCounts
  where
    -- Simplified example pulse sequence
    pulseSequences =
        [ [("button", Low, "broadcaster")]
        , [("broadcaster", Low, "a"), ("broadcaster", Low, "b")]
        , [("a", High, "b"), ("b", High, "c")]
        , [("c", High, "inv")]
        , [("inv", Low, "a")]
        , []
        , [("button", Low, "broadcaster")]
        , [("broadcaster", Low, "a"), ("broadcaster", Low, "b")]
        , [("a", High, "b")]
        , []
        ]
    lowCounts = scanl (+) 0 [length [() | (_, Low, _) <- ps] | ps <- pulseSequences]
    highCounts = scanl (+) 0 [length [() | (_, High, _) <- ps] | ps <- pulseSequences]

renderFrame :: (Int, [PulseEvent], Int, Int) -> IO ()
renderFrame (pressNum, pulses, lowTotal, highTotal) = do
    setCursorPosition 0 0
    putStrLn $ "Pulse Propagation - Button Press #" ++ show pressNum
    putStrLn "Part context: [Part 1] count pulses; [Part 2] find cycle."
    putStrLn ""

    putStrLn "Active pulses:"
    mapM_ (\(from, pulse, to) -> putStrLn $ "  " ++ from ++ " -" ++ show pulse ++ "-> " ++ to) pulses
    putStrLn ""

    putStrLn $ "Total low pulses: " ++ show lowTotal
    putStrLn $ "Total high pulses: " ++ show highTotal
    putStrLn $ "Product: " ++ show (lowTotal * highTotal)
    putStrLn ""

    -- Simple visualization grid
    let width = 40
        height = 5
        points = [(i, 0) | i <- [0..min (pressNum-1) 39]]
        asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList [("Progress", points)]
            , asciiWorldWidth = width
            }
        bgChar = '.'
        maskToChar = id
        pointsToChar = const '*'
        nameZOrder = compare
        worldStr = showAsciiWorld height bgChar maskToChar pointsToChar nameZOrder asciiWorld

    putStr worldStr
    threadDelay 300000
