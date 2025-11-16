#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
   --package split-0.2.3.5
-}

-- |
-- Animated visualization for AoC 2023 Day 14 ("Parabolic Reflector Dish").
-- Shows rocks rolling when platform tilts.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.List (intercalate, partition, transpose, reverse)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, hSetBuffering, stdout, utf8, BufferMode(NoBuffering))

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let grid = lines contents
        frames = buildFrames grid
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    setCursorPosition 25 0
    putStrLn "Parabolic Reflector Dish animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "14"
        filename = case inputType of
            "data" -> "day" ++ dayNum ++ " (data).csv"
            "example2" -> "day" ++ dayNum ++ " (example 2).csv"
            "example3" -> "day" ++ dayNum ++ " (example 3).csv"
            _ -> "day" ++ dayNum ++ " (example).csv"
        path = "test/2023/day" ++ dayNum ++ "/standard/" ++ filename
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure $ unlines
            [ "O....#...."
            , "O.OO#....#"
            , ".....##..."
            , "OO.#O....O"
            , ".O.....O#."
            , "O.#..O.#.#"
            , "..O..#O..O"
            , ".......O.."
            , "#....###.."
            , "#OO..#...."
            ]

data Frame = Frame
    { frameGrid :: [String]
    , frameDirection :: String
    , frameStep :: Int
    }

buildFrames :: [String] -> [Frame]
buildFrames grid =
    let initialFrame = Frame grid "Start" 0
        spinCycle g = tiltEast . tiltSouth . tiltWest . tiltNorth $ g
        frames = [Frame grid "Initial" 0]
              ++ tiltFrames grid "North" 1
              ++ tiltFrames (tiltNorth grid) "West" 2
              ++ tiltFrames (tiltWest . tiltNorth $ grid) "South" 3
              ++ tiltFrames (tiltSouth . tiltWest . tiltNorth $ grid) "East" 4
              ++ [Frame (spinCycle grid) "Complete Cycle" 5]
    in take 30 frames
  where
    tiltFrames g dir step = [Frame g dir step]

-- Roll rocks within each segment (between '#' cubes)
roll :: [String] -> [String]
roll = map rollRow
  where
    rollRow = intercalate "#" . map sortSegment . splitOn "#"
    sortSegment seg = rocks ++ spaces
      where (rocks, spaces) = partition (== 'O') seg

-- Tilt in different directions
tiltNorth :: [String] -> [String]
tiltNorth = transpose . roll . transpose

tiltSouth :: [String] -> [String]
tiltSouth = transpose . map Data.List.reverse . roll . map Data.List.reverse . transpose

tiltWest :: [String] -> [String]
tiltWest = roll

tiltEast :: [String] -> [String]
tiltEast = map Data.List.reverse . roll . map Data.List.reverse

rotateCW90 :: [String] -> [String]
rotateCW90 = transpose . Data.List.reverse

renderFrame :: Frame -> IO ()
renderFrame frame = do
    -- Calculate load (Part 1 metric)
    let grid = frameGrid frame
        height = length grid
        load = sum [ if c == 'O' then height - y else 0
                   | (y, row) <- zip [0..] grid
                   , c <- row
                   ]

    -- Build entire frame as single string and output atomically
    let frameContent = unlines
            [ "Parabolic Reflector Dish - Rolling Rocks"
            , "[Part 2] Spin cycle: North -> West -> South -> East"
            , ""
            , "Direction: " ++ frameDirection frame
            , "Step: " ++ show (frameStep frame)
            , ""
            , "Load on north support: " ++ show load
            , ""
            ] ++ unlines (frameGrid frame)

    setCursorPosition 0 0
    putStr frameContent
    threadDelay 200000  -- 200ms delay
