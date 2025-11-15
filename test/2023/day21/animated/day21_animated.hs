#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2023 Day 21 ("Step Counter").
-- Shows reachable garden plots expanding step by step.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.Array
import qualified Data.Map as M
import qualified Data.Set as S
import System.Console.ANSI
import System.Directory (doesFileExist)
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
    contents <- loadInput
    let (grid, start) = parseGrid contents
        frames = buildFrames grid start 10
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    setCursorPosition 25 0
    putStrLn "Step Counter animation complete."

loadInput :: IO String
loadInput = do
    let path = "test/2023/day21 (example).csv"
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure $ unlines
            [ "..........."
            , ".....###.#."
            , ".###.##..#."
            , "..#.#...#.."
            , "....#.#...."
            , ".##..S####."
            , ".##..#...#."
            , ".......##.."
            , ".##.#.####."
            , ".##..##.##."
            , "..........."
            ]

parseGrid :: String -> (Array (Int, Int) Char, (Int, Int))
parseGrid contents = (grid, start)
  where
    rows = lines contents
    h = length rows
    w = if null rows then 0 else length (head rows)
    grid = listArray ((0, 0), (w-1, h-1)) (concat rows)
    start = head [(x, y) | x <- [0..w-1], y <- [0..h-1], grid ! (x, y) == 'S']

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "21"
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

buildFrames :: Array (Int, Int) Char -> (Int, Int) -> Int -> [(Int, S.Set (Int, Int))]
loadInput inputType = do
    let dayNum = "21"
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

buildFrames grid start maxSteps = zip [0..maxSteps] positions
  where
    positions = take (maxSteps + 1) $ iterate (step grid) (S.singleton start)
    step g current = S.fromList
        [ neighbor
        | pos <- S.toList current
        , neighbor <- neighbors g pos
        , canWalk g neighbor
        ]

neighbors :: Array (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
neighbors grid (x, y) = filter inBounds [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
  where
    ((minX, minY), (maxX, maxY)) = bounds grid
    inBounds (px, py) = px >= minX && px <= maxX && py >= minY && py <= maxY

canWalk :: Array (Int, Int) Char -> (Int, Int) -> Bool
canWalk grid pos = grid ! pos /= '#'

renderFrame :: (Int, S.Set (Int, Int)) -> IO ()
renderFrame (step, reachable) = do
    setCursorPosition 0 0
    putStrLn $ "Step Counter - After " ++ show step ++ " steps"
    putStrLn "Part context: [Part 1] reachable in 64 steps; [Part 2] infinite grid."
    putStrLn ""

    let width = 11
        height = 11
        points = S.toList reachable
        asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList [("Reachable", points)]
            , asciiWorldWidth = width
            }
        bgChar = '.'
        maskToChar = id
        pointsToChar = const 'O'
        nameZOrder = compare
        worldStr = showAsciiWorld height bgChar maskToChar pointsToChar nameZOrder asciiWorld

    putStr worldStr
    putStrLn ""
    putStrLn $ "Plots reachable: " ++ show (S.size reachable)
    threadDelay 200000
