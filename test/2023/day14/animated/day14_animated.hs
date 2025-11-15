#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2023 Day 14 ("Parabolic Reflector Dish").
-- Shows rocks rolling when platform tilts.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.Array
import Data.List (sort)
import qualified Data.Map as M
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.IO (hSetEncoding, stdout, utf8)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

type Grid = Array (Int, Int) Char

main :: IO ()
main = do
    hSetEncoding stdout utf8
    contents <- loadInput
    let grid = parseGrid contents
        frames = buildFrames grid
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    setCursorPosition 25 0
    putStrLn "Parabolic Reflector Dish animation complete."

loadInput :: IO String
loadInput = do
    let path = "test/2023/day14 (example).csv"
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

parseGrid :: String -> Grid
parseGrid contents = listArray ((0, 0), (w-1, h-1)) (concat rows)
  where
    rows = lines contents
    h = length rows
    w = if null rows then 0 else length (head rows)

buildFrames :: Grid -> [Grid]
buildFrames grid = take 5 $ iterate tiltNorth grid

tiltNorth :: Grid -> Grid
tiltNorth grid = grid // updates
  where
    ((0,0), (w,h)) = bounds grid
    rocks = [(x,y) | x <- [0..w], y <- [0..h], grid ! (x,y) == 'O']
    -- Simplified: just move rocks slightly north
    updates = [((x,y), '.') | (x,y) <- rocks] ++
              [((x, max 0 (y-1)), 'O') | (x,y) <- rocks, y > 0, grid ! (x, y-1) == '.']

renderFrame :: Grid -> IO ()
renderFrame grid = do
    let ((0,0), (w,h)) = bounds grid
    setCursorPosition 0 0
    putStrLn "Parabolic Reflector Dish - Tilting North"
    putStrLn "Part context: [Part 1] tilt north; [Part 2] cycle detection."
    putStrLn ""

    let rocks = [(x,y) | x <- [0..w], y <- [0..h], grid ! (x,y) == 'O']
        cubes = [(x,y) | x <- [0..w], y <- [0..h], grid ! (x,y) == '#']
        asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList [("Rocks", rocks), ("Cubes", cubes)]
            , asciiWorldWidth = w+1
            }
        bgChar = '.'
        maskToChar = id
        pointsToChar name = case name of
            "Rocks" -> 'O'
            "Cubes" -> '#'
            _ -> '?'
        nameZOrder = compare
        worldStr = showAsciiWorld (h+1) bgChar maskToChar pointsToChar nameZOrder asciiWorld

    putStr worldStr
    putStrLn ""
    putStrLn $ "Rounded rocks: " ++ show (length rocks)
    threadDelay 300000
