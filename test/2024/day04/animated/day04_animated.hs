#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2024 Day 04 ("Ceres Search").
-- Shows XMAS word search highlighting found words.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.Array
import qualified Data.Map as M
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.IO (hSetEncoding, stdout, utf8)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    contents <- loadInput
    let grid = parseGrid contents
        matches = findXMAS grid
        frames = take 20 $ zip [1..] (scanl (++) [] (map return matches))
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ (renderFrame grid) frames
    setCursorPosition 25 0
    putStrLn "Ceres Search animation complete."

loadInput :: IO String
loadInput = do
    let path = "test/2024/day04 (example).csv"
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure $ unlines
            [ "MMMSXXMASM"
            , "MSAMXMSMSA"
            , "AMXSXMAAMM"
            , "MSAMASMSMX"
            , "XMASAMXAMM"
            , "XXAMMXXAMA"
            , "SMSMSASXSS"
            , "SAXAMASAAA"
            , "MAMMMXMMMM"
            , "MXMXAXMASX"
            ]

parseGrid :: String -> Array (Int, Int) Char
parseGrid contents = listArray ((0, 0), (w-1, h-1)) (concat rows)
  where
    rows = lines contents
    h = length rows
    w = if null rows then 0 else length (head rows)

findXMAS :: Array (Int, Int) Char -> [[(Int, Int)]]
findXMAS grid = concat
    [ searchFrom (x, y)
    | x <- [0..w-1]
    , y <- [0..h-1]
    ]
  where
    ((0,0), (w,h)) = bounds grid
    searchFrom pos = filter (/= []) [checkDir pos dx dy | dx <- [-1,0,1], dy <- [-1,0,1], (dx,dy) /= (0,0)]
    checkDir (x,y) dx dy =
        let positions = [(x+i*dx, y+i*dy) | i <- [0..3]]
            inBounds (px,py) = px >= 0 && px <= w && py >= 0 && py <= h
        in if all inBounds positions && map (grid !) positions == "XMAS"
           then positions
           else []

renderFrame :: Array (Int, Int) Char -> (Int, [[(Int, Int)]]) -> IO ()
renderFrame grid (count, found) = do
    let ((0,0), (w,h)) = bounds grid
    setCursorPosition 0 0
    putStrLn "Ceres Search - Finding XMAS"
    putStrLn "Part context: [Part 1] find all XMAS; [Part 2] find X-MAS patterns."
    putStrLn ""
    putStrLn $ "Found: " ++ show count
    putStrLn ""

    -- Show found positions
    let foundPoints = concat found
        asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList [("Found", foundPoints)]
            , asciiWorldWidth = w+1
            }
        bgChar = '.'
        maskToChar = id
        pointsToChar = const '#'
        nameZOrder = compare
        worldStr = showAsciiWorld (h+1) bgChar maskToChar pointsToChar nameZOrder asciiWorld

    putStr worldStr
    threadDelay 100000
