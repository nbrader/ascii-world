#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
   --package array-0.5.4.0
-}

-- |
-- Animated visualization for AoC 2024 Day 04 ("Ceres Search").
-- Shows XMAS word search highlighting found words in the actual grid.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.Array
import qualified Data.Set as S
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, hSetBuffering, stdout, utf8, BufferMode(NoBuffering))

type Point = (Int, Int)
type Grid = Array Point Char

data Frame = Frame
    { framePos :: Point
    , frameFoundWords :: [[(Point, Char)]]
    , frameCurrentMatch :: [(Point, Char)]
    , frameMatchFound :: Bool
    }

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let grid = parseGrid contents
        frames = generateFrames grid
        total = length frames
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ (renderFrame grid total) (zip [0..] frames)
    let ((0,0), (w,h)) = bounds grid
    setCursorPosition (h + 10) 0
    putStrLn "Ceres Search animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "04"
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

parseGrid :: String -> Grid
parseGrid contents = listArray ((0, 0), (w-1, h-1)) (concat rows)
  where
    rows = lines contents
    h = length rows
    w = if null rows then 0 else length (head rows)

-- Generate all frames showing the search process
generateFrames :: Grid -> [Frame]
generateFrames grid = go [] allPositions
  where
    ((0,0), (w,h)) = bounds grid
    allPositions = [(x, y) | y <- [0..h], x <- [0..w]]

    go foundSoFar [] = []
    go foundSoFar (pos:rest) =
        let allDirs = [(dx, dy) | dx <- [-1,0,1], dy <- [-1,0,1], (dx,dy) /= (0,0)]
            results = map (checkDirection pos) allDirs
            validMatches = [match | (match, True) <- results, not (null match)]
        in if null validMatches
           then Frame pos foundSoFar [] False : go foundSoFar rest
           else concatMap (\match ->
                   [ Frame pos foundSoFar match False         -- Show potential match
                   , Frame pos (foundSoFar ++ [match]) [] True -- Show confirmed match
                   ]) validMatches ++ go (foundSoFar ++ validMatches) rest

    checkDirection (x, y) (dx, dy) =
        let positions = [(x+i*dx, y+i*dy) | i <- [0..3]]
            inBounds (px, py) = px >= 0 && px <= w && py >= 0 && py <= h
            chars = if all inBounds positions
                    then map (grid !) positions
                    else []
            found = chars == "XMAS"
        in (zip positions chars, found)

renderFrame :: Grid -> Int -> (Int, Frame) -> IO ()
renderFrame grid total (idx, Frame pos foundWords currentMatch matchFound) = do
    let ((0,0), (w,h)) = bounds grid
        allFound = S.fromList $ concat [[p | (p, _) <- word] | word <- foundWords]
        current = S.fromList [p | (p, _) <- currentMatch]
        totalWords = length foundWords

        frameContent = unlines (
            [ "Ceres Search - Finding XMAS - frame " ++ show (idx + 1) ++ " / " ++ show total
            , "Part context: [Part 1] find all XMAS; [Part 2] find X-MAS patterns."
            , ""
            , "Current position: " ++ show pos
            , "Found words: " ++ show totalWords
            , if matchFound then "*** MATCH FOUND! ***" else ""
            , ""
            ] ++ gridRows grid w h pos allFound current ++
            [ ""
            , "Legend: @ = scanning, # = found word, * = current match"
            ])

    setCursorPosition 0 0
    putStr frameContent
    threadDelay (if matchFound then 300000 else 50000)

gridRows :: Grid -> Int -> Int -> Point -> S.Set Point -> S.Set Point -> [String]
gridRows grid w h scanPos found current =
    [ [ cellChar (x, y)
      | x <- [0..w]
      ]
    | y <- [0..h]
    ]
  where
    cellChar point
        | point `S.member` current = '*'  -- Current potential match
        | point `S.member` found = '#'    -- Previously found
        | point == scanPos = '@'           -- Current scan position
        | otherwise = grid ! point
