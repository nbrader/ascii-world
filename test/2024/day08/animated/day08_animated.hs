#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2024 Day 08 ("Resonant Collinearity").
-- Shows antennas and their antinodes.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.Char (isAlphaNum)
import Data.List (groupBy, sort)
import qualified Data.Map as M
import qualified Data.Set as S
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

type Grid = [[Char]]

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let grid = parseGrid contents
        frames = buildFrames grid
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    setCursorPosition 25 0
    putStrLn "Resonant Collinearity animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "08"
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
        [ "............"
        , "........0..."
        , ".....0......"
        , ".......0...."
        , "....0......."
        , "......A....."
        , "............"
        , "............"
        , "........A..."
        , ".........A.."
        , "............"
        , "............"
        ]

parseGrid :: String -> Grid
parseGrid = lines

buildFrames :: Grid -> [(Char, [Point], [Point])]
buildFrames grid = frames
  where
    height = length grid
    width = if null grid then 0 else length (head grid)
    antennas = [(c, (x, y)) | (y, row) <- zip [0..] grid, (x, c) <- zip [0..] row, isAlphaNum c]
    freqs = S.toList $ S.fromList [c | (c, _) <- antennas]
    frames = concatMap (buildFreqFrames width height antennas) freqs

buildFreqFrames :: Int -> Int -> [(Char, Point)] -> Char -> [(Char, [Point], [Point])]
buildFreqFrames width height allAntennas freq =
    [(freq, antPoints, take i antinodes) | i <- [1..length antinodes]]
  where
    antPoints = [p | (c, p) <- allAntennas, c == freq]
    antinodes = calculateAntinodes width height antPoints

calculateAntinodes :: Int -> Int -> [Point] -> [Point]
calculateAntinodes width height points = S.toList $ S.fromList
    [ antinode
    | (x1, y1) <- points
    , (x2, y2) <- points
    , (x1, y1) /= (x2, y2)
    , let dx = x2 - x1
          dy = y2 - y1
          antinode = (x2 + dx, y2 + dy)
    , fst antinode >= 0 && fst antinode < width
    , snd antinode >= 0 && snd antinode < height
    ]

renderFrame :: (Char, [Point], [Point]) -> IO ()
renderFrame (freq, antennas, antinodes) = do
    let width = 12
        height = 12

    setCursorPosition 0 0
    putStrLn $ "Resonant Collinearity - Frequency: " ++ [freq]
    putStrLn "Part context: [Part 1] find antinodes at 2:1 ratio; [Part 2] resonant harmonics."
    putStrLn ""

    -- Build AsciiWorld
    let asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList
                [ ("Antennas-" ++ [freq], antennas)
                , ("Antinodes", antinodes)
                ]
            , asciiWorldWidth = width
            }
        bgChar = '.'
        maskToChar = id
        pointsToChar name
            | "Antennas-" `isPrefixOf` name = head (drop 9 name)
            | name == "Antinodes" = '#'
            | otherwise = '?'
        nameZOrder a b = case (a, b) of
            (PointsIndex n1, PointsIndex n2) | "Antennas-" `isPrefixOf` n1 -> GT
            _ -> compare a b
        worldStr = showAsciiWorld height bgChar maskToChar pointsToChar nameZOrder asciiWorld

    putStr worldStr
    putStrLn ""

    putStrLn $ "Antennas: " ++ show (length antennas)
    putStrLn $ "Antinodes found: " ++ show (length antinodes)

    threadDelay 150000
  where
    isPrefixOf = isPrefix
    isPrefix [] _ = True
    isPrefix _ [] = False
    isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys
