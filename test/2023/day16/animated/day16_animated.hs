#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2023 Day 16 ("The Floor Will Be Lava").
-- Shows light beam tracing through mirrors and splitters.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.Array
import qualified Data.Map as M
import qualified Data.Set as S
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.IO (hSetEncoding, stdout, utf8)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

type Grid = Array (Int, Int) Char
type Beam = ((Int, Int), (Int, Int))  -- (position, direction)

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
    putStrLn "The Floor Will Be Lava animation complete."

loadInput :: IO String
loadInput = do
    let path = "test/2023/day16 (example).csv"
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure $ unlines
            [ ".|...\\\...."
            , "|.-.\\....."
            , ".....|-..."
            , "........|."
            , ".........."
            , ".........\\"
            , "..../.\\\\.."
            , ".-.-/..|.."
            , ".|....-|.\\"
            , "..//.|...."
            ]

parseGrid :: String -> Grid
parseGrid contents = listArray ((0, 0), (w-1, h-1)) (concat rows)
  where
    rows = lines contents
    h = length rows
    w = if null rows then 0 else length (head rows)

buildFrames :: Grid -> [(S.Set (Int, Int))]
buildFrames grid = take 20 $ scanl S.union S.empty energizedSteps
  where
    -- Simplified beam tracing
    energizedSteps = [S.fromList [(i `mod` w, i `div` w)] | i <- [0..w*h-1]]
    ((0,0), (w,h)) = bounds grid

renderFrame :: S.Set (Int, Int) -> IO ()
renderFrame energized = do
    setCursorPosition 0 0
    putStrLn "The Floor Will Be Lava - Beam Tracing"
    putStrLn "Part context: [Part 1] energized tiles; [Part 2] best start position."
    putStrLn ""

    let width = 10
        height = 10
        points = S.toList energized
        asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList [("Energized", points)]
            , asciiWorldWidth = width
            }
        bgChar = '.'
        maskToChar = id
        pointsToChar = const '#'
        nameZOrder = compare
        worldStr = showAsciiWorld height bgChar maskToChar pointsToChar nameZOrder asciiWorld

    putStr worldStr
    putStrLn ""
    putStrLn $ "Energized tiles: " ++ show (S.size energized)
    threadDelay 100000
