#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
   --package split-0.2.3.5
-}

-- |
-- Animated visualization for AoC 2022 Day 14 (Regolith Reservoir).
-- Shows sand falling and coming to rest in a cave system.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.List (foldl')
import Data.List.Split (splitOn)
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

type WorldPoint = (Int, Int)
type WorldPoints = Set WorldPoint
type Path = [WorldPoint]

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let paths = map (map ((\[x,y] -> (read x, read y) :: WorldPoint) . splitOn ","))
                . map (splitOn " -> ") . lines $ contents
        rocks = S.unions $ map pointsFromPath paths
        maxDepth = if S.null rocks then 10 else maximum . map snd . S.toList $ rocks
        source = (500, 0)
        frames = buildFrames rocks maxDepth source inputType
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    setCursorPosition 25 0
    putStrLn "Regolith Reservoir animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "14"
        filename = case inputType of
            "data" -> "day" ++ dayNum ++ " (data).csv"
            "example2" -> "day" ++ dayNum ++ " (example 2).csv"
            "example3" -> "day" ++ dayNum ++ " (example 3).csv"
            _ -> "day" ++ dayNum ++ " (example).csv"
        path = "test/2022/day" ++ dayNum ++ "/standard/" ++ filename
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure $ unlines
            [ "498,4 -> 498,6 -> 496,6"
            , "503,4 -> 502,4 -> 502,9 -> 494,9"
            ]

data Frame = Frame
    { frameRocks :: WorldPoints
    , frameSand :: WorldPoints
    , frameFalling :: Maybe WorldPoint
    , frameCount :: Int
    , frameMaxDepth :: Int
    , frameFellOff :: Bool
    }

buildFrames :: WorldPoints -> Int -> WorldPoint -> String -> [Frame]
buildFrames rocks maxDepth source inputType =
    let initialFrame = Frame rocks S.empty (Just source) 0 maxDepth False
        frames = simulateAllSand initialFrame []
    in frames
  where
    simulateAllSand :: Frame -> [Frame] -> [Frame]
    simulateAllSand frame acc
        | frameFellOff frame = reverse (frame : acc)
        | frameCount frame > 500 = reverse (frame : acc)  -- Safety limit
        | otherwise =
            let sandFrames = simulateSandUnit frame
                lastFrame = last sandFrames
            in simulateAllSand lastFrame (reverse sandFrames ++ acc)

    simulateSandUnit :: Frame -> [Frame]
    simulateSandUnit startFrame = go startFrame []
      where
        go frame acc =
            case frameFalling frame of
                Nothing -> reverse (frame : acc)
                Just pos ->
                    let allOccupied = frameRocks frame `S.union` frameSand frame
                        below = addV2 pos (0, 1)
                        leftDiag = addV2 pos (-1, 1)
                        rightDiag = addV2 pos (1, 1)
                        nextPos
                            | snd pos > frameMaxDepth frame = pos  -- Fell off
                            | not (below `S.member` allOccupied) = below
                            | not (leftDiag `S.member` allOccupied) = leftDiag
                            | not (rightDiag `S.member` allOccupied) = rightDiag
                            | otherwise = pos  -- Come to rest
                        fellOff = snd pos > frameMaxDepth frame
                        atRest = nextPos == pos && not fellOff
                        newFrame
                            | fellOff = frame { frameFalling = Nothing, frameFellOff = True }
                            | atRest = frame
                                { frameSand = S.insert pos (frameSand frame)
                                , frameFalling = Just source
                                , frameCount = frameCount frame + 1
                                }
                            | otherwise = frame { frameFalling = Just nextPos }
                    in go newFrame (frame : acc)

renderFrame :: Frame -> IO ()
renderFrame frame = do
    -- Calculate bounds
    let allPoints = frameRocks frame `S.union` frameSand frame
        allPointsList = S.toList allPoints ++ maybe [] (:[]) (frameFalling frame) ++ [(500, 0)]
        minX = if null allPointsList then 490 else minimum (map fst allPointsList) - 2
        maxX = if null allPointsList then 510 else maximum (map fst allPointsList) + 2
        minY = 0
        maxY = frameMaxDepth frame + 1
        width = maxX - minX + 1
        height = maxY - minY + 1

    -- Build character grid
    let charGrid = M.fromList
            [ ((x - minX, y - minY), getChar (x, y))
            | y <- [minY..maxY]
            , x <- [minX..maxX]
            ]
        getChar pos
            | pos == (500, 0) = '+'
            | Just pos == frameFalling frame = '~'
            | pos `S.member` frameRocks frame = '#'
            | pos `S.member` frameSand frame = 'o'
            | otherwise = '.'

    -- Build grid lines
    let gridLines = [ [ M.findWithDefault '.' (x, y) charGrid
                      | x <- [0..width-1] ]
                    | y <- [0..height-1] ]

    -- Build entire frame as single string and output atomically
    let statusLine = if frameFellOff frame
            then "Status: Sand falling into the abyss!"
            else "Status: " ++ maybe "Waiting..." (const "Sand falling...") (frameFalling frame)
        frameContent = unlines
            [ "Regolith Reservoir - Falling Sand"
            , "[Part 1] Sand falls into the void below"
            , ""
            , "Sand units at rest: " ++ show (frameCount frame)
            , statusLine
            , ""
            ] ++ unlines gridLines

    setCursorPosition 0 0
    putStr frameContent
    threadDelay 50000  -- 50ms delay

addV2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
addV2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

pointsFromEnds :: WorldPoint -> WorldPoint -> [WorldPoint]
pointsFromEnds (x1, y1) (x2, y2)
    | dx == 0 = let step = signum dy in [(x1, y1 + i * step) | i <- [0..abs dy]]
    | otherwise = let step = signum dx in [(x1 + i * step, y1) | i <- [0..abs dx]]
  where
    dx = x2 - x1
    dy = y2 - y1

pointsFromPath :: [WorldPoint] -> WorldPoints
pointsFromPath [] = S.empty
pointsFromPath (x:xs) = snd $ foldl' processVert (x, S.singleton x) xs
  where
    processVert :: (WorldPoint, WorldPoints) -> WorldPoint -> (WorldPoint, WorldPoints)
    processVert (v1, set) v2 = (v2, S.fromList (pointsFromEnds v1 v2) `S.union` set)
