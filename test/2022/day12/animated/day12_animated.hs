#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2022 Day 12 (Hill Climbing Algorithm).
-- Shows pathfinding on a heightmap using BFS.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.Char (ord)
import Data.List (foldl')
import Data.Maybe (listToMaybe, fromMaybe)
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, hSetBuffering, stdout, utf8, BufferMode(NoBuffering))

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

type Pos = (Int, Int)
type HeightMap = M.Map Pos Int

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let (heightMap, start, end) = parseMap contents
        frames = buildFrames heightMap start end
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    setCursorPosition 25 0
    putStrLn "Hill Climbing animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "12"
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
            [ "Sabqponm"
            , "abcryxxl"
            , "accszExk"
            , "acctuvwj"
            , "abdefghi"
            ]

parseMap :: String -> (HeightMap, Pos, Pos)
parseMap contents =
    let gridLines = lines contents
        positions = [ ((y, x), c)
                    | (y, line) <- zip [0..] gridLines
                    , (x, c) <- zip [0..] line
                    ]
        heightMap = M.fromList [ (pos, charToHeight c) | (pos, c) <- positions ]
        start = fst . head $ filter ((== 'S') . snd) positions
        end = fst . head $ filter ((== 'E') . snd) positions
    in (heightMap, start, end)

charToHeight :: Char -> Int
charToHeight 'S' = 1  -- 'a'
charToHeight 'E' = 26  -- 'z'
charToHeight c = ord c - ord 'a' + 1

data Frame = Frame
    { frameHeightMap :: HeightMap
    , frameVisited :: Set Pos
    , frameFrontier :: Set Pos
    , framePath :: [Pos]
    , frameStart :: Pos
    , frameEnd :: Pos
    , frameFound :: Bool
    , frameStep :: Int
    }

buildFrames :: HeightMap -> Pos -> Pos -> [Frame]
buildFrames heightMap start end =
    let initialFrame = Frame heightMap S.empty (S.singleton start) [] start end False 0
    in bfsSearch initialFrame M.empty
  where
    bfsSearch frame cameFrom
        | frameFound frame = reconstructPath frame cameFrom
        | S.null (frameFrontier frame) = [frame]  -- No path found
        | otherwise =
            let newFrontier = S.fromList $ do
                    current <- S.toList (frameFrontier frame)
                    neighbor <- neighbors heightMap current
                    if S.member neighbor (frameVisited frame) || M.member neighbor cameFrom
                        then []
                        else return neighbor
                newCameFrom = M.union cameFrom $ M.fromList
                    [ (neighbor, current)
                    | current <- S.toList (frameFrontier frame)
                    , neighbor <- neighbors heightMap current
                    , not (S.member neighbor (frameVisited frame))
                    , not (M.member neighbor cameFrom)
                    ]
                newVisited = frameVisited frame `S.union` frameFrontier frame
                foundEnd = end `S.member` newFrontier
                newFrame = frame
                    { frameVisited = newVisited
                    , frameFrontier = newFrontier
                    , frameFound = foundEnd
                    , frameStep = frameStep frame + 1
                    }
            in frame : bfsSearch newFrame newCameFrom

    reconstructPath frame cameFrom =
        let path = buildPath (frameEnd frame) cameFrom
            finalFrame = frame { framePath = path }
        in [finalFrame]

    buildPath pos cameFrom =
        case M.lookup pos cameFrom of
            Nothing -> [pos]
            Just prev -> pos : buildPath prev cameFrom

neighbors :: HeightMap -> Pos -> [Pos]
neighbors heightMap (y, x) =
    let currentHeight = fromMaybe 0 $ M.lookup (y, x) heightMap
        candidates = [(y-1, x), (y+1, x), (y, x-1), (y, x+1)]
        isValid pos = case M.lookup pos heightMap of
            Nothing -> False
            Just h -> h <= currentHeight + 1  -- Can climb at most 1 higher
    in filter isValid candidates

renderFrame :: Frame -> IO ()
renderFrame frame = do
    -- Calculate bounds
    let allPos = M.keys (frameHeightMap frame)
        minY = minimum (map fst allPos)
        maxY = maximum (map fst allPos)
        minX = minimum (map snd allPos)
        maxX = maximum (map snd allPos)

    -- Build character grid
    let pathSet = S.fromList (framePath frame)
        charGrid = M.fromList
            [ ((y, x), getChar (y, x))
            | y <- [minY..maxY]
            , x <- [minX..maxX]
            ]
        getChar pos
            | pos == frameStart frame = 'S'
            | pos == frameEnd frame = 'E'
            | pos `S.member` pathSet = '*'
            | pos `S.member` frameFrontier frame = '@'
            | pos `S.member` frameVisited frame = '.'
            | otherwise = heightToChar (M.findWithDefault 0 pos (frameHeightMap frame))
        heightToChar h
            | h <= 3 = ' '
            | h <= 8 = '-'
            | h <= 15 = '='
            | h <= 22 = '#'
            | otherwise = '%'

    -- Build grid lines
    let gridLines = [ [ M.findWithDefault ' ' (y, x) charGrid
                      | x <- [minX..maxX] ]
                    | y <- [minY..maxY] ]

    -- Build entire frame as single string and output atomically
    let statusLine = if frameFound frame
            then "Path found! Length: " ++ show (length $ framePath frame)
            else "Searching..."
        frameContent = unlines
            [ "Hill Climbing Algorithm - Pathfinding"
            , "[Part 1] Find path from S to E, climbing at most +1 per step"
            , ""
            , "Step: " ++ show (frameStep frame)
            , "Visited: " ++ show (S.size $ frameVisited frame)
            , "Frontier: " ++ show (S.size $ frameFrontier frame)
            , statusLine
            , ""
            ] ++ unlines gridLines

    setCursorPosition 0 0
    putStr frameContent
    threadDelay 100000  -- 100ms delay
