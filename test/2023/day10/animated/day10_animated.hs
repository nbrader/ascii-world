#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2023 Day 10 (Pipe Maze).
-- Shows following a pipe loop from the starting position.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.Maybe (listToMaybe, fromMaybe)
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

type Pos = (Int, Int)
type Grid = M.Map Pos Char

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let (grid, start) = parseGrid contents
        frames = buildFrames grid start
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    setCursorPosition 25 0
    putStrLn "Pipe Maze animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "10"
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
            [ "..F7."
            , ".FJ|."
            , "SJ.L7"
            , "|F--J"
            , "LJ..."
            ]

parseGrid :: String -> (Grid, Pos)
parseGrid contents =
    let gridLines = lines contents
        positions = [ ((y, x), c)
                    | (y, line) <- zip [0..] gridLines
                    , (x, c) <- zip [0..] line
                    ]
        grid = M.fromList positions
        start = case filter ((== 'S') . snd) positions of
                    ((pos, _):_) -> pos
                    [] -> (0, 0)  -- Default to origin if no start found
    in (grid, start)

data Frame = Frame
    { frameGrid :: Grid
    , frameLoop :: [Pos]
    , frameCurrent :: Maybe Pos
    , frameStart :: Pos
    , frameStep :: Int
    }

buildFrames :: Grid -> Pos -> [Frame]
buildFrames grid start =
    let path = findLoop grid start
        frames = [ Frame grid (take i path) (Just $ path !! (i-1)) start i
                 | i <- [1..length path]
                 ]
    in frames

findLoop :: Grid -> Pos -> [Pos]
findLoop grid start =
    case getConnected grid start of
        [] -> [start]  -- No connections from start, return just start
        (firstDir:_) -> reverse $ traverseLoop grid start firstDir [start]
  where
    traverseLoop grid start current visited
        | current == start && length visited > 2 = visited
        | otherwise =
            case filter (`notElem` visited) (getConnected grid current) of
                [] -> visited  -- Dead end or back at start, return current path
                (next:_) -> traverseLoop grid start next (current : visited)

getConnected :: Grid -> Pos -> [Pos]
getConnected grid (y, x) =
    case M.lookup (y, x) grid of
        Just '|' -> [(y-1, x), (y+1, x)]
        Just '-' -> [(y, x-1), (y, x+1)]
        Just 'L' -> [(y-1, x), (y, x+1)]
        Just 'J' -> [(y-1, x), (y, x-1)]
        Just '7' -> [(y+1, x), (y, x-1)]
        Just 'F' -> [(y+1, x), (y, x+1)]
        Just 'S' -> findStartConnections grid (y, x)
        _ -> []

findStartConnections :: Grid -> Pos -> [Pos]
findStartConnections grid (y, x) =
    let candidates = [(y-1, x), (y+1, x), (y, x-1), (y, x+1)]
    in filter (\pos -> (y, x) `elem` getConnected grid pos) candidates

renderFrame :: Frame -> IO ()
renderFrame frame = do
    -- Calculate bounds
    let allPos = M.keys (frameGrid frame)
        minY = minimum (map fst allPos)
        maxY = maximum (map fst allPos)
        minX = minimum (map snd allPos)
        maxX = maximum (map snd allPos)

    -- Build character grid
    let loopSet = S.fromList (frameLoop frame)
        charGrid = M.fromList
            [ ((y, x), getChar (y, x))
            | y <- [minY..maxY]
            , x <- [minX..maxX]
            ]
        getChar pos
            | pos == frameStart frame = 'S'
            | Just pos == frameCurrent frame = '@'
            | pos `S.member` loopSet = '#'
            | otherwise = M.findWithDefault '.' pos (frameGrid frame)

    -- Build grid lines
    let gridLines = [ [ M.findWithDefault ' ' (y, x) charGrid
                      | x <- [minX..maxX] ]
                    | y <- [minY..maxY] ]

    -- Build entire frame as single string and output atomically
    let farthestLine = if length (frameLoop frame) > 1
            then "Farthest point: " ++ show (length (frameLoop frame) `div` 2)
            else "Farthest point: 0"
        frameContent = unlines
            [ "Pipe Maze - Following the Loop"
            , "[Part 1] Trace the pipe loop to find the farthest point"
            , ""
            , "Step: " ++ show (frameStep frame)
            , "Loop length: " ++ show (length $ frameLoop frame)
            , farthestLine
            , ""
            ] ++ unlines gridLines

    setCursorPosition 0 0
    putStr frameContent
    threadDelay 100000  -- 100ms delay
