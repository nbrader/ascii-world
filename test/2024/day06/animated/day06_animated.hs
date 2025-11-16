#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization of AoC 2024 Day 6 ("Guard Gallivant").
-- The guard walks the example grid and we show each step, turn,
-- and exit/loop detection using ANSI control codes.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import qualified Data.Set as S
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

type Point = (Int, Int)
type Direction = (Int, Int)
type State = (Point, Direction)

data FrameTag = Stepping | Turning | Exiting | Looping deriving (Eq, Show)

data Frame = Frame
    { frameState   :: State
    , frameVisited :: S.Set Point
    , frameTag     :: FrameTag
    }

main :: IO ()
main = do
    hSetEncoding stdout utf8  -- Windows compatibility
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadMap inputType
    let gridLines = lines contents
        (width, height, startState, obstacles) = parseMap gridLines
        frames = simulateFrames width height obstacles startState
        total = length frames
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ (renderFrame width height obstacles total) (zip [0 ..] frames)
    setCursorPosition (height + 8) 0
    putStrLn "Guard path animation complete. (Part 1 path + Part 2 loop cues)"

loadMap :: String -> IO String
loadMap inputType = do
    let dayNum = "06"
        filename = case inputType of
            "data" -> "day" ++ dayNum ++ " (data).csv"
            "example2" -> "day" ++ dayNum ++ " (example 2).csv"
            "example3" -> "day" ++ dayNum ++ " (example 3).csv"
            _ -> "day" ++ dayNum ++ " (example).csv"
        path = "test/2024/day" ++ dayNum ++ "/standard/" ++ filename
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure $ unlines defaultMap
  where
    defaultMap =
        [ "....#....."
        , ".........#"
        , ".........."
        , "..#......."
        , ".......#.."
        , ".........."
        , ".#..^....."
        , "........#."
        , "#........."
        , "......#..."
        ]

parseMap :: [String] -> (Int, Int, State, S.Set Point)
parseMap rows = (width, height, (startPos, startDir), obstacleSet)
  where
    height = length rows
    width = if null rows then 0 else length (head rows)
    coords = [((x, y), c) | (y, row) <- zip [0 ..] rows, (x, c) <- zip [0 ..] row]
    obstacleSet = S.fromList [pos | (pos, '#') <- coords]
    guardOptions = [(pos, dirFromChar c) | (pos, c) <- coords, c `elem` "^v<>"]
    (startPos, startDir) =
        case guardOptions of
            (g : _) -> g
            [] -> error "Guard start not found in grid"

dirFromChar :: Char -> Direction
dirFromChar '^' = (0, -1)
dirFromChar 'v' = (0, 1)
dirFromChar '<' = (-1, 0)
dirFromChar '>' = (1, 0)
dirFromChar _   = (0, -1)

simulateFrames :: Int -> Int -> S.Set Point -> State -> [Frame]
simulateFrames width height obstacles = go S.empty S.empty
  where
    go seen visited state@(pos, dir)
        | state `S.member` seen = [Frame state (S.insert pos visited) Looping]
        | otherwise =
            let visited' = S.insert pos visited
                seen' = S.insert state seen
            in case classify state of
                Exit -> [Frame state visited' Exiting]
                Turn newState -> Frame state visited' Turning : go seen' visited' newState
                Step newState -> Frame state visited' Stepping : go seen' visited' newState

    classify (pos, dir) =
        let nextPos = addPoint pos dir
        in if not (inBounds width height nextPos)
               then Exit
               else if nextPos `S.member` obstacles
                        then Turn (pos, turnRight dir)
                        else Step (nextPos, dir)

data StepResult = Step State | Turn State | Exit

addPoint :: Point -> Direction -> Point
addPoint (x, y) (dx, dy) = (x + dx, y + dy)

turnRight :: Direction -> Direction
turnRight (0, -1) = (1, 0)
turnRight (1, 0)  = (0, 1)
turnRight (0, 1)  = (-1, 0)
turnRight (-1, 0) = (0, -1)
turnRight dir     = dir

inBounds :: Int -> Int -> Point -> Bool
inBounds width height (x, y) = x >= 0 && x < width && y >= 0 && y < height

renderFrame :: Int -> Int -> S.Set Point -> Int -> (Int, Frame) -> IO ()
renderFrame width height obstacles total (idx, Frame (pos, dir) visited tag) = do
    setCursorPosition 0 0
    putStrLn $ "Guard Gallivant animation - frame " ++ show (idx + 1) ++ " / " ++ show total
    putStrLn "Part context: [Part 1] Follow the guard's route; [Part 2] watch for loop warnings."
    putStr (unlines (gridRows width height pos dir visited obstacles))
    putStrLn ""
    putStrLn $ "Status: " ++ describe tag
    putStrLn "Legend: ^v<> guard, # obstacle, . visited path"
    threadDelay 120000

gridRows :: Int -> Int -> Point -> Direction -> S.Set Point -> S.Set Point -> [String]
gridRows width height guardPos guardDir visited obstacles =
    [ [ cellChar (x, y)
      | x <- [0 .. width - 1]
      ]
    | y <- [0 .. height - 1]
    ]
  where
    cellChar point
        | point == guardPos = dirSymbol guardDir
        | point `S.member` obstacles = '#'
        | point `S.member` visited = '.'
        | otherwise = ' '

dirSymbol :: Direction -> Char
dirSymbol (0, -1) = '^'
dirSymbol (0, 1)  = 'v'
dirSymbol (1, 0)  = '>'
dirSymbol (-1, 0) = '<'
dirSymbol _       = '?'

describe :: FrameTag -> String
describe Stepping = "[Part 1] Moving forward"
describe Turning  = "[Part 1] Obstacle ahead - turning right"
describe Exiting  = "[Part 1] Guard stepped outside the mapped area"
describe Looping  = "[Part 2] Detected a repeated state (infinite loop)"
