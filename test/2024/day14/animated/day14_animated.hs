#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
   --package split-0.2.3.5
   --package linear
-}

-- |
-- Animated visualization for AoC 2024 Day 14 ("Restroom Redoubt").
-- Shows robots moving on a grid with wrapping edges using the AsciiWorld library.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.Char (isDigit)
import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Linear (V2(..))
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

type Velocity = V2 Int
data Robot = Robot {rPos :: V2 Int, rVel :: Velocity} deriving (Show, Eq)

data Frame = Frame
    { fTime :: Int
    , fRobots :: [Robot]
    , fWidth :: Int
    , fHeight :: Int
    }

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let (robots, width, height) = parseInput contents
        frames = buildFrames robots width height
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ (renderFrame width height) (zip [0..] frames)
    setCursorPosition (height + 10) 0
    putStrLn "Restroom Redoubt animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "14"
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
        [ "p=0,4 v=3,-3"
        , "p=6,3 v=-1,-3"
        , "p=10,3 v=-1,2"
        , "p=2,0 v=2,-1"
        , "p=0,0 v=1,3"
        , "p=3,0 v=-2,-2"
        , "p=7,6 v=-1,-3"
        , "p=3,0 v=-1,-2"
        , "p=9,3 v=2,3"
        , "p=7,3 v=-1,2"
        , "p=2,4 v=2,-3"
        , "p=9,5 v=-3,-3"
        ]

parseInput :: String -> ([Robot], Int, Int)
parseInput contents = (robots, width, height)
  where
    robots = map parseLine (lines contents)
    -- Use example dimensions (11x7) or real dimensions (101x103)
    (width, height) = if length robots <= 12 then (11, 7) else (101, 103)

    parseLine line =
        -- Replace " v=" with "," to properly separate position and velocity
        let normalized = map (\c -> if c == ' ' then ',' else c) line
            cleaned = filter (\c -> c `elem` (',':'-':[]) || isDigit c) normalized
            parts = map read . filter (not . null) . splitOn "," $ cleaned
        in case parts of
            [px, py, vx, vy] -> Robot (V2 px py) (V2 vx vy)
            _ -> error $ "Invalid robot line (expected 4 numbers): " ++ line

buildFrames :: [Robot] -> Int -> Int -> [Frame]
buildFrames robots width height = take numFrames frames
  where
    numFrames = if width == 11 then 20 else 100
    frames = [Frame t (simulateRobots t robots width height) width height | t <- [0..numFrames-1]]

simulateRobots :: Int -> [Robot] -> Int -> Int -> [Robot]
simulateRobots time robots width height = map (moveRobot time width height) robots

moveRobot :: Int -> Int -> Int -> Robot -> Robot
moveRobot time width height (Robot (V2 px py) vel@(V2 vx vy)) =
    Robot (V2 newX newY) vel
  where
    newX = (px + vx * time) `mod` width
    newY = (py + vy * time) `mod` height

toPoint :: V2 Int -> Point
toPoint (V2 x y) = (x, y)

renderFrame :: Int -> Int -> (Int, Frame) -> IO ()
renderFrame width height (idx, Frame time robots w h) = do
    setCursorPosition 0 0
    putStrLn $ "Restroom Redoubt - Time: " ++ show time ++ "s (frame " ++ show (idx + 1) ++ ")"
    putStrLn "Part context: [Part 1] safety factor after 100s; [Part 2] find Christmas tree pattern."
    putStrLn ""

    -- Create AsciiWorld with robot positions
    let robotPoints = map (toPoint . rPos) robots
        asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList [("Robots", robotPoints)]
            , asciiWorldWidth = w
            }
        bgChar = '.'
        maskToChar = id
        pointsToChar = const 'R'
        nameZOrder = compare
        worldStr = showAsciiWorld h bgChar maskToChar pointsToChar nameZOrder asciiWorld

    putStr worldStr
    putStrLn ""
    putStrLn $ "Total robots: " ++ show (length robots)

    -- Show quadrant counts at t=100
    when (time == 100 && w == 101) $ do
        let (q1, q2, q3, q4) = countQuadrants robots w h
        putStrLn $ "Quadrants (at t=100): " ++ show [q1, q2, q3, q4]
        putStrLn $ "Safety factor: " ++ show (q1 * q2 * q3 * q4)

    threadDelay 100000
  where
    when True action = action
    when False _ = pure ()

countQuadrants :: [Robot] -> Int -> Int -> (Int, Int, Int, Int)
countQuadrants robots width height = (length q1, length q2, length q3, length q4)
  where
    midX = width `div` 2
    midY = height `div` 2
    q1 = [r | r@(Robot (V2 x y) _) <- robots, x < midX && y < midY]
    q2 = [r | r@(Robot (V2 x y) _) <- robots, x > midX && y < midY]
    q3 = [r | r@(Robot (V2 x y) _) <- robots, x < midX && y > midY]
    q4 = [r | r@(Robot (V2 x y) _) <- robots, x > midX && y > midY]
