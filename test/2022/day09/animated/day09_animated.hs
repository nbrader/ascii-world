#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
   --package split-0.2.3.5
-}

-- |
-- Animated visualization for AoC 2022 Day 9 (Rope Bridge).
-- Shows a rope with head and tail following rope physics.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.List (foldl', scanl')
import Data.List.Split (splitOn)
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

type Pos = (Int, Int)
type Rope = [Pos]  -- Head is first, tail segments follow

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let commands = parseCommands contents
        frames = buildFrames commands
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    setCursorPosition 25 0
    putStrLn "Rope Bridge animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "09"
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
            [ "R 4"
            , "U 4"
            , "L 3"
            , "D 1"
            , "R 4"
            , "D 1"
            , "L 5"
            , "R 2"
            ]

data Command = Command Char Int deriving Show

parseCommands :: String -> [Command]
parseCommands = map parseLine . lines
  where
    parseLine line = case words line of
        [dir:_, amt] -> Command dir (read amt)
        _ -> Command 'R' 0

data Frame = Frame
    { frameRope :: Rope
    , frameVisited :: Set Pos
    , frameCommand :: String
    , frameStep :: Int
    }

buildFrames :: [Command] -> [Frame]
buildFrames commands =
    let ropeLength = 10  -- Part 2: 10 knots (head + 9 tails)
        initialRope = replicate ropeLength (0, 0)
        initialFrame = Frame initialRope (S.singleton (0, 0)) "Start" 0
    in reverse $ snd $ foldl' processCommand (initialFrame, [initialFrame]) commands
  where
    processCommand (prevFrame, acc) (Command dir amt) =
        let cmdStr = dir : " " ++ show amt
            steps = expandCommand (Command dir amt)
            (lastFrame, frames) = foldl' (processStep cmdStr) (prevFrame, []) steps
        in (lastFrame, reverse frames ++ acc)

    processStep cmdStr (prevFrame, acc) move =
        let newHead = addV2 (head $ frameRope prevFrame) move
            newRope = updateRope (newHead : tail (frameRope prevFrame))
            newTail = last newRope
            newVisited = S.insert newTail (frameVisited prevFrame)
            newFrame = Frame newRope newVisited cmdStr (frameStep prevFrame + 1)
        in (newFrame, newFrame : acc)

expandCommand :: Command -> [Pos]
expandCommand (Command dir amt) = replicate amt (dirToVec dir)
  where
    dirToVec 'U' = (0, -1)
    dirToVec 'D' = (0, 1)
    dirToVec 'L' = (-1, 0)
    dirToVec 'R' = (1, 0)
    dirToVec _ = (0, 0)

updateRope :: Rope -> Rope
updateRope [] = []
updateRope [h] = [h]
updateRope (h:t:rest) = h : updateRope (followHead h t : rest)

followHead :: Pos -> Pos -> Pos
followHead (hx, hy) (tx, ty) =
    let dx = hx - tx
        dy = hy - ty
    in if abs dx <= 1 && abs dy <= 1
        then (tx, ty)  -- Adjacent, no movement needed
        else (tx + signum dx, ty + signum dy)  -- Move toward head

renderFrame :: Frame -> IO ()
renderFrame frame = do
    setCursorPosition 0 0
    putStrLn "Rope Bridge - Rope Physics Simulation"
    putStrLn "[Part 2] 10-knot rope: H + tails 1-9"
    putStrLn ""
    putStrLn $ "Command: " ++ frameCommand frame
    putStrLn $ "Step: " ++ show (frameStep frame)
    putStrLn $ "Tail visited positions: " ++ show (S.size $ frameVisited frame)
    putStrLn ""

    -- Calculate bounds
    let rope = frameRope frame
        visited = S.toList $ frameVisited frame
        allPoints = rope ++ visited ++ [(0, 0)]
        minX = minimum (map fst allPoints) - 2
        maxX = maximum (map fst allPoints) + 2
        minY = minimum (map snd allPoints) - 2
        maxY = maximum (map snd allPoints) + 2
        width = maxX - minX + 1
        height = maxY - minY + 1

    -- Build character grid
    let ropeMap = M.fromList $ zip rope (['H'] ++ ['1'..'9'])
        charGrid = M.fromList
            [ ((x - minX, y - minY), getChar (x, y))
            | y <- [minY..maxY]
            , x <- [minX..maxX]
            ]
        getChar pos
            | pos == (0, 0) && pos `notElem` rope = 's'  -- Start
            | Just c <- M.lookup pos ropeMap = c  -- Rope segment
            | pos `S.member` frameVisited frame = '#'  -- Visited by tail
            | otherwise = '.'

    -- Print grid
    let gridLines = [ [ M.findWithDefault '.' (x, y) charGrid
                      | x <- [0..width-1] ]
                    | y <- [0..height-1] ]
    mapM_ putStrLn gridLines
    threadDelay 100000  -- 100ms delay

addV2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
addV2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
