#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2022 Day 23.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import qualified Data.Map as M
import System.Console.ANSI
import System.IO (hSetEncoding, stdout, utf8)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    let inputType = if null args then "example" else head args
    frames <- buildFrames inputType
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame (zip [0..] frames)
    setCursorPosition 25 0
    putStrLn "AoC 2022 Day 23 animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "23"
        filename = case inputType of
            "data" -> "day" ++ dayNum ++ " (data).csv"
            "example2" -> "day" ++ dayNum ++ " (example 2).csv"
            "example3" -> "day" ++ dayNum ++ " (example 3).csv"
            _ -> "day" ++ dayNum ++ " (example).csv"
        path = "test/2022/day" ++ dayNum ++ "/standard/" ++ filename
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure "No data available"

buildFrames :: String -> IO [[(Int, Int)]]
buildFrames inputType = do
    contents <- loadInput inputType
    let fileRows = lines contents
    let initElfPositions = readElfPositions fileRows
    let elfHistory = take 11 $ scanl (flip ($!)) (initElfPositions, initElfMoves) (repeat doElfRound)
    return $ map fst elfHistory

renderFrame :: (Int, [(Int, Int)]) -> IO ()
renderFrame (step, elfPositions) = do
    setCursorPosition 0 0
    putStrLn $ "AoC 2022 Day 23 - Round " ++ show step ++ "/10"
    putStrLn "Unstable Diffusion - Elf positions"
    putStrLn ""

    let (minX, minY, maxX, maxY) = getBounds elfPositions
        width = maxX - minX + 3
        height = maxY - minY + 3
        adjustedPoints = [(x - minX, y - minY) | (x, y) <- elfPositions]
        asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList [("Elves", adjustedPoints)]
            , asciiWorldWidth = width
            }
        bgChar = '.'
        maskToChar = id
        pointsToChar = const '#'
        nameZOrder = compare
        worldStr = showAsciiWorld height bgChar maskToChar pointsToChar nameZOrder asciiWorld

    putStr worldStr
    threadDelay 300000

-- Helper functions and simulation logic
getBounds :: [(Int, Int)] -> (Int, Int, Int, Int)
getBounds [] = (0, 0, 0, 0)
getBounds ((x0, y0):rest) = foldl step (x0, y0, x0, y0) rest
  where
    step (minX, minY, maxX, maxY) (x, y) =
        (min minX x, min minY y, max maxX x, max maxY y)

type V2 = (Int, Int)
type ElfMove = (V2, (V2, V2))

readElfPositions :: [String] -> [V2]
readElfPositions rows = [(colNum, rowNum) | (rowNum, row) <- zip [1..] rows, (colNum, c) <- zip [1..] row, c == '#']

doElfRound :: ([V2], [ElfMove]) -> ([V2], [ElfMove])
doElfRound (prevPositions, prevMoves) = (afterFullPositions, tail prevMoves)
  where
    prevPosSet = M.fromList [(pos, ()) | pos <- prevPositions]
    moves = take 4 prevMoves
    afterHalf = do
        prevPos <- prevPositions
        let viablePositions = [prevPos `addV2` move | (move, (sideMove1, sideMove2)) <- moves,
                              not (any (`M.member` prevPosSet) [prevPos `addV2` move' | move' <- [move, sideMove1, sideMove2]])]
        let noOfViable = length viablePositions
        if noOfViable == 4 || noOfViable == 0
            then return prevPos
            else return (head viablePositions)
    afterFullPositions = [if afterPos == prevPos then prevPos else if length overlapping == 1 then afterPos else prevPos
                         | (prevPos, afterPos) <- zip prevPositions afterHalf,
                           let overlapping = [() | afterPos' <- afterHalf, afterPos' == afterPos]]

addV2 :: V2 -> V2 -> V2
addV2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

vecN, vecS, vecE, vecW, vecNE, vecNW, vecSE, vecSW :: V2
vecN = (0, -1)
vecS = (0, 1)
vecE = (-1, 0)
vecW = (1, 0)
vecNE = (-1, -1)
vecNW = (1, -1)
vecSE = (-1, 1)
vecSW = (1, 1)

initElfMoves :: [ElfMove]
initElfMoves = cycle [
    (vecN, (vecNE, vecNW)),
    (vecS, (vecSE, vecSW)),
    (vecE, (vecNE, vecSE)),
    (vecW, (vecNW, vecSW))
    ]
