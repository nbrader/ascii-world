#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2024 Day 11 ("Plutonian Pebbles").
-- Shows stones transforming with each blink.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import qualified Data.Map as M
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

type Stone = Int
type Stones = [Stone]

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let stones = parseStones contents
        frames = buildFrames stones
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    setCursorPosition 25 0
    putStrLn "Plutonian Pebbles animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "11"
        filename = case inputType of
            "data" -> "day" ++ dayNum ++ " (data).csv"
            "example2" -> "day" ++ dayNum ++ " (example 2).csv"
            "example3" -> "day" ++ dayNum ++ " (example 3).csv"
            _ -> "day" ++ dayNum ++ " (example).csv"
        path = "test/2024/day" ++ dayNum ++ "/standard/" ++ filename
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure "125 17"

parseStones :: String -> Stones
parseStones = map read . words

buildFrames :: Stones -> [(Int, Stones)]
buildFrames initial = take 10 $ zip [0..] $ iterate blink initial

blink :: Stones -> Stones
blink = concatMap transform
  where
    transform 0 = [1]
    transform n
        | even (length (show n)) =
            let s = show n
                mid = length s `div` 2
                (left, right) = splitAt mid s
            in [read left, read right]
        | otherwise = [n * 2024]

renderFrame :: (Int, Stones) -> IO ()
renderFrame (blinkNum, stones) = do
    setCursorPosition 0 0
    putStrLn $ "Plutonian Pebbles - Blink #" ++ show blinkNum
    putStrLn "Part context: [Part 1] count stones after 25 blinks; [Part 2] after 75 blinks."
    putStrLn ""
    putStrLn $ "Stones (" ++ show (length stones) ++ "):"
    putStrLn $ unwords (map show $ take 50 stones) ++ if length stones > 50 then " ..." else ""
    putStrLn ""

    -- Visualize stone positions on a line
    let width = min 80 (length stones)
        points = [(i `mod` width, i `div` width) | i <- [0..min 200 (length stones - 1)]]
        asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList [("Stones", points)]
            , asciiWorldWidth = width
            }
        height = if null stones then 1 else min 10 ((length stones `div` width) + 1)
        bgChar = '.'
        maskToChar = id
        pointsToChar = const 'O'
        nameZOrder = compare
        worldStr = showAsciiWorld height bgChar maskToChar pointsToChar nameZOrder asciiWorld

    putStr worldStr
    putStrLn ""

    putStrLn "Rules: 0→1, even-digits→split, else→×2024"
    threadDelay 300000
