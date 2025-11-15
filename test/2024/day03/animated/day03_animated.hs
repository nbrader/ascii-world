#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2024 Day 03 ("Mull It Over").
-- Shows finding and evaluating mul instructions.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.Char (isDigit)
import qualified Data.Map as M
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let muls = findMuls contents
        frames = zip [1..] (scanl (+) 0 muls)
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame (zip muls frames)
    setCursorPosition 25 0
    putStrLn "Mull It Over animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "03"
        filename = case inputType of
            "data" -> "day" ++ dayNum ++ " (data).csv"
            "example2" -> "day" ++ dayNum ++ " (example 2).csv"
            "example3" -> "day" ++ dayNum ++ " (example 3).csv"
            _ -> "day" ++ dayNum ++ " (example).csv"
        path = "test/2024/day" ++ dayNum ++ "/standard/" ++ filename
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

findMuls :: String -> [Int]
findMuls "" = []
findMuls str@(_:rest)
    | take 4 str == "mul(" = case parseMul (drop 4 str) of
        Just (a, b, _) -> (a * b) : findMuls rest
        Nothing -> findMuls rest
    | otherwise = findMuls rest

parseMul :: String -> Maybe (Int, Int, String)
parseMul str = do
    (a, rest1) <- readNum str
    if take 1 rest1 /= "," then Nothing else do
        (b, rest2) <- readNum (drop 1 rest1)
        if take 1 rest2 /= ")" then Nothing else
            Just (a, b, drop 1 rest2)
  where
    readNum s = case span isDigit s of
        ("", _) -> Nothing
        (ds, rest) | length ds <= 3 -> Just (read ds, rest)
        _ -> Nothing

renderFrame :: (Int, (Int, Int)) -> IO ()
renderFrame (mul, (idx, total)) = do
    setCursorPosition 0 0
    putStrLn "Mull It Over - Finding mul() Instructions"
    putStrLn "Part context: [Part 1] sum all mul(); [Part 2] handle do()/don't()."
    putStrLn ""
    putStrLn $ "Current mul result: " ++ show mul
    putStrLn $ "Running total: " ++ show total
    putStrLn $ "Instructions processed: " ++ show idx
    putStrLn ""

    -- Simple visualization
    let width = 40
        point = [(idx `mod` width, idx `div` width)]
        asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList [("Progress", point)]
            , asciiWorldWidth = width
            }
        bgChar = '.'
        maskToChar = id
        pointsToChar = const '*'
        nameZOrder = compare
        worldStr = showAsciiWorld 5 bgChar maskToChar pointsToChar nameZOrder asciiWorld

    putStr worldStr
    threadDelay 150000
