#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2022 Day 25.

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
    let frames = buildFrames
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    setCursorPosition 25 0
    putStrLn "AoC 2022 Day 25 animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "25"
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

-- SNAFU number type and conversion functions
data SNAFUDigit = Digit2 | Digit1 | Digit0 | DigitMinus | DigitDoubleMinus deriving (Show, Eq)
newtype SNAFU = SNAFU {snafuDigits :: [SNAFUDigit]} deriving (Show)

evalSNAFUDigit :: SNAFUDigit -> Int
evalSNAFUDigit Digit2 = 2
evalSNAFUDigit Digit1 = 1
evalSNAFUDigit Digit0 = 0
evalSNAFUDigit DigitMinus = -1
evalSNAFUDigit DigitDoubleMinus = -2

snafuToInt :: SNAFU -> Int
snafuToInt x = sum [(evalSNAFUDigit d)*5^i | (i,d) <- zip [0..] (reverse $ snafuDigits x)]

readSNAFU :: String -> SNAFU
readSNAFU = SNAFU . map readCharAsDigit

readCharAsDigit :: Char -> SNAFUDigit
readCharAsDigit '2' = Digit2
readCharAsDigit '1' = Digit1
readCharAsDigit '0' = Digit0
readCharAsDigit '-' = DigitMinus
readCharAsDigit '=' = DigitDoubleMinus
readCharAsDigit _ = Digit0

showDigit :: SNAFUDigit -> String
showDigit Digit2 = "2"
showDigit Digit1 = "1"
showDigit Digit0 = "0"
showDigit DigitMinus = "-"
showDigit DigitDoubleMinus = "="

showSNAFU :: SNAFU -> String
showSNAFU = concatMap showDigit . snafuDigits

-- Frame data type for animation
data Frame = Frame String Int Int Int  -- SNAFU string, current position, calculated value, expected value

buildFrames :: [Frame]
buildFrames =
    let exampleNumbers = [
            ("1=-0-2", 1747),
            ("12111", 906),
            ("2=0=", 198),
            ("21", 11),
            ("2=01", 201),
            ("111", 31),
            ("20012", 1257),
            ("112", 32),
            ("1=-1=", 353),
            ("1-12", 107),
            ("12", 7),
            ("1=", 3),
            ("122", 37)
          ]
    in concatMap makeFramesForNumber (take 5 exampleNumbers)
  where
    makeFramesForNumber (snafuStr, expectedInt) =
        let snafu = readSNAFU snafuStr
            digits = snafuDigits snafu
            positions = length digits
            -- Create frames showing each digit being processed
            digitFrames = [Frame snafuStr i (snafuToInt snafu) expectedInt | i <- [0..positions]]
        in digitFrames

renderFrame :: Frame -> IO ()
renderFrame (Frame snafuStr pos calcValue expectedValue) = do
    setCursorPosition 0 0
    putStrLn "╔══════════════════════════════════════════════════════════════╗"
    putStrLn "║        AoC 2022 Day 25 - SNAFU Number Conversion           ║"
    putStrLn "╚══════════════════════════════════════════════════════════════╝"
    putStrLn ""
    putStrLn $ "SNAFU Number: " ++ snafuStr
    putStrLn $ "              " ++ replicate pos ' ' ++ "^"
    putStrLn ""
    putStrLn $ "Decimal Value: " ++ show calcValue
    putStrLn $ "Expected:      " ++ show expectedValue
    putStrLn ""

    -- Visual representation using ASCII art
    let width = 60
        height = 8
        -- Create points for visual display
        barLength = min width (calcValue `div` 30)
        points = [(i, 4) | i <- [0..barLength]]
        asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList [("Value", points)]
            , asciiWorldWidth = width
            }
        bgChar = '.'
        maskToChar = id
        pointsToChar = const '█'
        nameZOrder = compare
        worldStr = showAsciiWorld height bgChar maskToChar pointsToChar nameZOrder asciiWorld

    putStr worldStr
    putStrLn ""
    threadDelay 250000
