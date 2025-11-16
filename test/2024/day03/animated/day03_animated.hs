#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2024 Day 03 ("Mull It Over").
-- Shows character-by-character parsing of mul() instructions with state machine.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, hSetBuffering, stdout, utf8, BufferMode(NoBuffering))

data ParserState
    = Searching
    | InMul String String  -- arg1, arg2
    | MulDisabled
    deriving (Show, Eq)

data Frame = Frame
    { framePos :: Int
    , frameState :: ParserState
    , frameContext :: String
    , frameTotal :: Int
    , frameLastMul :: Maybe (Int, Int, Int)  -- (a, b, result)
    , frameEnabled :: Bool
    }

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let frames = generateFrames contents
        total = length frames
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ (renderFrame total) (zip [0..] frames)
    setCursorPosition 30 0
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

generateFrames :: String -> [Frame]
generateFrames input = go 0 Searching 0 Nothing True input
  where
    go pos state total lastMul enabled [] = []
    go pos state total lastMul enabled str =
        let context = getContext pos input
            frame = Frame pos state context total lastMul enabled
        in frame : parseNext pos state total lastMul enabled str

    parseNext pos state total lastMul enabled str
        | "don't()" `isPrefixOf` str =
            go (pos + 7) MulDisabled total Nothing False (drop 7 str)
        | "do()" `isPrefixOf` str =
            go (pos + 4) Searching total Nothing True (drop 4 str)
        | "mul(" `isPrefixOf` str && enabled =
            case tryParseMul (drop 4 str) of
                Just (a, b, len) ->
                    let result = a * b
                        newTotal = total + result
                    in go (pos + 4) (InMul (show a) (show b)) total (Just (a, b, result))  enabled (drop 4 str)
                    ++ go (pos + 4 + len) Searching newTotal Nothing enabled (drop (4 + len) str)
                Nothing -> go (pos + 1) Searching total Nothing enabled (drop 1 str)
        | not (null str) = go (pos + 1) state total Nothing enabled (drop 1 str)
        | otherwise = []

    tryParseMul str = do
        (a, rest1) <- readNum str
        if take 1 rest1 /= "," then Nothing else do
            (b, rest2) <- readNum (drop 1 rest1)
            if take 1 rest2 /= ")" then Nothing else
                Just (a, b, length str - length rest2 + 1)

    readNum s = case span isDigit s of
        ("", _) -> Nothing
        (ds, rest) | length ds <= 3 -> Just (read ds, rest)
        _ -> Nothing

    getContext pos str =
        let start = max 0 (pos - 30)
            end = min (length str) (pos + 30)
        in take (end - start) (drop start str)

renderFrame :: Int -> (Int, Frame) -> IO ()
renderFrame total (idx, Frame pos state context runningTotal lastMul enabled) = do
    let contextWidth = min 60 (length context)
        cursorInContext = min 30 (pos - max 0 (pos - 30))

        stateDesc = case state of
            Searching -> if enabled then "Searching for mul()" else "Disabled (waiting for do())"
            InMul a b -> "Found mul(" ++ a ++ "," ++ b ++ ")"
            MulDisabled -> "Disabled by don't()"

        mulInfo = case lastMul of
            Just (a, b, r) -> "Last mul: " ++ show a ++ " * " ++ show b ++ " = " ++ show r
            Nothing -> "No mul yet"

        frameContent = unlines
            [ "Mull It Over - Parsing mul() Instructions - frame " ++ show (idx + 1) ++ " / " ++ show total
            , "Part context: [Part 1] sum all mul(); [Part 2] handle do()/don't()."
            , ""
            , "Position: " ++ show pos
            , "State: " ++ stateDesc
            , "Running total: " ++ show runningTotal
            , mulInfo
            , ""
            , "Context (^ = current position):"
            , take contextWidth context
            , replicate cursorInContext ' ' ++ "^"
            , ""
            , "Parser states: Searching | InMul | MulDisabled"
            ]

    setCursorPosition 0 0
    putStr frameContent
    threadDelay (if lastMul /= Nothing then 200000 else 40000)
