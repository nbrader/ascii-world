#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2024 Day 07 ("Bridge Repair").
-- Shows equation solving with different operators.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.Char (isDigit)
import Data.List (intersperse)
import qualified Data.Map as M
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

data Equation = Equation { eqTarget :: Int, eqNumbers :: [Int] } deriving (Show)
data Operator = Add | Mul deriving (Show, Eq)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let equations = parseEquations contents
        frames = concatMap buildFramesForEq (take 3 equations)
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    setCursorPosition 25 0
    putStrLn "Bridge Repair animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "07"
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
        [ "190: 10 19"
        , "3267: 81 40 27"
        , "83: 17 5"
        , "156: 15 6"
        , "7290: 6 8 6 15"
        , "161011: 16 10 13"
        , "192: 17 8 14"
        , "21037: 9 7 18 13"
        , "292: 11 6 16 20"
        ]

parseEquations :: String -> [Equation]
parseEquations = map parseLine . lines
  where
    parseLine line =
        let (target:rest) = filter (not . null) $ words $ map (\c -> if c == ':' then ' ' else c) line
        in Equation (read target) (map read rest)

buildFramesForEq :: Equation -> [(Equation, [Operator], Int, Bool)]
buildFramesForEq eq@(Equation target nums) =
    let attempts = generateAttempts (length nums - 1)
    in map (\ops -> (eq, ops, evaluate nums ops, evaluate nums ops == target)) attempts

generateAttempts :: Int -> [[Operator]]
generateAttempts 0 = [[]]
generateAttempts n = [op:rest | op <- [Add, Mul], rest <- generateAttempts (n-1)]

evaluate :: [Int] -> [Operator] -> Int
evaluate [] _ = 0
evaluate [n] _ = n
evaluate (n:ns) (op:ops) =
    let rest = evaluate ns ops
    in case op of
        Add -> n + rest
        Mul -> n * rest
evaluate _ _ = 0

renderFrame :: (Equation, [Operator], Int, Bool) -> IO ()
renderFrame (Equation target nums, ops, result, solved) = do
    setCursorPosition 0 0
    putStrLn $ "Bridge Repair - Target: " ++ show target
    putStrLn "Part context: [Part 1] + and ×; [Part 2] add concatenation ||."
    putStrLn ""

    let expr = buildExpression nums ops
    putStrLn $ "Testing: " ++ expr
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Match: " ++ if solved then "YES!" else "no"
    putStrLn ""

    -- Visualize as a simple grid showing success/failure
    let width = 40
        height = 3
        statusPoint = if solved then [(20, 1)] else []
        asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList [("Success", statusPoint)]
            , asciiWorldWidth = width
            }
        bgChar = '.'
        maskToChar = id
        pointsToChar = const '√'
        nameZOrder = compare
        worldStr = showAsciiWorld height bgChar maskToChar pointsToChar nameZOrder asciiWorld

    putStr worldStr

    threadDelay (if solved then 500000 else 50000)
  where
    buildExpression :: [Int] -> [Operator] -> String
    buildExpression [] _ = ""
    buildExpression [n] _ = show n
    buildExpression (n:ns) (op:ops) =
        show n ++ " " ++ showOp op ++ " " ++ buildExpression ns ops
    buildExpression _ _ = ""

    showOp Add = "+"
    showOp Mul = "*"
