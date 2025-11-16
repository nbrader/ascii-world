#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2024 Day 07 ("Bridge Repair").
-- Shows equation solving with step-by-step operator testing.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.Char (isDigit)
import Data.List (intersperse)
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, hSetBuffering, stdout, utf8, BufferMode(NoBuffering))

data Equation = Equation { eqTarget :: Int, eqNumbers :: [Int] } deriving (Show)
data Operator = Add | Mul deriving (Show, Eq)

data Frame = Frame
    { frameEq :: Equation
    , frameOps :: [Operator]
    , frameSteps :: [(String, Int)]  -- Evaluation steps
    , frameResult :: Int
    , frameMatches :: Bool
    , frameEqIndex :: Int
    , frameTotalEqs :: Int
    , frameSolved :: Bool  -- Whether this equation was solved
    }

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let equations = parseEquations contents
        totalEqs = length equations
        frames = concatMap (buildFramesForEq totalEqs) (zip [0..] (take 5 equations))
        total = length frames
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ (renderFrame total) (zip [0..] frames)
    setCursorPosition 30 0
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

buildFramesForEq :: Int -> (Int, Equation) -> [Frame]
buildFramesForEq totalEqs (eqIdx, eq@(Equation target nums)) =
    let attempts = generateAttempts (length nums - 1)
        results = map (\ops -> (ops, evaluateWithSteps nums ops)) attempts
        solved = any (\(_, (steps, result)) -> result == target) results
    in concatMap (\(ops, (steps, result)) ->
        let matches = result == target
        in [Frame eq ops steps result matches eqIdx totalEqs solved]
        ++ if matches then [Frame eq ops steps result True eqIdx totalEqs solved] else []
       ) results

generateAttempts :: Int -> [[Operator]]
generateAttempts 0 = [[]]
generateAttempts n = [op:rest | op <- [Add, Mul], rest <- generateAttempts (n-1)]

evaluateWithSteps :: [Int] -> [Operator] -> ([(String, Int)], Int)
evaluateWithSteps nums ops = go nums ops []
  where
    go [n] [] steps = (reverse steps, n)
    go (n1:n2:ns) (op:ops) steps =
        let result = applyOp op n1 n2
            step = show n1 ++ " " ++ showOp op ++ " " ++ show n2 ++ " = " ++ show result
        in go (result:ns) ops ((step, result):steps)
    go _ _ steps = (reverse steps, 0)

    applyOp Add a b = a + b
    applyOp Mul a b = a * b

showOp :: Operator -> String
showOp Add = "+"
showOp Mul = "*"

renderFrame :: Int -> (Int, Frame) -> IO ()
renderFrame total (idx, Frame (Equation target nums) ops steps result matches eqIdx totalEqs solved) = do
    let expr = buildExpression nums ops
        frameContent = unlines (
            [ "Bridge Repair - Equation " ++ show (eqIdx + 1) ++ " / " ++ show totalEqs
            , "Frame " ++ show (idx + 1) ++ " / " ++ show total
            , "Part context: [Part 1] + and ×; [Part 2] add concatenation ||."
            , ""
            , "Target: " ++ show target
            , "Testing: " ++ expr
            , ""
            , "Evaluation steps:"
            ] ++ ["  " ++ step | (step, _) <- steps] ++
            [ ""
            , "Final result: " ++ show result
            , if matches
              then "*** MATCH FOUND! ***"
              else "No match (continuing search...)"
            , ""
            , "Operators: + (add), * (multiply)"
            ])

    setCursorPosition 0 0
    putStr frameContent
    threadDelay (if matches then 400000 else 30000)
  where
    buildExpression :: [Int] -> [Operator] -> String
    buildExpression [] _ = ""
    buildExpression [n] _ = show n
    buildExpression (n:ns) (op:ops) =
        show n ++ " " ++ showOp op ++ " " ++ buildExpression ns ops
    buildExpression _ _ = ""
