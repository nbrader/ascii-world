#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
   --package split-0.2.3.5
-}

-- |
-- Animated visualization for AoC 2024 Day 13 ("Claw Contraption").
-- Shows the claw moving toward prizes by pressing buttons A and B.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.Char (isDigit)
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.IO (hSetEncoding, stdout, utf8)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

data Machine = Machine
    { mButtonA :: (Int, Int)
    , mButtonB :: (Int, Int)
    , mPrize :: (Int, Int)
    } deriving (Show)

data Frame = Frame
    { fMachineIdx :: Int
    , fMachine :: Machine
    , fPosition :: (Int, Int)
    , fPressesA :: Int
    , fPressesB :: Int
    , fSolved :: Bool
    }

main :: IO ()
main = do
    hSetEncoding stdout utf8
    contents <- loadInput
    let machines = parseInput contents
        frames = concatMap buildMachineFrames (zip [0..] machines)
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    setCursorPosition 30 0
    putStrLn "Claw Contraption animation complete."

loadInput :: IO String
loadInput = do
    let path = "test/2024/day13 (example).csv"
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure defaultInput
  where
    defaultInput = unlines
        [ "Button A: X+94, Y+34"
        , "Button B: X+22, Y+67"
        , "Prize: X=8400, Y=5400"
        , ""
        , "Button A: X+26, Y+66"
        , "Button B: X+67, Y+21"
        , "Prize: X=12748, Y=12176"
        , ""
        , "Button A: X+17, Y+86"
        , "Button B: X+84, Y+37"
        , "Prize: X=7870, Y=6450"
        , ""
        , "Button A: X+69, Y+23"
        , "Button B: X+27, Y+71"
        , "Prize: X=18641, Y=10279"
        ]

parseInput :: String -> [Machine]
parseInput contents = map toMachine parsed
  where
    cleaned = filter (\c -> c `elem` (',':'\n':[]) || isDigit c) contents
    numbers = map (map read . splitOn ",") . filter (not . null) . lines $ cleaned
    parsed = chunksOf 3 numbers
    toMachine [[ax,ay],[bx,by],[px,py]] = Machine (ax, ay) (bx, by) (px, py)

buildMachineFrames :: (Int, Machine) -> [Frame]
buildMachineFrames (idx, machine) =
    case solveMachine machine of
        Nothing -> [Frame idx machine (0, 0) 0 0 False]
        Just (pressesA, pressesB) ->
            let steps = min 50 (pressesA + pressesB)
                stepSize = max 1 ((pressesA + pressesB) `div` steps)
                positions = takeSteps machine pressesA pressesB stepSize
            in positions ++ [Frame idx machine (fst $ mPrize machine, snd $ mPrize machine) pressesA pressesB True]
  where
    takeSteps machine totalA totalB stepSize =
        [ let a = min totalA (i * stepSize * totalA `div` (totalA + totalB))
              b = min totalB (i * stepSize * totalB `div` (totalA + totalB))
              (ax, ay) = mButtonA machine
              (bx, by) = mButtonB machine
              pos = (a * ax + b * bx, a * ay + b * by)
          in Frame idx machine pos a b False
        | i <- [0..steps-1]
        ]

solveMachine :: Machine -> Maybe (Int, Int)
solveMachine (Machine (ax, ay) (bx, by) (px, py))
    | det == 0 = Nothing
    | sx_det `mod` det /= 0 || sy_det `mod` det /= 0 = Nothing
    | otherwise = Just (sx_det `div` det, sy_det `div` det)
  where
    det = ax * by - bx * ay
    sx_det = by * px - bx * py
    sy_det = -ay * px + ax * py

renderFrame :: Frame -> IO ()
renderFrame (Frame idx (Machine (ax, ay) (bx, by) (px, py)) (cx, cy) pa pb solved) = do
    setCursorPosition 0 0
    putStrLn $ "Claw Contraption - Machine #" ++ show (idx + 1)
    putStrLn "Part context: [Part 1] solve linear equations; [Part 2] add huge offset."
    putStrLn ""
    putStrLn $ "Button A: X+" ++ show ax ++ ", Y+" ++ show ay ++ " (cost: 3 tokens)"
    putStrLn $ "Button B: X+" ++ show bx ++ ", Y+" ++ show by ++ " (cost: 1 token)"
    putStrLn $ "Prize at: X=" ++ show px ++ ", Y=" ++ show py
    putStrLn ""
    putStrLn $ "Current position: (" ++ show cx ++ ", " ++ show cy ++ ")"
    putStrLn $ "Button A pressed: " ++ show pa ++ " times"
    putStrLn $ "Button B pressed: " ++ show pb ++ " times"
    putStrLn $ "Tokens spent: " ++ show (pa * 3 + pb)
    putStrLn ""

    if solved
        then putStrLn "PRIZE WON!"
        else if pa == 0 && pb == 0
            then putStrLn "No solution found."
            else putStrLn "Moving toward prize..."

    putStrLn ""

    -- Visualize using AsciiWorld
    let scale = 100
        width = min 60 ((px `div` scale) + 5)
        height = min 20 ((py `div` scale) + 3)
        asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList
                [ ("Claw", [(cx `div` scale, cy `div` scale)])
                , ("Prize", [(px `div` scale, py `div` scale)])
                ]
            , asciiWorldWidth = width
            }
        bgChar = '.'
        maskToChar = id
        pointsToChar name = case name of
            "Claw" -> 'C'
            "Prize" -> 'P'
            _ -> '?'
        nameZOrder = compare
        worldStr = showAsciiWorld height bgChar maskToChar pointsToChar nameZOrder asciiWorld

    putStr worldStr

    threadDelay (if solved then 500000 else 80000)
