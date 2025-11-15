#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package array
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2024 Day 10 ("Hoof It").
-- We load the day10 example grid and show the height-by-height
-- wavefront of reachable cells expanding from all trailheads.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.Array
import Data.Char (digitToInt, intToDigit)
import qualified Data.Set as S
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

type Point = (Int, Int)
type HeightMap = Array Point Int

data FrontFrame = FrontFrame
    { ffLevel  :: Int
    , ffPoints :: S.Set Point
    }

main :: IO ()
main = do
    hSetEncoding stdout utf8  -- Windows compatibility
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadMap inputType
    let grid = parseGrid contents
        frames = buildFrames grid
        total = length frames
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ (renderFrame grid total) (zip [0 ..] frames)
    let (_, (maxX, maxY)) = bounds grid
    setCursorPosition ((maxY + 1) * 2 + 6) 0
    putStrLn "Hoof It wave animation complete. (Part 1 scores + Part 2 ratings)"

loadMap :: String -> IO String
loadMap inputType = do
    let dayNum = "10"
        filename = case inputType of
            "data" -> "day" ++ dayNum ++ " (data).csv"
            "example2" -> "day" ++ dayNum ++ " (example 2).csv"
            "example3" -> "day" ++ dayNum ++ " (example 3).csv"
            _ -> "day" ++ dayNum ++ " (example).csv"
        path = "test/2024/day" ++ dayNum ++ "/standard/" ++ filename
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure $ unlines defaultMap
  where
    defaultMap =
        [ "89010123"
        , "78121874"
        , "87430965"
        , "96549874"
        , "45678903"
        , "32019012"
        , "01298765"
        , "10107654"
        ]

parseGrid :: String -> HeightMap
parseGrid rawInput = listArray ((0, 0), (width - 1, height - 1)) cells
  where
    rows = filter (not . null) . map (filter (/= '\r')) $ lines rawInput
    height = length rows
    width = if null rows then 0 else length (head rows)
    cells = map digitToInt (concat rows)

buildFrames :: HeightMap -> [FrontFrame]
buildFrames grid = zipWith FrontFrame [0 .. 9] levelSets
  where
    start = S.fromList [pos | (pos, h) <- assocs grid, h == 0]
    levelSets = map fst $ take 10 $ iterate advance (start, 0)

    advance (current, level) =
        let nextLevel = level + 1
            nextFront =
                S.fromList
                    [ neighbour
                    | point <- S.toList current
                    , heightAt point == level
                    , neighbour <- neighbours point
                    , heightAt neighbour == nextLevel
                    ]
        in (nextFront, nextLevel)

    heightAt point = grid ! point

    neighbours (x, y) =
        filter (inRange (bounds grid))
            [ (x + 1, y)
            , (x - 1, y)
            , (x, y + 1)
            , (x, y - 1)
            ]

renderFrame :: HeightMap -> Int -> (Int, FrontFrame) -> IO ()
renderFrame grid total (idx, FrontFrame level active) = do
    let (_, (maxX, maxY)) = bounds grid
    setCursorPosition 0 0
    putStrLn $ "Hoof It - height " ++ show level ++ " (" ++ show (idx + 1) ++ " / " ++ show total ++ ")"
    putStrLn "Part context: [Part 1] counts trailhead scores; [Part 2] uses the same wave for ratings."
    mapM_ putStrLn (renderRows maxX maxY active)
    putStrLn ""
    putStrLn $ "Active cells at this height: " ++ show (S.size active)
    putStrLn "Legend: digits show heights, [d] marks the current wavefront"
    threadDelay 150000
  where
    renderRows maxX maxY active =
        [ concat
            [ renderCell (x, y)
            | x <- [0 .. maxX]
            ]
        | y <- [0 .. maxY]
        ]

    renderCell point =
        let digit = intToDigit (grid ! point)
        in if point `S.member` active
              then '[' : digit : "]"
              else ' ' : digit : " "
