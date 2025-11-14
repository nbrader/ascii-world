#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package split-0.2.3.5
-}

{-|
Advent of Code 2024 - Day 4: Ceres Search

This solution demonstrates:
- Grid representation as 2D character array
- Multi-directional pattern searching (8 directions)
- Pattern matching at specific positions
- Using grid lookups for character access

Problem: Find occurrences of "XMAS" in a word search grid.

Part 1: Count all occurrences of "XMAS" in any of 8 directions:
        horizontal, vertical, and diagonal (forwards and backwards).

Part 2: Count all occurrences of X-MAS patterns, where two "MAS"
        strings form an X shape (each "MAS" can be forwards or backwards).
-}

module Main where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)

-- We'll use a simple character grid representation
type Grid = M.Map (Int, Int) Char
type Point = (Int, Int)

main :: IO ()
main = do
    -- Example word search grid
    let exampleInput = unlines
          [ "MMMSXXMASM"
          , "MSAMXMSMSA"
          , "AMXSXMAAMM"
          , "MSAMASMSMX"
          , "XMASAMXAMM"
          , "XXAMMXXAMA"
          , "SMSMSASXSS"
          , "SAXAMASAAA"
          , "MAMMMXMMMM"
          , "MXMXAXMASX"
          ]

    putStrLn "=== Day 4: Ceres Search ===\n"

    -- Parse the grid
    let (grid, width, height) = parseGrid exampleInput

    putStrLn $ "Grid: " ++ show width ++ "x" ++ show height
    putStrLn $ "Total cells: " ++ show (M.size grid)
    putStrLn ""

    -- Part 1: Find all "XMAS" in 8 directions
    let xmasCount = countXMAS grid width height
    putStrLn $ "Part 1: 'XMAS' found: " ++ show xmasCount ++ " times"

    -- Part 2: Find all X-MAS patterns
    let xmasPatternCount = countXMASPattern grid width height
    putStrLn $ "Part 2: X-MAS patterns found: " ++ show xmasPatternCount ++ " times"
    putStrLn ""

    -- Show some example findings
    let xPositions = findAllX grid width height
    putStrLn $ "Found " ++ show (length xPositions) ++ " 'X' characters (potential XMAS starts)"
    putStrLn "Sample positions (first 5):"
    mapM_ (putStrLn . ("  " ++) . show) (take 5 xPositions)

-- | Parse input into a grid map
parseGrid :: String -> (Grid, Int, Int)
parseGrid input =
    let rows = lines input
        height = length rows
        width = if null rows then 0 else length (head rows)

        -- Build map with (x, y) -> Char
        -- Using (0,0) at bottom-left to match library convention
        grid = M.fromList
            [ ((x, height - 1 - y), char)
            | (y, row) <- zip [0..] rows
            , (x, char) <- zip [0..] row
            ]
    in (grid, width, height)

-- | Get character at position (returns Nothing if out of bounds)
getAt :: Grid -> Point -> Maybe Char
getAt grid pos = M.lookup pos grid

-- | All 8 directions for searching
directions :: [Point]
directions =
    [ (1, 0)    -- Right
    , (-1, 0)   -- Left
    , (0, 1)    -- Up
    , (0, -1)   -- Down
    , (1, 1)    -- Up-Right
    , (1, -1)   -- Down-Right
    , (-1, 1)   -- Up-Left
    , (-1, -1)  -- Down-Left
    ]

-- | Check if a word exists starting at a position in a given direction
checkWord :: Grid -> Point -> Point -> String -> Bool
checkWord grid (x, y) (dx, dy) word =
    let positions = [(x + i*dx, y + i*dy) | i <- [0..length word - 1]]
        chars = map (getAt grid) positions
        expectedChars = map Just word
    in chars == expectedChars

-- | Find all positions where a character appears
findChar :: Grid -> Char -> [Point]
findChar grid char = [pos | (pos, c) <- M.toList grid, c == char]

-- | Count all occurrences of "XMAS" in any direction
countXMAS :: Grid -> Int -> Int -> Int
countXMAS grid width height =
    let -- Find all 'X' positions (start of XMAS)
        xPositions = findChar grid 'X'

        -- For each X position, check all 8 directions
        checkFromPosition pos = length $ filter (checkWord grid pos) directions
          where
            checkWord' = \dir -> checkWord grid pos dir "XMAS"

        counts = map (\pos -> length [dir | dir <- directions, checkWord grid pos dir "XMAS"]) xPositions
    in sum counts

-- | Find all 'X' positions for debugging
findAllX :: Grid -> Int -> Int -> [Point]
findAllX grid _ _ = findChar grid 'X'

-- | Count X-MAS patterns (two "MAS" forming an X)
-- Pattern looks like:
--   M.S    or    M.M    or    S.M    or    S.S
--   .A.          .A.          .A.          .A.
--   M.S          S.S          S.M          M.M
--
-- The 'A' is at the center, and we need "MAS" or "SAM" on both diagonals
countXMASPattern :: Grid -> Int -> Int -> Int
countXMASPattern grid width height =
    let -- Find all 'A' positions (center of X)
        aPositions = findChar grid 'A'

        -- Check if an X-MAS pattern exists centered at this A
        isXMASAt (x, y) =
            let -- Check diagonal from top-left to bottom-right
                topLeft = getAt grid (x-1, y+1)
                bottomRight = getAt grid (x+1, y-1)
                diag1 = case (topLeft, bottomRight) of
                    (Just 'M', Just 'S') -> True
                    (Just 'S', Just 'M') -> True
                    _ -> False

                -- Check diagonal from top-right to bottom-left
                topRight = getAt grid (x+1, y+1)
                bottomLeft = getAt grid (x-1, y-1)
                diag2 = case (topRight, bottomLeft) of
                    (Just 'M', Just 'S') -> True
                    (Just 'S', Just 'M') -> True
                    _ -> False

            in diag1 && diag2

        validPatterns = filter isXMASAt aPositions
    in length validPatterns

-- Example expected outputs:
-- For the given example:
-- Part 1: 18 occurrences of "XMAS"
-- Part 2: 9 occurrences of X-MAS pattern
