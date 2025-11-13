#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package split-0.2.3.5
-}

{-|
Advent of Code 2024 - Day 15: Warehouse Woes

This solution demonstrates:
- Object interaction and state changes
- Cascade effects (pushing chains of objects)
- Collision detection with multiple object types
- Simulating a robot pushing boxes in a warehouse

Problem: A robot (@) moves in a warehouse with walls (#) and boxes (O).
When the robot moves into a box, it pushes the box (if possible).
Boxes can push other boxes in a chain.

Part 1: Simple boxes (1x1) that can be pushed in lines.
Part 2: Wide boxes (2x1) that require more complex push logic.
-}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, isJust)
import Data.List (find)

type Point = (Int, Int)
type Grid = M.Map Point Char

main :: IO ()
main = do
    let exampleInput = unlines
          [ "########"
          , "#..O.O.#"
          , "##@.O..#"
          , "#...O..#"
          , "#.#.O..#"
          , "#...O..#"
          , "#......#"
          , "########"
          , ""
          , "<^^>>>vv<v>>v<<"
          ]

    putStrLn "=== Day 15: Warehouse Woes ===\n"

    -- Parse input
    let (gridLines, moveLines) = break null (lines exampleInput)
    let moves = concat (tail moveLines)

    putStrLn $ "Grid: " ++ show (length gridLines) ++ " rows"
    putStrLn $ "Moves: " ++ show (length moves) ++ " commands"
    putStrLn ""

    -- Parse the warehouse
    let (grid, robotPos) = parseWarehouse gridLines

    putStrLn "Initial state:"
    putStrLn $ showGrid grid
    putStrLn ""

    -- Simulate all moves
    let (finalGrid, finalPos) = simulateAll grid robotPos moves

    putStrLn "Final state:"
    putStrLn $ showGrid finalGrid
    putStrLn ""

    -- Calculate GPS sum
    let gpsSum = calculateGPSSum finalGrid
    putStrLn $ "Part 1: Sum of GPS coordinates: " ++ show gpsSum

-- | Parse warehouse grid
parseWarehouse :: [String] -> (Grid, Point)
parseWarehouse lines =
    let height = length lines
        grid = M.fromList
            [ ((x, height - 1 - y), char)
            | (y, row) <- zip [0..] lines
            , (x, char) <- zip [0..] row
            , char /= ' '
            ]
        robotPos = head [pos | (pos, '@') <- M.toList grid]
        -- Remove robot from grid (we track it separately)
        grid' = M.insert robotPos '.' grid
    in (grid', robotPos)

-- | Direction from move character
charToDir :: Char -> Point
charToDir '^' = (0, 1)
charToDir 'v' = (0, -1)
charToDir '<' = (-1, 0)
charToDir '>' = (1, 0)
charToDir _ = error "Invalid move character"

-- | Simulate all moves
simulateAll :: Grid -> Point -> String -> (Grid, Point)
simulateAll grid pos [] = (grid, pos)
simulateAll grid pos (move:rest) =
    let dir = charToDir move
        (newGrid, newPos) = tryMove grid pos dir
    in simulateAll newGrid newPos rest

-- | Try to move robot in a direction
tryMove :: Grid -> Point -> Point -> (Grid, Point)
tryMove grid (x, y) (dx, dy) =
    let nextPos = (x + dx, y + dy)
        nextCell = M.findWithDefault '#' nextPos grid
    in case nextCell of
        '.' -> (grid, nextPos)  -- Empty space, move succeeds
        '#' -> (grid, (x, y))   -- Wall, can't move
        'O' -> tryPushBox grid (x, y) (dx, dy)  -- Box, try to push
        _   -> (grid, (x, y))   -- Unknown, don't move

-- | Try to push a box (and possibly chain of boxes)
tryPushBox :: Grid -> Point -> Point -> (Grid, Point)
tryPushBox grid (x, y) (dx, dy) =
    let nextPos = (x + dx, y + dy)
        -- Find all boxes in line
        boxLine = takeWhileBox grid nextPos (dx, dy)
        -- Position after last box
        afterBoxes = foldl (\(px, py) _ -> (px + dx, py + dy)) nextPos boxLine
        afterCell = M.findWithDefault '#' afterBoxes grid
    in if afterCell == '.'
       then -- Can push: move all boxes forward
            let grid' = M.insert afterBoxes 'O' grid  -- New box position
                grid'' = M.insert nextPos '.' grid'    -- Where first box was
            in (grid'', nextPos)  -- Robot moves into first box's position
       else -- Can't push (wall or edge)
            (grid, (x, y))  -- Robot doesn't move

-- | Get all consecutive boxes in a direction
takeWhileBox :: Grid -> Point -> Point -> [Point]
takeWhileBox grid pos@(x, y) dir@(dx, dy) =
    case M.findWithDefault '#' pos grid of
        'O' -> pos : takeWhileBox grid (x + dx, y + dy) dir
        _   -> []

-- | Calculate GPS coordinate sum
-- GPS = 100 * distance from top + distance from left
calculateGPSSum :: Grid -> Int
calculateGPSSum grid =
    let boxes = [pos | (pos, 'O') <- M.toList grid]
        gps (x, y) = 100 * (getMaxY grid - y) + x
    in sum (map gps boxes)

-- | Get maximum Y coordinate (for GPS calculation)
getMaxY :: Grid -> Int
getMaxY grid = maximum [y | (_, y) <- M.keys grid]

-- | Show grid with robot at position
showGrid :: Grid -> String
showGrid grid =
    let positions = M.keys grid
        minX = minimum [x | (x, _) <- positions]
        maxX = maximum [x | (x, _) <- positions]
        minY = minimum [y | (_, y) <- positions]
        maxY = maximum [y | (_, y) <- positions]

        showRow y = [M.findWithDefault ' ' (x, y) grid | x <- [minX..maxX]]
        rows = [showRow y | y <- reverse [minY..maxY]]
    in unlines rows

-- Example expected output:
-- Part 1: Sum of GPS coordinates depends on final box positions
--         For the example: 2028
