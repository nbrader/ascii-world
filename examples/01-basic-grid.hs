#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19

{-|
Tutorial 1: Basic Grid Operations
==================================

This tutorial teaches you how to:
1. Create an AsciiWorld from a string
2. Display the grid
3. Access and manipulate grid data
4. Understand the coordinate system
-}

import AsciiWorld
import qualified Data.Map as M

-- Step 1: Define how to parse characters
-- Each character maps to either a Mask (grid layer) or Points (individual positions)
charMap :: Char -> Maybe (MaskOrPointsIndex String String)
charMap '#' = Just (MaskIndex "walls")     -- Walls are a grid layer
charMap '@' = Just (PointsIndex "player")  -- Player is a single point
charMap '.' = Nothing                      -- Empty spaces are ignored
charMap  _  = Nothing                      -- Unknown characters ignored

-- Step 2: Create a simple maze
simpleMaze :: String
simpleMaze = unlines
    [ "#####"
    , "#.@.#"
    , "#...#"
    , "#####" ]

-- Step 3: Read the maze into an AsciiWorld
main :: IO ()
main = do
    putStrLn "=== Tutorial 1: Basic Grid Operations ==="
    putStrLn ""

    -- Parse the maze
    let (height, world) = readAsciiWorld charMap simpleMaze

    putStrLn $ "Grid dimensions: " ++ show (asciiWorldWidth world) ++ "x" ++ show height
    putStrLn ""

    -- Check what layers exist
    putStrLn "Layers in the grid:"
    putStrLn $ "  Masks (grid layers): " ++ show (M.keys $ asciiWorldMasks world)
    putStrLn $ "  Points (individual positions): " ++ show (M.keys $ asciiWorldPoints world)
    putStrLn ""

    -- Access the wall mask
    case M.lookup "walls" (asciiWorldMasks world) of
        Just wallMask -> do
            putStrLn $ "Wall mask (as integer): " ++ show wallMask
            putStrLn $ "Number of wall cells: " ++ show (popCount wallMask)
        Nothing -> putStrLn "No walls found!"
    putStrLn ""

    -- Access player position
    case M.lookup "player" (asciiWorldPoints world) of
        Just positions -> do
            putStrLn $ "Player position(s): " ++ show positions
            putStrLn ""
            putStrLn "Note: Coordinates are (x, y) where:"
            putStrLn "  - (0,0) is bottom-left"
            putStrLn "  - x increases rightward"
            putStrLn "  - y increases upward"
        Nothing -> putStrLn "No player found!"
    putStrLn ""

    -- Display the grid back
    putStrLn "Grid display:"
    let displayed = showAsciiWorld height '.'
                        (\maskKey -> head maskKey)  -- "walls" -> 'w'
                        (\pointKey -> head pointKey) -- "player" -> 'p'
                        compare  -- Simple ordering
                        world
    putStrLn displayed

    putStrLn ""
    putStrLn "Key Takeaways:"
    putStrLn "  1. charMap defines how to parse characters"
    putStrLn "  2. Masks store grid layers as integers"
    putStrLn "  3. Points store individual coordinates"
    putStrLn "  4. Coordinate system: (0,0) = bottom-left"
    putStrLn "  5. Y-axis is flipped compared to text file (bottom to top)"
