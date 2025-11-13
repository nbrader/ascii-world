#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19

{-|
Tutorial 3: Bitwise Operations & Mask Manipulation
==================================================

This tutorial teaches you how to:
1. Combine masks using bitwise operations
2. Move masks around the grid
3. Check for overlaps
4. Create custom transformations
-}

import AsciiWorld
import Mask
import qualified Data.Map as M

charMap :: Char -> Maybe (MaskOrPointsIndex String String)
charMap '#' = Just (MaskIndex "obstacles")
charMap '@' = Just (MaskIndex "player")
charMap 'E' = Just (MaskIndex "enemy")
charMap '.' = Nothing
charMap  _  = Nothing

-- Simple game map
gameMap :: String
gameMap = unlines
    [ "####"
    , "#@.#"
    , "#.E#"
    , "####" ]

main :: IO ()
main = do
    putStrLn "=== Tutorial 3: Bitwise Operations ==="
    putStrLn ""

    let (height, world) = readAsciiWorld charMap gameMap
        width = asciiWorldWidth world

    putStrLn "Original map:"
    printAsciiWorld height '.' head head compare world
    putStrLn ""

    -- Get masks
    let Just playerMask = M.lookup "player" (asciiWorldMasks world)
        Just enemyMask = M.lookup "enemy" (asciiWorldMasks world)
        Just obstacleMask = M.lookup "obstacles" (asciiWorldMasks world)

    putStrLn "=== Operation 1: Combining Masks ==="
    putStrLn "Combine player and enemy into 'entities'"

    let combinedMask = bitwiseOr playerMask enemyMask
        world1 = addMask "entities" combinedMask world

    case M.lookup "entities" (asciiWorldMasks world1) of
        Just m -> putStrLn $ "  Combined mask has " ++ show (popCount m) ++ " cells"
        Nothing -> return ()
    putStrLn ""

    putStrLn "=== Operation 2: Moving Masks ==="
    putStrLn "Move player right by 1"

    let movedPlayer = moveMask width (1, 0) playerMask
        world2 = addMask "player_moved" movedPlayer world

    putStrLn "  Before move: player at (1,2)"
    putStrLn "  After move:  player at (2,2)"
    putStrLn ""

    putStrLn "=== Operation 3: Checking Overlaps ==="
    putStrLn "Does moved player overlap with enemy?"

    if isOverlapping movedPlayer enemyMask
        then putStrLn "  ✓ Collision detected!"
        else putStrLn "  ✗ No collision"
    putStrLn ""

    putStrLn "Does moved player overlap with obstacles?"
    if isOverlapping movedPlayer obstacleMask
        then putStrLn "  ✓ Blocked by obstacle!"
        else putStrLn "  ✗ Move allowed"
    putStrLn ""

    putStrLn "=== Operation 4: Subtracting Masks ==="
    putStrLn "Remove obstacles from combined entities mask"

    let withoutObstacles = bitwiseSubtract combinedMask obstacleMask
        world3 = addMask "cleared" withoutObstacles world

    putStrLn $ "  Original cells: " ++ show (popCount combinedMask)
    putStrLn $ "  After removing obstacles: " ++ show (popCount withoutObstacles)
    putStrLn ""

    putStrLn "=== Operation 5: XOR (Symmetric Difference) ==="
    putStrLn "Find cells that are in player OR enemy, but not both"

    let xorMask = bitwiseXor playerMask enemyMask
    putStrLn $ "  Cells in exactly one: " ++ show (popCount xorMask)
    putStrLn "  (Useful for finding differences between frames)"
    putStrLn ""

    putStrLn "Key Takeaways:"
    putStrLn "  1. bitwiseOr: Combine masks (union)"
    putStrLn "  2. bitwiseAnd: Find common cells (intersection)"
    putStrLn "  3. bitwiseXor: Find differences (symmetric difference)"
    putStrLn "  4. bitwiseSubtract: Remove cells (set difference)"
    putStrLn "  5. moveMask: Shift entire layer in O(1)"
    putStrLn "  6. isOverlapping: Fast collision detection"
    putStrLn ""
    putStrLn "All operations work on entire layers at once!"
    putStrLn "This is much faster than iterating over individual cells."
