#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19

{-|
Tutorial 2: Region Detection (Connected Components)
====================================================

This tutorial teaches you how to:
1. Find all connected regions in a grid
2. Count the number of regions
3. Measure region properties (area, perimeter)
4. Use WalkableWorld for advanced analysis
-}

import WalkableWorld
import qualified Data.Map as M
import Data.Bits (popCount)

-- Parse different garden plot types
charMap :: Char -> Maybe (MaskOrPointsIndex Char Char)
charMap c | c >= 'A' && c <= 'Z' = Just (MaskIndex c)
          | otherwise = Nothing

-- Example garden with different plant regions
gardenMap :: String
gardenMap = unlines
    [ "AAAA"
    , "BBCD"
    , "BBCC"
    , "EEEC" ]

main :: IO ()
main = do
    putStrLn "=== Tutorial 2: Region Detection ==="
    putStrLn ""

    -- Parse the garden
    let world = readWorld charMap gardenMap

    putStrLn "Original garden:"
    putStrLn gardenMap

    -- Find all plant types
    let plantTypes = maskIndices world
    putStrLn $ "Plant types found: " ++ show plantTypes
    putStrLn ""

    -- Partition each plant type into connected regions
    let allRegions = partitionAllMasksByReachableLRDU world

    putStrLn "Connected regions for each plant type:"
    M.forM_ allRegions $ \(plantType, regions) -> do
        putStrLn $ "\nPlant " ++ [plantType] ++ ": " ++ show (length regions) ++ " region(s)"

        -- Show details for each region
        mapM_ (\(i, mask) -> do
            let area = popCount mask
            putStrLn $ "  Region " ++ show i ++ ": " ++ show area ++ " cells"
          ) (zip [1..] regions)

    putStrLn ""
    putStrLn "Analyzing region properties:"
    putStrLn ""

    -- Calculate area and perimeter for each region
    forM_ plantTypes $ \plantType -> do
        let area = totalPoints plantType world
            perimeter = totalEdgesOverPoints plantType world

        putStrLn $ "Plant " ++ [plantType] ++ ":"
        putStrLn $ "  Total area: " ++ show area
        putStrLn $ "  Total perimeter: " ++ show perimeter
        putStrLn $ "  Score (area × perimeter): " ++ show (area * perimeter)
        putStrLn ""

    putStrLn "Key Takeaways:"
    putStrLn "  1. partitionAllMasksByReachableLRDU finds connected components"
    putStrLn "  2. Regions only connect horizontally/vertically (not diagonally)"
    putStrLn "  3. totalPoints counts cells in a region"
    putStrLn "  4. totalEdgesOverPoints counts perimeter edges"
    putStrLn "  5. popCount on a Mask gives you the number of active cells"

-- Import needed for M.forM_
import Control.Monad (forM_)
