#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19

{-|
Tutorial 4: Complete AoC-Style Solution
========================================

This tutorial shows a complete workflow for solving a grid-based puzzle:
Problem: Calculate the total "fence cost" for a garden
- Each region of same plant type needs fencing
- Cost = area × perimeter
- Regions are connected orthogonally (not diagonally)

This demonstrates the full power of ascii-world library!
-}

import WalkableWorld
import qualified Data.Map as M
import Data.Bits (popCount)
import Data.List (intercalate)

-- Step 1: Define the parsing function
charMap :: Char -> Maybe (MaskOrPointsIndex Char Char)
charMap c | c >= 'A' && c <= 'Z' = Just (MaskIndex c)
          | otherwise = Nothing

-- Step 2: Define test data
exampleGarden :: String
exampleGarden = unlines
    [ "AAAA"
    , "BBCD"
    , "BBCC"
    , "EEEC" ]

-- Step 3: Solve the problem
solvePart1 :: String -> Integer
solvePart1 input = sum regionScores
  where
    world = readWorld charMap input
    plantTypes = maskIndices world

    regionScores = [ totalPoints plantType world * totalEdgesOverPoints plantType world
                   | plantType <- plantTypes ]

-- Bonus: Part 2 with discount (count sides, not edges)
solvePart2 :: String -> Integer
solvePart2 input = sum regionScores
  where
    world = readWorld charMap input
    plantTypes = maskIndices world

    regionScores = [ totalPoints plantType world * totalConnectedOneSidedEdges plantType world
                   | plantType <- plantTypes ]

-- Step 4: Detailed analysis function
analyzeGarden :: String -> IO ()
analyzeGarden input = do
    let world = readWorld charMap input
        plantTypes = maskIndices world
        allRegions = partitionAllMasksByReachableLRDU world

    putStrLn "Garden Analysis:"
    putStrLn "================"
    putStrLn ""

    -- Show the garden
    putStrLn "Map:"
    putStr input
    putStrLn ""

    -- Analyze each plant type
    putStrLn "Plant Regions:"
    putStrLn "--------------"

    let details = [ (plantType, regions, totalPoints plantType world, totalEdgesOverPoints plantType world)
                  | plantType <- plantTypes
                  , let regions = M.findWithDefault [] plantType allRegions ]

    forM_ details $ \(plantType, regions, area, perimeter) -> do
        putStrLn $ "\nPlant " ++ [plantType] ++ ":"
        putStrLn $ "  Regions: " ++ show (length regions)
        putStrLn $ "  Total area: " ++ show area ++ " cells"
        putStrLn $ "  Total perimeter: " ++ show perimeter ++ " edges"
        putStrLn $ "  Fence cost: " ++ show (area * perimeter) ++ " (area × perimeter)"

        -- Show individual region sizes
        when (length regions > 1) $ do
            let sizes = map popCount regions
            putStrLn $ "  Individual region sizes: " ++ intercalate ", " (map show sizes)

    putStrLn ""
    putStrLn "Summary:"
    putStrLn "--------"
    putStrLn $ "Total plant types: " ++ show (length plantTypes)
    putStrLn $ "Total regions: " ++ show (sum [length regions | (_, regions) <- M.toList allRegions])
    putStrLn $ "Total fence cost (Part 1): " ++ show (solvePart1 input)
    putStrLn $ "Total discounted cost (Part 2): " ++ show (solvePart2 input)

-- Main program
main :: IO ()
main = do
    putStrLn "=== Tutorial 4: Complete Solution ==="
    putStrLn ""

    putStrLn "Example Problem:"
    putStrLn "----------------"
    putStrLn "Calculate fence costs for garden regions"
    putStrLn "Cost = area × perimeter for each region"
    putStrLn ""

    -- Analyze the example
    analyzeGarden exampleGarden

    putStrLn ""
    putStrLn "Solution Breakdown:"
    putStrLn "==================="
    putStrLn ""
    putStrLn "1. Parse Input → WalkableWorld"
    putStrLn "   readWorld charMap input"
    putStrLn ""
    putStrLn "2. Find Regions → Map Char [Mask]"
    putStrLn "   partitionAllMasksByReachableLRDU world"
    putStrLn ""
    putStrLn "3. Measure Properties → Integer"
    putStrLn "   totalPoints: counts cells in region"
    putStrLn "   totalEdgesOverPoints: counts perimeter edges"
    putStrLn ""
    putStrLn "4. Calculate Score → Integer"
    putStrLn "   sum [area * perimeter | region <- regions]"
    putStrLn ""
    putStrLn "Key Library Functions Used:"
    putStrLn "---------------------------"
    putStrLn "• readWorld - Parse ASCII into WalkableWorld"
    putStrLn "• maskIndices - Get all region types"
    putStrLn "• partitionAllMasksByReachableLRDU - Find connected components"
    putStrLn "• totalPoints - Count cells in region"
    putStrLn "• totalEdgesOverPoints - Count perimeter edges"
    putStrLn "• totalConnectedOneSidedEdges - Count distinct sides"
    putStrLn ""
    putStrLn "This pattern works for many grid puzzles:"
    putStrLn "Parse → Partition → Measure → Calculate!"

-- Import for when and forM_
import Control.Monad (when, forM_)
