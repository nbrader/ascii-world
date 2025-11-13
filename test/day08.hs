#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package split-0.2.3.5
-}

{-|
Advent of Code 2024 - Day 8: Resonant Collinearity

This solution demonstrates:
- Working with multiple distinct point sets (antenna frequencies)
- Mathematical operations on coordinates
- Finding collinear points and antinodes
- Combining results from multiple calculations using bitwise OR

Problem: Given a grid with antennas marked by frequency (0-9, a-z, A-Z),
find antinode positions created by pairs of antennas with the same frequency.

Part 1: An antinode occurs at any point exactly in line with two antennas
        of the same frequency, where one antenna is twice as far as the other.

Part 2: An antinode occurs at any grid position exactly in line with at least
        two antennas of the same frequency (including the antenna positions).
-}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)
import Mask
import AsciiWorld
import WalkableWorld

main :: IO ()
main = do
    -- Example map
    let exampleInput = unlines
          [ "............"
          , "........0..."
          , ".....0......"
          , ".......0...."
          , "....0......."
          , "......A....."
          , "............"
          , "............"
          , "........A..."
          , ".........A.."
          , "............"
          , "............"
          ]

    putStrLn "=== Day 8: Resonant Collinearity ===\n"

    -- Parse the grid - each antenna frequency gets its own point layer
    let charMap c
          | isAlphaNum c = Just (PointsIndex [c])
          | otherwise    = Nothing

    let (height, world) = readWorld charMap exampleInput
    let width = asciiWorldWidth world

    putStrLn $ "Grid: " ++ show width ++ "x" ++ show height

    -- Get all antenna frequencies and their positions
    let antennaMap = asciiWorldPoints world
    let frequencies = M.keys antennaMap

    putStrLn $ "Frequencies found: " ++ show (length frequencies)
    mapM_ (\freq -> do
        let positions = fromMaybe [] $ M.lookup freq antennaMap
        putStrLn $ "  " ++ show freq ++ ": " ++ show (length positions) ++ " antennas"
        ) frequencies
    putStrLn ""

    -- Part 1: Find antinodes at 2:1 distance ratio
    let antinodesP1 = findAntinodes width height antennaMap False
    let countP1 = popCount antinodesP1

    putStrLn $ "Part 1: Unique antinode locations: " ++ show countP1

    -- Part 2: Find all collinear points
    let antinodesP2 = findAntinodes width height antennaMap True
    let countP2 = popCount antinodesP2

    putStrLn $ "Part 2: Unique antinode locations (with harmonics): " ++ show countP2
    putStrLn ""

    -- Show some example antinode positions
    let sampleAntinodes = take 10 $ maskToPoints width antinodesP1
    putStrLn "Sample antinode positions (Part 1, first 10):"
    mapM_ (putStrLn . ("  " ++) . show) sampleAntinodes

-- | Find all antinodes created by antenna pairs
--
-- Part 1 (harmonics=False): Only the 2:1 ratio points
-- Part 2 (harmonics=True): All collinear points including antennas
findAntinodes :: Int -> Int -> M.Map String [Point] -> Bool -> Mask
findAntinodes width height antennaMap withHarmonics =
    let -- For each frequency, find all antinodes from pairs
        frequencyAntinodes = M.mapWithKey processFrequency antennaMap

        -- Combine all antinodes from all frequencies using bitwise OR
        allAntinodes = M.foldr bitwiseOr 0 frequencyAntinodes
    in allAntinodes
  where
    processFrequency freq positions =
        let -- Get all pairs of antennas with this frequency
            pairs = [(p1, p2) | p1 <- positions, p2 <- positions, p1 /= p2]

            -- For each pair, find antinodes
            antinodeSets = map (findAntinodesForPair width height withHarmonics) pairs

            -- Combine all antinode masks
        in foldr bitwiseOr 0 antinodeSets

-- | Find antinodes for a specific pair of antennas
findAntinodesForPair :: Int -> Int -> Bool -> (Point, Point) -> Mask
findAntinodesForPair width height withHarmonics (p1@(x1, y1), p2@(x2, y2)) =
    let -- Calculate the displacement vector from p1 to p2
        dx = x2 - x1
        dy = y2 - y1

        -- Part 1: Only the 2:1 ratio points
        simpleAntinodes =
            let -- Antinode beyond p2 (p2 + displacement)
                beyond2 = (x2 + dx, y2 + dy)
                -- Antinode before p1 (p1 - displacement)
                before1 = (x1 - dx, y1 - dy)

                validPoints = filter (inBounds width height) [beyond2, before1]
            in foldr (\p acc -> bitwiseOr acc (pointToMask width p)) 0 validPoints

        -- Part 2: All collinear points (including antennas)
        harmonicAntinodes =
            let -- Generate all points along the line in both directions
                -- Start from p1 and go backwards
                backwards = takeWhile (inBounds width height)
                    [(x1 - n*dx, y1 - n*dy) | n <- [0..]]
                -- Start from p1 and go forwards
                forwards = takeWhile (inBounds width height)
                    [(x1 + n*dx, y1 + n*dy) | n <- [0..]]

                allPoints = backwards ++ tail forwards  -- tail to avoid duplicate p1
            in foldr (\p acc -> bitwiseOr acc (pointToMask width p)) 0 allPoints

    in if withHarmonics then harmonicAntinodes else simpleAntinodes

-- | Check if a point is within grid bounds
inBounds :: Int -> Int -> Point -> Bool
inBounds width height (x, y) =
    x >= 0 && x < width && y >= 0 && y < height

-- Example expected outputs:
-- For the given example:
-- Part 1: 14 unique antinode locations
-- Part 2: 34 unique antinode locations (includes antennas and all harmonics)
