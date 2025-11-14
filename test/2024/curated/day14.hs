#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package split-0.2.3.5
-}

{-|
Advent of Code 2024 - Day 14: Restroom Redoubt

This solution demonstrates:
- Simulating multiple moving entities (robots)
- Coordinate wrapping with modular arithmetic
- Tracking positions over time
- Quadrant analysis for spatial distribution
- Pattern detection in iterative simulation

Problem: Robots move in a rectangular space with wrapping at edges.
Each robot has a position and constant velocity.

Part 1: After 100 seconds, count robots in each quadrant (excluding middle)
        and compute safety factor (product of quadrant counts).

Part 2: Find the first time when robots form a specific pattern
        (Easter egg - a Christmas tree picture).
-}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.List (isPrefixOf)
import Text.Read (readMaybe)

type Point = (Int, Int)
type Velocity = (Int, Int)
type Robot = (Point, Velocity)

main :: IO ()
main = do
    -- Example input (smaller grid for demonstration)
    let exampleInput = unlines
          [ "p=0,4 v=3,-3"
          , "p=6,3 v=-1,-3"
          , "p=10,3 v=-1,2"
          , "p=2,0 v=2,-1"
          , "p=0,0 v=1,3"
          , "p=3,0 v=-2,-2"
          , "p=7,6 v=-1,-3"
          , "p=3,0 v=-1,-2"
          , "p=9,3 v=2,3"
          , "p=7,3 v=-1,2"
          , "p=2,4 v=2,-3"
          , "p=9,5 v=-3,-3"
          ]

    putStrLn "=== Day 14: Restroom Redoubt ===\n"

    -- Parse robots
    let robots = parseRobots exampleInput
    putStrLn $ "Robots: " ++ show (length robots)

    -- Example uses 11x7 grid, actual input uses 101x103
    let width = 11
        height = 7

    putStrLn $ "Space: " ++ show width ++ "x" ++ show height
    putStrLn ""

    -- Part 1: Simulate 100 seconds
    let finalPositions = map (simulateRobot width height 100) robots
    let safetyFactor = calculateSafetyFactor width height finalPositions

    putStrLn $ "Part 1: After 100 seconds"
    putStrLn $ "  Safety factor: " ++ show safetyFactor

    -- Show quadrant distribution
    let (q1, q2, q3, q4) = countQuadrants width height finalPositions
    putStrLn $ "  Quadrants: NE=" ++ show q1 ++ ", NW=" ++ show q2
    putStrLn $ "             SE=" ++ show q3 ++ ", SW=" ++ show q4
    putStrLn ""

    -- Part 2: Find pattern (Easter egg detection)
    -- In the real problem, this finds when robots form a Christmas tree
    -- For demonstration, we'll look for when robots form a connected cluster
    let patternTime = findPattern width height robots 10000
    putStrLn $ "Part 2: Pattern detected at second: " ++ show patternTime
    putStrLn ""

    -- Visualize initial state
    putStrLn "Initial state (first 3 seconds):"
    mapM_ (\t -> do
        let positions = map (simulateRobot width height t) robots
        putStrLn $ "Second " ++ show t ++ ":"
        putStrLn $ visualizeGrid width height positions
        ) [0..2]

-- | Parse robot specifications
parseRobots :: String -> [Robot]
parseRobots input = map parseRobot (lines input)
  where
    parseRobot line =
        let parts = words line
            pPart = dropWhile (/= '=') (parts !! 0)
            vPart = dropWhile (/= '=') (parts !! 1)
            parseCoord s = let [x, y] = splitOn "," (drop 1 s)
                          in (read x, read y)
            pos = parseCoord pPart
            vel = parseCoord vPart
        in (pos, vel)

-- | Simulate a robot for n seconds with wrapping
simulateRobot :: Int -> Int -> Int -> Robot -> Point
simulateRobot width height seconds ((px, py), (vx, vy)) =
    let -- Calculate final position with wrapping
        finalX = (px + vx * seconds) `mod` width
        finalY = (py + vy * seconds) `mod` height
        -- Handle negative modulo
        wrappedX = if finalX < 0 then finalX + width else finalX
        wrappedY = if finalY < 0 then finalY + height else finalY
    in (wrappedX, wrappedY)

-- | Count robots in each quadrant (excluding middle lines)
countQuadrants :: Int -> Int -> [Point] -> (Int, Int, Int, Int)
countQuadrants width height positions =
    let midX = width `div` 2
        midY = height `div` 2

        -- Filter out robots on middle lines
        filtered = filter (\(x, y) -> x /= midX && y /= midY) positions

        -- Count in each quadrant
        q1 = length [p | p@(x, y) <- filtered, x > midX && y < midY]  -- NE
        q2 = length [p | p@(x, y) <- filtered, x < midX && y < midY]  -- NW
        q3 = length [p | p@(x, y) <- filtered, x > midX && y > midY]  -- SE
        q4 = length [p | p@(x, y) <- filtered, x < midX && y > midY]  -- SW
    in (q1, q2, q3, q4)

-- | Calculate safety factor (product of quadrant counts)
calculateSafetyFactor :: Int -> Int -> [Point] -> Int
calculateSafetyFactor width height positions =
    let (q1, q2, q3, q4) = countQuadrants width height positions
    in q1 * q2 * q3 * q4

-- | Find when robots form a pattern (connected cluster)
-- In the real problem, this detects a Christmas tree pattern
findPattern :: Int -> Int -> [Robot] -> Int -> Int
findPattern width height robots maxSeconds =
    let -- For each second, check if robots form a connected cluster
        checkSecond t =
            let positions = map (simulateRobot width height t) robots
                -- A pattern is detected when robots have unusually high clustering
                -- (measured by having many neighbors)
                hasCluster = detectCluster positions
            in hasCluster

        -- Find first second where pattern appears
        results = [(t, checkSecond t) | t <- [0..maxSeconds]]
        firstPattern = head ([t | (t, True) <- results] ++ [maxSeconds])
    in firstPattern

-- | Detect if positions form a cluster (many adjacent robots)
detectCluster :: [Point] -> Bool
detectCluster positions =
    let posSet = S.fromList positions
        -- Count robots that have at least one neighbor
        neighborsCount = length [p | p <- positions, hasNeighbor posSet p]
        -- If more than 50% have neighbors, consider it clustered
    in neighborsCount > (length positions `div` 2)

-- | Check if a position has an adjacent neighbor
hasNeighbor :: S.Set Point -> Point -> Bool
hasNeighbor posSet (x, y) =
    let neighbors = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    in any (`S.member` posSet) neighbors

-- | Visualize grid with robot positions
visualizeGrid :: Int -> Int -> [Point] -> String
visualizeGrid width height positions =
    let posMap = M.fromListWith (+) [(p, 1) | p <- positions]
        rows = [[posMap M.! (x, y) | x <- [0..width-1]] | y <- [0..height-1]]
        showCell count = if count == 0 then '.' else head (show count)
        showRow row = map showCell row
    in unlines [showRow row | row <- rows]
      where
        posMap = M.fromListWith (+) [(p, 1) | p <- positions]
        getCount x y = M.findWithDefault 0 (x, y) posMap

-- Example expected outputs:
-- Part 1: Safety factor depends on robot distribution after 100 seconds
--         For the example: 12 (computed from quadrants)
-- Part 2: Pattern appears at specific second (varies by input)
--         Real input: Around 7000-8000 seconds
