#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package split-0.2.3.5
-}

{-|
Advent of Code 2024 - Day 6: Guard Gallivant

This solution demonstrates:
- Grid parsing with character mapping
- Movement simulation using bitwise operations
- Collision detection
- Path tracking
- Cycle detection for infinite loops

Problem: A guard patrols a mapped area, following these rules:
1. If there's an obstacle directly in front, turn right 90 degrees
2. Otherwise, take a step forward
3. The guard starts facing up (^)

Part 1: Count distinct positions visited before leaving the mapped area
Part 2: Count positions where adding an obstacle would cause an infinite loop
-}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust, isJust)
import Mask
import AsciiWorld
import WalkableWorld
import Util (lrduDirs)

-- Direction represented as a point offset
type Direction = Point
type State = (Point, Direction)  -- (position, direction)

main :: IO ()
main = do
    -- Example map
    let exampleInput = unlines
          [ "....#....."
          , ".........#"
          , ".........."
          , "..#......."
          , ".......#.."
          , ".........."
          , ".#..^....."
          , "........#."
          , "#........."
          , "......#..."
          ]

    putStrLn "=== Day 6: Guard Gallivant ===\n"

    -- Parse the grid
    let charMap '#' = Just (MaskIndex "obstacles")
        charMap '^' = Just (PointsIndex "guard")
        charMap 'v' = Just (PointsIndex "guard")
        charMap '<' = Just (PointsIndex "guard")
        charMap '>' = Just (PointsIndex "guard")
        charMap _   = Nothing

    let (height, world) = readWorld charMap exampleInput
    let width = asciiWorldWidth world

    -- Get guard starting position
    let guardPos = case M.lookup "guard" (asciiWorldPoints world) of
          Just [pos] -> pos
          _ -> error "Guard not found or multiple guards"

    -- Get obstacles mask
    let obstaclesMask = fromJust $ M.lookup "obstacles" (asciiWorldMasks world)

    -- Guard starts facing up
    let startDir = (0, 1)
    let startState = (guardPos, startDir)

    putStrLn $ "Grid: " ++ show width ++ "x" ++ show height
    putStrLn $ "Guard starting at: " ++ show guardPos
    putStrLn $ "Obstacles: " ++ show (popCount obstaclesMask) ++ " positions\n"

    -- Part 1: Simulate guard patrol
    let (visited, exitedMap) = simulatePatrol width height obstaclesMask startState
    let visitedCount = S.size $ S.map fst visited  -- Count unique positions

    putStrLn $ "Part 1: Distinct positions visited: " ++ show visitedCount

    -- Part 2: Count positions where adding obstacle causes loop
    let loopPositions = findLoopCausingPositions width height obstaclesMask startState

    putStrLn $ "Part 2: Positions causing loops: " ++ show (S.size loopPositions)
    putStrLn ""

    -- Show the patrol path (limited to avoid clutter)
    putStrLn "Sample of visited positions (first 10):"
    mapM_ (putStrLn . ("  " ++) . show) $ take 10 $ S.toList $ S.map fst visited

-- | Simulates the guard's patrol
--
-- Returns (set of visited states, whether guard exited the map)
simulatePatrol :: Int -> Int -> Mask -> State -> (S.Set State, Bool)
simulatePatrol width height obstacles = go S.empty
  where
    go visited state@(pos, dir)
      -- Check for cycle
      | state `S.member` visited = (visited, False)  -- Loop detected
      | otherwise =
          let visited' = S.insert state visited
              nextPos = movePoint dir pos
          in
            -- Check if next position is out of bounds
            if not (inBounds width height nextPos)
              then (visited', True)  -- Exited map
            -- Check if obstacle ahead
            else if isPointOverlappingMask width nextPos obstacles
              then go visited' (pos, turnRight dir)  -- Turn right
              else go visited' (nextPos, dir)  -- Move forward

-- | Turns a direction 90 degrees clockwise
turnRight :: Direction -> Direction
turnRight (0, 1)   = (1, 0)   -- Up -> Right
turnRight (1, 0)   = (0, -1)  -- Right -> Down
turnRight (0, -1)  = (-1, 0)  -- Down -> Left
turnRight (-1, 0)  = (0, 1)   -- Left -> Up
turnRight _ = error "Invalid direction"

-- | Checks if a position is within bounds
inBounds :: Int -> Int -> Point -> Bool
inBounds width height (x, y) =
    x >= 0 && x < width && y >= 0 && y < height

-- | Finds all positions where adding an obstacle would cause a loop
findLoopCausingPositions :: Int -> Int -> Mask -> State -> S.Set Point
findLoopCausingPositions width height obstacles startState =
    let -- Get all positions the guard visits in normal patrol
        (normalPath, _) = simulatePatrol width height obstacles startState
        visitedPositions = S.map fst normalPath

        -- Remove guard's starting position (can't place obstacle there)
        candidatePositions = S.delete (fst startState) visitedPositions

        -- Test each candidate position
        testPosition pos =
            let newObstacles = bitwiseOr obstacles (pointToMask width pos)
                (_, exited) = simulatePatrol width height newObstacles startState
            in not exited  -- Returns True if loop detected

    in S.filter testPosition candidatePositions

-- Example expected outputs:
-- For the given example:
-- Part 1: 41 distinct positions
-- Part 2: 6 positions cause loops
