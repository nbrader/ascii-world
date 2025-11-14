#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package split-0.2.3.5
-}

{-|
Advent of Code 2024 - Day 16: Reindeer Maze

This solution demonstrates:
- Dijkstra's algorithm with directional state
- Priority queue implementation for pathfinding
- Grid parsing and representation
- Cost-based pathfinding (movement + rotation costs)

Problem: A reindeer navigates through a maze from S to E.
- Moving forward costs 1 point
- Rotating 90 degrees (clockwise or counterclockwise) costs 1000 points
- The reindeer starts at S facing East
- Find the lowest score to reach E

Part 1: Find the minimum cost path from S to E
Part 2: Count all tiles that are part of any optimal path
-}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust, isNothing, fromMaybe)
import Data.List (minimumBy, sortBy)
import Data.Ord (comparing)
import Mask
import AsciiWorld
import WalkableWorld

type Direction = Point
type State = (Point, Direction)  -- (position, direction)
type Cost = Int

-- Priority queue entry: (cost, state, path)
type PQEntry = (Cost, State, [Point])

main :: IO ()
main = do
    -- Read example input
    contents <- readFile "test/2024/day16 (example).csv"

    putStrLn "=== Day 16: Reindeer Maze ===\n"

    -- Parse the grid
    let charMap '#' = Just (MaskIndex "walls")
        charMap 'S' = Just (PointsIndex "start")
        charMap 'E' = Just (PointsIndex "end")
        charMap '.' = Nothing
        charMap _   = Nothing

    let (height, world) = readAsciiWorld charMap contents
    let width = asciiWorldWidth world

    -- Get start and end positions
    let startPos = case M.lookup "start" (asciiWorldPoints world) of
          Just [pos] -> pos
          _ -> error "Start position not found"

    let endPos = case M.lookup "end" (asciiWorldPoints world) of
          Just [pos] -> pos
          _ -> error "End position not found"

    -- Get walls mask
    let wallsMask = fromMaybe 0 $ M.lookup "walls" (asciiWorldMasks world)

    -- Reindeer starts facing East
    let startDir = (1, 0)
    let startState = (startPos, startDir)

    putStrLn $ "Grid: " ++ show width ++ "x" ++ show height
    putStrLn $ "Start: " ++ show startPos ++ " (facing East)"
    putStrLn $ "End: " ++ show endPos
    putStrLn $ "Walls: " ++ show (popCount wallsMask) ++ " positions\n"

    -- Part 1: Find minimum cost path
    let (minCost, bestPaths) = dijkstra width height wallsMask startState endPos

    putStrLn $ "Part 1: Minimum cost: " ++ show minCost

    -- Part 2: Count all tiles on any optimal path
    let tilesOnOptimalPaths = S.fromList $ concat bestPaths
    putStrLn $ "Part 2: Tiles on optimal paths: " ++ show (S.size tilesOnOptimalPaths)
    putStrLn ""

-- | Dijkstra's algorithm with directional state
-- Returns (minimum cost, list of all optimal paths)
dijkstra :: Int -> Int -> Mask -> State -> Point -> (Cost, [[Point]])
dijkstra width height walls startState@(startPos, startDir) endPos = go initialQueue S.empty M.empty Nothing []
  where
    -- Priority queue starts with initial state
    initialQueue = [(0, startState, [startPos])]

    go :: [PQEntry] -> S.Set State -> M.Map State Cost -> Maybe Cost -> [[Point]] -> (Cost, [[Point]])
    go [] _ _ Nothing paths = (maxBound, [])  -- No path found
    go [] _ _ (Just cost) paths = (cost, paths)  -- Finished
    go queue visited costs bestCost paths
      | null queue = (fromJust bestCost, paths)
      | otherwise =
          let -- Get lowest cost entry
              (currentCost, currentState@(pos, dir), path) = minimumBy (comparing (\(c,_,_) -> c)) queue
              restQueue = filter (\e -> e /= (currentCost, currentState, path)) queue

              -- Check if we should process this state
              shouldProcess =
                case M.lookup currentState costs of
                  Nothing -> True
                  Just prevCost -> currentCost <= prevCost

              -- Check if we reached the end
              reachedEnd = pos == endPos

              -- Update best cost if reached end
              newBestCost = if reachedEnd
                then case bestCost of
                  Nothing -> Just currentCost
                  Just bc -> Just (min bc currentCost)
                else bestCost

              -- Add path if it's optimal
              newPaths = if reachedEnd && Just currentCost == newBestCost
                then path : paths
                else paths

              -- Skip if we already found better cost or this state was visited
              shouldContinue = shouldProcess &&
                (not reachedEnd || isNothing bestCost || currentCost <= fromJust bestCost)

          in if not shouldContinue || currentState `S.member` visited
             then go restQueue visited costs bestCost paths
             else
               let visited' = S.insert currentState visited
                   costs' = M.insert currentState currentCost costs

                   -- Generate neighbors: move forward, turn left, turn right
                   nextPos = movePoint dir pos

                   -- Try moving forward
                   moveForwardEntry =
                     if inBounds width height nextPos && not (isPointOverlappingMask width nextPos walls)
                     then [(currentCost + 1, (nextPos, dir), path ++ [nextPos])]
                     else []

                   -- Try turning left (counterclockwise)
                   turnLeftDir = turnLeft dir
                   turnLeftEntry = [(currentCost + 1000, (pos, turnLeftDir), path)]

                   -- Try turning right (clockwise)
                   turnRightDir = turnRight dir
                   turnRightEntry = [(currentCost + 1000, (pos, turnRightDir), path)]

                   -- Add all valid neighbors to queue
                   newQueue = restQueue ++ moveForwardEntry ++ turnLeftEntry ++ turnRightEntry

               in go newQueue visited' costs' newBestCost newPaths

-- | Turns a direction 90 degrees counterclockwise
turnLeft :: Direction -> Direction
turnLeft (0, 1)   = (-1, 0)  -- Up -> Left
turnLeft (-1, 0)  = (0, -1)  -- Left -> Down
turnLeft (0, -1)  = (1, 0)   -- Down -> Right
turnLeft (1, 0)   = (0, 1)   -- Right -> Up
turnLeft _ = error "Invalid direction"

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

-- Example expected outputs:
-- For the first example:
-- Part 1: 7036
-- Part 2: 45 tiles
--
-- For the second example:
-- Part 1: 11048
-- Part 2: 64 tiles
