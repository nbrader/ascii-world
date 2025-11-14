#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package split-0.2.3.5
-}

{-|
Advent of Code 2024 - Day 20: Race Condition

This solution demonstrates:
- BFS for finding the main path
- Manhattan distance for finding shortcuts
- Cheat detection through walls
- Path optimization by wall-passing

Problem: A race track with walls where you can "cheat" by passing through walls
for up to 2 picoseconds (Part 1) or up to 20 picoseconds (Part 2).
- Find the path from S to E
- Count cheats that save at least 100 picoseconds (or threshold for examples)

Part 1: Count cheats with duration ≤ 2 that save ≥ threshold time
Part 2: Count cheats with duration ≤ 20 that save ≥ threshold time
-}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Maybe (fromMaybe)
import Data.List (sort)

type Point = (Int, Int)
type Path = [Point]
type DistanceMap = M.Map Point Int

main :: IO ()
main = do
    -- Read example input
    contents <- readFile "test/day20 (example).csv"

    putStrLn "=== Day 20: Race Condition ===\n"

    -- Parse the track
    let (walls, start, end, width, height) = parseTrack contents

    putStrLn $ "Grid: " ++ show width ++ "x" ++ show height
    putStrLn $ "Start: " ++ show start
    putStrLn $ "End: " ++ show end
    putStrLn $ "Walls: " ++ show (S.size walls) ++ " positions\n"

    -- Find the main path without cheating
    case bfsWithDistances width height walls start end of
        Nothing -> putStrLn "No path found!"
        Just (path, distMap) -> do
            let pathLength = length path - 1
            putStrLn $ "Base path length: " ++ show pathLength
            putStrLn ""

            -- Part 1: Find cheats with duration ≤ 2 (example uses threshold of 1)
            let threshold1 = 1  -- Use lower threshold for example
                cheats1 = findCheats distMap path 2 threshold1
            putStrLn $ "Part 1: Cheats (≤2 picoseconds) saving ≥" ++ show threshold1 ++ ": " ++ show (length cheats1)

            -- Show distribution of savings
            let savings1 = map (\(_, _, saved) -> saved) cheats1
                distribution1 = countDistribution savings1
            putStrLn "Savings distribution:"
            mapM_ (\(saved, count) -> putStrLn $ "  " ++ show count ++ " cheats save " ++ show saved ++ " picoseconds") distribution1

            putStrLn ""

            -- Part 2: Find cheats with duration ≤ 20 (example uses threshold of 50)
            let threshold2 = 50
                cheats2 = findCheats distMap path 20 threshold2
            putStrLn $ "Part 2: Cheats (≤20 picoseconds) saving ≥" ++ show threshold2 ++ ": " ++ show (length cheats2)

            let savings2 = map (\(_, _, saved) -> saved) cheats2
                distribution2 = countDistribution savings2
            putStrLn "Savings distribution:"
            mapM_ (\(saved, count) -> putStrLn $ "  " ++ show count ++ " cheats save " ++ show saved ++ " picoseconds") (take 10 distribution2)

    putStrLn ""

-- | Parse track from input
parseTrack :: String -> (S.Set Point, Point, Point, Int, Int)
parseTrack contents = (walls, start, end, width, height)
  where
    rows = filter (not . null) . map (filter (/= '\r')) $ lines contents
    height = length rows
    width = if null rows then 0 else length (head rows)

    -- Parse grid (flip y for bottom-left origin)
    cells = [ ((x, height - 1 - y), char)
            | (y, row) <- zip [0..] rows
            , (x, char) <- zip [0..] row
            ]

    walls = S.fromList [pos | (pos, '#') <- cells]
    start = head [pos | (pos, 'S') <- cells]
    end = head [pos | (pos, 'E') <- cells]

-- | BFS to find path and create distance map
bfsWithDistances :: Int -> Int -> S.Set Point -> Point -> Point -> Maybe (Path, DistanceMap)
bfsWithDistances width height walls start end = go (Seq.singleton (start, 0, [start])) S.empty M.empty
  where
    go queue visited distMap
      | Seq.null queue = Nothing
      | otherwise =
          let ((pos, dist, path) Seq.:< restQueue) = Seq.viewl queue
              distMap' = M.insert pos dist distMap
          in
            if pos == end
              then Just (reverse path, distMap')
            else if pos `S.member` visited
              then go restQueue visited distMap
            else
              let visited' = S.insert pos visited
                  neighbors = getNeighbors width height walls pos
                  unvisitedNeighbors = filter (`S.notMember` visited') neighbors
                  newQueue = foldl (Seq.|>) restQueue [(n, dist + 1, n:path) | n <- unvisitedNeighbors]
              in go newQueue visited' distMap'

-- | Get valid neighbors (not walls, within bounds)
getNeighbors :: Int -> Int -> S.Set Point -> Point -> [Point]
getNeighbors width height walls (x, y) =
    [ (nx, ny)
    | (dx, dy) <- [(0, 1), (0, -1), (1, 0), (-1, 0)]
    , let nx = x + dx
    , let ny = y + dy
    , nx >= 0 && nx < width
    , ny >= 0 && ny < height
    , (nx, ny) `S.notMember` walls
    ]

-- | Find all cheats that save at least threshold time
findCheats :: DistanceMap -> Path -> Int -> Int -> [(Point, Point, Int)]
findCheats distMap path maxCheatDuration threshold =
    [ (startPos, endPos, timeSaved)
    | startPos <- path
    , endPos <- path
    , let startDist = fromMaybe 0 $ M.lookup startPos distMap
    , let endDist = fromMaybe 0 $ M.lookup endPos distMap
    , endDist > startDist  -- Only consider forward progress
    , let manhattan = manhattanDistance startPos endPos
    , manhattan > 0 && manhattan <= maxCheatDuration
    , let normalTime = endDist - startDist
    , let cheatTime = manhattan
    , let timeSaved = normalTime - cheatTime
    , timeSaved >= threshold
    ]

-- | Calculate Manhattan distance
manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Count distribution of values
countDistribution :: [Int] -> [(Int, Int)]
countDistribution values = M.toAscList $ M.fromListWith (+) [(v, 1) | v <- values]

-- Example expected outputs:
-- For the example:
-- Part 1 with threshold 1: Multiple cheats found
-- - There are 14 cheats that save 2 picoseconds
-- - There are 14 cheats that save 4 picoseconds
-- - etc.
