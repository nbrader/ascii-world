#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package split-0.2.3.5
-}

{-|
Advent of Code 2024 - Day 18: RAM Run

This solution demonstrates:
- Breadth-First Search (BFS) for shortest path
- Dynamic obstacle placement
- Binary search for finding blocking point
- Grid pathfinding with evolving constraints

Problem: Bytes are falling into a memory space grid at specific coordinates.
- Grid is 71x71 (0,0 to 70,70) for real input, 7x7 (0,0 to 6,6) for example
- Each byte falls at a specific coordinate
- Find shortest path from (0,0) to (70,70) or (6,6)

Part 1: After 1024 bytes have fallen (12 for example), find the shortest path
Part 2: Find the coordinates of the first byte that blocks all paths
-}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Maybe (isJust, fromJust)
import Data.List (find)
import Data.List.Split (splitOn)

type Point = (Int, Int)
type Grid = S.Set Point  -- Set of obstacles

main :: IO ()
main = do
    -- Read example input
    contents <- readFile "test/2024/day18 (example).csv"

    putStrLn "=== Day 18: RAM Run ===\n"

    -- Parse byte coordinates
    let byteCoords = parseBytes contents
        -- Use example grid size (7x7) and byte limit (12)
        gridSize = 7
        initialByteCount = 12
        startPos = (0, 0)
        endPos = (gridSize - 1, gridSize - 1)

    putStrLn $ "Grid size: " ++ show gridSize ++ "x" ++ show gridSize
    putStrLn $ "Total bytes: " ++ show (length byteCoords)
    putStrLn $ "Start: " ++ show startPos
    putStrLn $ "End: " ++ show endPos
    putStrLn ""

    -- Part 1: Find shortest path after initial bytes have fallen
    let initialObstacles = S.fromList $ take initialByteCount byteCoords
    case bfs gridSize initialObstacles startPos endPos of
        Nothing -> putStrLn "Part 1: No path found!"
        Just pathLength -> putStrLn $ "Part 1: Shortest path length: " ++ show pathLength

    -- Part 2: Find first byte that blocks all paths
    case findBlockingByte gridSize byteCoords startPos endPos of
        Nothing -> putStrLn "Part 2: No blocking byte found!"
        Just (x, y) -> putStrLn $ "Part 2: First blocking byte: " ++ show x ++ "," ++ show y

    putStrLn ""

-- | Parse byte coordinates from input
parseBytes :: String -> [Point]
parseBytes contents = map parseLine $ lines contents
  where
    parseLine line = case splitOn "," line of
        [xStr, yStr] -> (read xStr, read yStr)
        _ -> error $ "Invalid line: " ++ line

-- | Breadth-First Search to find shortest path
bfs :: Int -> Grid -> Point -> Point -> Maybe Int
bfs gridSize obstacles start end = go (Seq.singleton (start, 0)) S.empty
  where
    go queue visited
      | Seq.null queue = Nothing
      | otherwise =
          let ((pos, dist) Seq.:< restQueue) = Seq.viewl queue
          in
            if pos == end
              then Just dist
            else if pos `S.member` visited
              then go restQueue visited
            else
              let visited' = S.insert pos visited
                  neighbors = getNeighbors gridSize obstacles pos
                  unvisitedNeighbors = filter (`S.notMember` visited') neighbors
                  newQueue = foldl (Seq.|>) restQueue [(n, dist + 1) | n <- unvisitedNeighbors]
              in go newQueue visited'

-- | Get valid neighbors (not obstacles, within bounds)
getNeighbors :: Int -> Grid -> Point -> [Point]
getNeighbors gridSize obstacles (x, y) =
    [ (nx, ny)
    | (dx, dy) <- [(0, 1), (0, -1), (1, 0), (-1, 0)]
    , let nx = x + dx
    , let ny = y + dy
    , nx >= 0 && nx < gridSize
    , ny >= 0 && ny < gridSize
    , (nx, ny) `S.notMember` obstacles
    ]

-- | Find the first byte that blocks all paths using binary search
findBlockingByte :: Int -> [Point] -> Point -> Point -> Maybe Point
findBlockingByte gridSize bytes start end = binarySearch 0 (length bytes)
  where
    binarySearch low high
      | low >= high = Nothing
      | low + 1 == high =
          let obstacles = S.fromList $ take high bytes
          in if isJust (bfs gridSize obstacles start end)
             then Nothing
             else Just (bytes !! (high - 1))
      | otherwise =
          let mid = (low + high) `div` 2
              obstacles = S.fromList $ take mid bytes
          in case bfs gridSize obstacles start end of
               Just _ -> binarySearch mid high  -- Path exists, try later bytes
               Nothing -> binarySearch low mid  -- No path, try earlier bytes

-- Example expected outputs:
-- For the example with 7x7 grid and 12 bytes:
-- Part 1: 22 steps
-- Part 2: 6,1 (the first byte that blocks all paths)
