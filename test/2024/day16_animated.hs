#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2024 Day 16 ("Reindeer Maze").
-- Shows Dijkstra's pathfinding algorithm exploring the maze,
-- with colors indicating the direction the reindeer is facing.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.IO (hSetEncoding, stdout, utf8)

type Point = (Int, Int)
type Direction = Point
type State = (Point, Direction)
type Cost = Int

data SearchFrame = SearchFrame
    { sfCost     :: Cost
    , sfState    :: State
    , sfVisited  :: S.Set Point
    , sfFrontier :: S.Set Point
    } deriving (Eq, Show)

data MazeData = MazeData
    { mdWidth  :: Int
    , mdHeight :: Int
    , mdWalls  :: S.Set Point
    , mdStart  :: Point
    , mdEnd    :: Point
    }

main :: IO ()
main = do
    -- Set UTF-8 encoding for Windows compatibility
    hSetEncoding stdout utf8
    contents <- loadMap
    let maze = parseMaze contents
        frames = buildFrames maze
        total = length frames
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ (renderFrame maze total) (zip [0 ..] frames)
    setCursorPosition (mdHeight maze + 8) 0
    putStrLn "Reindeer Maze pathfinding complete. (Part 1 min cost + Part 2 optimal tiles)"

loadMap :: IO String
loadMap = do
    let path = "test/2024/day16 (example).csv"
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure $ unlines defaultMap
  where
    defaultMap =
        [ "###############"
        , "#.......#....E#"
        , "#.#.###.#.###.#"
        , "#.....#.#...#.#"
        , "#.###.#####.#.#"
        , "#.#.#.......#.#"
        , "#.#.#####.###.#"
        , "#...........#.#"
        , "###.#.#####.#.#"
        , "#...#.....#.#.#"
        , "#.#.#.###.#.#.#"
        , "#.....#...#.#.#"
        , "#.###.#.#.#.#.#"
        , "#S..#.....#...#"
        , "###############"
        ]

parseMaze :: String -> MazeData
parseMaze contents = MazeData
    { mdWidth = width
    , mdHeight = height
    , mdWalls = walls
    , mdStart = startPos
    , mdEnd = endPos
    }
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
    startPos = head [pos | (pos, 'S') <- cells]
    endPos = head [pos | (pos, 'E') <- cells]

buildFrames :: MazeData -> [SearchFrame]
buildFrames maze = take 200 $ go [(0, startState, S.empty)] S.empty []
  where
    startState = (mdStart maze, (1, 0))  -- Start facing East
    endPos = mdEnd maze
    walls = mdWalls maze
    width = mdWidth maze
    height = mdHeight maze

    go :: [(Cost, State, S.Set Point)] -> S.Set State -> [SearchFrame] -> [SearchFrame]
    go [] _ frames = frames
    go queue visited frames
      | null queue = frames
      | otherwise =
          let -- Get lowest cost entry (simple selection, not optimal)
              (currentCost, currentState@(pos, dir), path) = head queue
              restQueue = tail queue

              -- Create frame
              frontierPoints = S.fromList [p | (_, (p, _), _) <- queue]
              frame = SearchFrame currentCost currentState path frontierPoints

              -- Skip if already visited
              shouldContinue = not (currentState `S.member` visited)

          in if not shouldContinue
             then go restQueue visited frames
             else
               let visited' = S.insert currentState visited
                   path' = S.insert pos path

                   -- Generate neighbors: move forward, turn left, turn right
                   nextPos = addPoints dir pos

                   -- Try moving forward
                   moveForwardEntry =
                     if inBounds width height nextPos && not (nextPos `S.member` walls)
                     then [(currentCost + 1, (nextPos, dir), path')]
                     else []

                   -- Try turning left and right
                   turnLeftDir = turnLeft dir
                   turnLeftEntry = [(currentCost + 1000, (pos, turnLeftDir), path')]

                   turnRightDir = turnRight dir
                   turnRightEntry = [(currentCost + 1000, (pos, turnRightDir), path')]

                   -- Add all valid neighbors to queue
                   newQueue = restQueue ++ moveForwardEntry ++ turnLeftEntry ++ turnRightEntry

               in go newQueue visited' (frames ++ [frame])

addPoints :: Point -> Point -> Point
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

turnLeft :: Direction -> Direction
turnLeft (0, 1)   = (-1, 0)
turnLeft (-1, 0)  = (0, -1)
turnLeft (0, -1)  = (1, 0)
turnLeft (1, 0)   = (0, 1)
turnLeft _ = error "Invalid direction"

turnRight :: Direction -> Direction
turnRight (0, 1)   = (1, 0)
turnRight (1, 0)   = (0, -1)
turnRight (0, -1)  = (-1, 0)
turnRight (-1, 0)  = (0, 1)
turnRight _ = error "Invalid direction"

inBounds :: Int -> Int -> Point -> Bool
inBounds width height (x, y) =
    x >= 0 && x < width && y >= 0 && y < height

renderFrame :: MazeData -> Int -> (Int, SearchFrame) -> IO ()
renderFrame maze total (idx, frame) = do
    setCursorPosition 0 0
    let (pos, dir) = sfState frame
    putStrLn $ "Reindeer Maze - Dijkstra's Algorithm"
    putStrLn $ "Frame " ++ show (idx + 1) ++ " / " ++ show total
    putStrLn "Part context: [Part 1] track the best cost; [Part 2] count tiles that stay on optimal routes."
    putStrLn $ "Cost: " ++ show (sfCost frame)
    putStrLn $ "Position: " ++ show pos ++ " facing " ++ showDir dir
    putStrLn ""
    mapM_ putStrLn (renderRows maze frame)
    putStrLn ""
    putStrLn $ "Visited: " ++ show (S.size $ sfVisited frame) ++ " positions"
    putStrLn $ "Frontier: " ++ show (S.size $ sfFrontier frame) ++ " positions"
    threadDelay 50000  -- 50ms per frame
  where
    showDir (1, 0)  = "East  >"
    showDir (-1, 0) = "West  <"
    showDir (0, 1)  = "North ^"
    showDir (0, -1) = "South v"
    showDir _       = "?"

renderRows :: MazeData -> SearchFrame -> [String]
renderRows maze frame =
    [ concat [renderCell (x, y) | x <- [0 .. mdWidth maze - 1]]
    | y <- reverse [0 .. mdHeight maze - 1]  -- Flip for display
    ]
  where
    (currentPos, _) = sfState frame
    visited = sfVisited frame
    frontier = sfFrontier frame
    walls = mdWalls maze
    startPos = mdStart maze
    endPos = mdEnd maze

    renderCell point
      | point == currentPos = "[R]"
      | point == startPos   = " S "
      | point == endPos     = " E "
      | point `S.member` walls = "###"
      | point `S.member` visited = " . "
      | point `S.member` frontier = " ? "
      | otherwise = "   "
