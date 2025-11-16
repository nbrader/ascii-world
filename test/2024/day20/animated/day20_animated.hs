#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2024 Day 20 ("Race Condition").
-- Shows the race track, the main path, and potential cheat shortcuts.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Maybe (fromMaybe)
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

type Point = (Int, Int)
type Path = [Point]
type DistanceMap = M.Map Point Int

data PartTag = Part1Tag | Part2Tag deriving (Eq, Show)

data AnimFrame
    = PathFrame Int Point  -- Progress along path
    | CheatFrame PartTag Point Point Int  -- Start, end, time saved

main :: IO ()
main = do
    hSetEncoding stdout utf8  -- Windows compatibility
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadMap inputType
    let (walls, start, end, width, height) = parseTrack contents
    case bfsWithDistances width height walls start end of
        Nothing -> putStrLn "No path found!"
        Just (path, distMap) -> do
            let shortCheats = findCheats distMap path 2 1
                longCheats  = findCheats distMap path 20 50
                frames = buildFrames path shortCheats longCheats
                total = length frames
            bracket_ hideCursor showCursor $ do
                clearScreen
                mapM_ (renderFrame width height walls path) (zip [0 ..] frames)
            setCursorPosition (height + 10) 0
            putStrLn "Race Condition animation complete. (Part 1 short cheats + Part 2 long cheats)"

loadMap :: String -> IO String
loadMap inputType = do
    let dayNum = "20"
        filename = case inputType of
            "data" -> "day" ++ dayNum ++ " (data).csv"
            "example2" -> "day" ++ dayNum ++ " (example 2).csv"
            "example3" -> "day" ++ dayNum ++ " (example 3).csv"
            _ -> "day" ++ dayNum ++ " (example).csv"
        path = "test/2024/day" ++ dayNum ++ "/standard/" ++ filename
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure $ unlines defaultMap
  where
    defaultMap =
        [ "###############"
        , "#...#...#.....#"
        , "#.#.#.#.#.###.#"
        , "#S#...#.#.#...#"
        , "#######.#.#.###"
        , "#######.#.#...#"
        , "#######.#.###.#"
        , "###..E#...#...#"
        , "###.#######.###"
        , "#...###...#...#"
        , "#.#####.#.###.#"
        , "#.#...#.#.#...#"
        , "#.#.#.#.#.#.###"
        , "#...#...#...###"
        , "###############"
        ]

parseTrack :: String -> (S.Set Point, Point, Point, Int, Int)
parseTrack contents = (walls, start, end, width, height)
  where
    rows = filter (not . null) . map (filter (/= '\r')) $ lines contents
    height = length rows
    width = if null rows then 0 else length (head rows)
    cells = [ ((x, height - 1 - y), char)
            | (y, row) <- zip [0..] rows
            , (x, char) <- zip [0..] row
            ]
    walls = S.fromList [pos | (pos, '#') <- cells]
    start = head [pos | (pos, 'S') <- cells]
    end = head [pos | (pos, 'E') <- cells]

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

findCheats :: DistanceMap -> Path -> Int -> Int -> [(Point, Point, Int)]
findCheats distMap path maxCheatDuration threshold =
    [ (startPos, endPos, timeSaved)
    | startPos <- path
    , endPos <- path
    , let startDist = fromMaybe 0 $ M.lookup startPos distMap
    , let endDist = fromMaybe 0 $ M.lookup endPos distMap
    , endDist > startDist
    , let manhattan = manhattanDistance startPos endPos
    , manhattan > 0 && manhattan <= maxCheatDuration
    , let normalTime = endDist - startDist
    , let cheatTime = manhattan
    , let timeSaved = normalTime - cheatTime
    , timeSaved >= threshold
    ]

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

buildFrames :: Path -> [(Point, Point, Int)] -> [(Point, Point, Int)] -> [AnimFrame]
buildFrames path shortCheats longCheats =
    -- First show the path being traced
    [PathFrame i pos | (i, pos) <- zip [0..] path] ++
    -- Then show some example cheats
    [CheatFrame Part1Tag start end saved | (start, end, saved) <- take 20 shortCheats] ++
    [CheatFrame Part2Tag start end saved | (start, end, saved) <- take 20 longCheats]

renderFrame :: Int -> Int -> S.Set Point -> Path -> (Int, AnimFrame) -> IO ()
renderFrame width height walls path (idx, frame) = do
    -- Build frame content based on frame type
    let (frameContent, delay) = case frame of
            PathFrame step currentPos ->
                let pathSoFar = S.fromList $ take (step + 1) path
                    content = unlines
                        [ "Race Condition - Track Analysis"
                        , "[Part 1] Tracing the main path"
                        , "Step " ++ show step ++ " / " ++ show (length path - 1)
                        , "Position: " ++ show currentPos
                        , ""
                        ] ++ unlines (renderRows width height walls pathSoFar Nothing Nothing)
                in (content, 30000)  -- 30ms per step

            CheatFrame partTag cheatStart cheatEnd timeSaved ->
                let pathSet = S.fromList path
                    content = unlines
                        [ "Race Condition - Track Analysis"
                        , partLabel partTag ++ " Showing cheat shortcut"
                        , "From: " ++ show cheatStart ++ " To: " ++ show cheatEnd
                        , "Time saved: " ++ show timeSaved ++ " picoseconds"
                        , ""
                        ] ++ unlines (renderRows width height walls pathSet (Just cheatStart) (Just cheatEnd))
                in (content, 200000)  -- 200ms per cheat

    setCursorPosition 0 0
    putStr frameContent
    threadDelay delay

partLabel :: PartTag -> String
partLabel Part1Tag = "[Part 1]"
partLabel Part2Tag = "[Part 2]"

renderRows :: Int -> Int -> S.Set Point -> S.Set Point -> Maybe Point -> Maybe Point -> [String]
renderRows width height walls pathPoints cheatStart cheatEnd =
    [ concat [renderCell (x, y) | x <- [0 .. width - 1]]
    | y <- reverse [0 .. height - 1]
    ]
  where
    renderCell point
      | Just point == cheatStart = "[S]"
      | Just point == cheatEnd = "[E]"
      | point `S.member` walls = "###"
      | point `S.member` pathPoints = " . "
      | otherwise = "   "
