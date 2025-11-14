#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2024 Day 18 ("RAM Run").
-- Shows bytes falling into memory space and the path being recalculated.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Maybe (isJust, fromMaybe)
import Data.List.Split (splitOn)
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.IO (hSetEncoding, stdout, utf8)

type Point = (Int, Int)
type Grid = S.Set Point

data AnimFrame = AnimFrame
    { afByteCount  :: Int
    , afObstacles  :: Grid
    , afPath       :: Maybe [Point]
    , afNewByte    :: Maybe Point
    } deriving (Eq, Show)

main :: IO ()
main = do
    hSetEncoding stdout utf8  -- Windows compatibility
    contents <- loadMap
    let bytes = parseBytes contents
        gridSize = 7  -- Example grid size
        frames = buildFrames gridSize bytes
        total = length frames
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ (renderFrame gridSize total) (zip [0 ..] frames)
    setCursorPosition (gridSize + 10) 0
    putStrLn "RAM Run animation complete!"

loadMap :: IO String
loadMap = do
    let path = "test/day18 (example).csv"
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure defaultBytes
  where
    defaultBytes = unlines
        [ "5,4", "4,2", "4,5", "3,0", "2,1"
        , "6,3", "2,4", "1,5", "0,6", "3,3"
        , "2,6", "5,1", "1,2", "5,5", "2,5"
        , "6,5", "1,4", "0,4", "6,4", "1,1"
        , "6,1", "1,0", "0,5", "1,6", "2,0"
        ]

parseBytes :: String -> [Point]
parseBytes contents = map parseLine $ filter (not . null) $ lines contents
  where
    parseLine line = case splitOn "," line of
        [xStr, yStr] -> (read xStr, read yStr)
        _ -> error $ "Invalid line: " ++ line

buildFrames :: Int -> [Point] -> [AnimFrame]
buildFrames gridSize bytes = go 0 S.empty
  where
    startPos = (0, 0)
    endPos = (gridSize - 1, gridSize - 1)

    go n obstacles
      | n > length bytes = []
      | otherwise =
          let obstacles' = if n > 0 && n <= length bytes
                           then S.insert (bytes !! (n - 1)) obstacles
                           else obstacles
              path = bfsPath gridSize obstacles' startPos endPos
              newByte = if n > 0 && n <= length bytes
                        then Just (bytes !! (n - 1))
                        else Nothing
              frame = AnimFrame n obstacles' path newByte
          in frame : go (n + 1) obstacles'

-- | BFS to find the actual path (not just length)
bfsPath :: Int -> Grid -> Point -> Point -> Maybe [Point]
bfsPath gridSize obstacles start end = go (Seq.singleton (start, [start])) S.empty
  where
    go queue visited
      | Seq.null queue = Nothing
      | otherwise =
          let ((pos, path) Seq.:< restQueue) = Seq.viewl queue
          in
            if pos == end
              then Just (reverse path)
            else if pos `S.member` visited
              then go restQueue visited
            else
              let visited' = S.insert pos visited
                  neighbors = getNeighbors gridSize obstacles pos
                  unvisitedNeighbors = filter (`S.notMember` visited') neighbors
                  newQueue = foldl (Seq.|>) restQueue [(n, n:path) | n <- unvisitedNeighbors]
              in go newQueue visited'

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

renderFrame :: Int -> Int -> (Int, AnimFrame) -> IO ()
renderFrame gridSize total (idx, frame) = do
    setCursorPosition 0 0
    putStrLn $ "RAM Run - Bytes Falling into Memory"
    putStrLn $ "Frame " ++ show (idx + 1) ++ " / " ++ show total
    putStrLn $ "Bytes fallen: " ++ show (afByteCount frame)
    case afNewByte frame of
        Just (x, y) -> putStrLn $ "New byte at: (" ++ show x ++ "," ++ show y ++ ")"
        Nothing -> putStrLn "Initial state"
    putStrLn ""
    mapM_ putStrLn (renderRows gridSize frame)
    putStrLn ""
    case afPath frame of
        Just path -> putStrLn $ "Path exists! Length: " ++ show (length path - 1)
        Nothing -> putStrLn "No path to exit!"
    threadDelay 150000  -- 150ms per frame
  where
    renderRows gSize frame =
        [ concat [renderCell (x, y) frame | x <- [0 .. gSize - 1]]
        | y <- [0 .. gSize - 1]
        ]

    renderCell :: Point -> AnimFrame -> String
    renderCell point frame
      | point == (0, 0) = " S "
      | point == (gridSize - 1, gridSize - 1) = " E "
      | point `S.member` afObstacles frame = "###"
      | maybe False (point `elem`) (afPath frame) = " . "
      | otherwise = " . "
