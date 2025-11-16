#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
   --package array
-}

-- |
-- Animated visualization for AoC 2024 Day 12 ("Garden Groups").
-- Shows flood-fill region detection for garden plots.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.Array
import Data.Char (isAlpha)
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, hSetBuffering, stdout, utf8, NoBuffering(..))

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

type Grid = Array (Int, Int) Char
type Region = S.Set (Int, Int)

data Frame = Frame
    { fGrid :: Grid
    , fCurrentRegion :: Region
    , fCompletedRegions :: [Region]
    , fCurrentPlant :: Char
    , fRegionNum :: Int
    }

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let grid = parseGrid contents
        frames = buildFrames grid
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    let (_, (maxX, maxY)) = bounds grid
    setCursorPosition (maxY + 15) 0
    putStrLn "Garden Groups animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "12"
        filename = case inputType of
            "data" -> "day" ++ dayNum ++ " (data).csv"
            "example2" -> "day" ++ dayNum ++ " (example 2).csv"
            "example3" -> "day" ++ dayNum ++ " (example 3).csv"
            _ -> "day" ++ dayNum ++ " (example).csv"
        path = "test/2024/day" ++ dayNum ++ "/standard/" ++ filename
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure defaultInput
  where
    defaultInput = unlines
        [ "AAAA"
        , "BBCD"
        , "BBCC"
        , "EEEC"
        ]

parseGrid :: String -> Grid
parseGrid contents = listArray ((0, 0), (width - 1, height - 1)) cells
  where
    -- Clean rows: remove \r characters and filter empty lines
    rows = filter (not . null) . map (filter (/= '\r')) $ lines contents
    width = if null rows then 0 else length (head rows)
    -- Ensure all rows have the same width
    validRows = filter (\row -> length row == width) rows
    height = length validRows
    cells = concat validRows

buildFrames :: Grid -> [Frame]
buildFrames grid = scanRegions (S.fromList $ indices grid) [] 0
  where
    scanRegions unvisited completed regionNum
        | S.null unvisited = []
        | otherwise =
            let start = S.findMin unvisited
                plant = grid ! start
                region = floodFill grid plant (S.singleton start) (S.singleton start)
                regionFrames = growthFrames grid plant region completed regionNum
                newUnvisited = unvisited S.\\ region
                newCompleted = completed ++ [region]
            in regionFrames ++ scanRegions newUnvisited newCompleted (regionNum + 1)

    growthFrames grid plant fullRegion completed regionNum =
        let steps = S.toList fullRegion
            growth = scanl (\acc pt -> S.insert pt acc) S.empty steps
        in [Frame grid g completed plant regionNum | g <- tail growth]

floodFill :: Grid -> Char -> Region -> Region -> Region
floodFill grid plant visited frontier
    | S.null frontier = visited
    | otherwise =
        let newFrontier = S.fromList
                [ neighbor
                | point <- S.toList frontier
                , neighbor <- neighbors (bounds grid) point
                , neighbor `S.notMember` visited
                , grid ! neighbor == plant
                ]
            newVisited = visited `S.union` newFrontier
        in floodFill grid plant newVisited newFrontier

neighbors :: ((Int, Int), (Int, Int)) -> (Int, Int) -> [(Int, Int)]
neighbors ((minX, minY), (maxX, maxY)) (x, y) =
    filter inBounds [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
  where
    inBounds (x', y') = x' >= minX && x' <= maxX && y' >= minY && y' <= maxY

renderFrame :: Frame -> IO ()
renderFrame (Frame grid current completed plant regionNum) = do
    let (_, (maxX, maxY)) = bounds grid
        width = maxX + 1
        height = maxY + 1

    -- Build AsciiWorld visualization
    let completedPoints = concat [[("Completed-" ++ show i, S.toList region) | (i, region) <- zip [0::Int ..] completed]]
        currentPoints = if S.null current then [] else [("Current", S.toList current)]
        asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList (completedPoints ++ currentPoints)
            , asciiWorldWidth = width
            }
        bgChar = '.'
        maskToChar = id
        pointsToChar name
            | "Completed-" `isPrefixOf` name = '#'
            | name == "Current" = '*'
            | otherwise = '?'
        nameZOrder a b = case (a, b) of
            (PointsIndex "Current", _) -> GT
            (_, PointsIndex "Current") -> LT
            _ -> compare a b
        worldStr = showAsciiWorld height bgChar maskToChar pointsToChar nameZOrder asciiWorld

        area = S.size current
        perimeter = calculatePerimeter grid current

    -- Build entire frame as single string and output atomically
    let frameContent = unlines
            [ "Garden Groups - Region #" ++ show (regionNum + 1) ++ " (Plant: " ++ [plant] ++ ")"
            , "Part context: [Part 1] area × perimeter; [Part 2] area × sides."
            , ""
            ] ++ worldStr ++ unlines
            [ ""
            , "Current region: area=" ++ show area ++ ", perimeter=" ++ show perimeter
            , "Completed regions: " ++ show (length completed)
            ]

    setCursorPosition 0 0
    putStr frameContent
    threadDelay 50000
  where
    isPrefixOf = isPrefix
    isPrefix [] _ = True
    isPrefix _ [] = False
    isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

calculatePerimeter :: Grid -> Region -> Int
calculatePerimeter grid region = sum [edgeCount pt | pt <- S.toList region]
  where
    edgeCount (x, y) = length $ filter (not . inRegion) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    inRegion pt = pt `S.member` region
