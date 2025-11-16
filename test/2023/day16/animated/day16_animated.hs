#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2023 Day 16 ("The Floor Will Be Lava").
-- Shows light beam tracing through mirrors and splitters.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, hSetBuffering, stdout, utf8, NoBuffering(..))

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

type Pos = (Int, Int)
type Dir = (Int, Int)
type Beam = (Pos, Dir)
type Grid = M.Map Pos Char

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let (grid, width, height) = parseGrid contents
        frames = buildFrames grid width height
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    setCursorPosition 25 0
    putStrLn "The Floor Will Be Lava animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "16"
        filename = case inputType of
            "data" -> "day" ++ dayNum ++ " (data).csv"
            "example2" -> "day" ++ dayNum ++ " (example 2).csv"
            "example3" -> "day" ++ dayNum ++ " (example 3).csv"
            _ -> "day" ++ dayNum ++ " (example).csv"
        path = "test/2023/day" ++ dayNum ++ "/standard/" ++ filename
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure $ unlines
            [ ".|...\\\\......"
            , "|.-.\\\\....."
            , ".....|-..."
            , "........|."
            , ".........."
            , ".........\\\\"
            , "..../.\\\\\\\\.."
            , ".-.-/..|.."
            , ".|....-|.\\\\"
            , "..//.|...."
            ]

parseGrid :: String -> (Grid, Int, Int)
parseGrid contents =
    let gridLines = lines contents
        height = length gridLines
        width = if null gridLines then 0 else length (head gridLines)
        grid = M.fromList [ ((y, x), c)
                          | (y, line) <- zip [0..] gridLines
                          , (x, c) <- zip [0..] line
                          ]
    in (grid, width, height)

data Frame = Frame
    { frameGrid :: Grid
    , frameBeams :: [Beam]
    , frameEnergized :: Set Pos
    , frameVisited :: Set Beam
    , frameWidth :: Int
    , frameHeight :: Int
    , frameStep :: Int
    }

buildFrames :: Grid -> Int -> Int -> [Frame]
buildFrames grid width height =
    let initialBeam = ((0, 0), (0, 1))  -- Start at top-left, moving right
        initialFrame = Frame grid [initialBeam] S.empty S.empty width height 0
    in simulateBeams initialFrame
  where
    simulateBeams frame
        | null (frameBeams frame) = [frame]
        | frameStep frame > 200 = [frame]  -- Safety limit
        | otherwise =
            let newBeams = concatMap (stepBeam grid width height (frameVisited frame)) (frameBeams frame)
                energizedPositions = S.fromList (map fst newBeams)
                newEnergized = frameEnergized frame `S.union` energizedPositions
                newVisited = frameVisited frame `S.union` S.fromList newBeams
                uniqueNewBeams = filter (`S.notMember` frameVisited frame) newBeams
                newFrame = frame
                    { frameBeams = uniqueNewBeams
                    , frameEnergized = newEnergized
                    , frameVisited = newVisited
                    , frameStep = frameStep frame + 1
                    }
            in frame : simulateBeams newFrame

stepBeam :: Grid -> Int -> Int -> Set Beam -> Beam -> [Beam]
stepBeam grid width height visited ((y, x), (dy, dx)) =
    let newY = y + dy
        newX = x + dx
        newPos = (newY, newX)
    in if newY < 0 || newY >= height || newX < 0 || newX >= width
        then []  -- Out of bounds
        else case M.lookup newPos grid of
            Nothing -> [((newY, newX), (dy, dx))]  -- Continue in same direction
            Just '.' -> [((newY, newX), (dy, dx))]  -- Continue through empty space
            Just '/' ->  -- Reflect: right->up, up->right, down->left, left->down
                let newDir = if (dy, dx) == (0, 1) then (-1, 0)  -- right -> up
                             else if (dy, dx) == (-1, 0) then (0, 1)  -- up -> right
                             else if (dy, dx) == (1, 0) then (0, -1)  -- down -> left
                             else (1, 0)  -- left -> down
                in [((newY, newX), newDir)]
            Just '\\' ->  -- Reflect: right->down, down->right, up->left, left->up
                let newDir = if (dy, dx) == (0, 1) then (1, 0)  -- right -> down
                             else if (dy, dx) == (1, 0) then (0, 1)  -- down -> right
                             else if (dy, dx) == (-1, 0) then (0, -1)  -- up -> left
                             else (-1, 0)  -- left -> up
                in [((newY, newX), newDir)]
            Just '|' ->  -- Vertical splitter
                if dy /= 0
                    then [((newY, newX), (dy, dx))]  -- Pass through if moving vertically
                    else [((newY, newX), (-1, 0)), ((newY, newX), (1, 0))]  -- Split if horizontal
            Just '-' ->  -- Horizontal splitter
                if dx /= 0
                    then [((newY, newX), (dy, dx))]  -- Pass through if moving horizontally
                    else [((newY, newX), (0, -1)), ((newY, newX), (0, 1))]  -- Split if vertical
            _ -> [((newY, newX), (dy, dx))]

renderFrame :: Frame -> IO ()
renderFrame frame = do
    -- Build character grid
    let energized = frameEnergized frame
        beamPositions = S.fromList (map fst $ frameBeams frame)
        charGrid = M.fromList
            [ ((y, x), getChar (y, x))
            | y <- [0..frameHeight frame - 1]
            , x <- [0..frameWidth frame - 1]
            ]
        getChar pos
            | pos `S.member` beamPositions = '@'  -- Current beam position
            | pos `S.member` energized = '#'  -- Energized
            | otherwise = M.findWithDefault '.' pos (frameGrid frame)

    -- Build grid lines
    let gridLines = [ [ M.findWithDefault '.' (y, x) charGrid
                      | x <- [0..frameWidth frame - 1] ]
                    | y <- [0..frameHeight frame - 1] ]

    -- Build entire frame as single string and output atomically
    let frameContent = unlines
            [ "The Floor Will Be Lava - Beam Tracing"
            , "[Part 1] Light beams bounce off mirrors and split"
            , ""
            , "Step: " ++ show (frameStep frame)
            , "Active beams: " ++ show (length $ frameBeams frame)
            , "Energized tiles: " ++ show (S.size $ frameEnergized frame)
            , ""
            ] ++ unlines gridLines

    setCursorPosition 0 0
    putStr frameContent
    threadDelay 100000  -- 100ms delay
