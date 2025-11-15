#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package ascii-world
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2024 Day 09 ("Disk Fragmenter").
-- Shows disk blocks being compacted.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.Char (digitToInt, isDigit)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

import AsciiWorld (AsciiWorld(..), showAsciiWorld, MaskOrPointsIndex(..))
import Mask (Point)

type DiskMap = [Maybe Int]  -- Nothing = free space, Just n = file ID

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let disk = parseDisk contents
        frames = buildFrames disk
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ renderFrame frames
    setCursorPosition 25 0
    putStrLn "Disk Fragmenter animation complete."

loadInput :: String -> IO String
loadInput inputType = do
    let dayNum = "09"
        filename = case inputType of
            "data" -> "day" ++ dayNum ++ " (data).csv"
            "example2" -> "day" ++ dayNum ++ " (example 2).csv"
            "example3" -> "day" ++ dayNum ++ " (example 3).csv"
            _ -> "day" ++ dayNum ++ " (example).csv"
        path = "test/2024/day" ++ dayNum ++ "/standard/" ++ filename
    exists <- doesFileExist path
    if exists
        then readFile path
        else pure "2333133121414131402"

parseDisk :: String -> DiskMap
parseDisk input = concat $ zipWith expand [0..] (map digitToInt $ filter isDigit input)
  where
    expand idx len
        | even idx  = replicate len (Just (idx `div` 2))  -- File
        | otherwise = replicate len Nothing                -- Free space

buildFrames :: DiskMap -> [DiskMap]
buildFrames disk = takeWhile (not . isCompacted) $ iterate compactStep disk

compactStep :: DiskMap -> DiskMap
compactStep disk =
    case (findIndex (== Nothing) disk, findLastFile disk) of
        (Just freeIdx, Just fileIdx) | freeIdx < fileIdx ->
            let val = disk !! fileIdx
            in take freeIdx disk ++ [val] ++ take (fileIdx - freeIdx - 1) (drop (freeIdx + 1) disk) ++ [Nothing] ++ drop (fileIdx + 1) disk
        _ -> disk
  where
    findLastFile d = findIndex (/= Nothing) (reverse d) >>= \i -> Just (length d - 1 - i)

isCompacted :: DiskMap -> Bool
isCompacted disk =
    case (findIndex (== Nothing) disk, findLastFile disk) of
        (Just freeIdx, Just fileIdx) -> freeIdx >= fileIdx
        _ -> True
  where
    findLastFile d = findIndex (/= Nothing) (reverse d) >>= \i -> Just (length d - 1 - i)

renderFrame :: DiskMap -> IO ()
renderFrame disk = do
    setCursorPosition 0 0
    putStrLn "Disk Fragmenter - Compaction Progress"
    putStrLn "Part context: [Part 1] compact blocks; [Part 2] compact whole files."
    putStrLn ""

    -- Show disk as points
    let displayLen = min 80 (length disk)
        filePoints = [(i, 0) | (i, Just _) <- zip [0..displayLen-1] disk]
        freePoints = [(i, 1) | (i, Nothing) <- zip [0..displayLen-1] disk]
        asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList [("Files", filePoints), ("Free", freePoints)]
            , asciiWorldWidth = displayLen
            }
        bgChar = '.'
        maskToChar = id
        pointsToChar name = case name of
            "Files" -> '#'
            "Free" -> '.'
            _ -> '?'
        nameZOrder = compare
        worldStr = showAsciiWorld 3 bgChar maskToChar pointsToChar nameZOrder asciiWorld

    putStr worldStr
    putStrLn ""

    let fileCount = length $ filter (/= Nothing) disk
        freeCount = length $ filter (== Nothing) disk
    putStrLn $ "Blocks: " ++ show fileCount ++ " files, " ++ show freeCount ++ " free"
    putStrLn $ "Preview: " ++ concatMap showBlock (take 40 disk) ++ if length disk > 40 then "..." else ""

    threadDelay 100000
  where
    showBlock Nothing = "."
    showBlock (Just n) = show (n `mod` 10)
