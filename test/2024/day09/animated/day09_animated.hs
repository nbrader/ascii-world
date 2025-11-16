#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for AoC 2024 Day 09 ("Disk Fragmenter").
-- Shows disk blocks being compacted with highlighted moves.

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.Char (digitToInt, isDigit)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hSetEncoding, hSetBuffering, stdout, utf8, BufferMode(NoBuffering))

type DiskMap = [Maybe Int]  -- Nothing = free space, Just n = file ID

data Frame = Frame
    { frameDisk :: DiskMap
    , frameFromIdx :: Maybe Int
    , frameToIdx :: Maybe Int
    , frameMoves :: Int
    }

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    args <- getArgs
    let inputType = if null args then "example" else head args
    contents <- loadInput inputType
    let disk = parseDisk contents
        frames = buildFrames disk
        total = length frames
    bracket_ hideCursor showCursor $ do
        clearScreen
        mapM_ (renderFrame total) (zip [0..] frames)
    setCursorPosition 30 0
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

buildFrames :: DiskMap -> [Frame]
buildFrames disk = Frame disk Nothing Nothing 0 : go disk 0
  where
    go d moves
        | isCompacted d = []
        | otherwise =
            case (findIndex (== Nothing) d, findLastFile d) of
                (Just freeIdx, Just fileIdx) | freeIdx < fileIdx ->
                    let val = d !! fileIdx
                        newDisk = take freeIdx d ++ [val] ++ take (fileIdx - freeIdx - 1) (drop (freeIdx + 1) d) ++ [Nothing] ++ drop (fileIdx + 1) d
                    in Frame d (Just fileIdx) (Just freeIdx) moves :
                       Frame newDisk Nothing Nothing (moves + 1) :
                       go newDisk (moves + 1)
                _ -> []

    findLastFile d = findIndex (/= Nothing) (reverse d) >>= \i -> Just (length d - 1 - i)

isCompacted :: DiskMap -> Bool
isCompacted disk =
    case (findIndex (== Nothing) disk, findLastFile disk) of
        (Just freeIdx, Just fileIdx) -> freeIdx >= fileIdx
        _ -> True
  where
    findLastFile d = findIndex (/= Nothing) (reverse d) >>= \i -> Just (length d - 1 - i)

renderFrame :: Int -> (Int, Frame) -> IO ()
renderFrame total (idx, Frame disk fromIdx toIdx moves) = do
    let displayLen = min 70 (length disk)
        diskStr = concatMap showBlock (take displayLen disk) ++ if length disk > displayLen then "..." else ""

        frameContent = unlines
            [ "Disk Fragmenter - Compaction Progress - frame " ++ show (idx + 1) ++ " / " ++ show total
            , "Part context: [Part 1] compact blocks; [Part 2] compact whole files."
            , ""
            , "Moves completed: " ++ show moves
            , case (fromIdx, toIdx) of
                (Just from, Just to) -> "Moving block from position " ++ show from ++ " to " ++ show to
                _ -> "Finding next block to move..."
            , ""
            , "Disk state:"
            , diskStr
            , case (fromIdx, toIdx) of
                (Just from, Just to) | from < displayLen && to < displayLen ->
                    replicate to ' ' ++ "^" ++ replicate (from - to - 1) ' ' ++ "^"
                (Just from, Just to) | from < displayLen ->
                    replicate to ' ' ++ "^"
                (Just from, Just to) | to < displayLen ->
                    replicate (from - displayLen) ' ' ++ "^"
                _ -> ""
            , case (fromIdx, toIdx) of
                (Just from, Just to) | from < displayLen && to < displayLen ->
                    replicate to ' ' ++ "to" ++ replicate (from - to - 2) ' ' ++ "from"
                _ -> ""
            , ""
            , "Legend: . = free space, 0-9 = file ID"
            , "Compaction complete when all files are left-aligned"
            ]

    setCursorPosition 0 0
    putStr frameContent
    threadDelay (if fromIdx /= Nothing then 200000 else 100000)
  where
    showBlock Nothing = "."
    showBlock (Just n) = show (n `mod` 10)
