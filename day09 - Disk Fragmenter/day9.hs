#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package split-0.2.3.5 --package strict-0.4.0.1 --package unordered-containers-0.2.19.1

-----------------------------------
-----------------------------------
----  Day 9:  Disk Fragmenter  ----
-----------------------------------
-----------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 --package strict-0.4.0.1 --package unordered-containers-0.2.19.1 -- '.\day9.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day9part1
-- 

-- *Main> day9part2
-- 


-------------
-- Imports --
-------------
import Data.List
import Data.List.Split
import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import qualified Data.HashMap.Strict as Map
import Data.HashSet (HashSet)
import Data.HashSet as H hiding (map, foldl', filter)
import Data.Either (lefts, rights)
import Data.Maybe (fromJust, catMaybes, fromMaybe)
import Control.Monad (guard, forM, forM_)
import Data.Ratio
import Debug.Trace (trace)
import Data.Ord (comparing)


-------------
-- Program --
-------------
main = day9part2

readDiskMap :: String -> [Int]
readDiskMap = map (read . (:[]))

blocksStringFromDiskMap :: [Int] -> [[Maybe Int]]
blocksStringFromDiskMap diskMap = zipWith (\isFileBlock (blockID, blockLength) -> let char = if isFileBlock then Just blockID else Nothing in genericReplicate blockLength char) isFileBlockPredList (zip idList diskMap)
  where idList = concat $ map (genericReplicate 2) [0..]
        
        isFileBlockPredList :: [Bool]
        isFileBlockPredList = cycle [True,False]

defrag1 :: [Maybe Int] -> [Maybe Int]
defrag1 input
    = (take lengthWithoutSpaces $ fill withSpaces reversedAndWithoutSpaces) ++ (replicate (lengthWithSpaces - lengthWithoutSpaces) Nothing)
  where withSpaces = input
        reversedAndWithoutSpaces = reverse . map Just . catMaybes $ input
        
        lengthWithSpaces    = length input
        lengthWithoutSpaces = length reversedAndWithoutSpaces
        
        fill :: [Maybe a] -> [Maybe a] -> [Maybe a]
        fill [] ys = reverse ys
        fill xs [] = xs
        fill (Nothing:xs) (y:ys) = y : fill xs    ys
        fill   (x:xs) (y:ys) = x : fill xs (y:ys)

filesAndSpacesFromDiskMap :: [Int] -> ([(Int,Int)], [(Int,Int)], Int)
filesAndSpacesFromDiskMap blockLengths = foldl' updateLists ([],[],0) $ zip3 isFileBlockPredList idList blockLengths
  where idList = concat $ map (genericReplicate 2) [0..]
        isFileBlockPredList = cycle [True,False]
        
        updateLists (filePosAndSizeList, spacePosAndSizeList, currSize) (isFileBlock, blockID, blockLength)
            = if isFileBlock
                then (newEntry : filePosAndSizeList,
                                 spacePosAndSizeList,
                                 newLength)
                else (           filePosAndSizeList,
                      newEntry : spacePosAndSizeList,
                                 newLength)
          where newEntry = (currSize, blockLength)
                newLength = currSize + blockLength

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

defrag2 :: ([(Int,Int)], [(Int,Int)], Int) -> ([(Int,Int)], [(Int,Int)], Int)
defrag2 (filesDescending, spacesDescending, size)
    = until (\(_, _, processed) -> processed >= 10) go (filesDescending, reverse spacesDescending, 0)
  where go (files, spaces, processed)
            = let (filePos,  fileSize) = {-trace ("length files = " ++ show (length files) ++ ", processed = " ++ show processed) $-} files !! processed
                  isFittingIndex = (\n -> let (spacePos, spaceSize) = {-trace ("length spaces = " ++ show (length spaces) ++ ", n = " ++ show n) $-} spaces !! n
                                          in spaceSize >= fileSize)
              in case find isFittingIndex [0..(length spaces - 1)] of
                    Nothing -> (files, spaces, processed+1)
                    Just n  -> let (spacePos, spaceSize) = {-trace ("length spaces = " ++ show (length spaces) ++ ", n = " ++ show n) $-} spaces !! n
                               in (replaceNth processed (spacePos, fileSize) files,
                                   replaceNth n (spacePos+fileSize, spaceSize-fileSize) spaces,
                                   processed+1)

toBlocks :: ([(Int,Int)], [(Int,Int)], Int) -> [[Maybe Int]]
toBlocks (files, spaces, size) = zipWith pad startDistances
                                . map (\((filePos, fileSize), fileID) -> replicate fileSize fileID) $ sortedFilesWithIDs
-- toBlocks (files, spaces, size) = sortedFilesWithIDs
  where sortedFilesWithIDs :: [((Int,Int),Int)]
        sortedFilesWithIDs = sortBy (comparing (\((filePos, fileSize), fileID) -> filePos)) $ zip files [9,8..]
  
        starts :: [Int]
        starts = map (fst . fst) sortedFilesWithIDs
        
        finalFileSize :: Int
        finalFileSize = snd (last sortedFilesWithIDs)
        
        startDistances :: [Int]
        startDistances = zipWith (\x y -> y - x) starts (tail starts) ++ [finalFileSize]
        
        pad :: Int -> [Int] -> [Maybe Int]
        pad startDistance blocks = let justBlocks = map Just blocks :: [Maybe Int]
                                       padding = replicate (startDistance - length blocks) Nothing
                                   in justBlocks ++ padding

checksum = sum . zipWith (*) [0..] . map (fromMaybe 0)

day9part1 = do
    contents <- readFile "day9 (data).csv"
    print . checksum . defrag1 . concat . blocksStringFromDiskMap . readDiskMap $ contents

day9part2 = do
    contents <- readFile "day9 (example).csv"
    print . readDiskMap $ contents
    print . map (maybe '.' (head . show)) . concat . toBlocks . defrag2 . filesAndSpacesFromDiskMap . readDiskMap $ contents
    print . checksum . concat . toBlocks . defrag2 . filesAndSpacesFromDiskMap . readDiskMap $ contents
