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
import Data.Maybe (fromJust, catMaybes)
import Control.Monad (guard, forM, forM_)
import Data.Ratio


-------------
-- Program --
-------------
main = day9part1

readDiskMap :: String -> [Int]
readDiskMap = map (read . (:[]))

blocksStringFromDiskMap :: [Int] -> String
blocksStringFromDiskMap diskMap = concat $ zipWith (\isFileBlock (id, blockLength) -> let char = if isFileBlock then head (show id) else '.' in replicate blockLength char) isFileBlockPredList (zip idList diskMap)
  where idList = concat $ map (replicate 2) [0..]
        
        isFileBlockPredList :: [Bool]
        isFileBlockPredList = cycle [True,False]

defrag :: String -> String
defrag inputStr
    = (take lengthWithoutSpaces $ fill withSpaces reversedAndWithoutSpaces) ++ (replicate (lengthWithSpaces - lengthWithoutSpaces) '.')
  where withSpaces = inputStr
        reversedAndWithoutSpaces = reverse . filter (not . (=='.')) $ inputStr
        
        lengthWithSpaces    = length inputStr
        lengthWithoutSpaces = length reversedAndWithoutSpaces
        
        fill [] ys = ys
        fill xs [] = xs
        fill ('.':xs) (y:ys) = y : fill xs    ys
        fill   (x:xs) (y:ys) = x : fill xs (y:ys)

checksum = sum . zipWith (*) [0..] . map ((read :: String -> Integer) . (:[])) . filter (not . (=='.'))

day9part1 = do
    contents <- readFile "day9 (data).csv"
    
    let diskMap = readDiskMap contents
    let blocksStr = blocksStringFromDiskMap diskMap
    let defragged = defrag blocksStr
    
    -- print diskMap
    -- print blocksStr
    -- print defragged
    print . checksum $ defragged


addV2 (x1,y1) (x2,y2) = (x1+x2, y1+y2)
subV2 (x1,y1) (x2,y2) = (x1-x2, y1-y2)
scaleV2 s (x,y) = (s*x, s*y)
rot90V2 (x,y) = (y, -x)
invRot90V2 (x,y) = (-y, x)

type Point = (Integer, Integer)
type Antinode = Point

calcAntinodes :: [Point] -> [Point]
calcAntinodes xs
    = concat [[x1 `addV2` (x1 `subV2` x2),
               x2 `addV2` (x2 `subV2` x1)] | let n = length xs, let indices = [0 .. (n-1)], i <- indices, j <- indices, i < j, let x1 = xs !! i, let x2 = xs !! j]

calcHarmonicAntinodes :: (Integer,Integer) -> [Point] -> [Point]
calcHarmonicAntinodes (width,height) ps
    = concat [calculateLinePoints p1 p2 | let n = length ps, let indices = [0 .. (n-1)], i <- indices, j <- indices, i < j, let p1 = ps !! i, let p2 = ps !! j]
  where calculateLinePoints :: (Integer,Integer) -> (Integer,Integer) -> [Point]
        calculateLinePoints (x1,y1) (x2,y2) = nub $ xPoints ++ yPoints
          where rx1, ry1, rx2, ry2 :: Rational
                [rx1, ry1, rx2, ry2] = map fromInteger [x1, y1, x2, y2]
                [rWidth, rHeight] = map fromInteger [width, height]
                xPoints = [(numerator x, numerator y) | x <- [0..(rWidth-1)], let d = rx2-rx1, d /= 0, let m = (ry2-ry1) / d, let c = ry1-rx1*m, let y = m*x+c, y >= 0 && y < rHeight, denominator y == 1]
                yPoints = [(numerator x, numerator y) | y <- [0..(rHeight-1)], let d = ry2-ry1, d /= 0, let m = (rx2-rx1) / d, let c = rx1-ry1*m, let x = m*y+c, x >= 0 && x < rWidth,  denominator x == 1]

readAntennasAsMap :: [String] -> Map.HashMap Char [Point]
readAntennasAsMap = foldl' up Map.empty . readAntennas

up :: Map.HashMap Char [Point] -> (Char, Point) -> Map.HashMap Char [Point]
up = (\m (k,v) -> Map.insertWith (++) k [v] m)

readAntennas :: [String] -> [(Char,Point)]
readAntennas rows = concat [readChar c (colNum, rowNum) | (rowNum, row) <- zip [0..] rows, (colNum,c) <- zip [0..] row, case c of {' ' -> False; _ -> True}]
  where readChar c@'.' pos = []
        readChar c pos = [(c,pos)]