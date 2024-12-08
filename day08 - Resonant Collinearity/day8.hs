#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package split-0.2.3.5 --package strict-0.4.0.1 --package unordered-containers-0.2.19.1

-----------------------------------------
-----------------------------------------
----  Day 8:  Resonant Collinearity  ----
-----------------------------------------
-----------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 --package strict-0.4.0.1 --package unordered-containers-0.2.19.1 -- '.\day8.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day8part1
-- 

-- *Main> day8part2
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


-------------
-- Program --
-------------
main = day8part1

day8part1 = do
    contents <- readFile "day8 (data).csv"
    
    let rows = Prelude.lines $ contents
        width  = genericLength (head rows)
        height = genericLength rows
        offWorldEdge (x,y) = x < 0 || x > width-1 || y < 0 || y > height-1
        
        antennas = readAntennasAsMap rows
    
    -- forM_ (Map.keys antennas) $ \c -> do
        -- putStrLn [c]
        -- print $ fromJust $ Map.lookup c antennas
        -- putStrLn ""
        
        -- mapM_ print $ nub $ calcAntinodes $ fromJust $ Map.lookup c antennas
        -- putStrLn ""
    
    let antennaGroups = map (\c -> fromJust $ Map.lookup c antennas) (Map.keys antennas) 
        antinodeGroups_IgnoringWorldEdge = map calcAntinodes antennaGroups
        antinodeGroups = map (filter (not . offWorldEdge)) antinodeGroups_IgnoringWorldEdge
        antinodes = nub $ concat antinodeGroups
    
    -- print antennaGroups
    -- print antinodeGroups_IgnoringWorldEdge
    -- print antinodeGroups
    -- print antinodes
    
    print $ length antinodes
    

addV2 (x1,y1) (x2,y2) = (x1+x2, y1+y2)
subV2 (x1,y1) (x2,y2) = (x1-x2, y1-y2)
scaleV2 s (x,y) = (s*x, s*y)
rot90V2 (x,y) = (y, -x)
invRot90V2 (x,y) = (-y, x)

type Point = (Integer, Integer)
type Antinode = Point

calcAntinodes :: [Point] -> [Point]
calcAntinodes xs = concat [[x1 `addV2` (x1 `subV2` x2),
                            x2 `addV2` (x2 `subV2` x1)] | let n = length xs, let indices = [0 .. (n-1)], i <- indices, j <- indices, i < j, let x1 = xs !! i, let x2 = xs !! j]

readAntennasAsMap :: [String] -> Map.HashMap Char [Point]
readAntennasAsMap = foldl' up Map.empty . readAntennas

up :: Map.HashMap Char [Point] -> (Char, Point) -> Map.HashMap Char [Point]
up = (\m (k,v) -> Map.insertWith (++) k [v] m)

readAntennas :: [String] -> [(Char,Point)]
readAntennas rows = concat [readChar c (colNum, rowNum) | (rowNum, row) <- zip [0..] rows, (colNum,c) <- zip [0..] row, case c of {' ' -> False; _ -> True}]
  where readChar c@'.' pos = []
        readChar c pos = [(c,pos)]