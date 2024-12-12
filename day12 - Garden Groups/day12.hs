#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package strict --package memoize --package linear --package split --package parallel --package array --package search-algorithms --package unordered-containers --package hashable

{-# LANGUAGE DeriveGeneric #-}

---------------------------------
---------------------------------
----  Day 11: Garden Groups  ----
---------------------------------
---------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package strict --package memoize --package linear --package split --package parallel --package array --package search-algorithms --package unordered-containers --package hashable -- '.\day12.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day12part1
-- 

-- *Main> day12part2
-- 


-------------
-- Imports --
-------------
import Data.Char (digitToInt, ord)
import Control.Monad (guard)
import qualified Data.HashMap.Strict as Map
import Data.Function.Memoize
import GHC.Generics (Generic)
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (fromJust, maybeToList, catMaybes, isJust, isNothing)
import Linear hiding (trace)
import qualified Linear
import Linear.V2
import Control.Parallel.Strategies
import Data.Bits
import Data.HashSet as H hiding (foldl')
import Algorithm.Search
import Data.List as L (sort, intersperse, foldl', findIndex, map, delete, null, concatMap, minimumBy, filter, transpose)
import Data.Array as A
import Data.Ix
import Data.Hashable
import Data.Function (on)


-------------
-- Program --
-------------
main = day12part1

type CharGrid = Array (V2 Int) Char
type RegionRepGrid = Array (V2 Int) (Maybe (V2 Int))
type NumFencesGrid = Array (V2 Int) Int
type AreasGrid = Array (V2 Int) Int
type PerimetersGrid = Array (V2 Int) Int

readPlots :: String -> [[Char]]
readPlots = lines

day12part1 = do
    contents <- readFile "day12 (example 3).csv"
    let rows = lines contents
        height = length rows
        width = length $ head rows
        inBounds (V2 x y) = x >= 0 && x < width && y >= 0 && y < height
    
        charGrid :: Array (V2 Int) Char
        charGrid = listArray ((V2 0 0), V2 (width - 1) (height - 1)) $ concat $ L.transpose rows
        
        connectedNeighbours dirs p = [neighbour | dir <- dirs, let neighbour = p + dir, inBounds neighbour, charGrid A.! p == charGrid A.! neighbour]
        
        allConnectedNeighbours       = connectedNeighbours [V2 (-1) 0, V2 0 (-1), V2 1 0, V2 0 1]
        upAndLeftConnectedNeighbours = connectedNeighbours [V2 (-1) 0, V2 0 (-1)]
        
        numFencesGrid :: Array (V2 Int) Int
        numFencesGrid = listArray ((V2 0 0), V2 (width - 1) (height - 1)) $ [4 - length (allConnectedNeighbours p) | p <- indices charGrid]
        
        ultimateRep :: V2 Int -> RegionRepGrid -> Maybe (V2 Int)
        ultimateRep v regionRepGrid
            | isNothing (regionRepGrid A.! v) = Nothing
            | otherwise = go v
          where go w = case regionRepGrid A.! w of
                        Just w' -> go w'
                        Nothing -> Just w
        
        initRegionRepGrid :: Array (V2 Int) (Maybe (V2 Int))
        initRegionRepGrid = listArray ((V2 0 0), V2 (width - 1) (height - 1)) $ repeat Nothing
        
        -- regionRepGridWithNeighbourReps = foldl' makeNeighboursRep initRegionRepGrid (indices initRegionRepGrid)
          -- where makeNeighboursRep :: RegionRepGrid -> V2 Int -> RegionRepGrid
                -- makeNeighboursRep regionRepGrid p
                  -- = let ns = upAndLeftConnectedNeighbours p
                    -- in case ns of
                        -- [] -> regionRepGrid
                        -- (neighbour:ns) -> regionRepGrid // ((p, Just neighbour) : [(theirUltRep, Just neighbour) | n <- ns, let theirUltRep = case (ultimateRep n regionRepGrid) of {Just r -> r; Nothing -> n}, neighbour /= theirUltRep])
        
        regionRepGridWithNeighbourReps = foldl' makeNeighboursRep initRegionRepGrid (indices initRegionRepGrid)
          where makeNeighboursRep :: RegionRepGrid -> V2 Int -> RegionRepGrid
                makeNeighboursRep regionRepGrid p
                  = let ns = upAndLeftConnectedNeighbours p
                    in case ns of
                        [] -> regionRepGrid
                        (newRep:ns) -> regionRepGrid // ((p, Just newRep) : [(n, Just newRep) | n <- ns])
        
        regionRepGridWithUltimateReps = foldl' makeRepUltimate regionRepGridWithNeighbourReps (indices regionRepGridWithNeighbourReps)
          where makeRepUltimate :: RegionRepGrid -> V2 Int -> RegionRepGrid
                makeRepUltimate regionRepGrid p
                    = let maybeRep = ultimateRep p regionRepGrid
                      in regionRepGrid // [(p, maybeRep)]
        
        areasGrid = foldl' update initAreasGrid (indices initAreasGrid)
          where update :: AreasGrid -> V2 Int -> AreasGrid
                update areasGrid p
                    = let rep = case regionRepGridWithUltimateReps A.! p of
                                    Just r -> r
                                    Nothing -> p
                          prevArea = areasGrid A.! rep
                          addedArea = 1
                      in areasGrid // [(rep, prevArea + addedArea)]
                
                initAreasGrid :: AreasGrid
                initAreasGrid = listArray ((V2 0 0), V2 (width - 1) (height - 1)) $ repeat 0
        
        perimetersGrid = foldl' update initAreasGrid (indices initAreasGrid)
          where update :: PerimetersGrid -> V2 Int -> PerimetersGrid
                update perimetersGrid p
                    = let rep = case regionRepGridWithUltimateReps A.! p of
                                    Just r -> r
                                    Nothing -> p
                          prevPerim = perimetersGrid A.! rep
                          addedPerimeter = numFencesGrid A.! p
                      in perimetersGrid // [(rep, prevPerim + addedPerimeter)]
                
                initAreasGrid :: AreasGrid
                initAreasGrid = listArray ((V2 0 0), V2 (width - 1) (height - 1)) $ repeat 0
        
        repPositions = [p | p <- indices regionRepGridWithUltimateReps, isNothing (regionRepGridWithUltimateReps A.! p)]
        repMapList = [(c, p, regionRepGridWithNeighbourReps A.! p) | p <- indices regionRepGridWithNeighbourReps, let c = charGrid A.! p]
        repUltMapList = [(c, p, regionRepGridWithUltimateReps A.! p) | p <- indices regionRepGridWithUltimateReps, let c = charGrid A.! p]
        repChars = [charGrid A.! p | p <- repPositions]
        repAreas = [areasGrid A.! p | p <- repPositions]
        repPerims = [perimetersGrid A.! p | p <- repPositions]
    
    -- print numFencesGrid
    -- print $ charGrid A.! (V2 3 0)
    mapM_ print $ repMapList
    putStrLn ""
    mapM_ print $ repUltMapList
    putStrLn ""
    mapM_ print $ zipWith (\x y -> (x,y)) repChars repPositions
    putStrLn ""
    -- print $ repAreas
    -- print $ repPerims
    mapM_ print $ zipWith (\x y -> (x,y)) repChars (zipWith (\x y -> (x,y)) repPerims repAreas)

-- A region of R plants with price 12 * 18 = 216.
-- A region of I plants with price 4 * 8 = 32.
-- A region of V plants with price 13 * 20 = 260.
-- A region of J plants with price 11 * 20 = 220.
-- A region of C plants with price 1 * 4 = 4.
-- A region of M plants with price 5 * 12 = 60.
-- A region of S plants with price 3 * 8 = 24.


-- A region of F plants with price 10 * 18 = 180.
-- A region of I plants with price 14 * 22 = 308.
-- A region of C plants with price 14 * 28 = 392.
-- A region of E plants with price 13 * 18 = 234.


-- ((12,18),'R')
-- ((4,8),'I')
-- ((13,20),'V')
-- ((11,20),'J')
-- ((1,4),'C')
-- ((5,12),'M')
-- ((3,8),'S')


-- ((2,5),'F')
-- ((8,13),'F')

-- ((13,19),'I')
-- ((1,3),'I')

-- ((13,26),'C')
-- ((1,2),'C')

-- ((9,11),'E')
-- ((4,7),'E')