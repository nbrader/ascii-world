#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package linear --package array

---------------------------------
---------------------------------
----  Day 11: Garden Groups  ----
---------------------------------
---------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package linear --package array -- '.\day12.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day12part1
-- 216042

-- *Main> day12part2
-- 


-------------
-- Imports --
-------------
import Data.Maybe (fromJust, isNothing)
import Linear hiding (trace)
import Data.List as L (foldl', transpose)
import Data.Array as A


-------------
-- Program --
-------------
main = day12part2

type CharGrid = Array (V2 Int) Char
type RegionRepGrid = Array (V2 Int) (Maybe (V2 Int))
type NumFencesGrid = Array (V2 Int) Int
type AreasGrid = Array (V2 Int) Int
type PerimetersGrid = Array (V2 Int) Int

readPlots :: String -> [[Char]]
readPlots = lines

day12part1 = do
    contents <- readFile "day12 (data).csv"
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
        
        regionRepGridWithNeighbourReps = foldl' makeNeighboursRep initRegionRepGrid (indices initRegionRepGrid)
          where makeNeighboursRep :: RegionRepGrid -> V2 Int -> RegionRepGrid
                makeNeighboursRep regionRepGrid p
                  = let ns = upAndLeftConnectedNeighbours p
                    in case ns of
                        [] -> regionRepGrid
                        (neighbour:ns) -> let regionRepGridAfterWeSetRep = regionRepGrid // [(p, Just neighbour)]
                                              ourUltRep = fromJust $ ultimateRep p regionRepGridAfterWeSetRep
                                          in regionRepGridAfterWeSetRep // [(theirUltRep, Just ourUltRep) | n <- ns, let theirUltRep = case (ultimateRep n regionRepGridAfterWeSetRep) of {Just r -> r; Nothing -> n}, theirUltRep /= ourUltRep]
        
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
    -- mapM_ print $ repMapList
    -- putStrLn ""
    -- mapM_ print $ repUltMapList
    -- putStrLn ""
    -- mapM_ print $ zipWith (\x y -> (x,y)) repChars repPositions
    -- putStrLn ""
    -- print $ repAreas
    -- print $ repPerims
    print $ sum $ zipWith (*) repPerims repAreas
    -- mapM_ print $ zipWith (\x y -> (x,y)) repChars (zipWith (\x y -> (x,y)) repPerims repAreas)


day12part2 = do
    contents <- readFile "day12 (data).csv"
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
        
        regionRepGridWithNeighbourReps = foldl' makeNeighboursRep initRegionRepGrid (indices initRegionRepGrid)
          where makeNeighboursRep :: RegionRepGrid -> V2 Int -> RegionRepGrid
                makeNeighboursRep regionRepGrid p
                  = let ns = upAndLeftConnectedNeighbours p
                    in case ns of
                        [] -> regionRepGrid
                        (neighbour:ns) -> let regionRepGridAfterWeSetRep = regionRepGrid // [(p, Just neighbour)]
                                              ourUltRep = fromJust $ ultimateRep p regionRepGridAfterWeSetRep
                                          in regionRepGridAfterWeSetRep // [(theirUltRep, Just ourUltRep) | n <- ns, let theirUltRep = case (ultimateRep n regionRepGridAfterWeSetRep) of {Just r -> r; Nothing -> n}, theirUltRep /= ourUltRep]
        -- I'm trying to get the above to not infinitely loop while also setting the ultimateReprenstative of neighbours to match the ultimate representative
        
        -- regionRepGridWithNeighbourReps = foldl' makeNeighboursRep initRegionRepGrid (indices initRegionRepGrid)
          -- where makeNeighboursRep :: RegionRepGrid -> V2 Int -> RegionRepGrid
                -- makeNeighboursRep regionRepGrid p
                  -- = let ns = upAndLeftConnectedNeighbours p
                    -- in case ns of
                        -- [] -> regionRepGrid
                        -- (newRep:ns) -> regionRepGrid // ((p, Just newRep) : [(n, Just newRep) | n <- ns])
        
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
    
    print $ sum $ zipWith (*) repPerims repAreas
