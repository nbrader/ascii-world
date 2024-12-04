#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci

--------------------------------
--------------------------------
----  Day 4:  Ceres Search  ----
--------------------------------
--------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc -- '.\day4.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day4part1
-- 2532

-- *Main> day4part2
-- 1941


-------------
-- Imports --
-------------
import Data.Array as A (Array, (!), listArray)
import Control.Monad (guard)


-------------
-- Program --
-------------
main = day4part2

addV2 (x1,y1) (x2,y2) = (x1+x2, y1+y2)
scaleV2 s (x,y) = (s*x, s*y)
rot90V2 (x,y) = (y, -x)
invRot90V2 (x,y) = (-y, x)

day4part1 = do
    contents <- readFile "day4 (data).csv"
    
    let lines = Prelude.lines $ contents
        width  = length (head lines)
        height = length lines
        offWorldEdge (x,y) = x < 0 || x > width-1 || y < 0 || y > height-1
        grid = listArray ((0,0),(width-1,height-1)) $ concat lines
        
        foundWords = do
            x <- [0..width-1]
            y <- [0..height-1]
            
            let wordStart = (x,y)
                startChar = grid A.! (x,y) :: Char
            
            guard (startChar == 'X')
            
            dir <- [(i,j) | i <- [-1,0,1], j <- [-1,0,1], i /= 0 || j /= 0]
            
            let fourthCharPos = wordStart `addV2` scaleV2 3 dir
                fourthCharInWorld = not $ offWorldEdge fourthCharPos
            
            guard (not . offWorldEdge $ fourthCharPos)
            
            let isXmas = [grid A.! (wordStart `addV2` v) :: Char | s <- [0,1,2,3], let v = scaleV2 s dir] == "XMAS"
            
            guard (isXmas)
            
            return ()
    
    print (length foundWords)

day4part2 = do
    contents <- readFile "day4 (data).csv"
    
    let lines = Prelude.lines $ contents
        width  = length (head lines)
        height = length lines
        offWorldEdge (x,y) = x < 0 || x > width-1 || y < 0 || y > height-1
        grid = listArray ((0,0),(width-1,height-1)) $ concat lines
        
        foundMasXs = do
            x <- [0..width-1]
            y <- [0..height-1]
            
            let wordStart = (x,y)
                startChar = grid A.! (x,y) :: Char
            
            guard (startChar == 'A')
            
            let cornerDirs = [(1,1),(1,-1),(-1,-1),(-1,1)]
            let dirPairs = [[dir1,dir2] | dir1 <- cornerDirs, dir2 <- [rot90V2 dir1, invRot90V2 dir1]]
            
            guard (all (not . offWorldEdge) [wordStart `addV2` v | v <- [(-1,-1), (1,1)]])
            
            let isMas = or [all (== "MAS") [[grid A.! (wordStart `addV2` v) :: Char | s <- [-1,0,1], let v = scaleV2 s dir] | dir <- dirPair] | dirPair <- dirPairs]
            
            guard (isMas)
            
            return ()
    
    print (length foundMasXs)
