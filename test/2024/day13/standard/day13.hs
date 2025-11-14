#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3 --package ansi-terminal-0.11.5 --package linear --package array

---------------------------------
---------------------------------
----  Day 13: Claw Contraption  ----
---------------------------------
---------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3 --package ansi-terminal-0.11.5 --package linear --package array -- '.\day13.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day13part1
-- 26299

-- *Main> day13part2
-- 107824497933339


-------------
-- Imports --
-------------
import Data.List.Split
import Data.Maybe (fromJust, isNothing, isJust, catMaybes)
import Linear hiding (trace)
import Data.List as L (foldl', transpose, findIndex)
import Data.Array as A
import qualified Data.Map as M
import Data.Ord
import Data.Bits
import Data.Function
import Data.Char


-------------
-- Program --
-------------
main = day13part1

readMachines inStr =
    inStr
        & filter (\c -> c `elem` [',', '\n'] || isDigit c)
        & lines
        & filter (not . null)
        & map (splitOn ",")
        & chunksOf 3
        & map (map (map (read :: String -> Int)))

determinant [x00, x01, x10, x11] = x00 * x11 - x01 * x10

mult [x00, x01, x10, x11] [y00, y01, y10, y11] = [x00 * y00 + x01 * y10, x00 * y01 + x01 * y11, x10 * y00 + x11 * y10, x10 * y01 + x11 * y11]

multVec [x00, x01, x10, x11] [y00, y10] = [x00 * y00 + x01 * y10, x10 * y00 + x11 * y10]

timesPressedAAndB :: [[Int]] -> Maybe [Int]
timesPressedAAndB [[ax,ay],[bx,by],[px,py]]
    | det == 0 = Nothing
    | (sx_det `mod` det) /= 0 || (sy_det `mod` det) /= 0 = Nothing
    | otherwise = Just [sx_det `div` det, sy_det `div` det]
  where x@[x00,x01,x10,x11] = [ax,bx,ay,by]
        det = determinant x
        sx_det = x11 * px - x01 * py
        sy_det = -x10 * px + x00 * py

tokensFromTimes [a,b] = 3*a+b

correct [[ax,ay],[bx,by],[px,py]] = [[ax,ay],[bx,by],[10000000000000+px,10000000000000+py]]

solve machine = do
    times <- timesPressedAAndB machine
    return $ tokensFromTimes times

day13part1 = do
    contents <- readFile "day13 (data).csv"
    print $ sum $ catMaybes $ map solve $ readMachines contents
    
day13part2 = do
    contents <- readFile "day13 (data).csv"
    print $ sum $ catMaybes $ map solve $ map correct $ readMachines contents
