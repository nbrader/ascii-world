#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci

module BitMask where

-------------
-- Imports --
-------------
import Data.Bits

-- Each obj has a shape encoded as bits of an Integer.
type Point = (Int,Int)
type BitMask = Integer

-- Converts a 2D point to a 1D index
pointToIndex :: Int -> Point -> Int
pointToIndex width (x, y) = y * width + x

pointToBitMask :: Int -> Point -> BitMask
pointToBitMask width (x,y) = moveBitMask width (x,y) 1

moveBitMask :: Int -> (Int,Int) -> BitMask -> BitMask
moveBitMask width (dx,dy) pts = pts `shift` pointToIndex width (dx,dy)

movePoint :: Int -> (Int,Int) -> (Int,Int) -> (Int,Int)
movePoint width (dx,dy) (x,y) = (x+dx,y+dy)


-- isOverlapping n n
-- time:  O(n) (approx.) [0 < n < 10000000000, 0 < secs < 7.97]
-- space: O(n) (approx.) [0 < n < 10000000000, 0 < bytes < 30000141704]
-- https://nux-pc/svn/Nux-SVN/My Programming/Scratchings/Advent of Code 2022/day17 (bits).hs/?r=1569
isOverlapping :: BitMask -> BitMask -> Bool
isOverlapping ps1 ps2 = (ps1 .&. ps2) /= zeroBits

bitwiseSubtract :: BitMask -> BitMask -> BitMask
bitwiseSubtract ps1 ps2 = (ps1 .&. complement ps2)

bitwiseAnd :: BitMask -> BitMask -> BitMask
bitwiseAnd ps1 ps2 = (ps1 .&. ps2)

bitwiseOr :: BitMask -> BitMask -> BitMask
bitwiseOr ps1 ps2 = (ps1 .|. ps2)

bitwiseXor :: BitMask -> BitMask -> BitMask
bitwiseXor ps1 ps2 = (ps1 `xor` ps2)

up, dn, lt, rt :: (Integral a) => (a,a)
up = (  0 ,   1 )
dn = (  0 , (-1))
lt = ((-1),   0 )
rt = (  1 ,   0 )

allDirs :: (Integral a) => [(a,a)]
allDirs = [up,dn,lt,rt]

combineBitMasks :: BitMask -> BitMask -> BitMask
combineBitMasks x y = x .|. y
