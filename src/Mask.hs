#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci

{-|
Module      : Mask
Description : Low-level bitwise operations on Integer-based grid masks
Copyright   : (c) 2024
License     : BSD3
Maintainer  : your.email@example.com
Stability   : experimental

This module provides efficient bitwise operations for working with 2D grids
represented as 'Integer' types. Each bit in the Integer corresponds to a cell
in the grid, enabling O(1) set operations on entire regions.

The coordinate system uses (0,0) as the bottom-left corner, with x increasing
rightward and y increasing upward.

Example usage:

@
-- Create a 5x5 grid with points at (0,0) and (2,1)
let width = 5
let mask1 = pointToMask width (0, 0)  -- Bit 0 set
let mask2 = pointToMask width (2, 1)  -- Bit 7 set
let combined = bitwiseOr mask1 mask2  -- Both bits set

-- Check if they overlap
isOverlapping mask1 mask2  -- False (different positions)

-- Move mask right by 2
let moved = moveMask width (2, 0) mask1  -- Now at position (2,0)
isOverlapping moved mask2  -- Still False (different y-coordinates)
@
-}

module Mask where

-------------
-- Imports --
-------------
import Data.Bits
import GHC.Num
import Data.Tuple (swap)

-- | A 2D coordinate pair (x, y) where (0,0) is bottom-left.
-- x increases rightward, y increases upward.
type Point = (Int, Int)

-- | A bitmask representing a set of grid positions.
-- Each bit corresponds to a cell in the grid, with bit n
-- representing the cell at index n (row-major order).
type Mask = Integer

-------------
-- Conversion Functions
-------------

-- | Converts a 2D point to a 1D index in a grid of the given width.
--
-- The index is calculated as: @index = y * width + x@
--
-- ==== __Examples__
--
-- >>> pointToIndex 10 (0, 0)
-- 0
-- >>> pointToIndex 10 (3, 2)
-- 23
-- >>> pointToIndex 5 (2, 1)
-- 7
pointToIndex :: Int -> Point -> Int
pointToIndex width (x, y) = y * width + x

-- | Converts a 1D index to a 2D point in a grid of the given width.
--
-- This is the inverse of 'pointToIndex'.
--
-- ==== __Examples__
--
-- >>> indexToPoint 10 0
-- (0, 0)
-- >>> indexToPoint 10 23
-- (3, 2)
-- >>> indexToPoint 5 7
-- (2, 1)
indexToPoint :: Integral a => a -> a -> (a, a)
indexToPoint width i = swap (i `divMod` width)

-- | Creates a mask with a single bit set at the given point.
--
-- ==== __Examples__
--
-- >>> pointToMask 10 (0, 0)
-- 1
-- >>> pointToMask 10 (1, 0)
-- 2
-- >>> pointToMask 10 (0, 1)
-- 1024
pointToMask :: Int -> Point -> Mask
pointToMask width (x,y) = moveMask width (x,y) 1

-- | Converts a mask to a list of points.
--
-- Returns all positions where bits are set in the mask.
--
-- ==== __Examples__
--
-- >>> maskToPoints 10 1
-- [(0, 0)]
-- >>> maskToPoints 10 3
-- [(0, 0), (1, 0)]
-- >>> maskToPoints 10 (shift 1 23)
-- [(3, 2)]
maskToPoints :: Int -> Mask -> [Point]
maskToPoints width mask = go 0 mask
  where
    go _ 0 = []
    go idx n
      | testBit n 0 = indexToPoint width idx : go (idx + 1) (n `shiftR` 1)
      | otherwise   = go (idx + 1) (n `shiftR` 1)

-------------
-- Movement Functions
-------------

-- | Moves a mask by the given offset (dx, dy).
--
-- This is equivalent to shifting all set bits by the displacement vector.
-- Positive dx moves right, positive dy moves up.
--
-- ==== __Examples__
--
-- >>> moveMask 10 (1, 0) 1  -- Move bit at (0,0) right by 1
-- 2
-- >>> moveMask 10 (0, 1) 1  -- Move bit at (0,0) up by 1
-- 1024
-- >>> moveMask 10 (0, 0) 42  -- Move by (0,0) doesn't change mask
-- 42
moveMask :: Int -> (Int,Int) -> Mask -> Mask
moveMask width (dx,dy) pts = pts `shift` pointToIndex width (dx,dy)

-- | Moves a point by the given offset (dx, dy).
--
-- ==== __Examples__
--
-- >>> movePoint (1, 0) (3, 5)
-- (4, 5)
-- >>> movePoint (0, 1) (3, 5)
-- (3, 6)
-- >>> movePoint (-1, -1) (3, 5)
-- (2, 4)
movePoint :: (Int,Int) -> (Int,Int) -> (Int,Int)
movePoint (dx,dy) (x,y) = (x+dx,y+dy)

-------------
-- Overlap and Intersection
-------------

-- | Checks if two masks have any overlapping bits.
--
-- This is an O(1) operation using bitwise AND.
--
-- ==== __Examples__
--
-- >>> isOverlapping 0b1100 0b0110
-- True
-- >>> isOverlapping 0b1100 0b0011
-- False
-- >>> isOverlapping 0 0b1111
-- False
--
-- Complexity: O(1)
isOverlapping :: Mask -> Mask -> Bool
isOverlapping ps1 ps2 = (ps1 .&. ps2) /= zeroBits

-- | Checks if a point overlaps with a mask.
--
-- ==== __Examples__
--
-- >>> let mask = pointToMask 10 (2, 1)
-- >>> isPointOverlappingMask 10 (2, 1) mask
-- True
-- >>> isPointOverlappingMask 10 (3, 1) mask
-- False
isPointOverlappingMask :: Int -> (Int,Int) -> Mask -> Bool
isPointOverlappingMask width p m = pointToMask width p `isOverlapping` m

-------------
-- Bitwise Set Operations
-------------

-- | Subtracts one mask from another (set difference).
--
-- Returns all bits in the first mask that are not in the second.
--
-- ==== __Examples__
--
-- >>> bitwiseSubtract 0b1111 0b0110
-- 9  -- 0b1001
-- >>> bitwiseSubtract 0b1010 0b0001
-- 10  -- 0b1010 (no change)
bitwiseSubtract :: Mask -> Mask -> Mask
bitwiseSubtract ps1 ps2 = (ps1 .&. complement ps2)

-- | Computes the bitwise AND of two masks (intersection).
--
-- Returns only bits that are set in both masks.
--
-- ==== __Examples__
--
-- >>> bitwiseAnd 0b1100 0b1010
-- 8  -- 0b1000
-- >>> bitwiseAnd 0 0b1111
-- 0
bitwiseAnd :: Mask -> Mask -> Mask
bitwiseAnd ps1 ps2 = (ps1 .&. ps2)

-- | Computes the bitwise OR of two masks (union).
--
-- Returns all bits that are set in either mask.
--
-- ==== __Examples__
--
-- >>> bitwiseOr 0b1100 0b0011
-- 15  -- 0b1111
-- >>> bitwiseOr 0b1010 0b0101
-- 15  -- 0b1111
bitwiseOr :: Mask -> Mask -> Mask
bitwiseOr ps1 ps2 = (ps1 .|. ps2)

-- | Computes the bitwise XOR of two masks (symmetric difference).
--
-- Returns bits that are set in exactly one of the masks.
--
-- ==== __Examples__
--
-- >>> bitwiseXor 0b1100 0b1010
-- 6  -- 0b0110
-- >>> bitwiseXor 0b1111 0b1111
-- 0
-- >>> bitwiseXor 0b1010 0
-- 10  -- 0b1010
bitwiseXor :: Mask -> Mask -> Mask
bitwiseXor ps1 ps2 = (ps1 `xor` ps2)

-------------
-- Bit Analysis Functions
-------------

-- | Finds the index of the most significant bit (MSB) in a mask.
--
-- Throws an error if the input is non-positive.
--
-- ==== __Examples__
--
-- >>> msbIndex 1
-- 0
-- >>> msbIndex 2
-- 1
-- >>> msbIndex 255
-- 7
-- >>> msbIndex 256
-- 8
--
-- Warning: fromIntegral may truncate an exceptionally large Integer (larger than max Int)
msbIndex :: Integer -> Int
msbIndex n = if n <= 0 then error "msb requires a positive integer input" else fromIntegral (integerLog2 n)

-- | Removes the MSB k times from an integer.
--
-- ==== __Examples__
--
-- >>> removeMsbNTimes 1 0b1010  -- Remove 1 MSB
-- 2  -- 0b0010
-- >>> removeMsbNTimes 2 0b1111  -- Remove 2 MSBs
-- 3  -- 0b0011
--
-- Warning: fromIntegral may truncate an exceptionally large Integer (larger than max Int)
removeMsbNTimes :: Int -> Integer -> Integer
removeMsbNTimes 0 n = n  -- Base case: No removals left
removeMsbNTimes _ 0 = 0  -- No MSB to remove
removeMsbNTimes k n
  | k <= 0    = n
  | otherwise = removeMsbNTimes (k - 1) (n `xor` bit (fromIntegral (integerLog2 n)))

-- | Finds the index of the middle bit in a mask (by count of set bits).
--
-- For a mask with n set bits, this returns the index of the (n/2)th set bit.
--
-- Warning: fromIntegral may truncate an exceptionally large Integer (larger than max Int)
middleIndex :: Integer -> Int
middleIndex n = fromIntegral . msbIndex $ (removeMsbNTimes halfActiveBits n)
  where halfActiveBits = popCount n `div` 2

-- | Finds the point corresponding to the MSB in a mask.
--
-- ==== __Examples__
--
-- >>> msbPoint 10 1
-- (0, 0)
-- >>> msbPoint 10 (shift 1 23)
-- (3, 2)
msbPoint :: Int -> Integer -> Point
msbPoint width n = indexToPoint width (msbIndex n)

-- | Finds the point corresponding to the middle bit in a mask.
--
-- ==== __Examples__
--
-- >>> middlePoint 10 0b1111  -- 4 bits set, middle is 2nd bit
-- (1, 0)
middlePoint :: Int -> Integer -> Point
middlePoint width n = indexToPoint width (middleIndex n)

-------------
-- Width Transformation Functions
-------------

-- | Expands a mask to a wider grid while preserving point positions.
--
-- Internal function for 'changeMaskWidthBy'. Only works for non-negative delta.
expandMaskWidth :: Int -> Int -> Mask -> Mask
expandMaskWidth oldWidth delta x
    | 1 `shift` oldWidth > x = x
    | otherwise = let bitsOnHigherRows = x `shift` (-oldWidth)
                      bitsOnThisRow = x - (bitsOnHigherRows `shift` oldWidth)
                  in bitsOnThisRow + ((expandMaskWidth oldWidth delta bitsOnHigherRows) `shift` (oldWidth+delta))

-- | Reduces a mask to a narrower grid while preserving point positions.
--
-- Internal function for 'changeMaskWidthBy'. Handles negative delta values.
reduceMaskWidth :: Int -> Int -> Mask -> Mask
reduceMaskWidth oldWidth delta x
    | 1 `shift` oldWidth > x = let choppedBits = x `shift` (delta-oldWidth)
                                   remaining = x - (choppedBits `shift` (oldWidth-delta))
                               in remaining
    | otherwise = let bitsOnHigherRows = x `shift` (-oldWidth)
                      bitsOnThisRow = x - (bitsOnHigherRows `shift` oldWidth)
                      choppedBits = bitsOnThisRow `shift` (delta-oldWidth)
                      remaining = bitsOnThisRow - (choppedBits `shift` (oldWidth-delta))
                  in remaining + ((reduceMaskWidth oldWidth delta bitsOnHigherRows) `shift` (oldWidth-delta))

-- | Changes a mask's grid width by the given delta.
--
-- This re-maps all points to maintain their (x,y) coordinates in a grid
-- with a different width. Positive delta expands, negative delta reduces.
--
-- ==== __Examples__
--
-- >>> let mask = pointToMask 3 (0, 2)  -- Point at (0,2) in 3-wide grid
-- >>> changeMaskWidthBy 3 2 mask  -- Expand to 5-wide grid
-- 1024  -- Point now at bit position (0,2) in 5-wide grid
changeMaskWidthBy :: Int -> Int -> Mask -> Mask
changeMaskWidthBy oldWidth delta x
    | delta <= 0 = reduceMaskWidth oldWidth (-delta) x
    | otherwise  = expandMaskWidth oldWidth delta x

-- | Sets a mask to a new grid width.
--
-- This is a convenience wrapper for 'changeMaskWidthBy'.
--
-- ==== __Examples__
--
-- >>> let mask = pointToMask 3 (1, 1)  -- Point at (1,1) in 3-wide grid
-- >>> setMaskWidth 3 5 mask  -- Convert to 5-wide grid
-- 32  -- Point now at correct position in 5-wide grid
setMaskWidth :: Int -> Int -> Mask -> Mask
setMaskWidth oldWidth newWidth x = changeMaskWidthBy oldWidth (newWidth - oldWidth) x
