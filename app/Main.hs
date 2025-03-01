#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3

{-# LANGUAGE OverloadedStrings #-}

module Main where

import AsciiWorld
import qualified Data.Map as M

-- A simple charMap: 
-- When reading a character, we decide whether to interpret it as a mask or as a point.
-- In this example:
--   'M' will be interpreted as a mask with name "M"
--   'P' will be interpreted as a point with name "P"
--   All other characters are ignored.
charMap :: Char -> Maybe (MaskOrPointsIndex String String)
charMap 'M' = Just (MaskIndex "M")
charMap 'P' = Just (PointsIndex "P")
charMap _   = Nothing

-- These two functions decide how to print the keys.
maskToChar :: String -> Char
maskToChar _ = 'X'  -- Print an 'X' for any mask

pointsToChar :: String -> Char
pointsToChar _ = 'O'  -- Print an 'O' for any point

-- A simple z-order comparator. For now we just use the standard ordering.
nameZOrder :: MaskOrPointsIndex String String -> MaskOrPointsIndex String String -> Ordering
nameZOrder = compare

main :: IO ()
main = do
  -- Define an ASCII representation.
  -- Note: rows are interpreted from top (first line) to bottom (last line)
  let input = unlines [ "M.."
                      , ".P."
                      , "..M" ]
  -- readAsciiWorld returns the height (number of rows) and an AsciiWorld value.
  let (height, world) = readAsciiWorld charMap input

  putStrLn "The ASCII world as read:"
  putStrLn $ showAsciiWorld height '.' maskToChar pointsToChar nameZOrder world
