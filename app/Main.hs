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
charMap :: Char -> Maybe (WorldKey String String)
charMap 'M' = Just (WKMask "M")
charMap 'P' = Just (WKPoints "P")
charMap _   = Nothing

-- These two functions decide how to print the keys.
maskToChar :: String -> Char
maskToChar _ = 'X'  -- Print an 'X' for any mask

pointsToChar :: String -> Char
pointsToChar _ = 'O'  -- Print an 'O' for any point

-- A simple z-order comparator. For now we just use the standard ordering.
nameZOrder :: WorldKey String String -> WorldKey String String -> Ordering
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
