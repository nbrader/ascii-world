#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3

-- To Do:
-- Make partitioner which gives every disconnected part of a layer it's own name by tacking on the next number not already existing in the bit mask map
--      Have the disconnected portions found by a flood fill algorithm using bit mask operations:
--          Loop until all points in world have been sufficiently checked to account for all part of all layers (need a nice way of checking this)
--              find an active point from the layer not so far a member of any partition part
--              Loop until latest found points are empty
--                  find new points by 'and'ing the latest found points in shifted up, down, left and right positions with the "visited" bit mask and 'or'ing them together
--                  xor these points (to subtract them) from the "visited" bit mask and make them the new "latest found points"
-- Make "WalkableWorld" with max walk distance fed in at construction to then add that much margin and so be able to detect reachability effects up to that distance.
-- Make non-zero bit with highest vertical position component tracked by data structure.

module WalkableWorld ( WalkableWorld(..)
                     , readWorld
                     , showWorld
                     , printWorld
                     , removeForbidden
                     , progressByAStep
                     , setOAtS
                     , oCount
                     , addNoGoToRightAndTop
                     , partitionLayerByReachableLRDU ) where

-------------
-- Imports --
-------------
import Data.List ( findIndex )
import qualified Data.Map as M
import Data.Maybe ( fromJust )
import Data.Ord
import Data.Bits

import Util ( lrduDirs )
import Mask ( bitwiseSubtract )

import AsciiWorld as AW ( AsciiWorld(..)
                        , readAsciiWorld
                        , showAsciiWorld
                        , combineAsciiWorlds
                        , moveNamedMask
                        , applyNamedMask
                        , insertMaskAtPoint
                        , prefixMasksAndPoints
                        , dropNCharsFromMasksAndPoints )

newtype WalkableWorld = WalkableWorld {asWorld :: AsciiWorld} deriving (Show)

-- Assumes all rows have equal length
readWorld :: Char -> String -> String -> (Int, WalkableWorld)
readWorld bgChar singularChars = fmap (WalkableWorld . prefixMasksAndPoints "_" ["#"]) . readAsciiWorld bgChar singularChars . addNoGoToRightAndTop

showWorld :: Int -> (String -> String -> Ordering) -> WalkableWorld -> String
showWorld height nameZOrder w = showAsciiWorld height nameZOrderWithSpecials . dropNCharsFromMasksAndPoints 1 ["#"] . asWorld $ w
  where nameZOrderWithSpecials :: String -> String -> Ordering
        nameZOrderWithSpecials s1 s2 = comparing specialRank s1 s2 <> nameZOrder s1 s2
          where specialRank s = findIndex (==s) ["O","S","#"]

printWorld :: Int -> (String -> String -> Ordering) -> WalkableWorld -> IO ()
printWorld height nameZOrder = putStrLn . showWorld height nameZOrder

removeForbidden :: WalkableWorld -> WalkableWorld
removeForbidden w = WalkableWorld $ applyNamedMask bitwiseSubtract "#" "O" (asWorld w)

progressByAStep :: WalkableWorld -> WalkableWorld
progressByAStep w = removeForbidden . WalkableWorld $ combineAsciiWorlds $ map (\dir -> moveNamedMask "O" dir (asWorld w)) lrduDirs

setOAtS :: WalkableWorld -> WalkableWorld
setOAtS = WalkableWorld . fromJust . insertMaskAtPoint "O" "S" . asWorld

oCount :: WalkableWorld -> Integer
oCount = toInteger . popCount . fromJust . M.lookup "O" . asciiWorldMasks . asWorld

addNoGoToRightAndTop :: String -> String
addNoGoToRightAndTop inStr = unlines . (\rows -> map (const '#') (head rows) : rows) . map (++"#") . lines $ inStr

-- Make partitioner which gives every disconnected part of a layer it's own name by tacking on the next number not already existing in the bit mask map
--      Have the disconnected portions found by a flood fill algorithm using bit mask operations:
--          Loop until all points in world have been sufficiently checked to account for all part of all layers (need a nice way of checking this)
--              find an active point from the layer not so far a member of any partition part
--              Loop until latest found points are empty
--                  find new points by 'and'ing the latest found points in shifted up, down, left and right positions with the "visited" bit mask and 'or'ing them together
--                  xor these points (to subtract them) from the "visited" bit mask and make them the new "latest found points"
partitionLayerByReachableLRDU :: String -> WalkableWorld -> WalkableWorld
partitionLayerByReachableLRDU = undefined
