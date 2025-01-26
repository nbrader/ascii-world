#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3

module WalkableBoundedWorld (WalkableBoundedWorld(WalkableBoundedWorld), nameOrder, addRocksToRightAndTop) where

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
                        , insertMaskAtPoint )

newtype WalkableBoundedWorld = WalkableBoundedWorld {asWorld :: AsciiWorld}

-- Assumes all rows have equal length
readWorld :: String -> (Int, WalkableBoundedWorld)
readWorld = fmap WalkableBoundedWorld . readAsciiWorld '.' ['S'] . addRocksToRightAndTop

showWorld :: Int -> WalkableBoundedWorld -> String
showWorld height w = showAsciiWorld height nameOrder (asWorld w)

removeForbidden :: WalkableBoundedWorld -> WalkableBoundedWorld
removeForbidden w = WalkableBoundedWorld $ applyNamedMask bitwiseSubtract "#" "O" (asWorld w)

progressByAStep :: WalkableBoundedWorld -> WalkableBoundedWorld
progressByAStep w = removeForbidden . WalkableBoundedWorld $ combineAsciiWorlds $ map (\dir -> moveNamedMask "O" dir (asWorld w)) lrduDirs

setOAtS :: WalkableBoundedWorld -> WalkableBoundedWorld
setOAtS = WalkableBoundedWorld . fromJust . insertMaskAtPoint "O" "S" . asWorld

oCount :: WalkableBoundedWorld -> Integer
oCount = toInteger . popCount . fromJust . M.lookup "O" . bWorldMasks . asWorld

nameOrder :: String -> String -> Ordering
nameOrder s1 s2 = comparing specialRank s1 s2 <> compare s1 s2
  where compareSpecial = comparing specialRank
        
        specialRank s = findIndex (==s) ["O","S","#","."]

addRocksToRightAndTop :: String -> String
addRocksToRightAndTop inStr = unlines . (\rows -> map (const '#') (head rows) : rows) . map (++"#") . lines $ inStr