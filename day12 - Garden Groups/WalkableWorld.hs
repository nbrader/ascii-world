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
                     , totalEdgesOverPoints
                     , maskNames
                     , totalPoints
                     , partitionMaskByReachableLRDU
                     , partitionAllMasksByReachableLRDU ) where

-------------
-- Imports --
-------------
import Data.List ( findIndex, foldl', nub, delete )
import qualified Data.Map as M
import Data.Maybe ( fromJust )
import Data.Either ( lefts )
import Data.Function
import Data.Ord
import Data.Bits

import Util ( lrduDirs )
import Mask ( bitwiseSubtract, bitwiseXor, msbIndex, middleIndex, msbPoint, middlePoint )

import AsciiWorld as AW ( AsciiWorld(..)
                        , WorldKey(..)
                        , readAsciiWorld
                        , showAsciiWorld
                        , combineAsciiWorlds
                        , moveNamedMask
                        , copyNamedMask
                        , applyNamedMask
                        , setPoint
                        , deletePoints
                        , insertMaskFromPoints
                        , setWidth
                        , changeWidthBy
                        , mapKeyForMasks
                        , mapKeyForPoints
                        , deleteMask
                        , lookupMask
                        , adjustMask
                        , updateMask
                        , alterMask
                        , msbPointOfMask
                        , middlePointOfMask )

data WWMaskKey = NoGo | Marked | MidPointMask deriving (Show, Eq, Ord)
data WWPointsKey = Agent | TemporaryPoints deriving (Show, Eq, Ord)
data WWKey kExt kInt = WWExternal kExt | WWInternal kInt deriving (Show, Eq, Ord)
fromWWKey :: WWKey kExt kInt -> Either kExt kInt
fromWWKey (WWExternal x) = Left x
fromWWKey (WWInternal y) = Right y
toWWKey :: Either kExt kInt -> WWKey kExt kInt
toWWKey (Left x)  = WWExternal x
toWWKey (Right y) = WWInternal y
fromWWExternal (WWExternal x) = x

type RawAsciiWorld km kp = AsciiWorld (WWKey km WWMaskKey) (WWKey kp WWPointsKey)

type WWNameZComp km kp = (WorldKey (WWKey km WWMaskKey) (WWKey kp WWPointsKey) -> WorldKey (WWKey km WWMaskKey) (WWKey kp WWPointsKey) -> Ordering)
newtype WalkableWorld km kp = WalkableWorld {getRawAsciiWorld :: RawAsciiWorld km kp} deriving (Show)

addNoGoToRightAndTop :: (Ord km, Ord kp) => RawAsciiWorld km kp -> RawAsciiWorld km kp
addNoGoToRightAndTop w = undefined -- To Do: Implement this

removeNoGoFromRightAndTop :: (Ord km, Ord kp) => RawAsciiWorld km kp -> RawAsciiWorld km kp
removeNoGoFromRightAndTop w = undefined -- To Do: Implement this

addWalkableWorldParts :: (Ord km, Ord kp) => RawAsciiWorld km kp -> WalkableWorld km kp
addWalkableWorldParts = WalkableWorld . addNoGoToRightAndTop . changeWidthBy 1

undoWalkableWorldParts :: (Ord km, Ord kp) => WalkableWorld km kp -> RawAsciiWorld km kp
undoWalkableWorldParts = changeWidthBy (-1) . removeNoGoFromRightAndTop . getRawAsciiWorld

-- Assumes all rows have equal length
readWorld :: (Ord km, Ord kp) => (Char -> Maybe (WorldKey km kp)) -> String -> (Int, WalkableWorld km kp)
readWorld charMap = fmap addWalkableWorldParts . readAsciiWorld charMap'
  where charMap' c
            = do
                c' <- charMap c
                return $ case c' of
                    WKMask   x1 -> WKMask   (WWExternal x1)
                    WKPoints x2 -> WKPoints (WWExternal x2)

-- This modify modifies the underlying asciiWorld directly, including all of the stuff that WalkableWorld did to it (such as NoGos and underscores in names)
modifyRawAsciiWorld :: (Ord km, Ord kp) => (RawAsciiWorld km kp -> RawAsciiWorld km kp) -> WalkableWorld km kp -> WalkableWorld km kp
modifyRawAsciiWorld f = WalkableWorld . f . getRawAsciiWorld

-- This modify allows you to modify the world in a way ignorant to the stuff that WalkableWorld added (such as NoGos and underscores in names)
-- Warning: I think this function will perform badly.
modifyAsciiWorld :: (Ord km, Ord kp) => (AsciiWorld km kp -> AsciiWorld km kp) -> WalkableWorld km kp -> WalkableWorld km kp
modifyAsciiWorld f = addWalkableWorldParts . mapKeyForMasks WWExternal . mapKeyForPoints WWExternal . f . mapKeyForPoints fromWWExternal . mapKeyForMasks fromWWExternal . undoWalkableWorldParts

showWorld :: (Ord km, Ord kp) => Int -> Char -> (WWKey km WWMaskKey -> Char) -> (WWKey kp WWPointsKey -> Char) -> WWNameZComp km kp -> WalkableWorld km kp -> String
showWorld height bgChar maskToChar pointsToChar nameZOrder w = showAsciiWorld height bgChar maskToChar pointsToChar nameZOrder . undoWalkableWorldParts $ w

-- Shows the raw underlying ascii world except for underscores which are stripped so that there aren't just underscores for all non-background point.
showRawAsciiWorld :: (Ord km, Ord kp) => Int -> Char -> (WWKey km WWMaskKey -> Char) -> (WWKey kp WWPointsKey -> Char) -> WWNameZComp km kp -> WalkableWorld km kp -> String
showRawAsciiWorld height bgChar maskToChar pointsToChar nameZOrder w = showAsciiWorld height bgChar maskToChar pointsToChar nameZOrderWithSpecials . getRawAsciiWorld $ w
  where --nameZOrderWithSpecials :: String -> String -> Ordering
        nameZOrderWithSpecials = undefined -- To Do: Make this more useful

printWorld :: (Ord km, Ord kp) => Int -> Char -> (WWKey km WWMaskKey -> Char) -> (WWKey kp WWPointsKey -> Char) -> WWNameZComp km kp -> WalkableWorld km kp -> IO ()
printWorld height bgChar maskToChar pointsToChar nameZOrder = putStrLn . showWorld height bgChar maskToChar pointsToChar nameZOrder

printRawAsciiWorld :: (Ord km, Ord kp) => Int -> Char -> (WWKey km WWMaskKey -> Char) -> (WWKey kp WWPointsKey -> Char) -> WWNameZComp km kp -> WalkableWorld km kp -> IO ()
printRawAsciiWorld height bgChar maskToChar pointsToChar nameZOrder = putStrLn . showRawAsciiWorld height bgChar maskToChar pointsToChar nameZOrder

-- removeForbidden :: (Ord km, Ord kp) => WalkableWorld km kp -> WalkableWorld km kp
-- removeForbidden w = WalkableWorld $ applyNamedMask bitwiseSubtract "#" "O" (getRawAsciiWorld w)

-- progressByAStep :: (Ord km, Ord kp) => WalkableWorld km kp -> WalkableWorld km kp
-- progressByAStep w = removeForbidden . WalkableWorld $ combineAsciiWorlds $ map (\dir -> moveNamedMask "O" dir (getRawAsciiWorld w)) lrduDirs

-- setOAtS :: (Ord km, Ord kp) => WalkableWorld km kp -> WalkableWorld km kp
-- setOAtS = WalkableWorld . fromJust . insertMaskAtPoint "O" "S" . getRawAsciiWorld

totalHorizontalEdgesOverPoints :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalHorizontalEdgesOverPoints maskName w =
    w & getRawAsciiWorld
      & copyNamedMask (WWExternal maskName) (WWInternal Marked)
      & moveNamedMask (WWInternal Marked) (0,1)
      & applyNamedMask bitwiseXor (WWExternal maskName) (WWInternal Marked)
      & getMarked
      & countMaskPoints
  
  where getMarked = fromJust . M.lookup (WWInternal Marked) . asciiWorldMasks
        countMaskPoints = toInteger . popCount

totalVerticalEdgesOverPoints :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalVerticalEdgesOverPoints maskName w =
    w & getRawAsciiWorld
      & copyNamedMask (WWExternal maskName) (WWInternal Marked)
      & moveNamedMask (WWInternal Marked) (1,0)
      & applyNamedMask bitwiseXor (WWExternal maskName) (WWInternal Marked)
      & getMarked
      & countMaskPoints
  where
    getMarked = fromJust . M.lookup (WWInternal Marked) . asciiWorldMasks
    countMaskPoints = toInteger . popCount

totalEdgesOverPoints :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalEdgesOverPoints maskName w =
    totalHorizontalEdgesOverPoints maskName w +
    totalVerticalEdgesOverPoints maskName w

totalPoints :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalPoints maskName w =
    w & getRawAsciiWorld
      & getMask
      & countMaskPoints
  where
    getMask = fromJust . M.lookup (WWExternal maskName) . asciiWorldMasks
    countMaskPoints = toInteger . popCount

maskNames :: WalkableWorld a kp -> [a]
maskNames w =
    w & getRawAsciiWorld
      & asciiWorldMasks
      & M.keys
      & map fromWWKey
      & lefts

-- Make partitioner which gives every disconnected part of a layer its own name by tacking on the next number not already existing in the bit mask map
--      Have the disconnected portions found by a flood fill algorithm using bit mask operations:
--          Loop until all points in world have been sufficiently checked to account for all part of all layers (need a nice way of checking this)
--              find an active point from the layer not so far a member of any partition part
--                  Note: -A MSB could be found using log.
--                        -Also, an active bit would ideally be found in the middle of the pack to maximise the expansion outwards per iteration
--                          This might be better achieved than simply finding the MSB by first removing half of the total active bits (rounding up) before finding the MSB after
--              Loop until latest found points are empty
--                  find new points by 'and'ing the latest found points in shifted up, down, left and right positions with the "visited" bit mask and 'or'ing them together
--                  xor these points (to subtract them) from the "visited" bit mask and make them the new "latest found points"
partitionMaskByReachableLRDU :: (Show kp, Ord kp) => [Char] -> WalkableWorld [Char] kp -> WalkableWorld [Char] kp
partitionMaskByReachableLRDU maskName (WalkableWorld w') = undefined --WalkableWorld newAsciiWorld
  where -- To Do: This implementation is a WIP. Make it behave like the above explained algorithm.
        --        Currently, what it does is simply removes the midpoint of every mask.
        
        middlePoint = let maybeMiddlePoint = middlePointOfMask (WWExternal maskName) w'
                       in case maybeMiddlePoint of
                            Just point -> point
                            Nothing -> error $ "middlePoint failed: \"" ++ maskName  ++ "\" not found in " ++ show w'
        wWithXMidpointMask = deletePoints (WWInternal TemporaryPoints) . fromJust . insertMaskFromPoints (WWInternal MidPointMask) (WWInternal TemporaryPoints) . setPoint (WWInternal TemporaryPoints) middlePoint $ w'
        wWithMidpointXoredWithMaskName = deleteMask (WWInternal MidPointMask) . applyNamedMask bitwiseXor (WWInternal MidPointMask) (WWExternal maskName) $ wWithXMidpointMask
        newAsciiWorld = wWithMidpointXoredWithMaskName

test = do
    contents <- readFile "day12 (data).csv"
    
    let 
        maskNameToKeep = 'C'
        masksToDelete = map WWExternal . delete maskNameToKeep . nub $ contents
        
        charMap c = Just (WKMask c)
        
        (height, initWorld) = readWorld charMap contents
        worldBeforePartition = foldl' (\asciiWorld maskName -> modifyRawAsciiWorld (deleteMask maskName) asciiWorld) initWorld masksToDelete
        
        -- world = partitionMaskByReachableLRDU "C" worldBeforePartition
        (WalkableWorld w') = worldBeforePartition
        
        middlePoint = let maybeMiddlePoint = middlePointOfMask (WWExternal maskNameToKeep) w'
                       in case maybeMiddlePoint of
                            Just point -> point
                            Nothing -> error $ "middlePoint failed: \"" ++ [maskNameToKeep]  ++ "\" not found in " ++ show w'
        wWithXMidpointMask = deletePoints (WWInternal TemporaryPoints) . fromJust . insertMaskFromPoints (WWInternal MidPointMask) (WWInternal TemporaryPoints) . setPoint (WWInternal TemporaryPoints) middlePoint $ w'
        
        wWithMidpointXoredWithMaskName = applyNamedMask bitwiseXor (WWInternal MidPointMask) (WWExternal maskNameToKeep) $ wWithXMidpointMask
        newAsciiWorld = wWithMidpointXoredWithMaskName
        
        newWorld = WalkableWorld newAsciiWorld
    
    printRawAsciiWorld height '.' (either id (head . show) . fromWWKey) (either id (head . show) . fromWWKey) (comparing id) newWorld
    print newWorld

partitionAllMasksByReachableLRDU :: (Ord km, Ord kp) => WalkableWorld km kp -> WalkableWorld km kp
-- partitionAllMasksByReachableLRDU w = foldl' (flip partitionMaskByReachableLRDU) w (maskNames w)
partitionAllMasksByReachableLRDU w = undefined
