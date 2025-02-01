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
import Data.Either ( fromLeft, isLeft )
import Data.Ord
import Data.Bits

import Util ( lrduDirs )
import Mask ( bitwiseSubtract, bitwiseXor, msbIndex, middleIndex, msbPoint, middlePoint )

import AsciiWorld as AW ( AsciiWorld(..)
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
                        , deleteMask
                        , lookupMask
                        , adjustMask
                        , updateMask
                        , alterMask
                        , msbPointOfMask
                        , middlePointOfMask )

data WWMaskKey = NoGo | Marked | MidPointMask deriving (Show, Eq, Ord)
data WWPointKey = Agent | TemporaryPoints deriving (Show, Eq, Ord)
type RawAsciiWorld km kp = AsciiWorld (Either km WWMaskKey) (Either kp WWPointKey)
type WWNameZComp km kp = (Either (Either km WWMaskKey) (Either kp WWPointKey) -> Either (Either km WWMaskKey) (Either kp WWPointKey) -> Ordering)
newtype WalkableWorld km kp = WalkableWorld {getRawAsciiWorld :: RawAsciiWorld km kp} deriving (Show)

addNoGoToRightAndTop :: (Ord km, Ord kp) => RawAsciiWorld km kp -> RawAsciiWorld km kp
addNoGoToRightAndTop w = undefined -- To Do: Implement this

removeNoGoFromRightAndTop :: (Ord km, Ord kp) => RawAsciiWorld km kp -> RawAsciiWorld km kp
removeNoGoFromRightAndTop w = undefined -- To Do: Implement this

addWalkableWorldParts :: (Ord km, Ord kp) => AsciiWorld (Either km WWMaskKey) (Either kp WWPointKey) -> WalkableWorld km kp
addWalkableWorldParts = WalkableWorld . addNoGoToRightAndTop . changeWidthBy 1

undoWalkableWorldParts :: (Ord km, Ord kp) => WalkableWorld km kp -> AsciiWorld (Either km WWMaskKey) (Either kp WWPointKey)
undoWalkableWorldParts = changeWidthBy (-1) . removeNoGoFromRightAndTop . getRawAsciiWorld

-- Assumes all rows have equal length
readWorld :: (Ord km, Ord kp) => (Char -> Maybe (Either km kp)) -> String -> (Int, WalkableWorld km kp)
readWorld charMap = fmap addWalkableWorldParts . readAsciiWorld charMap'
  where charMap' c
            = do
                c' <- charMap c
                return $ case c' of
                    Left  x1 -> Left (Left x1)
                    Right x2 -> Right (Left x2)

-- This modify modifies the underlying asciiWorld directly, including all of the stuff that WalkableWorld did to it (such as NoGos and underscores in names)
modifyRawAsciiWorld :: (Ord km, Ord kp) => (RawAsciiWorld km kp -> RawAsciiWorld km kp) -> WalkableWorld km kp -> WalkableWorld km kp
modifyRawAsciiWorld f = WalkableWorld . f . getRawAsciiWorld

-- This modify allows you to modify the world in a way ignorant to the stuff that WalkableWorld added (such as NoGos and underscores in names)
modifyAsciiWorld :: (Ord km, Ord kp) => (AsciiWorld km kp -> AsciiWorld km kp) -> WalkableWorld km kp -> WalkableWorld km kp
modifyAsciiWorld f = undefined -- Ensure the NoGos, underscores and whatever else are torn down before applying f before putting them back after.

showWorld :: (Ord km, Ord kp) => Int -> Char -> (Either km WWMaskKey -> Char) -> (Either kp WWPointKey -> Char) -> WWNameZComp km kp -> WalkableWorld km kp -> String
showWorld height bgChar maskToChar pointsToChar nameZOrder w = showAsciiWorld height bgChar maskToChar pointsToChar nameZOrder . undoWalkableWorldParts $ w

-- Shows the raw underlying ascii world except for underscores which are stripped so that there aren't just underscores for all non-background point.
showRawAsciiWorld :: (Ord km, Ord kp) => Int -> Char -> (Either km WWMaskKey -> Char) -> (Either kp WWPointKey -> Char) -> WWNameZComp km kp -> WalkableWorld km kp -> String
showRawAsciiWorld height bgChar maskToChar pointsToChar nameZOrder w = showAsciiWorld height bgChar maskToChar pointsToChar nameZOrderWithSpecials . getRawAsciiWorld $ w
  where --nameZOrderWithSpecials :: String -> String -> Ordering
        nameZOrderWithSpecials = undefined -- To Do: Make this more useful

printWorld :: (Ord km, Ord kp) => Int -> Char -> (Either km WWMaskKey -> Char) -> (Either kp WWPointKey -> Char) -> WWNameZComp km kp -> WalkableWorld km kp -> IO ()
printWorld height bgChar maskToChar pointsToChar nameZOrder = putStrLn . showWorld height bgChar maskToChar pointsToChar nameZOrder

printRawAsciiWorld :: (Ord km, Ord kp) => Int -> Char -> (Either km WWMaskKey -> Char) -> (Either kp WWPointKey -> Char) -> WWNameZComp km kp -> WalkableWorld km kp -> IO ()
printRawAsciiWorld height bgChar maskToChar pointsToChar nameZOrder = putStrLn . showRawAsciiWorld height bgChar maskToChar pointsToChar nameZOrder

-- removeForbidden :: (Ord km, Ord kp) => WalkableWorld km kp -> WalkableWorld km kp
-- removeForbidden w = WalkableWorld $ applyNamedMask bitwiseSubtract "#" "O" (getRawAsciiWorld w)

-- progressByAStep :: (Ord km, Ord kp) => WalkableWorld km kp -> WalkableWorld km kp
-- progressByAStep w = removeForbidden . WalkableWorld $ combineAsciiWorlds $ map (\dir -> moveNamedMask "O" dir (getRawAsciiWorld w)) lrduDirs

-- setOAtS :: (Ord km, Ord kp) => WalkableWorld km kp -> WalkableWorld km kp
-- setOAtS = WalkableWorld . fromJust . insertMaskAtPoint "O" "S" . getRawAsciiWorld

totalHorizontalEdgesOverPoints :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalHorizontalEdgesOverPoints maskName w = toInteger . popCount . fromJust . M.lookup (Right Marked) . asciiWorldMasks . applyNamedMask bitwiseXor (Left maskName) (Right Marked) . moveNamedMask (Right Marked) (0,1) . copyNamedMask (Left maskName) (Right Marked) . getRawAsciiWorld $ w

totalVerticalEdgesOverPoints :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalVerticalEdgesOverPoints maskName w = toInteger . popCount . fromJust . M.lookup (Right Marked) . asciiWorldMasks . applyNamedMask bitwiseXor (Left maskName) (Right Marked) . moveNamedMask (Right Marked) (1,0) . copyNamedMask (Left maskName) (Right Marked) . getRawAsciiWorld $ w

totalEdgesOverPoints :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalEdgesOverPoints maskName w = totalHorizontalEdgesOverPoints maskName w + totalVerticalEdgesOverPoints maskName w

totalPoints :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalPoints maskName w = toInteger . popCount . fromJust . M.lookup (Left maskName) . asciiWorldMasks . getRawAsciiWorld $ w

maskNames :: WalkableWorld a kp -> [Either (Either a WWMaskKey) b -> Either a WWMaskKey]
maskNames = map (fromLeft) . filter (isLeft) . M.keys . asciiWorldMasks . getRawAsciiWorld

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
        
        middlePoint = let maybeMiddlePoint = middlePointOfMask (Left maskName) w'
                       in case maybeMiddlePoint of
                            Just point -> point
                            Nothing -> error $ "middlePoint failed: \"" ++ maskName  ++ "\" not found in " ++ show w'
        wWithXMidpointMask = deletePoints (Right TemporaryPoints) . fromJust . insertMaskFromPoints (Right MidPointMask) (Right TemporaryPoints) . setPoint (Right TemporaryPoints) middlePoint $ w'
        wWithMidpointXoredWithMaskName = deleteMask (Right MidPointMask) . applyNamedMask bitwiseXor (Right MidPointMask) (Left maskName) $ wWithXMidpointMask
        newAsciiWorld = wWithMidpointXoredWithMaskName

test = do
    contents <- readFile "day12 (data).csv"
    
    let 
        maskNameToKeep = 'C'
        masksToDelete = map Left . delete maskNameToKeep . nub $ contents
        
        charMap c = Just (Left c)
        
        (height, initWorld) = readWorld charMap contents
        worldBeforePartition = foldl' (\asciiWorld maskName -> modifyRawAsciiWorld (deleteMask maskName) asciiWorld) initWorld masksToDelete
        
        -- world = partitionMaskByReachableLRDU "C" worldBeforePartition
        (WalkableWorld w') = worldBeforePartition
        
        middlePoint = let maybeMiddlePoint = middlePointOfMask (Left maskNameToKeep) w'
                       in case maybeMiddlePoint of
                            Just point -> point
                            Nothing -> error $ "middlePoint failed: \"" ++ [maskNameToKeep]  ++ "\" not found in " ++ show w'
        wWithXMidpointMask = deletePoints (Right TemporaryPoints) . fromJust . insertMaskFromPoints (Right MidPointMask) (Right TemporaryPoints) . setPoint (Right TemporaryPoints) middlePoint $ w'
        
        wWithMidpointXoredWithMaskName = applyNamedMask bitwiseXor (Right MidPointMask) (Left maskNameToKeep) $ wWithXMidpointMask
        newAsciiWorld = wWithMidpointXoredWithMaskName
        
        newWorld = WalkableWorld newAsciiWorld
    
    printRawAsciiWorld height '.' (either id (head . show)) (either id (head . show)) (comparing id) newWorld
    print newWorld

partitionAllMasksByReachableLRDU :: (Ord km, Ord kp) => WalkableWorld km kp -> WalkableWorld km kp
-- partitionAllMasksByReachableLRDU w = foldl' (flip partitionMaskByReachableLRDU) w (maskNames w)
partitionAllMasksByReachableLRDU w = undefined
