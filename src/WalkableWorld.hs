#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3

-- To Do:
-- Make "WalkableWorld" with max walk distance fed in at construction to then add that much margin and so be able to detect reachability effects up to that distance.

module WalkableWorld    ( WalkableWorld(..)
                        , WorldKey(..)
                        , readWorld
                        , showWorld
                        , printWorld
                        , Ext_Int(..)
                        , WWMaskKey
                        , WWPointsKey
                        , printRawAsciiWorld
                        , totalEdgesOverPoints
                        , totalConnectedEdges
                        , totalConnectedOneSidedEdges
                        , maskKeys
                        , totalPoints
                        , partitionMaskByReachableLRDU
                        , partitionAllMasksByReachableLRDU
                        , modifyRawAsciiWorld
                        -- , combineTwoWalkableWorlds
                        -- , combineWalkableWorlds
                        -- , inWWIsPointOverlappingPointsKey
                        -- , inWWIsPointOverlappingMaskKey
                        -- , inWWIsPointOverlappingPointsKeyOrMaskKey
                        -- , moveMaskOfNameByInWW
                        -- , movePointsOfNameByInWW
                        , addMaskInWW
                        -- , deleteMaskInWW
                        , filterMaskKeysInWW
                        -- , filterMasksInWW
                        -- , lookupMaskInWW
                        -- , adjustMaskInWW
                        -- , updateMaskInWW
                        -- , alterMaskInWW
                        -- , copyMaskInWW
                        -- , applyMaskInWW
                        -- , setPointInWW
                        -- , deletePointsInWW
                        -- , insertMaskFromPointsInWW
                        -- , inWWMaybeInsertMaskKeyFromPointsKey
                        -- , isOverlappingMasksInWW
                        ) where

-------------
-- Imports --
-------------
import Data.List ( findIndex, foldl', nub, delete )
import qualified Data.Map as M
import Data.Maybe ( fromJust )
import Data.Either ( lefts )
import Data.Function
import Data.Bifunctor
import Data.Ord
import Data.Bits
-- import Debug.Trace (trace)

import Util ( lrduDirs )
import Mask ( Mask
            , Point
            , bitwiseSubtract
            , bitwiseOr
            , bitwiseXor
            , bitwiseAnd
            , msbIndex
            , middleIndex
            , msbPoint
            , middlePoint
            , pointToMask )

import AsciiWorld as AW ( AsciiWorld(..)
                        , WorldKey(..)
                        , fromWorldKey
                        , toWorldKey
                        , readAsciiWorld
                        , showAsciiWorld
                        , setWidth
                        , changeWidthBy
                        , mapKeyForMasks
                        , mapKeyForPoints
                        , msbPointOfMask
                        , middlePointOfMask
                        , combineTwoAsciiWorlds
                        , combineAsciiWorlds
                        , inWorldIsPointOverlappingPointsKey
                        , inWorldIsPointsKeyOverlappingMaskKey
                        , inWorldIsPointOverlappingMaskKey
                        , inWorldIsPointOverlappingPointsKeyOrMaskKey
                        , moveMaskOfNameBy
                        , movePointsOfNameBy
                        , addMask
                        , deleteMask
                        , filterMaskKeys
                        , filterMasks
                        , lookupMask
                        , adjustMask
                        , updateMask
                        , alterMask
                        , copyMask
                        , applyMask
                        , setPoint
                        , deletePoints
                        , insertMaskFromPoints
                        , inWorldMaybeInsertMaskKeyFromPointsKey
                        , isOverlappingMasks )

data WalkableWorld km kp = WalkableWorld {wwHeight :: Int, wwRawAsciiWorld :: RawAsciiWorld km kp} deriving (Show)

type RawAsciiWorld km kp = AsciiWorld (Ext_Int km WWMaskKey) (Ext_Int kp WWPointsKey)
toHeightAndRawAsciiWorld :: WalkableWorld km kp -> (Int, RawAsciiWorld km kp)
toHeightAndRawAsciiWorld w = (wwHeight w, wwRawAsciiWorld w)
fromHeightAndRawAsciiWorld :: (Int, RawAsciiWorld km kp) -> WalkableWorld km kp
fromHeightAndRawAsciiWorld (h, w) = WalkableWorld h w
type WWNameZComp km kp = WorldKey (Ext_Int km WWMaskKey) (Ext_Int kp WWPointsKey) -> WorldKey (Ext_Int km WWMaskKey) (Ext_Int kp WWPointsKey) -> Ordering

data Ext_Int kExt kInt = External kExt | Internal kInt deriving (Show, Eq, Ord)
eitherFromExt_Int :: Ext_Int kExt kInt -> Either kExt kInt
eitherFromExt_Int (External x) = Left x
eitherFromExt_Int (Internal y) = Right y
eitherToExt_Int :: Either kExt kInt -> Ext_Int kExt kInt
eitherToExt_Int (Left x)  = External x
eitherToExt_Int (Right y) = Internal y
fromExternal (External x) = x

data WWMaskKey = NoGo | TemporaryMask1 | TemporaryMask2 | TemporaryMask3 | TemporaryMask4 | VisitedThisSearch | Unvisited | LatestVisited | ToBePartitioned deriving (Show, Eq, Ord, Enum, Bounded)
data WWPointsKey = TemporaryPoints deriving (Show, Eq, Ord, Enum, Bounded)
allWWMaskKeys :: [WWMaskKey]
allWWMaskKeys = [minBound .. maxBound]
allWWPointsKeys :: [WWPointsKey]
allWWPointsKeys = [minBound .. maxBound]

-- Assumes all rows have equal length
readWorld :: (Ord km, Ord kp) => (Char -> Maybe (WorldKey km kp)) -> String -> WalkableWorld km kp
readWorld charMap = addWalkableWorldParts . readAsciiWorld charMap'
  where charMap' c
            = do
                c' <- charMap c
                return $ case c' of
                    WKMask   x1 -> WKMask   (External x1)
                    WKPoints x2 -> WKPoints (External x2)

-- This modify modifies the underlying asciiWorld directly, including all of the stuff that WalkableWorld did to it (such as NoGos and underscores in names)
modifyRawAsciiWorld :: (Ord km, Ord kp) => (RawAsciiWorld km kp -> RawAsciiWorld km kp) -> WalkableWorld km kp -> WalkableWorld km kp
modifyRawAsciiWorld f = fromHeightAndRawAsciiWorld . fmap f . toHeightAndRawAsciiWorld

modifyHeightAndRawAsciiWorld :: (Ord km, Ord kp) => ((Int, RawAsciiWorld km kp) -> (Int, RawAsciiWorld km kp)) -> WalkableWorld km kp -> WalkableWorld km kp
modifyHeightAndRawAsciiWorld f = fromHeightAndRawAsciiWorld . f . toHeightAndRawAsciiWorld

-- This modify allows you to modify the world in a way ignorant to the stuff that WalkableWorld added (such as NoGos and underscores in names)
-- Warning: I think this function will perform badly. Also, it's not been properly tested.
-- 			Update: I tried to use this and got bad results so I expect this is broken. Further investigation required.
--					TO DO: See about either fixing this or better documenting how it should be used
modifyAsAsciiWorld :: (Ord km, Ord kp) => (AsciiWorld km kp -> AsciiWorld km kp) -> WalkableWorld km kp -> WalkableWorld km kp
modifyAsAsciiWorld f = addWalkableWorldParts . fmap (mapKeyForMasks External) . fmap (mapKeyForPoints External) . fmap f . fmap (mapKeyForPoints fromExternal) . fmap (mapKeyForMasks fromExternal) . undoWalkableWorldParts

showWorld :: (Ord km, Ord kp) => Char -> (km -> Char) -> (kp -> Char) -> (WorldKey km kp -> WorldKey km kp -> Ordering) -> WalkableWorld km kp -> String
showWorld bgChar maskToChar pointsToChar nameZOrder w = (\(height, w') -> showAsciiWorld height bgChar maskToChar' pointsToChar' nameZOrder' w') . undoWalkableWorldParts $ w
  where maskToChar'   = maskToChar . fromExternal
        pointsToChar' = pointsToChar . fromExternal
        nameZOrder'   = nameZOrder `on` conversion
        conversion = toWorldKey . bimap fromExternal fromExternal . fromWorldKey

-- Shows the raw underlying ascii world except for underscores which are stripped so that there aren't just underscores for all non-background point.
showRawAsciiWorld :: (Ord km, Ord kp) => Char -> (Ext_Int km WWMaskKey -> Char) -> (Ext_Int kp WWPointsKey -> Char) -> WWNameZComp km kp -> WalkableWorld km kp -> String
showRawAsciiWorld bgChar maskToChar pointsToChar nameZOrder w = showAsciiWorld (wwHeight w) bgChar maskToChar pointsToChar nameZOrder . wwRawAsciiWorld $ w

printWorld :: (Ord km, Ord kp) => Char -> (km -> Char) -> (kp -> Char) -> (WorldKey km kp -> WorldKey km kp -> Ordering) -> WalkableWorld km kp -> IO ()
printWorld bgChar maskToChar pointsToChar nameZOrder = putStrLn . showWorld bgChar maskToChar pointsToChar nameZOrder

printRawAsciiWorld :: (Ord km, Ord kp) => Char -> (Ext_Int km WWMaskKey -> Char) -> (Ext_Int kp WWPointsKey -> Char) -> WWNameZComp km kp -> WalkableWorld km kp -> IO ()
printRawAsciiWorld bgChar maskToChar pointsToChar nameZOrder w = putStrLn . showRawAsciiWorld bgChar maskToChar pointsToChar nameZOrder $ w



-- Assumes worlds are same size
-- Left-biased such that the background character and any singular points they share are taken from the left
-- combineTwoWalkableWorlds :: (Ord km, Ord kp) => WalkableWorld km kp -> WalkableWorld km kp -> WalkableWorld km kp
-- combineTwoWalkableWorlds (WalkableWorld h1 w1) (WalkableWorld h2 w2) = WalkableWorld (max h1 h2) $ combineTwoAsciiWorlds w1 w2

-- combineWalkableWorlds :: (Ord km, Ord kp) => [WalkableWorld km kp] -> WalkableWorld km kp
-- combineWalkableWorlds = combineAsciiWorlds

-- inWWIsPointOverlappingPointsKey :: (Ord km, Ord kp) => WalkableWorld km kp -> Point -> kp -> Bool
-- inWWIsPointOverlappingPointsKey (WalkableWorld _ asciiWorld) point pointsKey = inWorldIsPointOverlappingPointsKey asciiWorld point pointsKey

-- inWWIsPointOverlappingMaskKey :: (Ord km, Ord kp) => WalkableWorld km kp -> Point -> km -> Bool
-- inWWIsPointOverlappingMaskKey (WalkableWorld _ asciiWorld) point maskKey = inWorldIsPointOverlappingMaskKey asciiWorld point maskKey

-- inWWIsPointOverlappingPointsKeyOrMaskKey :: (Ord k) => WalkableWorld k k -> Point -> k -> Bool
-- inWWIsPointOverlappingPointsKeyOrMaskKey (WalkableWorld _ asciiWorld) point key = inWorldIsPointOverlappingPointsKeyOrMaskKey asciiWorld point key

-- moveMaskOfNameByInWW :: (Ord km, Ord kp) => km -> (Int,Int) -> WalkableWorld km kp -> WalkableWorld km kp
-- moveMaskOfNameByInWW name (dx,dy) (WalkableWorld height w) = WalkableWorld height $ moveMaskOfNameBy name (dx,dy) w

-- movePointsOfNameByInWW :: (Ord km, Ord kp) => kp -> (Int,Int) -> WalkableWorld km kp -> WalkableWorld km kp
-- movePointsOfNameByInWW name (dx,dy) (WalkableWorld height w) = WalkableWorld height $ movePointsOfNameBy name (dx,dy) w

addMaskInWW :: (Ord km, Ord kp) => km -> Mask -> WalkableWorld km kp -> WalkableWorld km kp
addMaskInWW maskKey mask (WalkableWorld height asciiWorld) = WalkableWorld height (addMask (External maskKey) mask asciiWorld)

-- deleteMaskInWW :: (Ord km, Ord kp) => km -> WalkableWorld km kp -> WalkableWorld km kp
-- deleteMaskInWW maskName (WalkableWorld height w) = WalkableWorld height $ deleteMask maskName w

filterMaskKeysInWW :: (Ord km, Ord kp) => (km -> Bool) -> WalkableWorld km kp -> WalkableWorld km kp
filterMaskKeysInWW p (WalkableWorld height asciiWorld) = WalkableWorld height (filterMaskKeys (\maskKey -> case maskKey of {External x -> p x; _ -> True}) asciiWorld)

-- filterMasksInWW :: (Ord km, Ord kp) => (Mask -> Bool) -> WalkableWorld km kp -> WalkableWorld km kp
-- filterMasksInWW p (WalkableWorld height w) = WalkableWorld height $ filterMasks p w

-- lookupMaskInWW :: (Ord km, Ord kp) => km -> WalkableWorld km kp -> Maybe Mask
-- lookupMaskInWW maskName (WalkableWorld height w) = lookupMask maskName w

-- adjustMaskInWW :: (Ord km, Ord kp) => (Mask -> Mask) -> km -> WalkableWorld km kp -> WalkableWorld km kp
-- adjustMaskInWW f maskName (WalkableWorld height w) = WalkableWorld height $ adjustMask f maskName w

-- updateMaskInWW :: (Ord km, Ord kp) => (Mask -> Maybe Mask) -> km -> WalkableWorld km kp -> WalkableWorld km kp
-- updateMaskInWW f maskName (WalkableWorld height w) = WalkableWorld height $ updateMask f maskName w

-- alterMaskInWW :: (Ord km, Ord kp) => (Maybe Mask -> Maybe Mask) -> km -> WalkableWorld km kp -> WalkableWorld km kp
-- alterMaskInWW f maskName (WalkableWorld height w) = WalkableWorld height $ alterMask f maskName w

-- copyMaskInWW :: (Ord km, Ord kp) => km -> km -> WalkableWorld km kp -> WalkableWorld km kp
-- copyMaskInWW srcName destName = modifyAsAsciiWorld (copyMask srcName destName)

-- applyMaskInWW :: (Ord km, Ord kp) => (Mask -> Mask -> Mask) -> km -> km -> WalkableWorld km kp -> WalkableWorld km kp
-- applyMaskInWW op modifier target (WalkableWorld height w) = WalkableWorld height $ applyMask op modifier target w

-- setPointInWW :: (Ord km, Ord kp) => kp -> (Int,Int) -> WalkableWorld km kp -> WalkableWorld km kp
-- setPointInWW name (x,y) (WalkableWorld height w) = WalkableWorld height $ setPoint name (x,y) w

-- deletePointsInWW :: (Ord km, Ord kp) => kp -> WalkableWorld km kp -> WalkableWorld km kp
-- deletePointsInWW name (WalkableWorld height w) = WalkableWorld height $ deletePoints name w

-- insertMaskFromPointsInWW :: (Ord km, Ord kp) => km -> [Point] -> WalkableWorld km kp -> WalkableWorld km kp
-- insertMaskFromPointsInWW newMaskName points (WalkableWorld height w) = WalkableWorld height $ insertMaskFromPoints newMaskName points w

-- inWWMaybeInsertMaskKeyFromPointsKey :: (Ord km, Ord kp) => WalkableWorld km kp -> km -> kp -> Maybe (WalkableWorld km kp)
-- inWWMaybeInsertMaskKeyFromPointsKey (WalkableWorld height asciiWorld) newMaskKey pointsKey = fmap (WalkableWorld height) $ inWorldMaybeInsertMaskKeyFromPointsKey asciiWorld newMaskName pointsName

-- isOverlappingMasksInWW :: (Ord km, Ord kp) => km -> km -> WalkableWorld km kp -> Bool
-- isOverlappingMasksInWW name1 name2 w = isOverlappingMasks name1 name2 w



-- removeForbidden :: (Ord km, Ord kp) => WalkableWorld km kp -> WalkableWorld km kp
-- removeForbidden w = WalkableWorld $ applyMask bitwiseSubtract "#" "O" (wwRawAsciiWorld w)

-- progressByAStep :: (Ord km, Ord kp) => WalkableWorld km kp -> WalkableWorld km kp
-- progressByAStep w = removeForbidden . WalkableWorld $ combineAsciiWorlds $ map (\dir -> moveMaskOfNameBy "O" dir (wwRawAsciiWorld w)) lrduDirs

-- setOAtS :: (Ord km, Ord kp) => WalkableWorld km kp -> WalkableWorld km kp
-- setOAtS = WalkableWorld . fromJust . insertMaskAtPoint "O" "S" . wwRawAsciiWorld


maskForNoGoRightAndTop :: Int -> Int -> Mask
maskForNoGoRightAndTop width height = rightNoGos + topNoGos
  where rightNoGos = sum [pointToMask width (x,y) |     x <- [0..(width-1)], let y = (height-1)      ]
        topNoGos   = sum [pointToMask width (x,y) | let x = (width-1),           y <- [0..(height-2)]]

addNoGoToRightAndTop :: (Ord km, Ord kp) => Int -> RawAsciiWorld km kp -> RawAsciiWorld km kp
addNoGoToRightAndTop height w = addMask (Internal NoGo) (maskForNoGoRightAndTop width height) w
  where width = asciiWorldWidth w

removeNoGoFromRightAndTop :: (Ord km, Ord kp) => RawAsciiWorld km kp -> RawAsciiWorld km kp
removeNoGoFromRightAndTop w = deleteMask (Internal NoGo) w

undoNoGo :: (Ord km, Ord kp) => WalkableWorld km kp -> (Int, RawAsciiWorld km kp)
undoNoGo w = (wwHeight w, changeWidthBy (-1) . removeNoGoFromRightAndTop . wwRawAsciiWorld $ w)

removeInternalMasks :: (Ord km, Ord kp) => RawAsciiWorld km kp -> RawAsciiWorld km kp
removeInternalMasks w = foldl' (\w maskKey -> deleteMask (Internal maskKey) w) w allWWMaskKeys

removeInternalPoints :: (Ord km, Ord kp) => RawAsciiWorld km kp -> RawAsciiWorld km kp
removeInternalPoints w = foldl' (\w maskKey -> deletePoints (Internal maskKey) w) w allWWPointsKeys

removeInternalMasksAndPoints :: (Ord km, Ord kp) => RawAsciiWorld km kp -> RawAsciiWorld km kp
removeInternalMasksAndPoints = removeInternalMasks . removeInternalPoints

addWalkableWorldParts :: (Ord km, Ord kp) => (Int, RawAsciiWorld km kp) -> WalkableWorld km kp
addWalkableWorldParts (height, w) = WalkableWorld height . addNoGoToRightAndTop (height + 1) . changeWidthBy 1 $ w

undoWalkableWorldParts :: (Ord km, Ord kp) => WalkableWorld km kp -> (Int, RawAsciiWorld km kp)
undoWalkableWorldParts w = (wwHeight w, changeWidthBy (-1) . removeInternalMasksAndPoints . wwRawAsciiWorld $ w)

maskKeys :: WalkableWorld a kp -> [a]
maskKeys w =
    w & wwRawAsciiWorld
      & asciiWorldMasks
      & M.keys
      & map eitherFromExt_Int
      & lefts


totalHorizontalEdgesOverPoints :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalHorizontalEdgesOverPoints maskName w =
    w & wwRawAsciiWorld
      & copyMask (External maskName) (Internal TemporaryMask1)
      & moveMaskOfNameBy (Internal TemporaryMask1) (0,1)
      & applyMask bitwiseXor (External maskName) (Internal TemporaryMask1)
      & getTemporaryMask1
      & countMaskPoints
  
  where getTemporaryMask1 = fromJust . M.lookup (Internal TemporaryMask1) . asciiWorldMasks
        countMaskPoints = toInteger . popCount

totalVerticalEdgesOverPoints :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalVerticalEdgesOverPoints maskName w =
    w & wwRawAsciiWorld
      & copyMask (External maskName) (Internal TemporaryMask1)
      & moveMaskOfNameBy (Internal TemporaryMask1) (1,0)
      & applyMask bitwiseXor (External maskName) (Internal TemporaryMask1)
      & getTemporaryMask1
      & countMaskPoints
  where
    getTemporaryMask1 = fromJust . M.lookup (Internal TemporaryMask1) . asciiWorldMasks
    countMaskPoints = toInteger . popCount

totalEdgesOverPoints :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalEdgesOverPoints maskName w =
    (  totalHorizontalEdgesOverPoints maskName w
     + totalVerticalEdgesOverPoints   maskName w)

totalConnectedHorizontalEdges :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalConnectedHorizontalEdges maskName w =
    w & wwRawAsciiWorld
      & copyMask (External maskName) (Internal TemporaryMask1)
      & moveMaskOfNameBy (Internal TemporaryMask1) (0,1)
      & applyMask bitwiseXor (External maskName) (Internal TemporaryMask1)
      & copyMask (Internal TemporaryMask1) (Internal TemporaryMask2)
      & moveMaskOfNameBy (Internal TemporaryMask2) (1,0)
      & applyMask bitwiseXor (Internal TemporaryMask1) (Internal TemporaryMask2)
      & getTemporaryMask2
      & countMaskPoints
      & (`div` 2)
  
  where getTemporaryMask2 = fromJust . M.lookup (Internal TemporaryMask2) . asciiWorldMasks
        countMaskPoints = toInteger . popCount

totalConnectedVerticalEdges :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalConnectedVerticalEdges maskName w =
    w & wwRawAsciiWorld
      & copyMask (External maskName) (Internal TemporaryMask1)
      & moveMaskOfNameBy (Internal TemporaryMask1) (1,0)
      & applyMask bitwiseXor (External maskName) (Internal TemporaryMask1)
      & copyMask (Internal TemporaryMask1) (Internal TemporaryMask2)
      & moveMaskOfNameBy (Internal TemporaryMask2) (0,1)
      & applyMask bitwiseXor (Internal TemporaryMask1) (Internal TemporaryMask2)
      & getTemporaryMask2
      & countMaskPoints
      & (`div` 2)
  where
    getTemporaryMask2 = fromJust . M.lookup (Internal TemporaryMask2) . asciiWorldMasks
    countMaskPoints = toInteger . popCount

totalConnectedEdges :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalConnectedEdges maskName w =
    (  totalConnectedHorizontalEdges maskName w
     + totalConnectedVerticalEdges   maskName w)

totalConnectedOneSidedHorizontalEdges :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalConnectedOneSidedHorizontalEdges maskName w =
    (    (worldWithEdgeMask & keepDownEdges & countConnectedEdges)
     +   (worldWithEdgeMask & keepUpEdges   & countConnectedEdges))
  
  where worldWithEdgeMask =
            w
                & wwRawAsciiWorld
                & copyMask (External maskName) (Internal TemporaryMask1)
                & copyMask (External maskName) (Internal TemporaryMask2)
                & moveMaskOfNameBy (Internal TemporaryMask2) (0,1)
        
        keepDownEdges w' =
            w'
                & applyMask bitwiseXor (Internal TemporaryMask1) (Internal TemporaryMask2)
                & copyMask (Internal TemporaryMask2) (Internal TemporaryMask3)
                & applyMask bitwiseAnd (Internal TemporaryMask1) (Internal TemporaryMask3)
        
        keepUpEdges w' =
            w'
                & applyMask bitwiseXor (Internal TemporaryMask2) (Internal TemporaryMask1)
                & copyMask (Internal TemporaryMask1) (Internal TemporaryMask3)
                & applyMask bitwiseAnd (Internal TemporaryMask2) (Internal TemporaryMask3)
        
        countConnectedEdges w' =
            w'
                & copyMask (Internal TemporaryMask3) (Internal TemporaryMask4)
                & moveMaskOfNameBy (Internal TemporaryMask4) (1,0)
                & applyMask bitwiseXor (Internal TemporaryMask3) (Internal TemporaryMask4)
                & getTemporaryMask4
                & countMaskPoints
                & (`div` 2)
        
        getTemporaryMask4 = fromJust . M.lookup (Internal TemporaryMask4) . asciiWorldMasks
        
        countMaskPoints = toInteger . popCount


totalConnectedOneSidedVerticalEdges :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalConnectedOneSidedVerticalEdges maskName w =
    (    (worldWithEdgeMask & keepLeftEdges  & countConnectedEdges)
     +   (worldWithEdgeMask & keepRightEdges & countConnectedEdges))
  
  where worldWithEdgeMask =
            w
                & wwRawAsciiWorld
                & copyMask (External maskName) (Internal TemporaryMask1)
                & copyMask (External maskName) (Internal TemporaryMask2)
                & moveMaskOfNameBy (Internal TemporaryMask2) (1,0)
        
        keepLeftEdges w' =
            w'
                & applyMask bitwiseXor (Internal TemporaryMask1) (Internal TemporaryMask2)
                & copyMask (Internal TemporaryMask2) (Internal TemporaryMask3)
                & applyMask bitwiseAnd (Internal TemporaryMask1) (Internal TemporaryMask3)
        
        keepRightEdges w' =
            w'
                & applyMask bitwiseXor (Internal TemporaryMask2) (Internal TemporaryMask1)
                & copyMask (Internal TemporaryMask1) (Internal TemporaryMask3)
                & applyMask bitwiseAnd (Internal TemporaryMask2) (Internal TemporaryMask3)
        
        countConnectedEdges w' =
            w'
                & copyMask (Internal TemporaryMask3) (Internal TemporaryMask4)
                & moveMaskOfNameBy (Internal TemporaryMask4) (0,1)
                & applyMask bitwiseXor (Internal TemporaryMask3) (Internal TemporaryMask4)
                & getTemporaryMask4
                & countMaskPoints
                & (`div` 2)
        
        getTemporaryMask4 = fromJust . M.lookup (Internal TemporaryMask4) . asciiWorldMasks
        
        countMaskPoints = toInteger . popCount

totalConnectedOneSidedEdges :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalConnectedOneSidedEdges maskName w =
    (  totalConnectedOneSidedHorizontalEdges maskName w
     + totalConnectedOneSidedVerticalEdges   maskName w)

totalPoints :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalPoints maskName w =
    w & wwRawAsciiWorld
      & getMask
      & countMaskPoints
  where
    getMask = fromJust . M.lookup (External maskName) . asciiWorldMasks
    countMaskPoints = toInteger . popCount

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
partitionMaskByReachableLRDU :: (Show km, Ord km, Show kp, Ord kp) => km -> WalkableWorld km kp -> [Mask]
partitionMaskByReachableLRDU maskKey (WalkableWorld height worldBeforePartition) = finalParts
  where worldAfterCopyingTargetMaskToToBePartitioned =
            worldBeforePartition
                & copyMask (External maskKey) (Internal ToBePartitioned)
        
        -- Start Loop until all connected parts of target mask found
        outerLoop (oldParts, outerInputWorld) = (newPart:oldParts, innerFinalWorld)
          where middlePoint = let maybeMiddlePoint = middlePointOfMask (Internal ToBePartitioned) outerInputWorld
                               in case maybeMiddlePoint of
                                    Just point -> point
                                    Nothing -> error $ "middlePoint failed: \"" ++ show maskKey ++ "\" not found in " ++ show outerInputWorld
                
                -- Init Loop until LatestVisited is empty
                initLatestVisitedMask = [middlePoint]
                
                worldWithInitLatestVisitedAndUnvisited =
                    outerInputWorld
                        & insertMaskFromPoints (Internal LatestVisited) initLatestVisitedMask
                        & addMask (Internal VisitedThisSearch) 0
                        & copyMask (Internal ToBePartitioned) (Internal Unvisited)
                        & applyMask bitwiseOr (Internal LatestVisited) (Internal VisitedThisSearch)
                
                -- Start Loop until LatestVisited is empty
                innerLoop innerInputWorld = wAfterOneIteration
                  where worldWithUnvisitedXoredByVisitedThisSearch =
                            innerInputWorld
                                & applyMask bitwiseXor (Internal LatestVisited) (Internal Unvisited)
                        
                        combinedMaskFromAllShiftedCopiesOfVisitedThisSearch =
                            lrduDirs
                                & combineAsciiWorlds . map (\dir -> moveMaskOfNameBy (Internal LatestVisited) dir worldWithUnvisitedXoredByVisitedThisSearch)
                                & fromJust . lookupMask (Internal LatestVisited)
                        
                        wAfterOneIteration =
                            worldWithUnvisitedXoredByVisitedThisSearch
                                & addMask (Internal LatestVisited) combinedMaskFromAllShiftedCopiesOfVisitedThisSearch
                                & applyMask bitwiseAnd (Internal Unvisited) (Internal LatestVisited)
                                & applyMask bitwiseOr (Internal LatestVisited) (Internal VisitedThisSearch)
                -- End Loop until LatestVisited is empty
                
                innerFinalWorld =
                    worldWithInitLatestVisitedAndUnvisited
                        & until ((== 0) . fromJust . lookupMask (Internal LatestVisited)) innerLoop
                        & applyMask bitwiseXor (Internal VisitedThisSearch) (Internal ToBePartitioned)
                
                newPart = fromJust . lookupMask (Internal VisitedThisSearch) $ innerFinalWorld
                
                -- VisitedThisSearch should now contain one of the connected components for the target mask.
                
                -- Add this as new numbered mask of its own (or perhaps I should be returning a list of masks to do with what I will...) and subtract it from the a copy of the target layer
        
        (finalParts, outerFinalWorld) =
            ([],worldAfterCopyingTargetMaskToToBePartitioned)
                & until ((== 0) . fromJust . lookupMask (Internal ToBePartitioned) . snd) outerLoop
        
        -- End Loop until all connected parts of target mask found
        
        
        -- PRINTING HELPER
        -- printTrace label w = trace (label ++ "\n" ++ showW w) w
          -- where showW = showAsciiWorld (height+1) '.' (either (head . dropWhile (== '\'') . show) (head . show) . eitherFromExt_Int) (either (head . dropWhile (== '\'') . show) (head . show) . eitherFromExt_Int) (comparing id)
                        -- . filterMaskKeys (\x -> case x of {External _ -> False; (Internal ToBePartitioned) -> False; _ -> True})

partitionAllMasksByReachableLRDU :: (Show km, Ord km, Show kp, Ord kp) => WalkableWorld km kp -> M.Map km [Mask]
partitionAllMasksByReachableLRDU w = M.fromList $ map (\maskKey -> (maskKey, partitionMaskByReachableLRDU maskKey w)) (maskKeys w)
