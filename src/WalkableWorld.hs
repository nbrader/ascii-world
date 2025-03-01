#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3

-- To Do:
-- Make "WalkableWorld" with max walk distance fed in at construction to then add that much margin and so be able to detect reachability effects up to that distance.

module WalkableWorld    ( WalkableWorld(..)
                        , wwWidth
                        , WorldKey(..)
                        , readWorld
                        -- , showWorld
                        , printWorld
                        , totalEdgesOverPoints
                        , totalConnectedEdges
                        , totalConnectedOneSidedEdges
                        , maskKeys
                        , totalPoints
                        , partitionMaskByReachableLRDU
                        , partitionAllMasksByReachableLRDU
                        -- , combineTwoWalkableWorlds
                        -- , combineWalkableWorlds
                        , inWWIsPointsKeyOverlappingMaskKey
                        , inWWIsPointOverlappingPointsKey
                        -- , inWWIsPointOverlappingMaskKey
                        -- , inWWIsPointOverlappingPointsKeyOrMaskKey
                        -- , moveMaskOfNameByInWW
                        -- , movePointsOfNameByInWW
                        , addMaskInWW
                        -- , deleteMaskInWW
                        , filterMaskKeysInWW
                        -- , filterMasksInWW
                        -- , lookupMaskInWW
                        , lookupPointsInWW
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
                        , movePointsKeyByVecFreelyInWW
                        , movePointsKeyByVecInWWUnlessNewWorldSatisfiesPred
                        , movePointsKeyByVecPushingPointsKeyBlockedByMaskKeysInWW
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
            , movePoint
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
                        , lookupPoints
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

data WalkableWorld mk pk = WalkableWorld {wwHeight :: Int, wwRawAsciiWorld :: RawAsciiWorld mk pk} deriving (Show)
wwWidth = asciiWorldWidth . wwRawAsciiWorld

type RawAsciiWorld mk pk = AsciiWorld (Ext_Int mk WWMaskKey) (Ext_Int pk WWPointsKey)
toHeightAndRawAsciiWorld :: WalkableWorld mk pk -> (Int, RawAsciiWorld mk pk)
toHeightAndRawAsciiWorld w = (wwHeight w, wwRawAsciiWorld w)
fromHeightAndRawAsciiWorld :: (Int, RawAsciiWorld mk pk) -> WalkableWorld mk pk
fromHeightAndRawAsciiWorld (h, w) = WalkableWorld h w
type WWNameZComp mk pk = WorldKey (Ext_Int mk WWMaskKey) (Ext_Int pk WWPointsKey) -> WorldKey (Ext_Int mk WWMaskKey) (Ext_Int pk WWPointsKey) -> Ordering

data Ext_Int kExt kInt = External kExt | Internal kInt deriving (Show, Eq, Ord)
eitherFromExt_Int :: Ext_Int kExt kInt -> Either kExt kInt
eitherFromExt_Int (External x) = Left x
eitherFromExt_Int (Internal y) = Right y
eitherToExt_Int :: Either kExt kInt -> Ext_Int kExt kInt
eitherToExt_Int (Left x)  = External x
eitherToExt_Int (Right y) = Internal y
fromExternal (External x) = x

data WWMaskKey = NoGo | TemporaryMask1 | TemporaryMask2 | TemporaryMask3 | TemporaryMask4 | VisitedThisSearch | Unvisited | LatestVisited | ToBePartitioned deriving (Show, Eq, Ord, Enum, Bounded)
data WWPointsKey = TemporaryPoints | Tagged deriving (Show, Eq, Ord, Enum, Bounded)
allWWMaskKeys :: [WWMaskKey]
allWWMaskKeys = [minBound .. maxBound]
allWWPointsKeys :: [WWPointsKey]
allWWPointsKeys = [minBound .. maxBound]

-- Assumes all rows have equal length
readWorld :: (Ord mk, Ord pk) => (Char -> Maybe (WorldKey mk pk)) -> String -> WalkableWorld mk pk
readWorld charMap = addWalkableWorldParts . readAsciiWorld charMap'
  where charMap' c
            = do
                c' <- charMap c
                return $ case c' of
                    MaskKey   x1 -> MaskKey   (External x1)
                    PointsKey x2 -> PointsKey (External x2)

-- This modify modifies the underlying asciiWorld directly, including all of the stuff that WalkableWorld did to it (such as NoGos and underscores in names)
modifyRawAsciiWorld :: (Ord mk, Ord pk) => (RawAsciiWorld mk pk -> RawAsciiWorld mk pk) -> WalkableWorld mk pk -> WalkableWorld mk pk
modifyRawAsciiWorld f = fromHeightAndRawAsciiWorld . fmap f . toHeightAndRawAsciiWorld

modifyHeightAndRawAsciiWorld :: (Ord mk, Ord pk) => ((Int, RawAsciiWorld mk pk) -> (Int, RawAsciiWorld mk pk)) -> WalkableWorld mk pk -> WalkableWorld mk pk
modifyHeightAndRawAsciiWorld f = fromHeightAndRawAsciiWorld . f . toHeightAndRawAsciiWorld

-- This modify allows you to modify the world in a way ignorant to the stuff that WalkableWorld added (such as NoGos and underscores in names)
-- Warning: I think this function will perform badly. Also, it's not been properly tested.
--          Update: I tried to use this and got bad results so I expect this is broken. Further investigation required.
--                  TO DO: See about either fixing this or better documenting how it should be used
modifyAsAsciiWorld :: (Ord mk, Ord pk) => (AsciiWorld mk pk -> AsciiWorld mk pk) -> WalkableWorld mk pk -> WalkableWorld mk pk
modifyAsAsciiWorld f = addWalkableWorldParts . fmap (mapKeyForMasks External) . fmap (mapKeyForPoints External) . fmap f . fmap (mapKeyForPoints fromExternal) . fmap (mapKeyForMasks fromExternal) . undoWalkableWorldParts

showWorld :: (Ord mk, Ord pk) => Char -> (mk -> Char) -> (pk -> Char) -> (WorldKey mk pk -> WorldKey mk pk -> Ordering) -> WalkableWorld mk pk -> String
showWorld bgChar maskToChar pointsToChar nameZOrder w = (\(height, w') -> showAsciiWorld height bgChar maskToChar' pointsToChar' nameZOrder' w') . undoWalkableWorldParts $ w
  where maskToChar'   = maskToChar . fromExternal
        pointsToChar' = pointsToChar . fromExternal
        nameZOrder'   = nameZOrder `on` conversion
        conversion = toWorldKey . bimap fromExternal fromExternal . fromWorldKey

-- Shows the raw underlying ascii world except for underscores which are stripped so that there aren't just underscores for all non-background point.
showRawAsciiWorld :: (Ord mk, Ord pk) => Char -> (Ext_Int mk WWMaskKey -> Char) -> (Ext_Int pk WWPointsKey -> Char) -> WWNameZComp mk pk -> WalkableWorld mk pk -> String
showRawAsciiWorld bgChar maskToChar pointsToChar nameZOrder w = showAsciiWorld (wwHeight w) bgChar maskToChar pointsToChar nameZOrder . wwRawAsciiWorld $ w

printWorld :: (Ord mk, Ord pk) => Char -> (mk -> Char) -> (pk -> Char) -> (WorldKey mk pk -> WorldKey mk pk -> Ordering) -> WalkableWorld mk pk -> IO ()
printWorld bgChar maskToChar pointsToChar nameZOrder = putStrLn . showWorld bgChar maskToChar pointsToChar nameZOrder

printRawAsciiWorld :: (Ord mk, Ord pk) => Char -> (Ext_Int mk WWMaskKey -> Char) -> (Ext_Int pk WWPointsKey -> Char) -> WWNameZComp mk pk -> WalkableWorld mk pk -> IO ()
printRawAsciiWorld bgChar maskToChar pointsToChar nameZOrder w = putStrLn . showRawAsciiWorld bgChar maskToChar pointsToChar nameZOrder $ w



-- Assumes worlds are same size
-- Left-biased such that the background character and any singular points they share are taken from the left
-- combineTwoWalkableWorlds :: (Ord mk, Ord pk) => WalkableWorld mk pk -> WalkableWorld mk pk -> WalkableWorld mk pk
-- combineTwoWalkableWorlds (WalkableWorld h1 w1) (WalkableWorld h2 w2) = WalkableWorld (max h1 h2) $ combineTwoAsciiWorlds w1 w2

-- combineWalkableWorlds :: (Ord mk, Ord pk) => [WalkableWorld mk pk] -> WalkableWorld mk pk
-- combineWalkableWorlds = combineAsciiWorlds

inWWIsPointsKeyOverlappingMaskKey :: (Ord mk, Ord pk) => WalkableWorld mk pk -> pk -> mk -> Bool
inWWIsPointsKeyOverlappingMaskKey (WalkableWorld _ asciiWorld) pointsKey maskKey = inWorldIsPointsKeyOverlappingMaskKey asciiWorld (External pointsKey) (External maskKey)

inWWIsPointOverlappingPointsKey :: (Ord mk, Ord pk) => WalkableWorld mk pk -> Point -> pk -> Bool
inWWIsPointOverlappingPointsKey (WalkableWorld _ asciiWorld) point pointsKey = inWorldIsPointOverlappingPointsKey asciiWorld point (External pointsKey)

inWWIsPointOverlappingMaskKey :: (Ord mk, Ord pk) => WalkableWorld mk pk -> Point -> mk -> Bool
inWWIsPointOverlappingMaskKey (WalkableWorld _ asciiWorld) point maskKey = inWorldIsPointOverlappingMaskKey asciiWorld point (External maskKey)

-- inWWIsPointOverlappingPointsKeyOrMaskKey :: (Ord k) => WalkableWorld k k -> Point -> k -> Bool
-- inWWIsPointOverlappingPointsKeyOrMaskKey (WalkableWorld _ asciiWorld) point key = inWorldIsPointOverlappingPointsKeyOrMaskKey asciiWorld point key

-- moveMaskOfNameByInWW :: (Ord mk, Ord pk) => mk -> (Int,Int) -> WalkableWorld mk pk -> WalkableWorld mk pk
-- moveMaskOfNameByInWW name (dx,dy) (WalkableWorld height w) = WalkableWorld height $ moveMaskOfNameBy name (dx,dy) w

-- movePointsOfNameByInWW :: (Ord mk, Ord pk) => pk -> (Int,Int) -> WalkableWorld mk pk -> WalkableWorld mk pk
-- movePointsOfNameByInWW name (dx,dy) (WalkableWorld height w) = WalkableWorld height $ movePointsOfNameBy name (dx,dy) w

addMaskInWW :: (Ord mk, Ord pk) => mk -> Mask -> WalkableWorld mk pk -> WalkableWorld mk pk
addMaskInWW maskKey mask (WalkableWorld height asciiWorld) = WalkableWorld height (addMask (External maskKey) mask asciiWorld)

-- deleteMaskInWW :: (Ord mk, Ord pk) => mk -> WalkableWorld mk pk -> WalkableWorld mk pk
-- deleteMaskInWW maskName (WalkableWorld height w) = WalkableWorld height $ deleteMask maskName w

filterMaskKeysInWW :: (Ord mk, Ord pk) => (mk -> Bool) -> WalkableWorld mk pk -> WalkableWorld mk pk
filterMaskKeysInWW p (WalkableWorld height asciiWorld) = WalkableWorld height (filterMaskKeys (\maskKey -> case maskKey of {External x -> p x; _ -> True}) asciiWorld)

-- filterMasksInWW :: (Ord mk, Ord pk) => (Mask -> Bool) -> WalkableWorld mk pk -> WalkableWorld mk pk
-- filterMasksInWW p (WalkableWorld height w) = WalkableWorld height $ filterMasks p w

lookupPointsInWW :: (Ord mk, Ord pk) => pk -> WalkableWorld mk pk -> Maybe [Point]
lookupPointsInWW pointsKey (WalkableWorld height w) = lookupPoints (External pointsKey) w

-- lookupMaskInWW :: (Ord mk, Ord pk) => mk -> WalkableWorld mk pk -> Maybe Mask
lookupMaskInWW maskKey (WalkableWorld height w) = lookupMask (External maskKey) w

-- adjustMaskInWW :: (Ord mk, Ord pk) => (Mask -> Mask) -> mk -> WalkableWorld mk pk -> WalkableWorld mk pk
-- adjustMaskInWW f maskName (WalkableWorld height w) = WalkableWorld height $ adjustMask f maskName w

-- updateMaskInWW :: (Ord mk, Ord pk) => (Mask -> Maybe Mask) -> mk -> WalkableWorld mk pk -> WalkableWorld mk pk
-- updateMaskInWW f maskName (WalkableWorld height w) = WalkableWorld height $ updateMask f maskName w

-- alterMaskInWW :: (Ord mk, Ord pk) => (Maybe Mask -> Maybe Mask) -> mk -> WalkableWorld mk pk -> WalkableWorld mk pk
-- alterMaskInWW f maskName (WalkableWorld height w) = WalkableWorld height $ alterMask f maskName w

-- copyMaskInWW :: (Ord mk, Ord pk) => mk -> mk -> WalkableWorld mk pk -> WalkableWorld mk pk
-- copyMaskInWW srcName destName = modifyAsAsciiWorld (copyMask srcName destName)

-- applyMaskInWW :: (Ord mk, Ord pk) => (Mask -> Mask -> Mask) -> mk -> mk -> WalkableWorld mk pk -> WalkableWorld mk pk
-- applyMaskInWW op modifier target (WalkableWorld height w) = WalkableWorld height $ applyMask op modifier target w

-- setPointInWW :: (Ord mk, Ord pk) => pk -> (Int,Int) -> WalkableWorld mk pk -> WalkableWorld mk pk
-- setPointInWW name (x,y) (WalkableWorld height w) = WalkableWorld height $ setPoint name (x,y) w

-- deletePointsInWW :: (Ord mk, Ord pk) => pk -> WalkableWorld mk pk -> WalkableWorld mk pk
-- deletePointsInWW name (WalkableWorld height w) = WalkableWorld height $ deletePoints name w

-- insertMaskFromPointsInWW :: (Ord mk, Ord pk) => mk -> [Point] -> WalkableWorld mk pk -> WalkableWorld mk pk
-- insertMaskFromPointsInWW newMaskName points (WalkableWorld height w) = WalkableWorld height $ insertMaskFromPoints newMaskName points w

-- inWWMaybeInsertMaskKeyFromPointsKey :: (Ord mk, Ord pk) => WalkableWorld mk pk -> mk -> pk -> Maybe (WalkableWorld mk pk)
-- inWWMaybeInsertMaskKeyFromPointsKey (WalkableWorld height asciiWorld) newMaskKey pointsKey = fmap (WalkableWorld height) $ inWorldMaybeInsertMaskKeyFromPointsKey asciiWorld newMaskName pointsName

-- isOverlappingMasksInWW :: (Ord mk, Ord pk) => mk -> mk -> WalkableWorld mk pk -> Bool
-- isOverlappingMasksInWW name1 name2 w = isOverlappingMasks name1 name2 w



-- removeForbidden :: (Ord mk, Ord pk) => WalkableWorld mk pk -> WalkableWorld mk pk
-- removeForbidden w = WalkableWorld $ applyMask bitwiseSubtract "#" "O" (wwRawAsciiWorld w)

-- progressByAStep :: (Ord mk, Ord pk) => WalkableWorld mk pk -> WalkableWorld mk pk
-- progressByAStep w = removeForbidden . WalkableWorld $ combineAsciiWorlds $ map (\dir -> moveMaskOfNameBy "O" dir (wwRawAsciiWorld w)) lrduDirs

-- setOAtS :: (Ord mk, Ord pk) => WalkableWorld mk pk -> WalkableWorld mk pk
-- setOAtS = WalkableWorld . fromJust . insertMaskAtPoint "O" "S" . wwRawAsciiWorld


maskForNoGoRightAndTop :: Int -> Int -> Mask
maskForNoGoRightAndTop width height = rightNoGos + topNoGos
  where rightNoGos = sum [pointToMask width (x,y) |     x <- [0..(width-1)], let y = (height-1)      ]
        topNoGos   = sum [pointToMask width (x,y) | let x = (width-1),           y <- [0..(height-2)]]

addNoGoToRightAndTop :: (Ord mk, Ord pk) => Int -> RawAsciiWorld mk pk -> RawAsciiWorld mk pk
addNoGoToRightAndTop height w = addMask (Internal NoGo) (maskForNoGoRightAndTop width height) w
  where width = asciiWorldWidth w

removeNoGoFromRightAndTop :: (Ord mk, Ord pk) => RawAsciiWorld mk pk -> RawAsciiWorld mk pk
removeNoGoFromRightAndTop w = deleteMask (Internal NoGo) w

undoNoGo :: (Ord mk, Ord pk) => WalkableWorld mk pk -> (Int, RawAsciiWorld mk pk)
undoNoGo w = (wwHeight w, changeWidthBy (-1) . removeNoGoFromRightAndTop . wwRawAsciiWorld $ w)

removeInternalMasks :: (Ord mk, Ord pk) => RawAsciiWorld mk pk -> RawAsciiWorld mk pk
removeInternalMasks w = foldl' (\w maskKey -> deleteMask (Internal maskKey) w) w allWWMaskKeys

removeInternalPoints :: (Ord mk, Ord pk) => RawAsciiWorld mk pk -> RawAsciiWorld mk pk
removeInternalPoints w = foldl' (\w maskKey -> deletePoints (Internal maskKey) w) w allWWPointsKeys

removeInternalMasksAndPoints :: (Ord mk, Ord pk) => RawAsciiWorld mk pk -> RawAsciiWorld mk pk
removeInternalMasksAndPoints = removeInternalMasks . removeInternalPoints

addWalkableWorldParts :: (Ord mk, Ord pk) => (Int, RawAsciiWorld mk pk) -> WalkableWorld mk pk
addWalkableWorldParts (height, w) = WalkableWorld height . addNoGoToRightAndTop (height + 1) . changeWidthBy 1 $ w

undoWalkableWorldParts :: (Ord mk, Ord pk) => WalkableWorld mk pk -> (Int, RawAsciiWorld mk pk)
undoWalkableWorldParts w = (wwHeight w, changeWidthBy (-1) . removeInternalMasksAndPoints . wwRawAsciiWorld $ w)

maskKeys :: WalkableWorld a pk -> [a]
maskKeys w =
    w & wwRawAsciiWorld
      & asciiWorldMasks
      & M.keys
      & map eitherFromExt_Int
      & lefts

totalHorizontalEdgesOverPoints :: (Ord a, Ord pk) => a -> WalkableWorld a pk -> Integer
totalHorizontalEdgesOverPoints maskName w =
    w & wwRawAsciiWorld
      & copyMask (External maskName) (Internal TemporaryMask1)
      & moveMaskOfNameBy (Internal TemporaryMask1) (0,1)
      & applyMask bitwiseXor (External maskName) (Internal TemporaryMask1)
      & getTemporaryMask1
      & countMaskPoints
  
  where getTemporaryMask1 = fromJust . M.lookup (Internal TemporaryMask1) . asciiWorldMasks
        countMaskPoints = toInteger . popCount

totalVerticalEdgesOverPoints :: (Ord a, Ord pk) => a -> WalkableWorld a pk -> Integer
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

totalEdgesOverPoints :: (Ord a, Ord pk) => a -> WalkableWorld a pk -> Integer
totalEdgesOverPoints maskName w =
    (  totalHorizontalEdgesOverPoints maskName w
     + totalVerticalEdgesOverPoints   maskName w)

totalConnectedHorizontalEdges :: (Ord a, Ord pk) => a -> WalkableWorld a pk -> Integer
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

totalConnectedVerticalEdges :: (Ord a, Ord pk) => a -> WalkableWorld a pk -> Integer
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

totalConnectedEdges :: (Ord a, Ord pk) => a -> WalkableWorld a pk -> Integer
totalConnectedEdges maskName w =
    (  totalConnectedHorizontalEdges maskName w
     + totalConnectedVerticalEdges   maskName w)

totalConnectedOneSidedHorizontalEdges :: (Ord a, Ord pk) => a -> WalkableWorld a pk -> Integer
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


totalConnectedOneSidedVerticalEdges :: (Ord a, Ord pk) => a -> WalkableWorld a pk -> Integer
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

totalConnectedOneSidedEdges :: (Ord a, Ord pk) => a -> WalkableWorld a pk -> Integer
totalConnectedOneSidedEdges maskName w =
    (  totalConnectedOneSidedHorizontalEdges maskName w
     + totalConnectedOneSidedVerticalEdges   maskName w)

totalPoints :: (Ord a, Ord pk) => a -> WalkableWorld a pk -> Integer
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
partitionMaskByReachableLRDU :: (Show mk, Ord mk, Show pk, Ord pk) => mk -> WalkableWorld mk pk -> [Mask]
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

partitionAllMasksByReachableLRDU :: (Show mk, Ord mk, Show pk, Ord pk) => WalkableWorld mk pk -> M.Map mk [Mask]
partitionAllMasksByReachableLRDU w = M.fromList $ map (\maskKey -> (maskKey, partitionMaskByReachableLRDU maskKey w)) (maskKeys w)

movePointsKeyByVecFreelyInWW :: (Ord mk, Ord pk) => pk -> (Int, Int) -> WalkableWorld mk pk -> WalkableWorld mk pk
movePointsKeyByVecFreelyInWW pointsKey v world = modifyRawAsciiWorld (movePointsOfNameBy (External pointsKey) v) world

movePointsKeyByVecInWWUnlessNewWorldSatisfiesPred :: (Ord mk, Ord pk) => pk -> (Int, Int) -> WalkableWorld mk pk -> (WalkableWorld mk pk -> Bool) -> WalkableWorld mk pk
movePointsKeyByVecInWWUnlessNewWorldSatisfiesPred pointsKey v initWorld pred 
    | pred worldAfterFreeMove
        = initWorld
    | otherwise
        = worldAfterFreeMove
  where worldAfterFreeMove = movePointsKeyByVecFreelyInWW pointsKey v initWorld

movePointsKeyByVecPushingPointsKeyBlockedByMaskKeysInWW :: (Ord mk, Ord pk) => pk -> (Int, Int) -> pk -> [mk] -> WalkableWorld mk pk -> Maybe (WalkableWorld mk pk)
movePointsKeyByVecPushingPointsKeyBlockedByMaskKeysInWW toMovePointsKey v pushablePointsKey blockingMaskKeys initWorld =
    case (maybePushablePoints, maybeBlockingMaskKeys) of
        (Just pushablePoints, Just blockingMasks) ->
            if length pushablePoints /= length (nub pushablePoints)
                then error "All pushable points must be unique!"
                else case maybeToMovePointsKey of 
                        Just [toMovePoint] ->
                            let
                                pushingPointDestination = toMovePoint `movePoint` v
                                
                                collidesWithMask point world =
                                    any (inWWIsPointOverlappingMaskKey world point) blockingMaskKeys
                                
                                collidesWithPushable point world =
                                    inWWIsPointOverlappingPointsKey world point pushablePointsKey
                            
                                movePointByVecPushingPointsKeyBlockedByMaskKeysInWW lastMovedPoint v pushablePointsKey blockingMaskKeys world
                                    | nextPoint `collidesWithMask` world = Nothing  -- Collision detected, stop
                                    | nextPoint `collidesWithPushable` world = movePointByVecPushingPointsKeyBlockedByMaskKeysInWW nextPoint v pushablePointsKey blockingMaskKeys initWorld
                                    | otherwise = let oldAsciiWorld = wwRawAsciiWorld world
                                                      oldWorldPoints = asciiWorldPoints oldAsciiWorld
                                                      newAsciiWorld = oldAsciiWorld {asciiWorldPoints =
                                                              M.adjust (delete pushingPointDestination) (External pushablePointsKey)
                                                            $ M.adjust (nextPoint:) (External pushablePointsKey)
                                                            $ M.insert (External toMovePointsKey) [pushingPointDestination]
                                                            $ oldWorldPoints }
                                                  in Just $ world {wwRawAsciiWorld = newAsciiWorld}
                                  where nextPoint = lastMovedPoint `movePoint` v
                            in if pushingPointDestination `collidesWithMask` initWorld
                                then Just initWorld
                                else movePointByVecPushingPointsKeyBlockedByMaskKeysInWW toMovePoint v pushablePointsKey blockingMaskKeys initWorld
                        
                        Just _  -> error "Points key must be associated with a single point!"
                        Nothing -> error "No such points key!"
  where
    maybeToMovePointsKey = lookupPoints (External toMovePointsKey) $ wwRawAsciiWorld initWorld
    maybePushablePoints = lookupPoints (External pushablePointsKey) $ wwRawAsciiWorld initWorld
    maybeBlockingMaskKeys = sequence $ map (\blockingMaskKey -> lookupMask (External blockingMaskKey) $ wwRawAsciiWorld initWorld) blockingMaskKeys
