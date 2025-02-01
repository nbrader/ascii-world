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
import Data.Bifunctor
import Data.Ord
import Data.Bits

import Util ( lrduDirs )
import Mask ( Mask, bitwiseSubtract, bitwiseXor, msbIndex, middleIndex, msbPoint, middlePoint, pointToMask )

import AsciiWorld as AW ( AsciiWorld(..)
                        , WorldKey(..)
                        , fromWorldKey
                        , toWorldKey
                        , readAsciiWorld
                        , showAsciiWorld
                        , combineAsciiWorlds
                        , moveMaskOfNameBy
                        , addMask
                        , copyMask
                        , applyMask
                        , setPoint
                        , deletePoints
                        , insertMaskFromPoints
                        , insertMaskFromNamedPoints
                        , setWidth
                        , changeWidthBy
                        , mapKeyForMasks
                        , mapKeyForPoints
                        , deleteMask
                        , filterMasks
                        , filterMaskKeys
                        , lookupMask
                        , adjustMask
                        , updateMask
                        , alterMask
                        , msbPointOfMask
                        , middlePointOfMask )

data WWMaskKey = NoGo | TemporaryMask | Visited | CopyOfTargetMask deriving (Show, Eq, Ord, Enum, Bounded)
data WWPointsKey = TemporaryPoints deriving (Show, Eq, Ord, Enum, Bounded)
allWWMaskKeys :: [WWMaskKey]
allWWMaskKeys = [minBound .. maxBound]

allWWPointsKeys :: [WWPointsKey]
allWWPointsKeys = [minBound .. maxBound]

data WWKey kExt kInt = WWExternal kExt | WWInternal kInt deriving (Show, Eq, Ord)
eitherFromWWKey :: WWKey kExt kInt -> Either kExt kInt
eitherFromWWKey (WWExternal x) = Left x
eitherFromWWKey (WWInternal y) = Right y
eitherToWWKey :: Either kExt kInt -> WWKey kExt kInt
eitherToWWKey (Left x)  = WWExternal x
eitherToWWKey (Right y) = WWInternal y
fromWWExternal (WWExternal x) = x

type RawAsciiWorld km kp = AsciiWorld (WWKey km WWMaskKey) (WWKey kp WWPointsKey)

type WWNameZComp km kp = WorldKey (WWKey km WWMaskKey) (WWKey kp WWPointsKey) -> WorldKey (WWKey km WWMaskKey) (WWKey kp WWPointsKey) -> Ordering
data WalkableWorld km kp = WalkableWorld {wwHeight :: Int, wwRawAsciiWorld :: RawAsciiWorld km kp} deriving (Show)
toHeightAndRawAsciiWorld :: WalkableWorld km kp -> (Int, RawAsciiWorld km kp)
toHeightAndRawAsciiWorld w = (wwHeight w, wwRawAsciiWorld w)
fromHeightAndRawAsciiWorld :: (Int, RawAsciiWorld km kp) -> WalkableWorld km kp
fromHeightAndRawAsciiWorld (h, w) = WalkableWorld h w

maskForNoGoRightAndTop :: Int -> Int -> Mask
maskForNoGoRightAndTop width height = rightNoGos + topNoGos
  where rightNoGos = sum [pointToMask width (x,y) |     x <- [0..(width-1)], let y = (height-1)      ]
        topNoGos   = sum [pointToMask width (x,y) | let x = (width-1),           y <- [0..(height-2)]]

addNoGoToRightAndTop :: (Ord km, Ord kp) => Int -> RawAsciiWorld km kp -> RawAsciiWorld km kp
addNoGoToRightAndTop height w = addMask (WWInternal NoGo) (maskForNoGoRightAndTop width height) w
  where width = asciiWorldWidth w

removeNoGoFromRightAndTop :: (Ord km, Ord kp) => RawAsciiWorld km kp -> RawAsciiWorld km kp
removeNoGoFromRightAndTop w = deleteMask (WWInternal NoGo) w

undoNoGo :: (Ord km, Ord kp) => WalkableWorld km kp -> (Int, RawAsciiWorld km kp)
undoNoGo w = (wwHeight w, changeWidthBy (-1) . removeNoGoFromRightAndTop . wwRawAsciiWorld $ w)

removeInternalMasks :: (Ord km, Ord kp) => RawAsciiWorld km kp -> RawAsciiWorld km kp
removeInternalMasks w = foldl' (\w maskKey -> deleteMask (WWInternal maskKey) w) w allWWMaskKeys

removeInternalPoints :: (Ord km, Ord kp) => RawAsciiWorld km kp -> RawAsciiWorld km kp
removeInternalPoints w = foldl' (\w maskKey -> deletePoints (WWInternal maskKey) w) w allWWPointsKeys

removeInternalMasksAndPoints :: (Ord km, Ord kp) => RawAsciiWorld km kp -> RawAsciiWorld km kp
removeInternalMasksAndPoints = removeInternalMasks . removeInternalPoints

addWalkableWorldParts :: (Ord km, Ord kp) => (Int, RawAsciiWorld km kp) -> WalkableWorld km kp
addWalkableWorldParts (height, w) = WalkableWorld height . addNoGoToRightAndTop (height+1) . changeWidthBy 1 $ w

undoWalkableWorldParts :: (Ord km, Ord kp) => WalkableWorld km kp -> (Int, RawAsciiWorld km kp)
undoWalkableWorldParts w = (wwHeight w, changeWidthBy (-1) . removeInternalMasksAndPoints . wwRawAsciiWorld $ w)

-- Assumes all rows have equal length
readWorld :: (Ord km, Ord kp) => (Char -> Maybe (WorldKey km kp)) -> String -> WalkableWorld km kp
readWorld charMap = addWalkableWorldParts . readAsciiWorld charMap'
  where charMap' c
            = do
                c' <- charMap c
                return $ case c' of
                    WKMask   x1 -> WKMask   (WWExternal x1)
                    WKPoints x2 -> WKPoints (WWExternal x2)

-- This modify modifies the underlying asciiWorld directly, including all of the stuff that WalkableWorld did to it (such as NoGos and underscores in names)
modifyRawAsciiWorld :: (Ord km, Ord kp) => (RawAsciiWorld km kp -> RawAsciiWorld km kp) -> WalkableWorld km kp -> WalkableWorld km kp
modifyRawAsciiWorld f = fromHeightAndRawAsciiWorld . fmap f . toHeightAndRawAsciiWorld

modifyHeightAndRawAsciiWorld :: (Ord km, Ord kp) => ((Int, RawAsciiWorld km kp) -> (Int, RawAsciiWorld km kp)) -> WalkableWorld km kp -> WalkableWorld km kp
modifyHeightAndRawAsciiWorld f = fromHeightAndRawAsciiWorld . f . toHeightAndRawAsciiWorld

-- This modify allows you to modify the world in a way ignorant to the stuff that WalkableWorld added (such as NoGos and underscores in names)
-- Warning: I think this function will perform badly. Also, it's not been properly tested.
modifyAsAsciiWorld :: (Ord km, Ord kp) => (AsciiWorld km kp -> AsciiWorld km kp) -> WalkableWorld km kp -> WalkableWorld km kp
modifyAsAsciiWorld f = addWalkableWorldParts . fmap (mapKeyForMasks WWExternal) . fmap (mapKeyForPoints WWExternal) . fmap f . fmap (mapKeyForPoints fromWWExternal) . fmap (mapKeyForMasks fromWWExternal) . undoNoGo

showWorld :: (Ord km, Ord kp) => Char -> (WWKey km WWMaskKey -> Char) -> (WWKey kp WWPointsKey -> Char) -> (WorldKey km kp -> WorldKey km kp -> Ordering) -> WalkableWorld km kp -> String
showWorld bgChar maskToChar pointsToChar nameZOrder w = (\(height, w') -> showAsciiWorld height bgChar maskToChar pointsToChar nameZOrderWithSpecials w') . undoWalkableWorldParts $ w
  where nameZOrderWithSpecials = nameZOrder `on` conversion
        conversion = toWorldKey . bimap fromWWExternal fromWWExternal . fromWorldKey

-- Shows the raw underlying ascii world except for underscores which are stripped so that there aren't just underscores for all non-background point.
showRawAsciiWorld :: (Ord km, Ord kp) => Int -> Char -> (WWKey km WWMaskKey -> Char) -> (WWKey kp WWPointsKey -> Char) -> WWNameZComp km kp -> WalkableWorld km kp -> String
showRawAsciiWorld height bgChar maskToChar pointsToChar nameZOrder w = showAsciiWorld height bgChar maskToChar pointsToChar nameZOrder . wwRawAsciiWorld $ w

printWorld :: (Ord km, Ord kp) => Char -> (WWKey km WWMaskKey -> Char) -> (WWKey kp WWPointsKey -> Char) -> (WorldKey km kp -> WorldKey km kp -> Ordering) -> WalkableWorld km kp -> IO ()
printWorld bgChar maskToChar pointsToChar nameZOrder = putStrLn . showWorld bgChar maskToChar pointsToChar nameZOrder

printRawAsciiWorld :: (Ord km, Ord kp) => Int -> Char -> (WWKey km WWMaskKey -> Char) -> (WWKey kp WWPointsKey -> Char) -> WWNameZComp km kp -> WalkableWorld km kp -> IO ()
printRawAsciiWorld height bgChar maskToChar pointsToChar nameZOrder = putStrLn . showRawAsciiWorld height bgChar maskToChar pointsToChar nameZOrder

-- removeForbidden :: (Ord km, Ord kp) => WalkableWorld km kp -> WalkableWorld km kp
-- removeForbidden w = WalkableWorld $ applyMask bitwiseSubtract "#" "O" (wwRawAsciiWorld w)

-- progressByAStep :: (Ord km, Ord kp) => WalkableWorld km kp -> WalkableWorld km kp
-- progressByAStep w = removeForbidden . WalkableWorld $ combineAsciiWorlds $ map (\dir -> moveMaskOfNameBy "O" dir (wwRawAsciiWorld w)) lrduDirs

-- setOAtS :: (Ord km, Ord kp) => WalkableWorld km kp -> WalkableWorld km kp
-- setOAtS = WalkableWorld . fromJust . insertMaskAtPoint "O" "S" . wwRawAsciiWorld

totalHorizontalEdgesOverPoints :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalHorizontalEdgesOverPoints maskName w =
    w & wwRawAsciiWorld
      & copyMask (WWExternal maskName) (WWInternal TemporaryMask)
      & moveMaskOfNameBy (WWInternal TemporaryMask) (0,1)
      & applyMask bitwiseXor (WWExternal maskName) (WWInternal TemporaryMask)
      & getTemporaryMask
      & countMaskPoints
  
  where getTemporaryMask = fromJust . M.lookup (WWInternal TemporaryMask) . asciiWorldMasks
        countMaskPoints = toInteger . popCount

totalVerticalEdgesOverPoints :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalVerticalEdgesOverPoints maskName w =
    w & wwRawAsciiWorld
      & copyMask (WWExternal maskName) (WWInternal TemporaryMask)
      & moveMaskOfNameBy (WWInternal TemporaryMask) (1,0)
      & applyMask bitwiseXor (WWExternal maskName) (WWInternal TemporaryMask)
      & getTemporaryMask
      & countMaskPoints
  where
    getTemporaryMask = fromJust . M.lookup (WWInternal TemporaryMask) . asciiWorldMasks
    countMaskPoints = toInteger . popCount

totalEdgesOverPoints :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalEdgesOverPoints maskName w =
    totalHorizontalEdgesOverPoints maskName w +
    totalVerticalEdgesOverPoints maskName w

totalPoints :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalPoints maskName w =
    w & wwRawAsciiWorld
      & getMask
      & countMaskPoints
  where
    getMask = fromJust . M.lookup (WWExternal maskName) . asciiWorldMasks
    countMaskPoints = toInteger . popCount

maskNames :: WalkableWorld a kp -> [a]
maskNames w =
    w & wwRawAsciiWorld
      & asciiWorldMasks
      & M.keys
      & map eitherFromWWKey
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
partitionMaskByReachableLRDU maskName w = undefined --WalkableWorld newAsciiWorld
  where -- To Do: This implementation is a WIP. Make it behave like the above explained algorithm.
        --        Currently, what it does is simply removes the midpoint of every mask.
        
        (_,w') = toHeightAndRawAsciiWorld w
        
        middlePoint = let maybeMiddlePoint = middlePointOfMask (WWExternal maskName) w'
                       in case maybeMiddlePoint of
                            Just point -> point
                            Nothing -> error $ "middlePoint failed: \"" ++ maskName  ++ "\" not found in " ++ show w'
        
        wWithXMidpointMask =
            w'  & setPoint (WWInternal TemporaryPoints) middlePoint
                & fromJust . insertMaskFromNamedPoints (WWInternal TemporaryMask) (WWInternal TemporaryPoints)
                & deletePoints (WWInternal TemporaryPoints)
        
        wWithMidpointXoredWithMaskName =
            wWithXMidpointMask  & deleteMask (WWInternal TemporaryMask)
                                & applyMask bitwiseXor (WWInternal TemporaryMask) (WWExternal maskName)
        
        newAsciiWorld = wWithMidpointXoredWithMaskName

test = do
    contents <- readFile "day12 (data).csv"
    
    let 
        maskNameToKeep = 'C'
        masksToDelete = map WWExternal . delete maskNameToKeep . nub $ contents
        
        charMap c = Just (WKMask c)
        
        initWorld = readWorld charMap contents
        
        (WalkableWorld height worldBeforePartition) = foldl' (\w maskName -> modifyRawAsciiWorld (deleteMask maskName) w) initWorld masksToDelete
        
        middlePoint = let maybeMiddlePoint = middlePointOfMask (WWExternal maskNameToKeep) worldBeforePartition
                       in case maybeMiddlePoint of
                            Just point -> point
                            Nothing -> error $ "middlePoint failed: \"" ++ [maskNameToKeep]  ++ "\" not found in " ++ show worldBeforePartition
        
        visited = [middlePoint]
        wWithVisitedMask = insertMaskFromPoints (WWInternal Visited) visited worldBeforePartition
        wWithVisitedMaskAndCopyOfTargetMask = copyMask (WWExternal maskNameToKeep) (WWInternal CopyOfTargetMask) wWithVisitedMask
        wWithMidpointWithCopyOfTargetMaskXoredWithVisited = applyMask bitwiseXor (WWInternal Visited) (WWExternal maskNameToKeep) wWithVisitedMaskAndCopyOfTargetMask
        
        newAsciiWorld = filterMaskKeys (\x -> case x of {WWExternal _ -> False; _ -> True}) wWithMidpointWithCopyOfTargetMaskXoredWithVisited
        newAsciiWorld' = filterMaskKeys (\x -> case x of {WWInternal CopyOfTargetMask -> False; _ -> True}) newAsciiWorld
        newWorld = WalkableWorld height newAsciiWorld'
    
    -- printWorld '.' (either id (head . show) . eitherFromWWKey) (either id (head . show) . eitherFromWWKey) (comparing id) newWorld
    -- putStrLn "\n"
    printRawAsciiWorld (height+1) '.' (either id (head . show) . eitherFromWWKey) (either id (head . show) . eitherFromWWKey) (comparing id) newWorld
    print newWorld

partitionAllMasksByReachableLRDU :: (Ord km, Ord kp) => WalkableWorld km kp -> WalkableWorld km kp
-- partitionAllMasksByReachableLRDU w = foldl' (flip partitionMaskByReachableLRDU) w (maskNames w)
partitionAllMasksByReachableLRDU w = undefined
