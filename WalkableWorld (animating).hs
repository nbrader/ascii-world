#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3 --package ansi-terminal-0.11.5
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3 --package ansi-terminal-0.11.5 -- '.\WalkableWorld.hs' -O2
-}

-- To Do:
-- Make "WalkableWorld" with max walk distance fed in at construction to then add that much margin and so be able to detect reachability effects up to that distance.

-- module WalkableWorld ( WalkableWorld(..)
                     -- , readWorld
                     -- , showWorld
                     -- , printWorld
                     -- , totalEdgesOverPoints
                     -- , maskKeys
                     -- , totalPoints
                     -- , partitionMaskByReachableLRDU
                     -- , partitionAllMasksByReachableLRDU ) where

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
import Control.Concurrent (threadDelay)
import System.Console.ANSI (clearScreen, setCursorPosition)

import Util ( lrduDirs )
import Mask ( Mask
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
                        , printAsciiWorld
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
fromExt_Int (External x) = x

data WWMaskKey = NoGo | TemporaryMask | VisitedThisSearch | Unvisited | LatestVisited | ToBePartitioned deriving (Show, Eq, Ord, Enum, Bounded)
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
modifyAsAsciiWorld :: (Ord km, Ord kp) => (AsciiWorld km kp -> AsciiWorld km kp) -> WalkableWorld km kp -> WalkableWorld km kp
modifyAsAsciiWorld f = addWalkableWorldParts . fmap (mapKeyForMasks External) . fmap (mapKeyForPoints External) . fmap f . fmap (mapKeyForPoints fromExt_Int) . fmap (mapKeyForMasks fromExt_Int) . undoNoGo

showWorld :: (Ord km, Ord kp) => Char -> (Ext_Int km WWMaskKey -> Char) -> (Ext_Int kp WWPointsKey -> Char) -> (WorldKey km kp -> WorldKey km kp -> Ordering) -> WalkableWorld km kp -> String
showWorld bgChar maskToChar pointsToChar nameZOrder w = (\(height, w') -> showAsciiWorld height bgChar maskToChar pointsToChar nameZOrderWithSpecials w') . undoWalkableWorldParts $ w
  where nameZOrderWithSpecials = nameZOrder `on` conversion
        conversion = toWorldKey . bimap fromExt_Int fromExt_Int . fromWorldKey

-- Shows the raw underlying ascii world except for underscores which are stripped so that there aren't just underscores for all non-background point.
showRawAsciiWorld :: (Ord km, Ord kp) => Int -> Char -> (Ext_Int km WWMaskKey -> Char) -> (Ext_Int kp WWPointsKey -> Char) -> WWNameZComp km kp -> WalkableWorld km kp -> String
showRawAsciiWorld height bgChar maskToChar pointsToChar nameZOrder w = showAsciiWorld height bgChar maskToChar pointsToChar nameZOrder . wwRawAsciiWorld $ w

printWorld :: (Ord km, Ord kp) => Char -> (Ext_Int km WWMaskKey -> Char) -> (Ext_Int kp WWPointsKey -> Char) -> (WorldKey km kp -> WorldKey km kp -> Ordering) -> WalkableWorld km kp -> IO ()
printWorld bgChar maskToChar pointsToChar nameZOrder = putStrLn . showWorld bgChar maskToChar pointsToChar nameZOrder

printRawAsciiWorld :: (Ord km, Ord kp) => Int -> Char -> (Ext_Int km WWMaskKey -> Char) -> (Ext_Int kp WWPointsKey -> Char) -> WWNameZComp km kp -> WalkableWorld km kp -> IO ()
printRawAsciiWorld height bgChar maskToChar pointsToChar nameZOrder = putStrLn . showRawAsciiWorld height bgChar maskToChar pointsToChar nameZOrder

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
addWalkableWorldParts (height, w) = WalkableWorld height . addNoGoToRightAndTop (height+1) . changeWidthBy 1 $ w

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
      & copyMask (External maskName) (Internal TemporaryMask)
      & moveMaskOfNameBy (Internal TemporaryMask) (0,1)
      & applyMask bitwiseXor (External maskName) (Internal TemporaryMask)
      & getTemporaryMask
      & countMaskPoints
  
  where getTemporaryMask = fromJust . M.lookup (Internal TemporaryMask) . asciiWorldMasks
        countMaskPoints = toInteger . popCount

totalVerticalEdgesOverPoints :: (Ord a, Ord kp) => a -> WalkableWorld a kp -> Integer
totalVerticalEdgesOverPoints maskName w =
    w & wwRawAsciiWorld
      & copyMask (External maskName) (Internal TemporaryMask)
      & moveMaskOfNameBy (Internal TemporaryMask) (1,0)
      & applyMask bitwiseXor (External maskName) (Internal TemporaryMask)
      & getTemporaryMask
      & countMaskPoints
  where
    getTemporaryMask = fromJust . M.lookup (Internal TemporaryMask) . asciiWorldMasks
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
partitionMaskByReachableLRDU maskKey initWorld = [] -- To Do: Implement with the below testLogic

testLogic :: (Show km, Show kp, Ord km, Ord kp) => km -> WalkableWorld km kp -> [AsciiWorld (Ext_Int km WWMaskKey) (Ext_Int kp WWPointsKey)]
testLogic maskKey (WalkableWorld height worldBeforePartition) = outerFinalWorlds
  where worldAfterCopyingTargetMaskToToBePartitioned = 
            worldBeforePartition
                & copyMask (External maskKey) (Internal ToBePartitioned)
        
        -- Start Loop until all connected parts of target mask found
        outerLoop outerInputWorlds = innerFinalWorlds ++ outerInputWorlds
          where outerInputWorld = head outerInputWorlds
                
                middlePoint = let maybeMiddlePoint = middlePointOfMask (Internal ToBePartitioned) outerInputWorld
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
                innerLoop innerInputWorlds = wAfterOneIteration : innerInputWorlds
                  where innerInputWorld = head innerInputWorlds
                        
                        worldWithUnvisitedXoredByVisitedThisSearch =
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
                
                innerFinalWorlds =
                    [worldWithInitLatestVisitedAndUnvisited]
                        & until ((== 0) . fromJust . lookupMask (Internal LatestVisited) . head) innerLoop
                        & (\(w:ws) -> applyMask bitwiseXor (Internal VisitedThisSearch) (Internal ToBePartitioned) w : w : ws)
                
                -- VisitedThisSearch should now contain one of the connected components for the target mask.
                
                -- Add this as new numbered mask of its own (or perhaps I should be returning a list of masks to do with what I will...) and subtract it from the a copy of the target layer
        
        outerFinalWorlds =
            [worldAfterCopyingTargetMaskToToBePartitioned]
                & until ((== 0) . fromJust . lookupMask (Internal ToBePartitioned) . head) (outerLoop) 
        
        -- End Loop until all connected parts of target mask found
        
        
        -- PRINTING HELPER
        -- printTrace label w = trace (label ++ "\n" ++ showW w) w
          -- where showW = showAsciiWorld (height+1) '.' (either (head . dropWhile (== '\'') . show) (head . show) . eitherFromExt_Int) (either (head . dropWhile (== '\'') . show) (head . show) . eitherFromExt_Int) (comparing id)
                        -- . filterMaskKeys (\x -> case x of {External _ -> False; (Internal ToBePartitioned) -> False; _ -> True})

test = do
    contents <- readFile "day12 (data).csv"
    
    let 
        maskNameToKeep = 'C'
        charMap c = Just (WKMask c)
        initWorld = readWorld charMap contents
        (WalkableWorld height _) = initWorld
        worldWithVisitedThisSearchMask = head $ testLogic maskNameToKeep initWorld
        
        newAsciiWorld = filterMaskKeys (\x -> case x of {External _ -> False; _ -> True}) worldWithVisitedThisSearchMask
        newAsciiWorld' = filterMaskKeys (\x -> case x of {Internal Unvisited -> False; _ -> True}) newAsciiWorld
        -- newAsciiWorld' = worldWithVisitedThisSearchMask
        newWorld = WalkableWorld height newAsciiWorld'
    
    -- printWorld '.' (either id (head . show) . eitherFromExt_Int) (either id (head . show) . eitherFromExt_Int) (comparing id) newWorld
    -- putStrLn "\n"
    printRawAsciiWorld (height+1) '.' (either id (head . show) . eitherFromExt_Int) (either id (head . show) . eitherFromExt_Int) (comparing id) newWorld
    print newWorld

main = test2

test2 = do
    contents <- readFile "day12 (data).csv"
    let
        maskNameToKeep = 'C'
        charMap c = Just (WKMask c)
        initWorld = (readWorld :: (Char -> Maybe (WorldKey Char Char)) -> String -> WalkableWorld Char Char) charMap contents
        (WalkableWorld height _) = initWorld
    
    let futureWorlds = reverse $ testLogic maskNameToKeep initWorld
    
    animateFrames 3 height futureWorlds

-- animateFrames :: Int -> Int -> [WalkableBoundedWorld] -> IO ()
animateFrames frameRate height worlds = mapM_ (animateStep frameRate height) worlds

-- animateStep :: Int -> Int -> WalkableBoundedWorld -> IO ()
animateStep frameRate height world = do
    clearScreen  -- Clear the console
    setCursorPosition 0 0  -- Move cursor to top-left
    printW height world  -- Print the current state
    threadDelay (50000 `div` frameRate)  -- Control frame rate

printW height = printAsciiWorld (height+1) '.' (either (head . dropWhile (== '\'') . show) (head . show) . eitherFromExt_Int) (either (head . dropWhile (== '\'') . show) (head . show) . eitherFromExt_Int) (comparing id)
                . filterMaskKeys (\x -> case x of {External _ -> False; (Internal ToBePartitioned) -> False; _ -> True})

partitionAllMasksByReachableLRDU :: (Show km, Ord km, Show kp, Ord kp) => WalkableWorld km kp -> M.Map km [Mask]
partitionAllMasksByReachableLRDU w = M.fromList $ map (\maskKey -> (maskKey, partitionMaskByReachableLRDU maskKey w)) (maskKeys w)
