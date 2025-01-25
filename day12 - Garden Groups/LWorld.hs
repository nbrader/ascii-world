#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3

module LWorld ( LWorld(..)
              , emptyLWorld
              , readLWorld
              , showLWorld
              , printLWorld
              , combineTwoLWorlds
              , combineLWorlds
              , hasPoint
              , moveBitMaskInLWorld
              , movePointInLWorld
              , cutBitMaskWithBitMask
              , setPoint
              , insertBitMaskAtPoint
              , isOverlappingBitMasks ) where

-------------
-- Imports --
-------------
import Data.List (sortBy, groupBy, delete, find, transpose)
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Data.Maybe (fromJust, catMaybes, fromMaybe)
import Data.Ord
import Data.Function
import Data.Bits
import Control.Monad (guard, join)
import Data.Monoid
import Data.Foldable
import Safe (atMay)

import Util (replace)
import BitMask (Point, BitMask, pointToIndex, pointToBitMask, moveBitMask, movePoint, isOverlapping, diff, up, dn, lt, rt, allDirs)

-- Each obj has a shape encoded as bits of an Integer.

data Layer
    = Layer { lyrOrigin :: Point
            , lyrBoundsLRDU :: (Int,Int,Int,Int)
            , lyrBitMask :: BitMask } deriving (Show)
lyrWidth  lyr = let (l,r,d,u) = lyrBoundsLRDU lyr in r-l
lyrHeight lyr = let (l,r,d,u) = lyrBoundsLRDU lyr in u-d

data LWorld
    = LWorld { lWorldBG :: Char
             , lWorldLayers :: M.Map Char Layer
             , lWorldPoints :: M.Map Char Point
             , lWorldOrigin :: Point
             , lWorldBoundsLRDU :: (Int,Int,Int,Int) } deriving (Show)
lWorldWidth  w = let (l,r,d,u) = lWorldBoundsLRDU w in r-l
lWorldHeight w = let (l,r,d,u) = lWorldBoundsLRDU w in u-d

emptyLWorld :: Char -> Int -> LWorld
emptyLWorld bgChar width = LWorld bgChar mempty mempty origin (l,r,d,u)
  where origin = (0,0)
        width = 0
        height = 0
        (l,r,d,u) = (0,0,width,height)

-- BoundsMode is currently unused but should be made to select the bounds (size and location) of loaded layers
data BoundsMode = MaxBounds | MinBounds | MarginLRDU (Int,Int,Int,Int)

-- Assumes all rows have equal length
readLWorld :: BoundsMode -> Char -> [Char] -> String -> LWorld
readLWorld boundsMode bgChar singularChars inStr
    = LWorld { lWorldBG = bgChar,
               lWorldLayers = foldr addToLayer M.empty $ filter (\(char,_) -> not (char `elem` singularChars)) char2Ds,
               lWorldPoints = singularPoints,
               lWorldOrigin = origin,
               lWorldBoundsLRDU = (l,r,d,u) }
       
  where rows = lines inStr
        height = length rows
        width
          | height == 0 = 0
          | otherwise   = length $ head rows
        origin = (0,0)
        (l,r,d,u) = (0,width,0,height)
        char2Ds = readChar2DsFromRows rows
        singularPoints = M.fromList . catMaybes
                                    . map (\c -> find (\(c', (x,y)) -> c' == c) char2Ds)
                                    $ singularChars
        
        readChar2DsFromRows :: [String] -> [(Char, (Int,Int))]
        readChar2DsFromRows rows = do
            (y',row) <- zip [0..] rows
            (x,char) <- zip [0..] row
            
            let y = height - 1 - y'
            
            guard $ char /= bgChar
            
            return (char, (x, y))
        
        addToBitMask :: (Char, (Int, Int)) -> M.Map Char BitMask -> M.Map Char BitMask
        addToBitMask (char, (x, y)) = M.alter (setBitInBitMask (x, y)) char
        
        addToLayer :: (Char, (Int, Int)) -> M.Map Char Layer -> M.Map Char Layer
        addToLayer (char, (x, y)) = M.alter (setBitInLayer (x, y)) char
        
        setBitInBitMask :: (Int, Int) -> Maybe BitMask -> Maybe BitMask
        setBitInBitMask (x, y) maybeOldBitMask = Just newBitMask
          where emptyBitMask = 0
                newBitMask = setBit (fromMaybe emptyBitMask maybeOldBitMask) (y * width + x)
        
        setBitInLayer :: (Int, Int) -> Maybe Layer -> Maybe Layer
        setBitInLayer (x, y) maybeOldLayer = Just newLayer
          where maybeOldBitMask = fmap lyrBitMask maybeOldLayer -- assumes all layers read in so far have been given origin (0,0) and bounds matching the world
                emptyBitMask = 0
                newBitMask   = setBit (fromMaybe emptyBitMask maybeOldBitMask) (y * width + x)
                newLayer   = Layer { lyrOrigin = (0,0), lyrBoundsLRDU = (l,r,d,u), lyrBitMask = newBitMask }

showLWorld :: Int -> (Char -> Char -> Ordering) -> LWorld -> String
showLWorld height charZOrder lWorld = unlines . reverse . take height . chunksOf width . map (fromMaybe bgChar) $ listOfMaybeCharsFromBitMasksAndPoints
  where (LWorld bgChar bitMasks points origin (l,r,d,u)) = lWorld
        width = lWorldWidth lWorld
        listsOfMaybeCharsFromBitMasks = prioritize charZOrder $ M.mapWithKey (\c n -> bitMaskToMaybeChars c n) bitMasks
        listOfMaybeCharsFromBitMasks = combineMaybeCharLists listsOfMaybeCharsFromBitMasks
        charsAndPoints = map head . groupBy ((==) `on` snd) . sortBy (\(aChar,aPos) (bChar,bPos) -> compare aPos bPos <> compare aChar bChar) . M.toList $ points
        charsAndIndices = map (fmap (pointToIndex width)) charsAndPoints
        listOfMaybeCharsFromBitMasksAndPoints = foldr (\(c,i) acc -> let maybeOld = join (acc `atMay` i) in replace acc (i, Just $ minimum $ catMaybes [maybeOld, Just c])) listOfMaybeCharsFromBitMasks charsAndIndices
        
        combineMaybeCharLists :: [[Maybe a]] -> [Maybe a]
        combineMaybeCharLists = map (getFirst . fold . map First) . transpose
        
        prioritize :: Ord a1 => (a1 -> a1 -> Ordering) -> M.Map a1 a2 -> [a2]
        prioritize charZOrder = (\m -> map (fromJust . flip M.lookup m) (sortBy charZOrder $ M.keys m))
        
        bitMaskToMaybeChars :: Char -> Integer -> [Maybe Char]
        bitMaskToMaybeChars c n = map (\i -> if n `testBit` i then Just c else Nothing) [0..]

printLWorld :: Int -> (Char -> Char -> Ordering) -> LWorld -> IO ()
printLWorld height charZOrder lWorld = putStrLn $ showLWorld height charZOrder lWorld


-- Testing
exampleLWorld1 :: LWorld
exampleLWorld1 = LWorld '.' (M.fromList [('U',3)]) (M.fromList [('U',(7,7))]) 10

exampleLWorld2 :: LWorld
exampleLWorld2 = LWorld '.' (M.fromList [('U',96)]) (M.fromList [('U',(0,6))]) 10

exampleLWorld3 :: LWorld
exampleLWorld3 = exampleLWorld1 `combineTwoLWorlds` exampleLWorld2

exampleLWorld4 :: LWorld
exampleLWorld4 = movePointInLWorld 'U' (1,1) $ exampleLWorld3

examplePrint1 = printLWorld 10 (comparing id) exampleLWorld1
examplePrint2 = printLWorld 10 (comparing id) exampleLWorld2
examplePrint3 = printLWorld 10 (comparing id) exampleLWorld3
examplePrint4 = printLWorld 10 (comparing id) exampleLWorld4


-- Assumes lWorlds are same size
-- Left-biased such that the background character and any singular points they share are taken from the left
combineTwoLWorlds :: LWorld -> LWorld -> LWorld
combineTwoLWorlds w1 w2
    = w1 { lWorldLayers = M.unionWith combineBitMasks (lWorldLayers w1) (lWorldLayers w2),
           lWorldPoints = M.unionWith combinePoints (lWorldPoints w1) (lWorldPoints w2) }
  where combineBitMasks :: BitMask -> BitMask -> BitMask
        combineBitMasks points1 points2 = points1 .|. points2
        
        combinePoints :: Point -> Point -> Point
        combinePoints point1 _ = point1

combineLWorlds :: [LWorld] -> LWorld
combineLWorlds = foldr1 combineTwoLWorlds

hasPoint :: Char -> Point -> LWorld -> Bool
hasPoint char point lWorld = inPoints || inBitMasks
  where
    inPoints = case M.lookup char (lWorldPoints lWorld) of
        Just p -> p == point
        Nothing -> False

    inBitMasks = case M.lookup char (lWorldLayers lWorld) of
        Just bits -> testBit bits (pointToIndex (lWorldWidth lWorld) point)
        Nothing -> False

moveBitMaskInLWorld :: Char -> (Int,Int) -> LWorld -> LWorld
moveBitMaskInLWorld c (dx,dy) w = w {lWorldLayers = M.update (\pts -> Just $ moveBitMask width (dx,dy) pts) c (lWorldLayers w)}
  where width = lWorldWidth w

movePointInLWorld :: Char -> (Int,Int) -> LWorld -> LWorld
movePointInLWorld c (dx,dy) w = w {lWorldPoints = M.update (\pt -> Just $ movePoint width (dx,dy) pt) c (lWorldPoints w)}
  where width = lWorldWidth w

cutBitMaskWithBitMask :: Char -> Char -> LWorld -> LWorld
cutBitMaskWithBitMask targetChar cuttingChar w
    |   targetChar  `M.member` lWorldLayers w
     && cuttingChar `M.member` lWorldLayers w = w {lWorldLayers = M.insert targetChar newBitMask (lWorldLayers w)}
    | otherwise = w
  where targetBitMask  = fromJust $ M.lookup targetChar  (lWorldLayers w)
        cuttingBitMask = fromJust $ M.lookup cuttingChar (lWorldLayers w)
        newBitMask = targetBitMask `diff` cuttingBitMask

setPoint :: Char -> (Int,Int) -> LWorld -> LWorld
setPoint c (x,y) w = w {lWorldPoints = M.insert c (x,y) (lWorldPoints w)}

insertBitMaskAtPoint :: Char -> Char -> LWorld -> Maybe LWorld
insertBitMaskAtPoint bitMaskChar pointChar w = do
    point <- M.lookup pointChar (lWorldPoints w)
    let newBitMask = pointToBitMask width point
    return $ w {lWorldLayers = M.insert bitMaskChar newBitMask (lWorldLayers w)}
  where width = lWorldWidth w

isOverlappingBitMasks :: Char -> Char -> LWorld -> Bool
isOverlappingBitMasks c1 c2 w
    = fromMaybe False $ do
        points1 <- M.lookup c1 (lWorldLayers w)
        points2 <- M.lookup c2 (lWorldLayers w)
        
        return $ points1 `isOverlapping` points2
