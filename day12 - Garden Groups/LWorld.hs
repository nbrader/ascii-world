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
              , moveMaskInLWorld
              , movePointInLWorld
              , subtractMask
              , setPoint
              , insertMaskAtPoint
              , isOverlappingLayers ) where

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
import Mask (Point, Mask, pointToIndex, pointToMask, moveMask, movePoint, isOverlapping, bitwiseSubtract, bitwiseAnd, bitwiseOr, bitwiseXor, up, dn, lt, rt, allDirs, combineMasks)

-- Each obj has a shape encoded as bits of an Integer referred to as a mask which is interpreted in 2D by stacking upwards rows of a given width.

-- A Layer is a bitmask of at least 1 bit (the least significant bit) coupled with a position of the least sig. bit (LSB) relative to the bottom
--    left of the world and a rectangular window that is defined in terms of a lyrWindowLRDU tuple giving the left, right (x bounds) and then down
--    and up (y bounds) on the same coordinate grid.
data Layer
    = Layer { lyrLSBPosition :: Point
            , lyrWindowLRDU :: (Int,Int,Int,Int)
            , lyrMaskWidth :: Int
            , lyrMask :: Mask } deriving (Show)
lyrWidth  lyr = let (l,r,d,u) = lyrWindowLRDU lyr in r-l
lyrHeight lyr = let (l,r,d,u) = lyrWindowLRDU lyr in u-d

data LWorld
    = LWorld { lWorldBG :: Char
             , lWorldLayers :: M.Map Char Layer
             , lWorldPoints :: M.Map Char Point
             , lWorldWidth :: Int
             , lWorldHeight :: Int } deriving (Show)

emptyLWorld :: Char -> Int -> LWorld
emptyLWorld bgChar width = LWorld bgChar mempty mempty width height
  where width = 0
        height = 0

-- BoundsMode is currently unused but should be made to select the bounds (size and location) of loaded layers
data BoundsMode = MaxBounds | MinBounds | MarginLRDU (Int,Int,Int,Int)

-- Assumes all rows have equal length
readLWorld :: BoundsMode -> Char -> [Char] -> String -> LWorld
readLWorld boundsMode bgChar singularChars inStr
    = LWorld { lWorldBG = bgChar,
               lWorldLayers = foldr addToLayer M.empty $ filter (\(char,_) -> not (char `elem` singularChars)) char2Ds,
               lWorldPoints = singularPoints,
               lWorldWidth = width,
               lWorldHeight = height }
       
  where rows = lines inStr
        height = length rows
        width
          | height == 0 = 0
          | otherwise   = length $ head rows
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
        
        addToMask :: (Char, (Int, Int)) -> M.Map Char Mask -> M.Map Char Mask
        addToMask (char, (x, y)) = M.alter (setBitInMask (x, y)) char
        
        addToLayer :: (Char, (Int, Int)) -> M.Map Char Layer -> M.Map Char Layer
        addToLayer (char, (x, y)) = M.alter (setBitInLayer (x, y)) char
        
        setBitInMask :: (Int, Int) -> Maybe Mask -> Maybe Mask
        setBitInMask (x, y) maybeOldMask = Just newMask
          where emptyMask = 0
                newMask = setBit (fromMaybe emptyMask maybeOldMask) (y * width + x)
        
        setBitInLayer :: (Int, Int) -> Maybe Layer -> Maybe Layer
        setBitInLayer (x, y) maybeOldLayer = Just newLayer
          where maybeOldMask = fmap lyrMask maybeOldLayer -- assumes all layers read in so far have been given origin (0,0) and bounds matching the world
                emptyMask = 0
                newMask = setBit (fromMaybe emptyMask maybeOldMask) (y * width + x)
                newLayer   = Layer { lyrLSBPosition = (0,0), lyrWindowLRDU = (0,width,0,height), lyrMaskWidth = width, lyrMask = newMask }

addVec2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
addVec2  (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

diffVec2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
diffVec2  (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- Applies a bitmask shift and sets bits in a new mask
applyMaskShift :: Mask -> Int -> Int -> (Int, Int) -> (Int, Int) -> Mask
applyMaskShift srcMask srcWidth destWidth (dx, dy) (maxX, maxY) =
    let
        setBits :: Mask -> Int -> Int -> Mask
        setBits acc x y
            | x >= srcWidth = acc  -- Stop at the right boundary
            | otherwise =
                let bitIndex = pointToIndex srcWidth (x, y)
                    newX = x + dx
                    newY = y + dy
                    newIndex = pointToIndex destWidth (newX, newY)
                    bitIsSet = testBit srcMask bitIndex
                    newMask = if bitIsSet && newX >= 0 && newY >= 0 && newX < maxX && newY < maxY
                              then setBit acc newIndex
                              else acc
                in setBits newMask (x + 1) y
    in foldl (\mask y -> setBits mask 0 y) zeroBits [0 .. maxY - 1]


-- To Do: Check that these blit functions (blitToLayer and blitToWorld) ensure that nothing is written from outside the src window and to outside the dest window or otherwise justify a different behaviour
-- Blit a layer onto another layer
blitToLayer :: Layer -> Layer -> Layer
blitToLayer src dest =
    let destLSB = lyrLSBPosition dest
        destWidth = lyrMaskWidth dest
        srcLSB = lyrLSBPosition src
        srcWidth = lyrMaskWidth src
        srcMask = lyrMask src
        destMask = lyrMask dest

        -- Compute height of the source layer
        (_, _, _, srcHeight) = lyrWindowLRDU src

        -- Compute coordinate shift
        shift = srcLSB `diffVec2` destLSB

        -- Shift bitmask
        srcMask_Shifted = applyMaskShift srcMask srcWidth destWidth shift (destWidth, srcHeight)

    in dest { lyrMask = combineMasks destMask srcMask_Shifted }

-- Blit a layer onto a world
blitToWorld :: Layer -> LWorld -> Layer
blitToWorld layer world =
    let worldWidth  = lWorldWidth world
        worldHeight = lWorldHeight world
        layerLSB = lyrLSBPosition layer
        layerWidth = lyrMaskWidth layer
        layerMask = lyrMask layer

        -- Compute height of the layer
        (_, _, _, layerHeight) = lyrWindowLRDU layer

        -- Compute shift (layer LSB to (0,0) in world space)
        shift = layerLSB `diffVec2` (0, 0)

        -- Shift bitmask
        newMask = applyMaskShift layerMask layerWidth worldWidth shift (worldWidth, worldHeight)

    in Layer
        { lyrLSBPosition = (0, 0)
        , lyrWindowLRDU = (0, worldWidth, 0, worldHeight)
        , lyrMaskWidth = worldWidth
        , lyrMask = newMask
        }

showLWorld :: Int -> (Char -> Char -> Ordering) -> LWorld -> String
showLWorld height charZOrder lWorld = unlines . reverse . take height . chunksOf width . map (fromMaybe bgChar) $ listOfMaybeCharsFromMasksAndPoints
  where (LWorld bgChar layers points width height) = lWorld
        masks = fmap lyrMask $ fmap (`blitToWorld` lWorld) layers
        listsOfMaybeCharsFromMasks = prioritize charZOrder $ M.mapWithKey (\c n -> maskToMaybeChars c n) masks
        listOfMaybeCharsFromMasks = combineMaybeCharLists listsOfMaybeCharsFromMasks
        charsAndPoints = map head . groupBy ((==) `on` snd) . sortBy (\(aChar,aPos) (bChar,bPos) -> compare aPos bPos <> compare aChar bChar) . M.toList $ points
        charsAndIndices = map (fmap (pointToIndex width)) charsAndPoints
        listOfMaybeCharsFromMasksAndPoints = foldr (\(c,i) acc -> let maybeOld = join (acc `atMay` i) in replace acc (i, Just $ minimum $ catMaybes [maybeOld, Just c])) listOfMaybeCharsFromMasks charsAndIndices
        
        combineMaybeCharLists :: [[Maybe a]] -> [Maybe a]
        combineMaybeCharLists = map (getFirst . fold . map First) . transpose
        
        prioritize :: Ord a1 => (a1 -> a1 -> Ordering) -> M.Map a1 a2 -> [a2]
        prioritize charZOrder = (\m -> map (fromJust . flip M.lookup m) (sortBy charZOrder $ M.keys m))
        
        maskToMaybeChars :: Char -> Integer -> [Maybe Char]
        maskToMaybeChars c n = map (\i -> if n `testBit` i then Just c else Nothing) [0..]

printLWorld :: Int -> (Char -> Char -> Ordering) -> LWorld -> IO ()
printLWorld height charZOrder lWorld = putStrLn $ showLWorld height charZOrder lWorld


-- Testing
exampleWorldWidth = 10
exampleWorldHeight = 10
exampleWindow = (0,exampleWorldWidth,0,exampleWorldHeight)
exampleMaskWidth = 10
simpleLayer mask = Layer (0,0) exampleWindow exampleMaskWidth mask

exampleLWorld1 :: LWorld
exampleLWorld1 = LWorld '.' (M.fromList [('U', simpleLayer 3)]) (M.fromList [('U',(7,7))]) exampleWorldWidth exampleWorldHeight

exampleLWorld2 :: LWorld
exampleLWorld2 = LWorld '.' (M.fromList [('U', let lyr = simpleLayer 3 in lyr { lyrLSBPosition = (0,1) }), ('V', let lyr = simpleLayer 6 in lyr { lyrLSBPosition = (0,2) })]) (M.fromList [('S',(2,1))]) exampleWorldWidth exampleWorldHeight

exampleLWorld3 :: LWorld
exampleLWorld3 = exampleLWorld1 `combineTwoLWorlds` exampleLWorld2

exampleLWorld4 :: LWorld
exampleLWorld4 = movePointInLWorld 'U' (1,1) $ exampleLWorld3

examplePrint1 = printLWorld exampleWorldWidth (comparing id) exampleLWorld1
examplePrint2 = printLWorld exampleWorldWidth (comparing id) exampleLWorld2
examplePrint3 = printLWorld exampleWorldWidth (comparing id) exampleLWorld3
examplePrint4 = printLWorld exampleWorldWidth (comparing id) exampleLWorld4


-- Assumes lWorlds are same size
-- Left-biased such that the background character and any singular points they share are taken from the left
combineTwoLWorlds :: LWorld -> LWorld -> LWorld
combineTwoLWorlds w1 w2
    = w1 { lWorldLayers = M.unionWith blitToLayer (lWorldLayers w1) (lWorldLayers w2),
           lWorldPoints = M.unionWith combinePoints (lWorldPoints w1) (lWorldPoints w2) }
  where combineMasks :: Mask -> Mask -> Mask
        combineMasks points1 points2 = points1 .|. points2
        
        combinePoints :: Point -> Point -> Point
        combinePoints point1 _ = point1

combineLWorlds :: [LWorld] -> LWorld
combineLWorlds = foldr1 combineTwoLWorlds

-- Bug: hasPoint currently ignores position and window information
hasPoint :: Char -> Point -> LWorld -> Bool
hasPoint char point lWorld = inPoints || inMasks
  where
    inPoints = case M.lookup char (lWorldPoints lWorld) of
        Just p -> p == point
        Nothing -> False

    inMasks = case M.lookup char (lWorldLayers lWorld) of
        Just layer -> testBit (lyrMask layer) (pointToIndex (lWorldWidth lWorld) point)
        Nothing -> False

moveMaskInLWorld :: Char -> (Int,Int) -> LWorld -> LWorld
moveMaskInLWorld c (dx,dy) w = w {lWorldLayers = M.update updateLayer c (lWorldLayers w)}
  where 
    width = lWorldWidth w
    updateLayer layer = Just $ layer { lyrLSBPosition = lyrLSBPosition layer `addVec2` (dx,dy) }

movePointInLWorld :: Char -> (Int,Int) -> LWorld -> LWorld
movePointInLWorld c (dx,dy) w = w {lWorldPoints = M.update (\pt -> Just $ movePoint width (dx,dy) pt) c (lWorldPoints w)}
  where width = lWorldWidth w

-- Bug: bitwiseSubtractMaskWithMask currently ignores position and window information
subtractMask :: Char -> Char -> LWorld -> LWorld
subtractMask targetChar cuttingChar w
    |   targetChar  `M.member` lWorldLayers w
     && cuttingChar `M.member` lWorldLayers w = w {lWorldLayers = M.insert targetChar newLayer (lWorldLayers w)}
    | otherwise = w
  where 
    targetLayer   = fromJust $ M.lookup targetChar  (lWorldLayers w)
    cuttingLayer = fromJust $ M.lookup cuttingChar (lWorldLayers w)
    newLayer = targetLayer { lyrMask = lyrMask targetLayer `bitwiseSubtract` lyrMask cuttingLayer }

setPoint :: Char -> (Int,Int) -> LWorld -> LWorld
setPoint c (x,y) w = w {lWorldPoints = M.insert c (x,y) (lWorldPoints w)}

insertMaskAtPoint :: Char -> Char -> LWorld -> Maybe LWorld
insertMaskAtPoint maskChar pointChar w = do
    point@(x,y) <- M.lookup pointChar (lWorldPoints w)
    let newLayer = Layer 
            { lyrLSBPosition = point
            , lyrWindowLRDU = (x, x, y, y)
            , lyrMaskWidth = width
            , lyrMask = 1
            }
    return $ w {lWorldLayers = M.insert maskChar newLayer (lWorldLayers w)}
  where 
    width = lWorldWidth w
    height = lWorldHeight w

isOverlappingLayers :: Char -> Char -> LWorld -> Bool
isOverlappingLayers c1 c2 w = fromMaybe False $ do
    layer1 <- M.lookup c1 (lWorldLayers w)
    layer2 <- M.lookup c2 (lWorldLayers w)
    
    let (l1, r1, d1, u1) = lyrWindowLRDU layer1
        (l2, r2, d2, u2) = lyrWindowLRDU layer2
        
    guard $ not $ 
        r1 < l2 || r2 < l1 ||  -- horizontal non-overlap 
        u1 < d2 || u2 < d1     -- vertical non-overlap
    
    let worldWidth = lWorldWidth w
        blittedLayer1 = blitToLayer layer1 (layer2 { lyrMask = 0 })
    
    return $ lyrMask blittedLayer1 `isOverlapping` lyrMask layer2
