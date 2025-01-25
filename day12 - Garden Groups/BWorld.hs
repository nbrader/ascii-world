#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3

module BWorld ( BWorld(..)
              , emptyBWorld
              , readBWorld
              , showBWorld
              , printBWorld
              , combineTwoBWorlds
              , combineBWorlds
              , hasPoint
              , moveBitMaskInBWorld
              , movePointInBWorld
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

data BWorld
    = BWorld { bWorldBG :: Char
             , bWorldBitMasks :: M.Map Char BitMask
             , bWorldPoints :: M.Map Char Point
             , bWorldWidth :: Int } deriving (Show)

emptyBWorld :: Char -> Int -> BWorld
emptyBWorld bgChar width = BWorld bgChar mempty mempty width


-- Assumes all rows have equal length
readBWorld :: Char -> [Char] -> String -> (Int,BWorld)
readBWorld bgChar singularChars inStr
    = ( height
      , BWorld { bWorldBG = bgChar,
                 bWorldBitMasks = foldr addToBitMask M.empty $ filter (\(char,_) -> not (char `elem` singularChars)) char2Ds,
                 bWorldPoints = singularPoints,
                 bWorldWidth = width } )
       
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

        addToBitMask :: (Char, (Int, Int)) -> M.Map Char BitMask -> M.Map Char BitMask
        addToBitMask (char, (x, y)) = M.alter (setBitInBitMask (x, y)) char

        setBitInBitMask :: (Int, Int) -> Maybe BitMask -> Maybe BitMask
        setBitInBitMask (x, y) maybeBitMask = Just $ setBit (fromMaybe 0 maybeBitMask) (y * width + x)


showBWorld :: Int -> (Char -> Char -> Ordering) -> BWorld -> String
showBWorld height charZOrder bWorld = unlines . reverse . take height . chunksOf width . map (fromMaybe bgChar) $ listOfMaybeCharsFromBitMasksAndPoints
  where (BWorld bgChar layers points width) = bWorld
        listsOfMaybeCharsFromBitMasks = prioritize charZOrder $ M.mapWithKey (\c n -> layerToMaybeChars c n) layers
        listOfMaybeCharsFromBitMasks = combineMaybeCharLists listsOfMaybeCharsFromBitMasks
        charsAndPoints = map head . groupBy ((==) `on` snd) . sortBy (\(aChar,aPos) (bChar,bPos) -> compare aPos bPos <> compare aChar bChar) . M.toList $ points
        charsAndIndices = map (fmap (pointToIndex width)) charsAndPoints
        listOfMaybeCharsFromBitMasksAndPoints = foldr (\(c,i) acc -> let maybeOld = join (acc `atMay` i) in replace acc (i, Just $ minimum $ catMaybes [maybeOld, Just c])) listOfMaybeCharsFromBitMasks charsAndIndices
        
        combineMaybeCharLists :: [[Maybe a]] -> [Maybe a]
        combineMaybeCharLists = map (getFirst . fold . map First) . transpose
        
        prioritize :: Ord a1 => (a1 -> a1 -> Ordering) -> M.Map a1 a2 -> [a2]
        prioritize charZOrder = (\m -> map (fromJust . flip M.lookup m) (sortBy charZOrder $ M.keys m))
        
        layerToMaybeChars :: Char -> Integer -> [Maybe Char]
        layerToMaybeChars c n = map (\i -> if n `testBit` i then Just c else Nothing) [0..]

printBWorld :: Int -> (Char -> Char -> Ordering) -> BWorld -> IO ()
printBWorld height charZOrder bWorld = putStrLn $ showBWorld height charZOrder bWorld


-- Testing
exampleBWorld1 :: BWorld
exampleBWorld1 = BWorld '.' (M.fromList [('U',3)]) (M.fromList [('U',(7,7))]) 10

exampleBWorld2 :: BWorld
exampleBWorld2 = BWorld '.' (M.fromList [('U',96)]) (M.fromList [('U',(0,6))]) 10

exampleBWorld3 :: BWorld
exampleBWorld3 = exampleBWorld1 `combineTwoBWorlds` exampleBWorld2

exampleBWorld4 :: BWorld
exampleBWorld4 = movePointInBWorld 'U' (1,1) $ exampleBWorld3

examplePrint1 = printBWorld 10 (comparing id) exampleBWorld1
examplePrint2 = printBWorld 10 (comparing id) exampleBWorld2
examplePrint3 = printBWorld 10 (comparing id) exampleBWorld3
examplePrint4 = printBWorld 10 (comparing id) exampleBWorld4


-- Assumes bWorlds are same size
-- Left-biased such that the background character and any singular points they share are taken from the left
combineTwoBWorlds :: BWorld -> BWorld -> BWorld
combineTwoBWorlds w1 w2
    = w1 { bWorldBitMasks = M.unionWith combineBitMasks (bWorldBitMasks w1) (bWorldBitMasks w2),
           bWorldPoints = M.unionWith combinePoints (bWorldPoints w1) (bWorldPoints w2) }
  where combineBitMasks :: BitMask -> BitMask -> BitMask
        combineBitMasks points1 points2 = points1 .|. points2
        
        combinePoints :: Point -> Point -> Point
        combinePoints point1 _ = point1

combineBWorlds :: [BWorld] -> BWorld
combineBWorlds = foldr1 combineTwoBWorlds

hasPoint :: Char -> Point -> BWorld -> Bool
hasPoint char point bWorld = inPoints || inBitMasks
  where
    inPoints = case M.lookup char (bWorldPoints bWorld) of
        Just p -> p == point
        Nothing -> False

    inBitMasks = case M.lookup char (bWorldBitMasks bWorld) of
        Just bits -> testBit bits (pointToIndex (bWorldWidth bWorld) point)
        Nothing -> False

moveBitMaskInBWorld :: Char -> (Int,Int) -> BWorld -> BWorld
moveBitMaskInBWorld c (dx,dy) w = w {bWorldBitMasks = M.update (\pts -> Just $ moveBitMask width (dx,dy) pts) c (bWorldBitMasks w)}
  where width = bWorldWidth w

movePointInBWorld :: Char -> (Int,Int) -> BWorld -> BWorld
movePointInBWorld c (dx,dy) w = w {bWorldPoints = M.update (\pt -> Just $ movePoint width (dx,dy) pt) c (bWorldPoints w)}
  where width = bWorldWidth w

cutBitMaskWithBitMask :: Char -> Char -> BWorld -> BWorld
cutBitMaskWithBitMask targetChar cuttingChar w
    |   targetChar  `M.member` bWorldBitMasks w
     && cuttingChar `M.member` bWorldBitMasks w = w {bWorldBitMasks = M.insert targetChar newBitMask (bWorldBitMasks w)}
    | otherwise = w
  where targetBitMask  = fromJust $ M.lookup targetChar  (bWorldBitMasks w)
        cuttingBitMask = fromJust $ M.lookup cuttingChar (bWorldBitMasks w)
        newBitMask = targetBitMask `diff` cuttingBitMask

setPoint :: Char -> (Int,Int) -> BWorld -> BWorld
setPoint c (x,y) w = w {bWorldPoints = M.insert c (x,y) (bWorldPoints w)}

insertBitMaskAtPoint :: Char -> Char -> BWorld -> Maybe BWorld
insertBitMaskAtPoint layerChar pointChar w = do
    point <- M.lookup pointChar (bWorldPoints w)
    let newBitMask = pointToBitMask width point
    return $ w {bWorldBitMasks = M.insert layerChar newBitMask (bWorldBitMasks w)}
  where width = bWorldWidth w

isOverlappingBitMasks :: Char -> Char -> BWorld -> Bool
isOverlappingBitMasks c1 c2 w
    = fromMaybe False $ do
        points1 <- M.lookup c1 (bWorldBitMasks w)
        points2 <- M.lookup c2 (bWorldBitMasks w)
        
        return $ points1 `isOverlapping` points2
