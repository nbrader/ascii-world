{-# LANGUAGE OverloadedStrings #-}

module WalkableWorldSpec (spec) where

import Test.Hspec
import qualified Data.Map as M
import Data.Maybe (fromJust)
import WalkableWorld
import AsciiWorld
import Mask

spec :: Spec
spec = do
  describe "Flood-fill and Region Detection" $ do
    it "partitions single region correctly" $ do
      let input = unlines [ "AAA"
                          , "AAA"
                          , "AAA" ]
      let charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input
      let regions = partitionAllMasksByReachableLRDU world

      -- Should have 1 region for 'A'
      M.size regions `shouldBe` 1
      case M.lookup 'A' regions of
        Just [region] -> totalPoints region world `shouldBe` 9
        _ -> expectationFailure "Expected exactly 1 region for 'A'"

    it "partitions multiple disconnected regions" $ do
      let input = unlines [ "A.A"
                          , "..."
                          , "A.A" ]
      let charMap '.' = Nothing
          charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input
      let regions = partitionAllMasksByReachableLRDU world

      -- Should have 4 separate regions for 'A'
      case M.lookup 'A' regions of
        Just rs -> length rs `shouldBe` 4
        Nothing -> expectationFailure "Expected 'A' regions"

    it "distinguishes different plant types" $ do
      let input = unlines [ "AAA"
                          , "BBB"
                          , "CCC" ]
      let charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input
      let regions = partitionAllMasksByReachableLRDU world

      -- Should have 3 plant types
      M.size regions `shouldBe` 3
      M.member 'A' regions `shouldBe` True
      M.member 'B' regions `shouldBe` True
      M.member 'C' regions `shouldBe` True

    it "handles complex connected regions" $ do
      let input = unlines [ "AAAA"
                          , "A..A"
                          , "A..A"
                          , "AAAA" ]
      let charMap '.' = Nothing
          charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input
      let regions = partitionAllMasksByReachableLRDU world

      -- Border forms 1 connected region
      case M.lookup 'A' regions of
        Just [region] -> totalPoints region world `shouldBe` 12
        _ -> expectationFailure "Expected 1 connected border region"

  describe "Edge and Perimeter Counting" $ do
    it "counts perimeter of single cell" $ do
      let input = "A"
      let charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input

      totalEdgesOverPoints 'A' world `shouldBe` 4

    it "counts perimeter of 2x2 square" $ do
      let input = unlines [ "AA"
                          , "AA" ]
      let charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input

      totalEdgesOverPoints 'A' world `shouldBe` 8

    it "counts perimeter of line correctly" $ do
      let input = "AAAA"
      let charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input

      -- Line has 2 long edges + 2 short edges = 2*4 + 2*1 = 10
      totalEdgesOverPoints 'A' world `shouldBe` 10

    it "counts perimeter with internal holes" $ do
      let input = unlines [ "AAA"
                          , "A.A"
                          , "AAA" ]
      let charMap '.' = Nothing
          charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input

      -- 8 cells, but hole adds internal perimeter
      totalEdgesOverPoints 'A' world `shouldBe` 16

  describe "Side Counting (Connected Edges)" $ do
    it "counts 4 sides for single cell" $ do
      let input = "A"
      let charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input

      totalConnectedOneSidedEdges 'A' world `shouldBe` 4

    it "counts 4 sides for 2x2 square" $ do
      let input = unlines [ "AA"
                          , "AA" ]
      let charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input

      -- Square has 4 sides regardless of size
      totalConnectedOneSidedEdges 'A' world `shouldBe` 4

    it "counts 4 sides for rectangle" $ do
      let input = unlines [ "AAA"
                          , "AAA" ]
      let charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input

      -- Rectangle also has 4 sides
      totalConnectedOneSidedEdges 'A' world `shouldBe` 4

    it "counts sides with internal hole" $ do
      let input = unlines [ "AAA"
                          , "A.A"
                          , "AAA" ]
      let charMap '.' = Nothing
          charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input

      -- Outer 4 sides + inner 4 sides = 8
      totalConnectedOneSidedEdges 'A' world `shouldBe` 8

  describe "Real-world AoC Examples" $ do
    it "solves simple garden example correctly" $ do
      let input = unlines [ "AAAA"
                          , "BBCD"
                          , "BBCC"
                          , "EEEC" ]
      let charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input
      let regions = partitionAllMasksByReachableLRDU world

      -- Verify A region: 4 cells, perimeter 10
      case M.lookup 'A' regions of
        Just [regionA] -> do
          totalPoints regionA world `shouldBe` 4
          totalEdgesOverPoints regionA world `shouldBe` 10
        _ -> expectationFailure "Expected 1 region for 'A'"

      -- Verify B region: 4 cells, perimeter 8
      case M.lookup 'B' regions of
        Just [regionB] -> do
          totalPoints regionB world `shouldBe` 4
          totalEdgesOverPoints regionB world `shouldBe` 8
        _ -> expectationFailure "Expected 1 region for 'B'"

    it "calculates total fence cost correctly" $ do
      let input = unlines [ "AAAA"
                          , "BBCD"
                          , "BBCC"
                          , "EEEC" ]
      let charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input
      let regions = partitionAllMasksByReachableLRDU world

      let calculateCost plantType regionList =
            sum [ totalPoints region world * totalEdgesOverPoints region world
                | region <- regionList ]

      let totalCost = sum [ calculateCost pt rs | (pt, rs) <- M.toList regions ]

      -- Expected: A(4*10) + B(4*8) + C(4*10) + D(1*4) + E(3*8) = 140
      totalCost `shouldBe` 140

    it "calculates discounted cost with sides correctly" $ do
      let input = unlines [ "AAAA"
                          , "BBCD"
                          , "BBCC"
                          , "EEEC" ]
      let charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input
      let regions = partitionAllMasksByReachableLRDU world

      let calculateDiscountedCost plantType regionList =
            sum [ totalPoints region world * totalConnectedOneSidedEdges region world
                | region <- regionList ]

      let totalCost = sum [ calculateDiscountedCost pt rs | (pt, rs) <- M.toList regions ]

      -- Expected: A(4*4) + B(4*4) + C(4*8) + D(1*4) + E(3*4) = 80
      totalCost `shouldBe` 80

  describe "Grid Coordinate System" $ do
    it "handles bottom-left origin correctly" $ do
      let input = unlines [ "ABC"  -- Top row (y=2)
                          , "DEF"  -- Middle row (y=1)
                          , "GHI" ] -- Bottom row (y=0)
      let charMap c = Just (MaskIndex [c])
      let (height, world) = readWorld charMap input

      -- Height should be 3
      height `shouldBe` 3

      -- 'G' should be at bottom-left (0,0) which is index 0
      let maskG = fromJust $ M.lookup 'G' (asciiWorldMasks world)
      maskToPoints (asciiWorldWidth world) maskG `shouldBe` [(0, 0)]

      -- 'C' should be at top-right (2,2) which is index 2 + 2*3 = 8
      let maskC = fromJust $ M.lookup 'C' (asciiWorldMasks world)
      maskToPoints (asciiWorldWidth world) maskC `shouldBe` [(2, 2)]

  describe "Bitwise Operations on Regions" $ do
    it "correctly identifies overlapping regions" $ do
      let input = unlines [ "AA"
                          , "AA" ]
      let charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input

      let maskA = fromJust $ M.lookup 'A' (asciiWorldMasks world)
      -- Create a mask for top-left cell
      let topLeft = pointToMask (asciiWorldWidth world) (0, 1)

      isOverlapping maskA topLeft `shouldBe` True

    it "combines regions with bitwiseOr" $ do
      let input = unlines [ "A.A"
                          , "..." ]
      let charMap '.' = Nothing
          charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input

      let regions = partitionAllMasksByReachableLRDU world
      case M.lookup 'A' regions of
        Just [r1, r2] -> do
          let combined = bitwiseOr r1 r2
          totalPoints combined world `shouldBe` 2
        _ -> expectationFailure "Expected 2 separate regions"

  describe "Edge Cases" $ do
    it "handles empty regions gracefully" $ do
      let input = "..."
      let charMap '.' = Nothing
          charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input

      M.null (asciiWorldMasks world) `shouldBe` True

    it "handles single point regions" $ do
      let input = unlines [ "..."
                          , ".A."
                          , "..." ]
      let charMap '.' = Nothing
          charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input

      totalPoints 'A' world `shouldBe` 1
      totalEdgesOverPoints 'A' world `shouldBe` 4

    it "handles large connected regions efficiently" $ do
      let input = unlines $ replicate 10 (replicate 10 'A')
      let charMap c = Just (MaskIndex [c])
      let (_, world) = readWorld charMap input

      totalPoints 'A' world `shouldBe` 100
      -- 10x10 square: perimeter = 4*10 = 40
      totalEdgesOverPoints 'A' world `shouldBe` 40
