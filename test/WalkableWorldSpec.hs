{-# LANGUAGE OverloadedStrings #-}

module WalkableWorldSpec (spec) where

import Test.Hspec
import qualified Data.Map as M
import Data.Bits (popCount)
import Data.Maybe (fromJust)

import WalkableWorld
import AsciiWorld (asciiWorldMasks, asciiWorldWidth)
import Mask (maskToPoints)

spec :: Spec
spec = do
  describe "partitionAllMasksByReachableLRDU" $ do
    it "returns a single region for a solid block" $ do
      let world = readGarden [ "AAA"
                             , "AAA"
                             , "AAA"
                             ]
          regions = partitionAllMasksByReachableLRDU world
      M.keys regions `shouldBe` ['A']
      case M.lookup 'A' regions of
        Just [region] -> popCount region `shouldBe` 9
        _ -> expectationFailure "Expected one connected region for A"

    it "splits diagonally separated areas" $ do
      let world = readGarden [ "A.A"
                             , "..."
                             , "A.A"
                             ]
          regions = partitionAllMasksByReachableLRDU world
      M.lookup 'A' regions `shouldSatisfy` maybe False ((== 4) . length)

  describe "perimeter calculations" $ do
    it "computes perimeter for a single cell" $ do
      let world = readGarden ["A"]
      totalEdgesOverPoints 'A' world `shouldBe` 4

    it "computes perimeter for a hollow square" $ do
      let world = readGarden [ "AAA"
                             , "A.A"
                             , "AAA"
                             ]
      totalEdgesOverPoints 'A' world `shouldBe` 16

  describe "side counting" $ do
    it "reports four sides for any solid rectangle" $ do
      let world = readGarden [ "AAAA"
                             , "AAAA"
                             ]
      totalConnectedOneSidedEdges 'A' world `shouldBe` 4

  describe "mask lookup helpers" $ do
    it "exposes raw coordinates for letter masks" $ do
      let world = readGarden [ "ABC"
                             , "DEF"
                             , "GHI"
                             ]
          maskG = fromJust $ lookupMaskInWW 'G' world
      maskToPoints (wwWidth world) maskG `shouldBe` [(0,0)]

    it "totalPoints matches popCount of stored mask" $ do
      let world = readGarden ["AAAA"]
          mask = fromJust $ lookupMaskInWW 'A' world
      totalPoints 'A' world `shouldBe` toInteger (popCount mask)

readGarden :: [String] -> WalkableWorld Char Char
readGarden rows = readWorld charMap (unlines rows)
  where
    charMap '.' = Nothing
    charMap c   = Just (MaskIndex c)
