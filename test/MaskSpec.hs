{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}

module MaskSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Bits
import Mask

spec :: Spec
spec = do
  describe "pointToIndex" $ do
    it "converts (0,0) to index 0" $
      pointToIndex 10 (0, 0) `shouldBe` 0

    it "converts (1,0) to index 1" $
      pointToIndex 10 (1, 0) `shouldBe` 1

    it "converts (0,1) to index width (10)" $
      pointToIndex 10 (0, 1) `shouldBe` 10

    it "converts (3,2) correctly in 10-wide grid" $
      pointToIndex 10 (3, 2) `shouldBe` 23

  describe "indexToPoint" $ do
    it "converts index 0 to (0,0)" $
      indexToPoint 10 0 `shouldBe` (0, 0)

    it "converts index 1 to (1,0)" $
      indexToPoint 10 1 `shouldBe` (1, 0)

    it "converts index width to (0,1)" $
      indexToPoint 10 10 `shouldBe` (0, 1)

    it "converts index 23 to (3,2) in 10-wide grid" $
      indexToPoint 10 23 `shouldBe` (3, 2)

  describe "pointToIndex <-> indexToPoint roundtrip" $ do
    it "roundtrips for width=10, point (3,5)" $
      let width = 10
          point = (3, 5)
      in indexToPoint width (pointToIndex width point) `shouldBe` point

    it "roundtrips for width=5, index 17" $
      let width = 5
          index = 17
      in pointToIndex width (indexToPoint width index) `shouldBe` index

  describe "pointToMask" $ do
    it "creates mask with single bit set at position 0" $
      pointToMask 10 (0, 0) `shouldBe` 1

    it "creates mask with single bit set at position 1" $
      pointToMask 10 (1, 0) `shouldBe` 2

    it "creates mask with bit set at position 10" $
      pointToMask 10 (0, 1) `shouldBe` (1 `shift` 10)

    it "creates mask with bit set at position 23" $
      pointToMask 10 (3, 2) `shouldBe` (1 `shift` 23)

  describe "moveMask" $ do
    it "moves mask right by 1" $
      moveMask 10 (1, 0) 1 `shouldBe` 2

    it "moves mask up by 1 (shift by width)" $
      moveMask 10 (0, 1) 1 `shouldBe` (1 `shift` 10)

    it "moves mask diagonally (1,1)" $
      moveMask 10 (1, 1) 1 `shouldBe` (1 `shift` 11)

    it "moving by (0,0) doesn't change mask" $
      moveMask 10 (0, 0) 42 `shouldBe` 42

  describe "movePoint" $ do
    it "moves point right by 1" $
      movePoint (1, 0) (3, 5) `shouldBe` (4, 5)

    it "moves point up by 1" $
      movePoint (0, 1) (3, 5) `shouldBe` (3, 6)

    it "moves point left by 1" $
      movePoint (-1, 0) (3, 5) `shouldBe` (2, 5)

    it "moves point down by 1" $
      movePoint (0, -1) (3, 5) `shouldBe` (3, 4)

  describe "isOverlapping" $ do
    it "detects overlap when masks share bits" $
      isOverlapping 0b1100 0b0110 `shouldBe` True

    it "detects no overlap when masks are disjoint" $
      isOverlapping 0b1100 0b0011 `shouldBe` False

    it "detects overlap when masks are identical" $
      isOverlapping 0b1010 0b1010 `shouldBe` True

    it "zero mask doesn't overlap with anything" $
      isOverlapping 0 0b1111 `shouldBe` False

  describe "bitwiseSubtract" $ do
    it "removes bits present in second mask" $
      bitwiseSubtract 0b1111 0b0110 `shouldBe` 0b1001

    it "doesn't affect bits not in second mask" $
      bitwiseSubtract 0b1010 0b0001 `shouldBe` 0b1010

    it "subtracting zero doesn't change mask" $
      bitwiseSubtract 0b1111 0 `shouldBe` 0b1111

  describe "bitwiseAnd" $ do
    it "keeps only shared bits" $
      bitwiseAnd 0b1100 0b1010 `shouldBe` 0b1000

    it "zero with anything gives zero" $
      bitwiseAnd 0 0b1111 `shouldBe` 0

  describe "bitwiseOr" $ do
    it "combines all bits" $
      bitwiseOr 0b1100 0b0011 `shouldBe` 0b1111

    it "is commutative" $
      bitwiseOr 0b1010 0b0101 `shouldBe` bitwiseOr 0b0101 0b1010

  describe "bitwiseXor" $ do
    it "toggles bits" $
      bitwiseXor 0b1100 0b1010 `shouldBe` 0b0110

    it "xor with self gives zero" $
      bitwiseXor 0b1111 0b1111 `shouldBe` 0

    it "xor with zero is identity" $
      bitwiseXor 0b1010 0 `shouldBe` 0b1010

  describe "msbIndex" $ do
    it "finds MSB of 1 (bit 0)" $
      msbIndex 1 `shouldBe` 0

    it "finds MSB of 2 (bit 1)" $
      msbIndex 2 `shouldBe` 1

    it "finds MSB of 4 (bit 2)" $
      msbIndex 4 `shouldBe` 2

    it "finds MSB of 255 (bit 7)" $
      msbIndex 255 `shouldBe` 7

    it "finds MSB of 256 (bit 8)" $
      msbIndex 256 `shouldBe` 8

  describe "msbPoint" $ do
    it "finds MSB point in 10-wide grid for mask 1" $
      msbPoint 10 1 `shouldBe` (0, 0)

    it "finds MSB point in 10-wide grid for mask with bit 23" $
      msbPoint 10 (1 `shift` 23) `shouldBe` (3, 2)

  describe "changeMaskWidthBy (expansion)" $ do
    it "expands single bit mask width from 3 to 5" $
      let oldWidth = 3
          delta = 2
          mask = 0b001  -- Bit at position 0
          expected = 0b001  -- Same position in wider grid
      in changeMaskWidthBy oldWidth delta mask `shouldBe` expected

    it "expands two-row mask width from 3 to 5" $
      let oldWidth = 3
          delta = 2
          mask = 0b1000001  -- Bits at (0,0) and (0,2)
          -- In 3-wide: bit 0 is (0,0), bit 6 is (0,2)
          -- In 5-wide: bit 0 is (0,0), bit 10 is (0,2)
          expected = 1 + (1 `shift` 10)
      in changeMaskWidthBy oldWidth delta mask `shouldBe` expected

  describe "changeMaskWidthBy (reduction)" $ do
    it "reduces mask width from 5 to 3" $
      let oldWidth = 5
          delta = -2
          mask = 0b00001  -- Bit at (0,0)
          expected = 0b001  -- Same position in narrower grid
      in changeMaskWidthBy oldWidth delta mask `shouldBe` expected

  describe "setMaskWidth" $ do
    it "changes width from 3 to 5" $
      let oldWidth = 3
          newWidth = 5
          mask = 1
      in setMaskWidth oldWidth newWidth mask `shouldBe` changeMaskWidthBy oldWidth (newWidth - oldWidth) mask
