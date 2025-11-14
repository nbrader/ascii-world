{-# LANGUAGE ScopedTypeVariables #-}

module PropertyTests (spec) where

import Test.Hspec
import Test.QuickCheck hiding ((.&.))
import Data.Bits
import Mask

-- Generators for valid test data
newtype ValidWidth = ValidWidth Int deriving Show
newtype ValidPoint = ValidPoint (Int, Int) deriving Show
newtype ValidMask = ValidMask Integer deriving Show

instance Arbitrary ValidWidth where
  arbitrary = ValidWidth <$> choose (1, 20)

instance Arbitrary ValidPoint where
  arbitrary = do
    x <- choose (0, 19)
    y <- choose (0, 19)
    return $ ValidPoint (x, y)

instance Arbitrary ValidMask where
  arbitrary = ValidMask . abs <$> arbitrary

spec :: Spec
spec = do
  describe "Point-Index conversion properties" $ do
    it "pointToIndex and indexToPoint are inverses (index -> point -> index)" $
      property $ \(ValidWidth width) ->
        width > 0 ==>
        forAll (choose (0, width * 10 - 1)) $ \index ->
          let point = indexToPoint width index
          in pointToIndex width point == index

    it "pointToIndex and indexToPoint are inverses (point -> index -> point)" $
      property $ \(ValidWidth width) ->
        width > 0 ==>
        forAll (choose (0, 9)) $ \x ->
        forAll (choose (0, 9)) $ \y ->
          x < width ==>
          let index = pointToIndex width (x, y)
              point' = indexToPoint width index
          in point' == (x, y)

  describe "Move operations preserve structure" $ do
    it "moving point by (0,0) is identity" $
      property $ \(ValidPoint point) ->
        movePoint (0, 0) point == point

    it "moving mask by (0,0) is identity" $
      property $ \(ValidMask mask) (ValidWidth width) ->
        width > 0 ==>
        moveMask width (0, 0) mask == mask

    it "movePoint is associative" $
      property $ \(ValidPoint point) ->
        forAll (choose (-10, 10)) $ \dx1 ->
        forAll (choose (-10, 10)) $ \dy1 ->
        forAll (choose (-10, 10)) $ \dx2 ->
        forAll (choose (-10, 10)) $ \dy2 ->
          let move1 = movePoint (dx1 + dx2, dy1 + dy2) point
              move2 = movePoint (dx2, dy2) (movePoint (dx1, dy1) point)
          in move1 == move2

  describe "Bitwise operations are commutative" $ do
    it "bitwiseAnd is commutative" $
      property $ \(ValidMask m1) (ValidMask m2) ->
        bitwiseAnd m1 m2 == bitwiseAnd m2 m1

    it "bitwiseOr is commutative" $
      property $ \(ValidMask m1) (ValidMask m2) ->
        bitwiseOr m1 m2 == bitwiseOr m2 m1

    it "bitwiseXor is commutative" $
      property $ \(ValidMask m1) (ValidMask m2) ->
        bitwiseXor m1 m2 == bitwiseXor m2 m1

  describe "Bitwise operations with identity elements" $ do
    it "bitwiseOr with 0 is identity" $
      property $ \(ValidMask mask) ->
        bitwiseOr mask 0 == mask && bitwiseOr 0 mask == mask

    it "bitwiseAnd with all-bits is identity (for non-negative)" $
      property $ \(ValidMask mask) ->
        mask >= 0 ==>
        let allBits = (1 `shift` (popCount mask + 10)) - 1
        in bitwiseAnd mask allBits == mask

    it "bitwiseXor with 0 is identity" $
      property $ \(ValidMask mask) ->
        bitwiseXor mask 0 == mask && bitwiseXor 0 mask == mask

  describe "Bitwise XOR properties" $ do
    it "xor with self gives zero" $
      property $ \(ValidMask mask) ->
        bitwiseXor mask mask == 0

    it "xor is its own inverse" $
      property $ \(ValidMask m1) (ValidMask m2) ->
        bitwiseXor (bitwiseXor m1 m2) m2 == m1

  describe "Overlap detection properties" $ do
    it "mask always overlaps with itself if non-zero" $
      property $ \(ValidMask mask) ->
        mask /= 0 ==> isOverlapping mask mask

    it "zero mask never overlaps with anything" $
      property $ \(ValidMask mask) ->
        not (isOverlapping 0 mask) && not (isOverlapping mask 0)

    it "isOverlapping is commutative" $
      property $ \(ValidMask m1) (ValidMask m2) ->
        isOverlapping m1 m2 == isOverlapping m2 m1

    it "if masks overlap, their AND is non-zero" $
      property $ \(ValidMask m1) (ValidMask m2) ->
        isOverlapping m1 m2 ==> bitwiseAnd m1 m2 /= 0

    it "if AND is zero, masks don't overlap" $
      property $ \(ValidMask m1) (ValidMask m2) ->
        bitwiseAnd m1 m2 == 0 ==> not (isOverlapping m1 m2)

  describe "bitwiseSubtract properties" $ do
    it "subtracting zero doesn't change mask" $
      property $ \(ValidMask mask) ->
        bitwiseSubtract mask 0 == mask

    it "subtracting all gives zero" $
      property $ \(ValidMask mask) ->
        bitwiseSubtract mask mask == 0

    it "result doesn't overlap with subtracted mask" $
      property $ \(ValidMask m1) (ValidMask m2) ->
        not (isOverlapping (bitwiseSubtract m1 m2) m2)

    it "subtracting twice is same as subtracting OR" $
      property $ \(ValidMask m1) (ValidMask m2) (ValidMask m3) ->
        bitwiseSubtract (bitwiseSubtract m1 m2) m3 ==
        bitwiseSubtract m1 (bitwiseOr m2 m3)

  describe "pointToMask creates single-bit masks" $ do
    it "pointToMask creates mask with exactly 1 bit set" $
      property $ \(ValidWidth width) ->
        width > 0 ==>
        forAll (choose (0, width - 1)) $ \x ->
        forAll (choose (0, 9)) $ \y ->
          popCount (pointToMask width (x, y)) == 1

    it "pointToMask positions match pointToIndex" $
      property $ \(ValidWidth width) ->
        width > 0 ==>
        forAll (choose (0, width - 1)) $ \x ->
        forAll (choose (0, 9)) $ \y ->
          let index = pointToIndex width (x, y)
              mask = pointToMask width (x, y)
          in mask == (1 `shift` index)

  describe "msbIndex and msbPoint properties" $ do
    it "msbIndex of power of 2 is the exponent" $
      property $ forAll (choose (0, 20)) $ \n ->
        msbIndex (2 ^ n) == n

    it "msbPoint recovers the highest set bit position" $
      property $ \(ValidWidth width) ->
        width > 0 ==>
        forAll (choose (0, width - 1)) $ \x ->
        forAll (choose (0, 9)) $ \y ->
          let mask = pointToMask width (x, y)
          in msbPoint width mask == (x, y)

  describe "Width changing preserves point positions" $ do
    it "expanding then reducing width is identity for valid points" $
      property $ \(ValidWidth width) ->
        width > 0 && width < 10 ==>
        forAll (choose (0, width - 1)) $ \x ->
        forAll (choose (0, 5)) $ \y ->
        forAll (choose (1, 5)) $ \delta ->
          let mask = pointToMask width (x, y)
              expanded = changeMaskWidthBy width delta mask
              reduced = changeMaskWidthBy (width + delta) (-delta) expanded
          in reduced == mask

  describe "Set theory properties" $ do
    it "OR is union: A union B contains all bits from A and B" $
      property $ \(ValidMask m1) (ValidMask m2) ->
        let union = bitwiseOr m1 m2
        in (bitwiseAnd m1 union == m1) && (bitwiseAnd m2 union == m2)

    it "AND is intersection: A intersect B is subset of both A and B" $
      property $ \(ValidMask m1) (ValidMask m2) ->
        let intersection = bitwiseAnd m1 m2
        in (bitwiseAnd intersection m1 == intersection) &&
           (bitwiseAnd intersection m2 == intersection)

    it "De Morgan's law: NOT(A AND B) = (NOT A) OR (NOT B)" $
      property $ \(ValidMask m1) (ValidMask m2) ->
        m1 >= 0 && m2 >= 0 ==>
        let maxBits = max (if m1 == 0 then 0 else msbIndex m1 + 1)
                          (if m2 == 0 then 0 else msbIndex m2 + 1) + 1
            allBits = (1 `shift` maxBits) - 1
            notAnd = complement (bitwiseAnd m1 m2) .&. allBits
            orNot = bitwiseOr (complement m1 .&. allBits) (complement m2 .&. allBits)
        in notAnd == orNot
