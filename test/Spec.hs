{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import qualified Data.Map as M
import AsciiWorld

-- Import test modules
import qualified MaskSpec
import qualified PropertyTests
import qualified WalkableWorldSpec

-- Reuse a similar charMap as in Main.hs for consistency.
charMap :: Char -> Maybe (MaskOrPointsIndex String String)
charMap 'M' = Just (MaskIndex "M")
charMap 'P' = Just (PointsIndex "P")
charMap _   = Nothing

main :: IO ()
main = hspec $ do
  -- Unit tests for Mask module
  describe "Mask" MaskSpec.spec

  -- Property-based tests
  describe "Properties" PropertyTests.spec

  -- Integration tests for WalkableWorld
  describe "WalkableWorld" WalkableWorldSpec.spec

  -- AsciiWorld tests
  describe "readAsciiWorld" $ do
    it "reads an ASCII world with a mask and a point" $ do
      let input = unlines [ "M.."
                          , ".P."
                          , "..M" ]
      let (height, world) = readAsciiWorld charMap input

      -- Check that the height is as expected (3 rows).
      height `shouldBe` 3

      -- Check that the mask "M" was registered.
      let masks = asciiWorldMasks world
      M.member "M" masks `shouldBe` True

      -- Check that the point "P" was registered.
      let points = asciiWorldPoints world
      M.member "P" points `shouldBe` True

  describe "setPoint" $ do
    it "sets a point at the given coordinates" $ do
      let world = emptyAsciiWorld 3 :: AsciiWorld String String
      let newWorld = setPoint "A" (1, 1) world
      let pts = asciiWorldPoints newWorld
      M.lookup "A" pts `shouldBe` Just [(1,1)]
