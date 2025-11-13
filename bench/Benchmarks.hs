{-# LANGUAGE BangPatterns #-}

{-|
Performance benchmarks for ascii-world library

This benchmark suite validates the claim that bitwise operations provide
"10-100x speedup" over naive implementations.

Run with: stack bench
-}

module Main where

import Criterion.Main
import qualified Data.Set as S
import Data.Bits
import Mask

-----------------
-- Naive Implementations (for comparison)
-----------------

-- | Naive set-based union (list of points)
naiveUnion :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
naiveUnion ps1 ps2 = S.toList $ S.union (S.fromList ps1) (S.fromList ps2)

-- | Naive set-based intersection
naiveIntersection :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
naiveIntersection ps1 ps2 = S.toList $ S.intersection (S.fromList ps1) (S.fromList ps2)

-- | Naive overlap check
naiveIsOverlapping :: [(Int, Int)] -> [(Int, Int)] -> Bool
naiveIsOverlapping ps1 ps2 = not $ null $ S.intersection (S.fromList ps1) (S.fromList ps2)

-- | Naive set difference
naiveDifference :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
naiveDifference ps1 ps2 = S.toList $ S.difference (S.fromList ps1) (S.fromList ps2)

-- | Naive move (add offset to each point)
naiveMove :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
naiveMove (dx, dy) = map (\(x, y) -> (x + dx, y + dy))

-----------------
-- Test Data Generation
-----------------

-- | Generate a list of points in a grid
generatePoints :: Int -> Int -> Int -> [(Int, Int)]
generatePoints width height density =
    [(x, y) | y <- [0..height-1], x <- [0..width-1], (x + y) `mod` density == 0]

-- | Convert list of points to mask
pointsToMask :: Int -> [(Int, Int)] -> Mask
pointsToMask width = foldr (\p acc -> bitwiseOr acc (pointToMask width p)) 0

-----------------
-- Benchmark Data
-----------------

-- Small grid (10x10, ~25 points)
smallWidth :: Int
smallWidth = 10

smallPoints1 :: [(Int, Int)]
smallPoints1 = generatePoints smallWidth 10 2

smallPoints2 :: [(Int, Int)]
smallPoints2 = generatePoints smallWidth 10 3

smallMask1 :: Mask
smallMask1 = pointsToMask smallWidth smallPoints1

smallMask2 :: Mask
smallMask2 = pointsToMask smallWidth smallPoints2

-- Medium grid (50x50, ~625 points)
mediumWidth :: Int
mediumWidth = 50

mediumPoints1 :: [(Int, Int)]
mediumPoints1 = generatePoints mediumWidth 50 2

mediumPoints2 :: [(Int, Int)]
mediumPoints2 = generatePoints mediumWidth 50 3

mediumMask1 :: Mask
mediumMask1 = pointsToMask mediumWidth mediumPoints1

mediumMask2 :: Mask
mediumMask2 = pointsToMask mediumWidth mediumPoints2

-- Large grid (200x200, ~10000 points)
largeWidth :: Int
largeWidth = 200

largePoints1 :: [(Int, Int)]
largePoints1 = generatePoints largeWidth 200 2

largePoints2 :: [(Int, Int)]
largePoints2 = generatePoints largeWidth 200 3

largeMask1 :: Mask
largeMask1 = pointsToMask largeWidth largePoints1

largeMask2 :: Mask
largeMask2 = pointsToMask largeWidth largePoints2

-----------------
-- Benchmarks
-----------------

main :: IO ()
main = defaultMain
  [ bgroup "union"
      [ bgroup "small (10x10, ~25 points)"
          [ bench "bitwise" $ whnf (bitwiseOr smallMask1) smallMask2
          , bench "naive"   $ nf (naiveUnion smallPoints1) smallPoints2
          ]
      , bgroup "medium (50x50, ~625 points)"
          [ bench "bitwise" $ whnf (bitwiseOr mediumMask1) mediumMask2
          , bench "naive"   $ nf (naiveUnion mediumPoints1) mediumPoints2
          ]
      , bgroup "large (200x200, ~10k points)"
          [ bench "bitwise" $ whnf (bitwiseOr largeMask1) largeMask2
          , bench "naive"   $ nf (naiveUnion largePoints1) largePoints2
          ]
      ]

  , bgroup "intersection"
      [ bgroup "small (10x10, ~25 points)"
          [ bench "bitwise" $ whnf (bitwiseAnd smallMask1) smallMask2
          , bench "naive"   $ nf (naiveIntersection smallPoints1) smallPoints2
          ]
      , bgroup "medium (50x50, ~625 points)"
          [ bench "bitwise" $ whnf (bitwiseAnd mediumMask1) mediumMask2
          , bench "naive"   $ nf (naiveIntersection mediumPoints1) mediumPoints2
          ]
      , bgroup "large (200x200, ~10k points)"
          [ bench "bitwise" $ whnf (bitwiseAnd largeMask1) largeMask2
          , bench "naive"   $ nf (naiveIntersection largePoints1) largePoints2
          ]
      ]

  , bgroup "overlap-check"
      [ bgroup "small (10x10, ~25 points)"
          [ bench "bitwise" $ whnf (isOverlapping smallMask1) smallMask2
          , bench "naive"   $ nf (naiveIsOverlapping smallPoints1) smallPoints2
          ]
      , bgroup "medium (50x50, ~625 points)"
          [ bench "bitwise" $ whnf (isOverlapping mediumMask1) mediumMask2
          , bench "naive"   $ nf (naiveIsOverlapping mediumPoints1) mediumPoints2
          ]
      , bgroup "large (200x200, ~10k points)"
          [ bench "bitwise" $ whnf (isOverlapping largeMask1) largeMask2
          , bench "naive"   $ nf (naiveIsOverlapping largePoints1) largePoints2
          ]
      ]

  , bgroup "difference"
      [ bgroup "small (10x10, ~25 points)"
          [ bench "bitwise" $ whnf (bitwiseSubtract smallMask1) smallMask2
          , bench "naive"   $ nf (naiveDifference smallPoints1) smallPoints2
          ]
      , bgroup "medium (50x50, ~625 points)"
          [ bench "bitwise" $ whnf (bitwiseSubtract mediumMask1) mediumMask2
          , bench "naive"   $ nf (naiveDifference mediumPoints1) mediumPoints2
          ]
      , bgroup "large (200x200, ~10k points)"
          [ bench "bitwise" $ whnf (bitwiseSubtract largeMask1) largeMask2
          , bench "naive"   $ nf (naiveDifference largePoints1) largePoints2
          ]
      ]

  , bgroup "movement"
      [ bgroup "small (10x10, ~25 points)"
          [ bench "bitwise" $ whnf (moveMask smallWidth (5, 5)) smallMask1
          , bench "naive"   $ nf (naiveMove (5, 5)) smallPoints1
          ]
      , bgroup "medium (50x50, ~625 points)"
          [ bench "bitwise" $ whnf (moveMask mediumWidth (10, 10)) mediumMask1
          , bench "naive"   $ nf (naiveMove (10, 10)) mediumPoints1
          ]
      , bgroup "large (200x200, ~10k points)"
          [ bench "bitwise" $ whnf (moveMask largeWidth (50, 50)) largeMask1
          , bench "naive"   $ nf (naiveMove (50, 50)) largePoints1
          ]
      ]
  ]

{-
Expected Results:
-----------------

Based on the implementation, bitwise operations should show:

1. **Union (bitwiseOr)**:
   - Small:  ~10-50x faster
   - Medium: ~50-200x faster
   - Large:  ~100-500x faster

2. **Intersection (bitwiseAnd)**:
   - Small:  ~10-50x faster
   - Medium: ~50-200x faster
   - Large:  ~100-500x faster

3. **Overlap Check (isOverlapping)**:
   - Small:  ~20-100x faster (early termination benefit)
   - Medium: ~100-500x faster
   - Large:  ~500-1000x faster

4. **Difference (bitwiseSubtract)**:
   - Small:  ~10-50x faster
   - Medium: ~50-200x faster
   - Large:  ~100-500x faster

5. **Movement (moveMask)**:
   - Small:  ~5-20x faster
   - Medium: ~20-100x faster
   - Large:  ~50-200x faster

The speedup increases with grid size because:
- Bitwise ops are O(1) regardless of density
- Naive ops are O(n) where n is number of points
- Set operations have O(n log n) complexity

Real-world AoC grids (typically 50-150x50-150) fall in the medium-large range,
giving the claimed "10-100x speedup" in practice.
-}
