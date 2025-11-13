# ascii-world Library Usage Guide

## Table of Contents
1. [Overview](#overview)
2. [Core Concepts](#core-concepts)
3. [Quick Start](#quick-start)
4. [Module Guide](#module-guide)
5. [Common Patterns](#common-patterns)
6. [Performance Tips](#performance-tips)
7. [Examples](#examples)

## Overview

The `ascii-world` library provides efficient tools for working with ASCII grid-based puzzles, particularly Advent of Code problems. It uses clever bitwise operations on `Integer` types to represent entire grid layers, enabling fast set operations and transformations.

### Key Features
- **Efficient grid representation** using bitwise operations
- **Connected component analysis** via flood-fill algorithms
- **Region detection and partitioning**
- **Edge and perimeter counting** for grid regions
- **Type-safe key management** with parameterized types

## Core Concepts

### 1. Masks (Bit Layers)

A **Mask** is an `Integer` where each bit represents a cell in a 2D grid:

```haskell
type Mask = Integer
type Point = (Int, Int)  -- (x, y)

-- Example: 3x3 grid with 4 cells marked
-- Grid:     Bits:
-- . X .     0 1 0
-- X . X  =  1 0 1
-- . X .     0 1 0
--
-- As a mask: bits 1, 3, 5, 7 are set
-- Integer value: 0b10101010 = 170
```

### 2. AsciiWorld

An **AsciiWorld** contains:
- **Masks**: Named layers (e.g., "walls", "player", "enemies")
- **Points**: Collections of individual point coordinates
- **Width**: Grid width (needed for index calculations)

```haskell
data AsciiWorld mk pk = AsciiWorld
    { asciiWorldMasks  :: M.Map mk Mask
    , asciiWorldPoints :: M.Map pk [Point]
    , asciiWorldWidth  :: Int
    }
```

### 3. WalkableWorld

A **WalkableWorld** wraps `AsciiWorld` with:
- Internal tracking masks for algorithms
- Height information
- Helper functions for pathfinding and region analysis

```haskell
data WalkableWorld mk pk = WalkableWorld
    { wwHeight :: Int
    , wwRawAsciiWorld :: RawAsciiWorld mk pk
    }
```

## Quick Start

### Installation

Add to your stack project or cabal file:

```yaml
dependencies:
  - base >= 4.12
  - containers >= 0.6.7
  - split >= 0.2.3.5
  - safe >= 0.3.19
  - QuickCheck >= 2.14.3  # optional, for testing
```

### Hello World Example

```haskell
import AsciiWorld
import qualified Data.Map as M

-- Define how to read characters
charMap :: Char -> Maybe (MaskOrPointsIndex String String)
charMap '#' = Just (MaskIndex "walls")
charMap '@' = Just (PointsIndex "player")
charMap _   = Nothing

main = do
    let input = unlines [ "###"
                        , "#@#"
                        , "###" ]
    let (height, world) = readAsciiWorld charMap input

    -- Check if walls exist
    print $ M.member "walls" (asciiWorldMasks world)  -- True

    -- Get player position
    print $ M.lookup "player" (asciiWorldPoints world)  -- Just [(1,1)]
```

## Module Guide

### Mask.hs - Low-Level Bitwise Operations

```haskell
import Mask

-- Point-Index conversion
pointToIndex :: Int -> Point -> Int
indexToPoint :: Int -> Int -> Point

-- Create masks
pointToMask :: Int -> Point -> Mask  -- Single bit at point
moveMask :: Int -> (Int,Int) -> Mask -> Mask  -- Shift mask by vector

-- Bitwise operations
bitwiseAnd, bitwiseOr, bitwiseXor :: Mask -> Mask -> Mask
bitwiseSubtract :: Mask -> Mask -> Mask  -- Remove bits
isOverlapping :: Mask -> Mask -> Bool    -- Check if masks share bits

-- Find positions
msbPoint :: Int -> Mask -> Point      -- Most significant bit position
middlePoint :: Int -> Mask -> Point   -- Approximate middle position
```

### AsciiWorld.hs - Grid Management

```haskell
import AsciiWorld

-- Create and read worlds
emptyAsciiWorld :: Int -> AsciiWorld mk pk
readAsciiWorld :: (Char -> Maybe (MaskOrPointsIndex mk pk)) -> String -> (Int, AsciiWorld mk pk)
showAsciiWorld :: Int -> Char -> (mk -> Char) -> (pk -> Char) -> ... -> AsciiWorld mk pk -> String

-- Manipulate masks
addMask :: mk -> Mask -> AsciiWorld mk pk -> AsciiWorld mk pk
deleteMask :: mk -> AsciiWorld mk pk -> AsciiWorld mk pk
lookupMask :: mk -> AsciiWorld mk pk -> Maybe Mask
moveMaskOfIndexBy :: mk -> (Int,Int) -> AsciiWorld mk pk -> AsciiWorld mk pk

-- Mask operations
applyMask :: (Mask -> Mask -> Mask) -> mk -> mk -> AsciiWorld mk pk -> AsciiWorld mk pk
copyMask :: mk -> mk -> AsciiWorld mk pk -> AsciiWorld mk pk

-- Points
setPoint :: pk -> Point -> AsciiWorld mk pk -> AsciiWorld mk pk
lookupPoints :: pk -> AsciiWorld mk pk -> Maybe [Point]
```

### WalkableWorld.hs - High-Level Algorithms

```haskell
import WalkableWorld

-- Create from string
readWorld :: (Char -> Maybe (MaskOrPointsIndex mk pk)) -> String -> WalkableWorld mk pk

-- Region analysis
partitionMaskByReachableLRDU :: mk -> WalkableWorld mk pk -> [Mask]
partitionAllMasksByReachableLRDU :: WalkableWorld mk pk -> M.Map mk [Mask]

-- Measurements
totalPoints :: mk -> WalkableWorld mk pk -> Integer
totalEdgesOverPoints :: mk -> WalkableWorld mk pk -> Integer
totalConnectedEdges :: mk -> WalkableWorld mk pk -> Integer
totalConnectedOneSidedEdges :: mk -> WalkableWorld mk pk -> Integer  -- Counts "sides" not individual edges

-- Get all mask keys
maskIndices :: WalkableWorld mk pk -> [mk]
```

## Common Patterns

### Pattern 1: Region Detection

**Problem**: Find all connected components in a grid.

```haskell
import WalkableWorld
import qualified Data.Map as M

findRegions :: String -> IO ()
findRegions input = do
    let world = readWorld charMap input
        regions = partitionAllMasksByReachableLRDU world

    -- regions :: M.Map Char [Mask]
    -- Each Mask is a connected component
    mapM_ print $ M.toList regions
```

### Pattern 2: Counting Perimeters

**Problem**: Count the perimeter of each region (Advent of Code Day 12 Part 1).

```haskell
import WalkableWorld

calculateScore :: WalkableWorld Char Char -> Integer
calculateScore world =
    let regions = maskIndices world
        scores = [ totalPoints r world * totalEdgesOverPoints r world
                 | r <- regions ]
    in sum scores
```

### Pattern 3: Counting Sides (Not Edges)

**Problem**: Count distinct sides of regions (Advent of Code Day 12 Part 2).

```haskell
calculateAdvancedScore :: WalkableWorld Char Char -> Integer
calculateAdvancedScore world =
    let regions = maskIndices world
        scores = [ totalPoints r world * totalConnectedOneSidedEdges r world
                 | r <- regions ]
    in sum scores
```

### Pattern 4: Custom Pathfinding

**Problem**: Find paths with specific constraints (Advent of Code Day 10).

```haskell
import Mask (Point)
import Util (lrduDirs)
import qualified Data.Map as M

-- Use Map for custom pathfinding
findPaths :: M.Map Point Int -> Point -> Int
findPaths heightMap start
    | M.lookup start heightMap == Just 9 = 1
    | otherwise = sum [ findPaths heightMap neighbor
                      | neighbor <- getValidNeighbors heightMap start ]

getValidNeighbors :: M.Map Point Int -> Point -> [Point]
getValidNeighbors hMap (x,y) =
    case M.lookup (x,y) hMap of
        Nothing -> []
        Just h -> [ (x+dx, y+dy)
                  | (dx,dy) <- lrduDirs
                  , M.lookup (x+dx, y+dy) hMap == Just (h+1) ]
```

### Pattern 5: Mask Transformations

**Problem**: Apply transformations to grid regions.

```haskell
import AsciiWorld

-- Move all enemies right by 1
moveEnemies :: AsciiWorld String String -> AsciiWorld String String
moveEnemies = moveMaskOfIndexBy "enemies" (1, 0)

-- Combine two layers
addPlayerToWalls :: AsciiWorld String String -> AsciiWorld String String
addPlayerToWalls world =
    world & copyMask "player" "temp"
          & applyMask bitwiseOr "temp" "walls"
          & deleteMask "temp"
```

## Performance Tips

### 1. Use Masks for Dense Regions
**Good**: When most cells in a region are active
**Avoid**: When only a few scattered cells are active

### 2. Prefer Bitwise Operations
```haskell
-- FAST: Bitwise operations are O(1)
combinedMask = bitwiseOr mask1 mask2

-- SLOW: List operations are O(n)
combinedPoints = points1 ++ points2
```

### 3. Minimize Width Changes
```haskell
-- SLOW: Changing width requires bit-shifting entire mask
newWorld = setWidth newWidth world

-- BETTER: Set width once at creation
world = emptyAsciiWorld correctWidth
```

### 4. Cache Mask Lookups
```haskell
-- SLOW: Multiple lookups
score = totalPoints "A" world + totalEdgesOverPoints "A" world

-- BETTER: Store mask index
let a = "A"
score = totalPoints a world + totalEdgesOverPoints a world
```

## Examples

See the `test/` directory for complete working examples:

- **`test/day12.hs`** - Region detection, perimeter counting, side counting
- **`test/day10.hs`** - Pathfinding with height constraints
- **`test/Spec.hs`** - Unit tests showing basic usage
- **`test/MaskSpec.hs`** - Comprehensive Mask module tests
- **`test/PropertyTests.hs`** - QuickCheck property-based tests

### Running Examples

```bash
cd test

# Run Day 12 solution
stack --resolver lts-21.22 runghc --package containers --package split -- day12.hs

# Run tests
stack test
```

## Advanced Topics

### Custom Key Types

You can use any `Ord` type as keys:

```haskell
data GameEntity = Player | Enemy Int | Wall
    deriving (Show, Eq, Ord)

type GameWorld = AsciiWorld GameEntity GameEntity

charMap :: Char -> Maybe (MaskOrPointsIndex GameEntity GameEntity)
charMap '@' = Just (PointsIndex Player)
charMap 'E' = Just (MaskIndex (Enemy 1))
charMap '#' = Just (MaskIndex Wall)
charMap _   = Nothing
```

### Error Handling

The library uses informative error messages:

```haskell
-- Before our fixes:
-- Error: Maybe.fromJust: Nothing

-- After our fixes:
-- Error: totalPoints: mask key not found: External 'A'
```

Always check if keys exist before operations:

```haskell
case lookupMask "player" world of
    Just mask -> process mask
    Nothing -> error "Player not found"
```

## FAQ

**Q: When should I use Masks vs Points?**
A: Use Masks for dense regions or when you need set operations. Use Points for sparse individual positions or when order matters.

**Q: How do I debug mask values?**
A: Use `popCount` to count bits, or convert to binary: `showIntAtBase 2 intToDigit mask ""`

**Q: Why is my mask width important?**
A: The width determines how bits map to 2D coordinates. Wrong width = wrong positions.

**Q: Can I have multiple types of masks?**
A: Yes! Use different key types: `AsciiWorld Char String` has Char mask keys and String point keys.

## Contributing

Found a bug or have a feature request? Please open an issue on the repository!

