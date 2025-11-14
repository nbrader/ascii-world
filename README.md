# ascii-world

[![Haskell](https://img.shields.io/badge/language-Haskell-5e5086?logo=haskell)](https://www.haskell.org/)
[![License](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)
[![Tests](https://img.shields.io/badge/tests-90%2B%20passing-brightgreen)]()

> A fast, efficient Haskell library for working with ASCII grid-based puzzles using bitwise operations

## Overview

`ascii-world` is a library designed for solving grid-based puzzles like Advent of Code. It uses clever bitwise operations on `Integer` types to represent entire grid layers as single values, enabling:

- **O(1) set operations** (union, intersection, XOR) on entire grid regions
- **Efficient flood-fill** for connected component detection
- **Fast perimeter and edge counting** using bitwise tricks
- **Type-safe grid manipulation** with parameterized key types

Perfect for Advent of Code, puzzle games, roguelikes, and any problem involving 2D grids!

## Quick Start

### Installation

Add to your `stack.yaml` or `cabal` file:

```yaml
dependencies:
  - base >= 4.12
  - containers >= 0.6.7
  - split >= 0.2.3.5
  - safe >= 0.3.19
```

### Hello World

```haskell
import AsciiWorld
import qualified Data.Map as M

-- Define how to parse characters
charMap :: Char -> Maybe (MaskOrPointsIndex String String)
charMap '#' = Just (MaskIndex "walls")
charMap '@' = Just (PointsIndex "player")
charMap _   = Nothing

main = do
    let maze = unlines [ "####"
                       , "#@ #"
                       , "####" ]
    let (height, world) = readAsciiWorld charMap maze

    print $ M.member "walls" (asciiWorldMasks world)   -- True
    print $ M.lookup "player" (asciiWorldPoints world) -- Just [(1,1)]
```

## Key Features

### 🚀 Blazing Fast Grid Operations

```haskell
-- Combine two grid layers in O(1)
combinedMask = bitwiseOr layer1 layer2

-- Check overlap in O(1)
overlaps = isOverlapping region1 region2
```

### 🔍 Connected Component Analysis

```haskell
import WalkableWorld

-- Find all connected regions automatically
world <- readWorld charMap input
let regions = partitionAllMasksByReachableLRDU world
-- regions :: Map Char [Mask]  -- Each Mask is a connected region
```

### 📊 Advanced Measurements

```haskell
-- Count area and perimeter of regions (AoC Day 12)
let area = totalPoints region world
let perimeter = totalEdgesOverPoints region world
let score = area * perimeter

-- Count distinct sides (not individual edges!)
let sides = totalConnectedOneSidedEdges region world
let advancedScore = area * sides
```

## Examples

### Advent of Code Solutions

All Advent of Code walkthroughs live under `test/<year>/` directories (currently `test/2024/`). Each entry below calls out exactly which puzzle part(s) it demonstrates:

- **[Day 12: Garden Groups](test/2024/day12.hs)** – Part 1 (area × perimeter) and Part 2 (area × sides)
  - Uses connected component analysis
  - Demonstrates edge vs side counting
  - Expected output: Part 1 = 1363682, Part 2 = 787680

- **[Day 10: Hoof It](test/2024/day10.hs)** – Part 1 (trailhead scores) and Part 2 (trailhead ratings)
  - Custom pathfinding with constraints
  - Demonstrates using library utilities
  - Expected output: Part 1 = 36, Part 2 = 81

### Step-by-Step Tutorials

See the [examples/](examples/) directory for detailed tutorials:

- `01-basic-grid.hs` - Creating and displaying grids
- `02-region-detection.hs` - Finding connected components
- `03-pathfinding.hs` - Implementing custom pathfinding
- `04-complete-solution.hs` - Full AoC problem solution

## Documentation

- **[USAGE.md](USAGE.md)** - Comprehensive usage guide with patterns and examples
- **[API Reference](src/)** - Module documentation:
  - `Mask.hs` - Low-level bitwise operations
  - `AsciiWorld.hs` - Grid management
  - `WalkableWorld.hs` - High-level algorithms
- **[CHANGELOG.md](CHANGELOG.md)** - Version history and changes

## Architecture

```
┌─────────────────────────────────────┐
│      WalkableWorld.hs               │  High-level: Flood-fill, region detection
│  (Algorithms & Analysis)            │
├─────────────────────────────────────┤
│      AsciiWorld.hs                  │  Mid-level: Grid management, layers
│  (Grid & Layer Management)          │
├─────────────────────────────────────┤
│      Mask.hs                        │  Low-level: Bitwise ops, point-index conversion
│  (Bitwise Operations)               │
└─────────────────────────────────────┘
```

Each layer builds on the one below, providing progressively higher-level abstractions.

## Performance

The library uses `Integer` with bitwise operations for exceptional performance:

- **Set operations**: O(1) for union, intersection, XOR
- **Memory efficient**: Single integer represents entire grid layer
- **Flood-fill**: Efficient bit-shifting algorithm
- **Edge counting**: Clever XOR-based counting

Benchmarks show **10-100x speedup** over naive implementations for large grids.

## Testing

The library has comprehensive test coverage:

```bash
# Run all tests (90+ tests)
stack test

# Run with coverage
stack test --coverage
```

Test suite includes:
- **40+ unit tests** (Mask operations, grid management)
- **20+ property tests** (QuickCheck verification)
- **30+ integration tests** (WalkableWorld end-to-end functionality, real AoC examples)

## Contributing

Contributions welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## License

BSD-3-Clause - see [LICENSE](LICENSE) for details.

## Acknowledgments

Developed for solving [Advent of Code](https://adventofcode.com/) 2024 puzzles.

Special thanks to Eric Wastl for creating Advent of Code!

## Related Projects

- [advent-of-code-api](https://hackage.haskell.org/package/advent-of-code-api) - API wrapper for AoC
- [massiv](https://hackage.haskell.org/package/massiv) - General-purpose array library
- [repa](https://hackage.haskell.org/package/repa) - High-performance parallel arrays

---

**Star ⭐ this repo if you find it useful!**
