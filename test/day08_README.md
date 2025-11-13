# Day 8: Resonant Collinearity - Solution

## Problem Description

You're analyzing a map of antennas transmitting at various frequencies. Antennas are marked with letters and digits (0-9, a-z, A-Z), where each character represents a different frequency.

An **antinode** is created when two antennas of the same frequency are perfectly in line with a specific position.

### Part 1: Simple Antinodes
An antinode occurs at any point that is:
- **In line** with two antennas of the same frequency
- Where **one antenna is twice as far** as the other from that point

For two antennas at positions A and B, antinodes occur at:
- Point beyond B at distance `|AB|` from B
- Point before A at distance `|AB|` from A

### Part 2: Harmonic Resonance
With resonant harmonics, antinodes occur at **any grid position** exactly in line with at least two antennas of the same frequency, including:
- The antenna positions themselves
- All integer multiples of the displacement vector

## Example Input

```
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
```

In this 12×12 grid:
- **Frequency '0'**: 4 antennas at various positions
- **Frequency 'A'**: 3 antennas forming a different pattern

### Part 1 Solution
Finding simple antinodes (2:1 ratio only): **14 unique locations**

### Part 2 Solution
Finding all collinear positions (with harmonics): **34 unique locations**

## Mathematical Approach

### Antinode Calculation

For two antennas at positions P₁ = (x₁, y₁) and P₂ = (x₂, y₂):

1. **Calculate displacement vector**:
   ```
   Δx = x₂ - x₁
   Δy = y₂ - y₁
   ```

2. **Part 1 - Simple antinodes**:
   ```
   Antinode₁ = P₁ - (Δx, Δy) = (x₁ - Δx, y₁ - Δy)
   Antinode₂ = P₂ + (Δx, Δy) = (x₂ + Δx, y₂ + Δy)
   ```

3. **Part 2 - All harmonics**:
   ```
   For n = 0, 1, 2, 3, ...
     Forward:  P₁ + n·(Δx, Δy)
     Backward: P₁ - n·(Δx, Δy)

   Continue until out of bounds
   ```

### Example Calculation

Given antennas at (4, 3) and (5, 5):
- Displacement: Δx = 1, Δy = 2
- Part 1 antinodes:
  - (3, 1) = (4, 3) - (1, 2)
  - (6, 7) = (5, 5) + (1, 2)
- Part 2 antinodes:
  - (3, 1), (4, 3), (5, 5), (6, 7), (7, 9), ...
  - (All points while in bounds)

## Implementation Notes

This solution demonstrates several key features of the ascii-world library:

### 1. **Multiple Point Layers**

Each antenna frequency gets its own point layer:

```haskell
charMap c
  | isAlphaNum c = Just (PointsIndex [c])
  | otherwise    = Nothing

let antennaMap = asciiWorldPoints world
-- antennaMap is: Map String [Point]
-- e.g., {"0" -> [(8,1), (5,2), (7,3), (4,4)], "A" -> [(6,5), (8,8), (9,9)]}
```

### 2. **Coordinate Mathematics**

Pure Haskell arithmetic for antinode calculations:

```haskell
let dx = x2 - x1
    dy = y2 - y1
    beyond2 = (x2 + dx, y2 + dy)
    before1 = (x1 - dx, y1 - dy)
```

No need for complex vector libraries - simple tuple arithmetic!

### 3. **Mask Combination with Bitwise OR**

Combine antinodes from all frequencies:

```haskell
let antinodesForFreq1 = pointToMask width (3, 1) `bitwiseOr` pointToMask width (6, 7)
let antinodesForFreq2 = pointToMask width (5, 5) `bitwiseOr` ...
let allAntinodes = antinodesForFreq1 `bitwiseOr` antinodesForFreq2
```

Efficient O(1) operations to combine results from multiple calculations.

### 4. **Lazy Evaluation for Infinite Sequences**

Part 2 generates all collinear points until hitting bounds:

```haskell
let backwards = takeWhile (inBounds width height)
      [(x1 - n*dx, y1 - n*dy) | n <- [0..]]
```

Haskell's laziness means we only compute points until we hit the boundary.

### 5. **Counting Set Bits**

Count unique antinode positions with `popCount`:

```haskell
let countP1 = popCount antinodesP1  -- Counts all set bits
```

O(1) operation to count all unique positions in the mask.

## Key Functions

### `findAntinodes`
Main function that processes all antenna frequencies.

**Parameters**:
- `width`, `height`: Grid dimensions
- `antennaMap`: Map from frequency to antenna positions
- `withHarmonics`: Boolean (False for Part 1, True for Part 2)

**Strategy**:
1. For each frequency, process all antenna pairs
2. For each pair, calculate antinodes
3. Combine all results with bitwise OR

### `findAntinodesForPair`
Calculate antinodes for a specific antenna pair.

**Part 1 Logic**:
```haskell
let beyond2 = (x2 + dx, y2 + dy)  -- Point beyond second antenna
    before1 = (x1 - dx, y1 - dy)  -- Point before first antenna
```

**Part 2 Logic**:
```haskell
let backwards = [(x1 - n*dx, y1 - n*dy) | n <- [0..]]
    forwards  = [(x1 + n*dx, y1 + n*dy) | n <- [0..]]
in takeWhile (inBounds width height) (backwards ++ forwards)
```

## Performance Characteristics

### Time Complexity

**Part 1**:
- For each frequency f with nf antennas:
  - Generate nf² pairs
  - Calculate 2 antinodes per pair: O(nf²)
- Total: O(Σ nf²) where f ranges over all frequencies
- Typical: O(n²) where n is total antenna count

**Part 2**:
- Similar to Part 1, but each pair generates O(max(width, height)) antinodes
- Total: O(Σ nf² · max(width, height))
- Typical: O(n² · grid_dimension)

### Space Complexity

**Mask-based storage**: O(1) per frequency
- Each frequency's antinodes stored in single Integer mask
- Final result is bitwise OR of all masks
- Memory usage independent of number of antinodes!

**Naive approach** would use O(antinode_count) space for Set/List storage.

### Optimizations Used

1. **Bitwise OR for deduplication**: O(1) merge of overlapping antinodes
2. **Lazy evaluation**: Don't compute out-of-bounds points
3. **Single Integer per result**: Memory-efficient storage
4. **Point-to-mask conversion**: O(1) per point

## Usage

```bash
# Run with stack
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package split-0.2.3.5 -- test/day08.hs

# Or compile for better performance
stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 -- test/day08.hs -O2
./day08
```

## Expected Output

```
=== Day 8: Resonant Collinearity ===

Grid: 12x12
Frequencies found: 2
  "0": 4 antennas
  "A": 3 antennas

Part 1: Unique antinode locations: 14
Part 2: Unique antinode locations (with harmonics): 34

Sample antinode positions (Part 1, first 10):
  (6, 0)
  (11, 0)
  (3, 1)
  (10, 1)
  (2, 2)
  (9, 2)
  (6, 3)
  (1, 3)
  (3, 5)
  (10, 5)
```

## Comparison with Other Solutions

### Day 6 (Guard Patrol)
- **Day 6**: Single moving entity with state
- **Day 8**: Multiple static entities with mathematical relationships

### Day 10 (Topographic Pathfinding)
- **Day 10**: Constraint-based pathfinding
- **Day 8**: Geometric collinearity calculations

### Day 12 (Garden Groups)
- **Day 12**: Region detection with flood-fill
- **Day 8**: Point-wise calculations with no connectivity

## Integration with ascii-world Library

This example showcases:

✅ **Multiple point layers** - Each frequency as separate layer
✅ **Coordinate arithmetic** - Simple tuple math for vectors
✅ **Mask combination** - Bitwise OR for merging results
✅ **Bounds checking** - Grid validation for generated points
✅ **Efficient storage** - Single Integer mask for all antinodes
✅ **Lazy evaluation** - Infinite lists with takeWhile

The combination of point layers (for antennas) and masks (for antinodes) demonstrates how ascii-world handles both sparse point data and dense regions efficiently.

## Related Examples

- **[Day 6: Guard Gallivant](day06_README.md)** - Movement and collision detection
- **[Day 10: Hoof It](day10_README.md)** - Pathfinding algorithms
- **[Day 12: Garden Groups](test/day12.hs)** - Region detection
- **[Tutorial 01: Basic Grid](../examples/01-basic-grid.hs)** - Grid fundamentals

---

**Challenge**: Can you extend this solution to handle antennas that create antinodes at different ratios (e.g., 3:1 or 1:2:1)? What about 3D antenna grids?
