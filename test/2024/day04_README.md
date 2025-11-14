# Day 4: Ceres Search - Solution

## Problem Description

You need to help an Elf find words in a word search puzzle. The word search allows words to be:
- Horizontal, vertical, or diagonal
- Written forwards or backwards
- Overlapping other words

### Part 1: Find "XMAS"
Count all occurrences of the word **"XMAS"** in the grid, searching in all 8 directions:
- Right / Left (horizontal)
- Up / Down (vertical)
- All 4 diagonal directions

### Part 2: Find X-MAS Patterns
Count all occurrences of two **"MAS"** strings arranged in an X pattern. Each "MAS" can be forwards or backwards on its diagonal.

Valid X-MAS patterns:
```
M.S    M.M    S.M    S.S
.A.    .A.    .A.    .A.
M.S    S.S    S.M    M.M
```

## Example Input

```
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
```

This 10×10 grid contains:
- Multiple instances of "XMAS" in various directions
- Several X-MAS patterns with "MAS" on diagonals

### Part 1 Solution
Counting all "XMAS" in 8 directions: **18 occurrences**

Found instances include:
- Horizontal: Row 4 has "XMAS" and "SAMX"
- Vertical: Columns contain several instances
- Diagonal: Multiple diagonal "XMAS" patterns

### Part 2 Solution
Counting X-MAS patterns: **9 patterns**

Each pattern has an 'A' at center with "MAS" or "SAM" on both diagonals.

## Algorithm Overview

### Part 1: Directional Word Search

**Approach**:
1. Find all positions with 'X' (start of "XMAS")
2. For each 'X', check all 8 directions
3. Follow each direction for 4 characters
4. Check if it spells "XMAS"

**8 Directions**:
```haskell
directions =
    [ (1, 0)    -- Right
    , (-1, 0)   -- Left
    , (0, 1)    -- Up
    , (0, -1)   -- Down
    , (1, 1)    -- Up-Right
    , (1, -1)   -- Down-Right
    , (-1, 1)   -- Up-Left
    , (-1, -1)  -- Down-Left
    ]
```

**Example Check**:
Starting at X at (4, 5), checking right:
- Position (4, 5): 'X' ✓
- Position (5, 5): 'M' ✓
- Position (6, 5): 'A' ✓
- Position (7, 5): 'S' ✓
- Match found!

### Part 2: X-Pattern Search

**Approach**:
1. Find all positions with 'A' (center of X)
2. For each 'A', check both diagonals
3. Diagonal 1 (↘): Top-left to bottom-right must be "MAS" or "SAM"
4. Diagonal 2 (↙): Top-right to bottom-left must be "MAS" or "SAM"
5. Both diagonals must match for valid X-MAS

**Diagonal Check**:
```
For 'A' at (x, y):
  Diagonal 1: (x-1, y+1) → (x, y) → (x+1, y-1)
  Diagonal 2: (x+1, y+1) → (x, y) → (x-1, y-1)

Valid if both form "MAS" or "SAM"
```

**Example Pattern**:
```
M . S     Positions:
. A .     (4,6): M, (5,5): A, (6,4): S  [Diagonal 1: MAS ✓]
M . S     (6,6): S, (5,5): A, (4,4): M  [Diagonal 2: SAM ✓]
          Both valid → X-MAS found!
```

## Implementation Notes

This solution uses a simpler grid representation compared to other examples, demonstrating flexibility:

### 1. **Direct Character Grid**

Instead of using Mask or AsciiWorld layers, we use a simple Map:

```haskell
type Grid = M.Map (Int, Int) Char

parseGrid :: String -> (Grid, Int, Int)
parseGrid input =
    let rows = lines input
        grid = M.fromList [((x, y), char) | ...]
    in (grid, width, height)
```

**Why this approach?**
- Word search doesn't need region operations
- Each cell has different data (letter)
- Direct character lookup is clearest

### 2. **Pattern Matching with Maybe**

Safe grid lookups return `Maybe Char`:

```haskell
getAt :: Grid -> Point -> Maybe Char
getAt grid pos = M.lookup pos grid

checkWord :: Grid -> Point -> Point -> String -> Bool
checkWord grid (x, y) (dx, dy) word =
    let positions = [(x + i*dx, y + i*dy) | i <- [0..length word - 1]]
        chars = map (getAt grid) positions
        expectedChars = map Just word
    in chars == expectedChars
```

Out-of-bounds checks are automatic via `Maybe`.

### 3. **Direction Vectors**

Uniform handling of all 8 directions:

```haskell
-- Check all 8 directions from one position
counts = map (\pos -> length [dir | dir <- directions,
                                    checkWord grid pos dir "XMAS"]) xPositions
```

Single implementation works for all directions!

### 4. **Filter-Based Counting**

Functional approach to counting matches:

```haskell
-- Part 1: Count XMAS in all directions from all X positions
countXMAS grid = sum [length [dir | dir <- directions,
                                    checkWord grid pos dir "XMAS"]
                     | pos <- findChar grid 'X']

-- Part 2: Count X-MAS patterns at all A positions
countXMASPattern grid = length [pos | pos <- findChar grid 'A',
                                      isXMASAt pos]
```

## Performance Characteristics

### Time Complexity

**Part 1**:
- Find all X positions: O(width × height)
- For each X (count = n):
  - Check 8 directions: O(8 × word_length) = O(8 × 4) = O(32)
- Total: O(width × height + n × 32) = O(width × height)

**Part 2**:
- Find all A positions: O(width × height)
- For each A (count = m):
  - Check 4 diagonal positions: O(4)
- Total: O(width × height + m × 4) = O(width × height)

Both parts scan the grid once, making them linear in grid size.

### Space Complexity

- Grid storage: O(width × height) for Map
- Position lists: O(n) where n is count of starting letters
- Total: O(width × height)

### Why Not Use Masks?

This problem differs from others:
- **Day 12**: Needs region operations (union, flood-fill) → Masks excel
- **Day 8**: Needs set operations (combine antinodes) → Masks excel
- **Day 4**: Needs character-by-character comparison → Simple Map is clearer

The library's flexibility allows choosing the right tool!

## Usage

```bash
# Run with stack
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package split-0.2.3.5 -- test/2024/day04.hs

# Or compile
stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 -- test/2024/day04.hs -O2
./day04
```

## Expected Output

```
=== Day 4: Ceres Search ===

Grid: 10x10
Total cells: 100

Part 1: 'XMAS' found: 18 times
Part 2: X-MAS patterns found: 9 times

Found 19 'X' characters (potential XMAS starts)
Sample positions (first 5):
  (5, 9)
  (6, 9)
  (9, 9)
  (4, 8)
  (9, 8)
```

## Comparison with Other Solutions

### Different Data Structures

| Solution | Data Structure | Why? |
|----------|---------------|------|
| **Day 4** | `Map (Int,Int) Char` | Direct character access |
| **Day 6** | `Mask` for obstacles | Collision detection |
| **Day 8** | Point lists + Masks | Sparse points + dense results |
| **Day 12** | Masks for regions | Region operations |

### Different Operations

| Solution | Key Operation | Complexity |
|----------|--------------|------------|
| **Day 4** | Pattern matching | O(pattern length) |
| **Day 6** | Collision check | O(1) with masks |
| **Day 8** | Mask combination | O(1) bitwise OR |
| **Day 12** | Flood-fill | O(region size) |

## Code Patterns Demonstrated

### 1. List Comprehensions for Counting

```haskell
-- Count all valid patterns in one expression
count = length [pattern | source <- sources,
                         dir <- directions,
                         checkPattern source dir]
```

### 2. Maybe-Based Validation

```haskell
-- Safe coordinate access
isValid (x, y) =
    case (getAt grid (x-1, y+1), getAt grid (x+1, y-1)) of
        (Just 'M', Just 'S') -> True
        _ -> False
```

### 3. Direction Abstraction

```haskell
-- Single function works for all directions
checkAllDirections pos =
    [dir | dir <- directions, checkWord grid pos dir word]
```

## Extensions and Challenges

**Easy**:
- Search for other words ("SANTA", "ELF")
- Count overlapping vs non-overlapping matches
- Find longest word in dictionary that appears in grid

**Medium**:
- Find all words from a dictionary in the grid
- Optimize: Use trie for multi-word search
- Visualize found words on the grid

**Hard**:
- 3D word search (cube grid)
- Circular word search (wrapping at edges)
- Find optimal path visiting all instances of a word

## Related Examples

- **[Day 6: Guard Gallivant](day06_README.md)** - Grid traversal with state
- **[Day 8: Resonant Collinearity](day08_README.md)** - Coordinate calculations
- **[Day 10: Hoof It](day10_README.md)** - Pathfinding with constraints
- **[Tutorial 01: Basic Grid](../examples/01-basic-grid.hs)** - Grid fundamentals

---

**Key Takeaway**: The ascii-world library's modular design lets you choose the right abstraction. Use Masks for region operations, Point lists for sparse data, and simple Maps for character-level access. Pick what fits your problem!
