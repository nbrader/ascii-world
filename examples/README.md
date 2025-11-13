# Examples and Tutorials

This directory contains step-by-step tutorials to help you learn the ascii-world library.

## Tutorial Structure

Each tutorial builds on the previous one, introducing new concepts progressively:

### 1. [Basic Grid Operations](01-basic-grid.hs)

**Learn**: Fundamentals of grid creation and manipulation

**Topics**:
- Creating an AsciiWorld from a string
- Understanding the coordinate system (0,0 = bottom-left)
- Accessing masks and points
- Displaying grids

**Run**:
```bash
stack --resolver lts-21.22 runghc --package containers --package split -- 01-basic-grid.hs
```

**Expected Output**:
```
=== Tutorial 1: Basic Grid Operations ===

Grid dimensions: 5x4
Layers in the grid:
  Masks (grid layers): ["walls"]
  Points (individual positions): ["player"]
...
```

---

### 2. [Region Detection](02-region-detection.hs)

**Learn**: Finding connected components using flood-fill

**Topics**:
- Partitioning masks by connectivity
- Counting regions
- Measuring area and perimeter
- Using WalkableWorld functions

**Run**:
```bash
stack --resolver lts-21.22 runghc --package containers --package split -- 02-region-detection.hs
```

**Expected Output**:
```
=== Tutorial 2: Region Detection ===

Plant types found: "ABCDE"
Plant A: 1 region(s)
  Region 1: 4 cells
Plant B: 1 region(s)
  Region 1: 4 cells
...
```

---

### 3. [Bitwise Operations](03-bitwise-operations.hs)

**Learn**: Manipulating masks with bitwise operations

**Topics**:
- Combining masks (OR, AND, XOR)
- Subtracting masks
- Moving masks
- Collision detection
- Fast set operations

**Run**:
```bash
stack --resolver lts-21.22 runghc --package containers --package split -- 03-bitwise-operations.hs
```

**Expected Output**:
```
=== Tutorial 3: Bitwise Operations ===

=== Operation 1: Combining Masks ===
Combined mask has 2 cells

=== Operation 2: Moving Masks ===
Before move: player at (1,2)
After move:  player at (2,2)
...
```

---

### 4. [Complete Solution](04-complete-solution.hs)

**Learn**: Solving a full Advent of Code-style problem

**Topics**:
- Complete workflow from input to answer
- Region analysis and measurement
- Calculating scores
- Best practices

**Run**:
```bash
stack --resolver lts-21.22 runghc --package containers --package split -- 04-complete-solution.hs
```

**Expected Output**:
```
=== Tutorial 4: Complete Solution ===

Garden Analysis:
================

Map:
AAAA
BBCD
BBCC
EEEC

Total fence cost (Part 1): 140
Total discounted cost (Part 2): 80
...
```

---

## Learning Path

### Beginners
1. Start with Tutorial 1 to understand the basics
2. Move to Tutorial 2 to learn about regions
3. Try Tutorial 4 for a complete example

### Advanced Users
1. Tutorial 3 for bitwise operation mastery
2. Review Tutorial 4 for optimization techniques
3. Check [test/day12.hs](../test/day12.hs) for real AoC solutions

## Key Concepts

### Masks vs Points

**Masks** (stored as Integer):
- Represent entire grid layers
- Fast bitwise operations
- Good for dense regions
- Example: walls, floors, regions

**Points** (stored as [(Int,Int)]):
- Individual coordinates
- Flexible for sparse data
- Order can matter
- Example: player position, enemies

### Coordinate System

```
Y-axis (increases upward)
^
|  (0,3) (1,3) (2,3)
|  (0,2) (1,2) (2,2)
|  (0,1) (1,1) (2,1)
|  (0,0) (1,0) (2,0)
+-------------------> X-axis (increases rightward)
```

**Important**: Text files are read from top to bottom, but the library uses mathematical coordinates where Y increases upward!

### Performance Tips

From the tutorials:

1. **Use bitwise operations** for set operations (O(1) vs O(n))
2. **Prefer masks** for dense regions
3. **Cache lookups** when accessing masks multiple times
4. **Use library functions** instead of manual iteration

## Real-World Examples

After completing the tutorials, check out these full solutions:

- **[Day 12: Garden Groups](../test/day12.hs)** - Region detection and fence counting
- **[Day 10: Hoof It](../test/day10.hs)** - Topographic pathfinding

## Need Help?

- **[USAGE.md](../USAGE.md)** - Comprehensive usage guide
- **[README.md](../README.md)** - Library overview
- **[API Reference](../src/)** - Module documentation

## Next Steps

After mastering these tutorials:

1. Try solving simple grid puzzles
2. Attempt Advent of Code problems
3. Optimize your solutions using bitwise operations
4. Contribute your own examples!

---

**Happy coding!** 🚀
