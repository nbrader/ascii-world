# Day 10: Hoof It - Solution

## Problem Description

You're hiking on a topographic map where:
- Each position has a height from 0 to 9
- A **trailhead** is any position with height 0
- A **hiking trail** must:
  - Start at height 0 (trailhead)
  - End at height 9 (peak)
  - Always move to adjacent positions (up/down/left/right)
  - Increase by exactly 1 at each step

### Part 1: Trailhead Score
The **score** of a trailhead is the number of distinct height-9 positions (peaks) reachable from it.

### Part 2: Trailhead Rating
The **rating** of a trailhead is the total number of distinct hiking trails (paths) from it to any height-9 position.

## Test Data

### Simple Example (`day10 (simple).csv`)
```
0123
1234
8765
9876
```

This has:
- 1 trailhead at position (0,0)
- 1 reachable peak at position (3,0)
- 1 distinct path from trailhead to peak
- **Score: 1**
- **Rating: 1**

### Full Example (`day10 (example).csv`)
```
89010123
78121874
87430965
96549874
45678903
32019012
01298765
10107654
```

This has:
- 9 trailheads (positions with height 0)
- **Expected Part 1 answer: 36** (sum of all scores)
- **Expected Part 2 answer: 81** (sum of all ratings)

## Implementation Notes

The solution demonstrates:

1. **Grid Reading**: Converts the text input into a coordinate map
2. **Pathfinding**: Recursive search to find reachable peaks
3. **Set Operations**: Using Data.Set to count unique peaks
4. **Path Counting**: Counting all distinct paths (not just unique destinations)

### Key Functions

- `readTopoMap`: Parses the input into a Point -> Height map
- `findHeights`: Finds all positions at a specific height
- `getUpwardNeighbors`: Returns neighbors exactly 1 height higher
- `scoreTrailhead`: Counts unique reachable peaks (Part 1)
- `ratingTrailhead`: Counts all distinct paths (Part 2)

## Usage

```bash
# Run with stack (if available)
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package split-0.2.3.5 -- day10.hs

# Or compile and run
stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 -- day10.hs -O2
./day10
```

## Expected Output

For the full example:
```
Trailheads: 9
Individual ratings: [3, 13, 4, 4, 5, 8, 5, 5, 34]
Total rating (Part 2): 81
```

## Integration with ascii-world Library

While this solution primarily uses standard Haskell data structures (Map, Set), it demonstrates:
- Using the `Point` type from `Mask.hs`
- Using the direction utilities from `Util.hs` (lrduDirs)
- Shows how ascii-world can be extended for custom grid problems

The ascii-world library excels at:
- Region detection and connected component analysis (as shown in Day 12)
- Bitwise operations on entire grid layers
- Efficient flood-fill algorithms

For pathfinding problems like Day 10, a Map-based approach is more natural, but the core utilities (Point, directions) remain useful!
