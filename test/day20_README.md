# Day 20: Race Condition - Solution

## Problem Description

You're racing through a track with walls. The twist: you can "cheat" by passing through walls for a limited time, creating shortcuts.

**Rules**:
- The track has a single path from S (start) to E (end)
- You can activate a "cheat" to pass through walls
- Cheats last up to 2 picoseconds (Part 1) or 20 picoseconds (Part 2)
- After cheating, you must be back on the track
- Goal: Count cheats that save significant time

### Part 1: Short Cheats
**Question**: How many cheats (≤2 picoseconds) save at least 100 picoseconds?

### Part 2: Long Cheats
**Question**: How many cheats (≤20 picoseconds) save at least 50 picoseconds?

## Example Input

```
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
```

In this example:
- `#` represents walls
- `S` is the start position
- `E` is the end position
- `.` represents the track

### Part 1 Solution
With a threshold of saving ≥1 picosecond:
- **14 cheats** save 2 picoseconds
- **14 cheats** save 4 picoseconds
- And many more...

### Part 2 Solution
With cheats ≤20 picoseconds and saving ≥50 picoseconds:
- Multiple cheats found with varying savings

## Implementation Notes

This solution uses **BFS + Manhattan Distance** for cheat detection:

### 1. **Find the Main Path**
```haskell
bfsWithDistances :: Int -> Int -> S.Set Point -> Point -> Point ->
                   Maybe (Path, DistanceMap)
```

First, find the normal path from S to E and create a distance map:
```haskell
type DistanceMap = M.Map Point Int
```

Each point on the path has its distance from the start.

### 2. **Cheat Detection with Manhattan Distance**
```haskell
findCheats :: DistanceMap -> Path -> Int -> Int -> [(Point, Point, Int)]
```

For every pair of points on the path:
1. Calculate Manhattan distance (shortest possible cheat)
2. Check if cheat duration ≤ maximum allowed
3. Calculate time saved = normal distance - cheat distance
4. Keep cheats that save ≥ threshold

```haskell
let manhattan = manhattanDistance startPos endPos
    normalTime = endDist - startDist
    cheatTime = manhattan
    timeSaved = normalTime - cheatTime
```

### 3. **Manhattan Distance**
```haskell
manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
```

This represents the minimum distance if you could walk through walls.

### 4. **Optimization Insights**

**Why this works:**
- Since the track is a single path, every point has a unique distance from start
- Any cheat that skips ahead on the path saves time
- Manhattan distance gives the minimum cheat duration needed
- We only consider pairs where the cheat would progress forward

## Visualization

The animated version (`day20_animated.hs`) shows:
- The race track layout
- The main path traced from S to E
- Examples of cheat shortcuts with start/end points
- Time savings for each cheat

Run with:
```bash
./test/day20_animated.hs
```

## Algorithm Complexity

### Step 1: BFS to find path
- **Time**: O(V + E) = O(W × H)
- **Space**: O(W × H)

### Step 2: Check all cheat pairs
- **Time**: O(P²) where P = path length
  - For each pair of points on path
  - Calculate Manhattan distance: O(1)
- **Space**: O(P) for storing cheats

### Total Complexity
- **Time**: O(W × H + P²)
- **Space**: O(W × H)

Where:
- W, H = grid width and height
- P = path length (≤ W × H)

## Key Insights

1. **Single-path track**: Simplifies the problem - there's only one normal route
2. **Manhattan distance**: Represents the minimum cheat duration between two points
3. **Forward-only cheats**: Only consider cheats that progress forward on the path
4. **Distance map optimization**: Pre-calculating distances makes cheat detection O(1) per pair
5. **Threshold filtering**: Many cheats exist, but we only count those with significant savings

## Cheat Mechanics

A cheat from point A to point B is valid if:
1. Both A and B are on the main path
2. Manhattan distance ≤ max cheat duration
3. B comes after A on the path (forward progress)
4. Time saved ≥ threshold

**Time saved calculation:**
```
Normal time = distance[B] - distance[A]
Cheat time = Manhattan distance(A, B)
Savings = Normal time - Cheat time
```

## Expected Outputs

### Example (lower thresholds for demonstration):
- Part 1 (≤2 ps, saving ≥1): Multiple cheats with various savings
- Part 2 (≤20 ps, saving ≥50): Fewer but more valuable cheats

### Real Input:
- Part 1 (≤2 ps, saving ≥100): Varies by input
- Part 2 (≤20 ps, saving ≥100): Many more opportunities with longer cheats
