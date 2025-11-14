# Day 16: Reindeer Maze - Solution

## Problem Description

A Reindeer must navigate through a maze from a start position (S) to an end position (E). The challenge involves two types of costs:

1. **Moving forward**: costs 1 point
2. **Rotating 90 degrees** (clockwise or counterclockwise): costs 1000 points

The Reindeer starts at S facing **East** (right direction).

### Part 1: Minimum Cost Path
**Question**: What is the lowest score (cost) to reach the end position?

### Part 2: Optimal Path Tiles
**Question**: How many tiles are part of at least one optimal (minimum cost) path?

## Example Input

```
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
```

In this example:
- Grid is 15x15
- `#` represents walls
- `S` is the start position (bottom-left area)
- `E` is the end position (top-right area)
- `.` represents open floor space

### Part 1 Solution
The minimum cost to reach the end is **7036 points**.

### Part 2 Solution
There are **45 tiles** that are part of at least one optimal path.

## Implementation Notes

This solution uses **Dijkstra's algorithm** with directional state to find optimal paths:

### 1. **State Representation**
```haskell
type Direction = Point
type State = (Point, Direction)  -- (position, direction)
type Cost = Int
```

Unlike simple pathfinding, we track both position AND direction because turning has a cost.

### 2. **Dijkstra's Algorithm with Priority Queue**
```haskell
type PQEntry = (Cost, State, [Point])

dijkstra :: Int -> Int -> Mask -> State -> Point -> (Cost, [[Point]])
```

The algorithm:
- Maintains a priority queue sorted by cost
- Explores neighbors: forward movement (cost +1) and rotations (cost +1000)
- Tracks all optimal paths, not just one

### 3. **Neighbor Generation**
```haskell
-- Three possible actions from any state:
moveForwardEntry   -- Cost +1, same direction, new position
turnLeftEntry      -- Cost +1000, new direction, same position
turnRightEntry     -- Cost +1000, new direction, same position
```

### 4. **Finding All Optimal Paths**
The implementation collects ALL paths that achieve the minimum cost, then counts unique tiles:
```haskell
let tilesOnOptimalPaths = S.fromList $ concat bestPaths
```

## Visualization

The animated version (`day16_animated.hs`) shows:
- The maze layout with walls
- The current position and facing direction of the Reindeer
- Visited positions marked with dots
- The frontier of exploration
- Real-time cost updates

Run with:
```bash
./test/2024/day16_animated.hs
```

## Algorithm Complexity

- **Time**: O(E log V) where E = edges and V = vertices (states)
- **Space**: O(V) for visited set and distance map
- **Grid size**: Each cell × 4 directions = width × height × 4 states

## Key Insights

1. **Directional state is crucial**: Unlike standard BFS/Dijkstra, we must track direction because turning costs differ from moving
2. **Multiple optimal paths**: There may be many paths with the same minimum cost
3. **Rotation costs dominate**: With rotation costing 1000× movement, the algorithm heavily favors straight paths
4. **Bidirectional consideration**: We need both left and right turns at each state

## Expected Outputs

### Example 1 (15×15):
- Part 1: **7036**
- Part 2: **45 tiles**

### Example 2 (17×17):
- Part 1: **11048**
- Part 2: **64 tiles**
