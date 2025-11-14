# Day 18: RAM Run - Solution

## Problem Description

Bytes are falling into a memory space grid at specific coordinates, creating obstacles. You need to find the shortest path from the top-left corner to the bottom-right corner.

- **Grid size**: 71×71 (0,0 to 70,70) for real input, 7×7 (0,0 to 6,6) for example
- **Bytes**: Fall at specific (x,y) coordinates in sequence
- **Goal**: Navigate from start (0,0) to exit

### Part 1: Shortest Path After N Bytes
**Question**: After the first N bytes have fallen (1024 for real input, 12 for example), what is the shortest path length?

### Part 2: First Blocking Byte
**Question**: What are the coordinates of the first byte that blocks ALL possible paths?

## Example Input

```
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
...
```

Each line represents a byte falling at coordinates (x, y).

In the example:
- Grid is 7×7 (coordinates 0-6)
- First byte falls at (5,4)
- Second byte falls at (4,2)
- And so on...

### Part 1 Solution (after 12 bytes)
The shortest path length is **22 steps**.

### Part 2 Solution
The first byte that blocks all paths is at coordinates **(6,1)**.

## Implementation Notes

This solution uses **Breadth-First Search (BFS)** with dynamic obstacles:

### 1. **Grid Representation**
```haskell
type Point = (Int, Int)
type Grid = S.Set Point  -- Set of obstacles
```

Using a Set for O(log n) obstacle lookup.

### 2. **BFS for Shortest Path**
```haskell
bfs :: Int -> Grid -> Point -> Point -> Maybe Int
bfs gridSize obstacles start end = go (Seq.singleton (start, 0)) S.empty
```

Classic BFS:
- Uses a queue (Data.Sequence) for frontier
- Tracks visited positions to avoid cycles
- Returns shortest distance (number of steps)

### 3. **Neighbor Generation**
```haskell
getNeighbors :: Int -> Grid -> Point -> [Point]
getNeighbors gridSize obstacles (x, y) =
    [ (nx, ny)
    | (dx, dy) <- [(0, 1), (0, -1), (1, 0), (-1, 0)]
    , let nx = x + dx
    , let ny = y + dy
    , nx >= 0 && nx < gridSize
    , ny >= 0 && ny < gridSize
    , (nx, ny) `S.notMember` obstacles  -- Not blocked
    ]
```

### 4. **Binary Search for Blocking Byte**
```haskell
findBlockingByte :: Int -> [Point] -> Point -> Point -> Maybe Point
findBlockingByte gridSize bytes start end = binarySearch 0 (length bytes)
```

Part 2 uses binary search:
1. Try mid-point: can we still reach the end with M bytes fallen?
2. If yes, try more bytes (search right half)
3. If no, try fewer bytes (search left half)
4. Converge on the exact byte that blocks all paths

Time complexity: O(B × log(B) × (W × H)) where B = number of bytes

## Visualization

The animated version (`day18_animated.hs`) shows:
- The grid with fallen bytes marked as obstacles
- The current shortest path (if it exists)
- Bytes falling one by one
- Path recalculation after each byte
- Indication when no path remains

Run with:
```bash
./test/day18_animated.hs
```

## Algorithm Complexity

### Part 1: BFS
- **Time**: O(V + E) = O(W × H) for a grid
- **Space**: O(V) = O(W × H) for visited set and queue

### Part 2: Binary Search + BFS
- **Time**: O(B × log(B) × (W × H))
  - Binary search: log(B) iterations
  - Each iteration: BFS in O(W × H)
- **Space**: O(W × H)

Where:
- W, H = grid width and height
- B = number of bytes

## Key Insights

1. **BFS is optimal for unweighted grids**: All moves have equal cost
2. **Binary search optimization**: Instead of trying every byte sequentially, we can find the blocking byte in logarithmic time
3. **Set-based obstacles**: Fast membership testing
4. **Early termination**: BFS stops as soon as we reach the end

## Expected Outputs

### Example (7×7 grid, 12 bytes):
- Part 1: **22 steps**
- Part 2: **6,1** (coordinates of blocking byte)

### Real Input (71×71 grid, 1024 bytes):
- Part 1: Path length varies by input
- Part 2: Specific coordinates vary by input
