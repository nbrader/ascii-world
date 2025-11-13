# Day 6: Guard Gallivant - Solution

## Problem Description

You're predicting the path of a guard patrolling a mapped area. The guard follows simple rules:

1. **If there's an obstacle** (`#`) directly in front, turn right 90 degrees
2. **Otherwise**, take a step forward
3. The guard starts at `^` facing up

The guard will either:
- Exit the mapped area (Part 1), OR
- Enter an infinite loop if an obstacle is added (Part 2)

### Part 1: Path Counting
**Question**: How many distinct positions does the guard visit before leaving the mapped area?

### Part 2: Loop Detection
**Question**: How many different positions could you add a single new obstruction to cause the guard to get stuck in a loop?

## Example Input

```
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
```

In this example:
- Grid is 10x10
- Guard starts at position (4, 3) facing up `^`
- Several obstacles `#` are scattered around

### Part 1 Solution
The guard's path visits **41 distinct positions** before exiting the map.

### Part 2 Solution
There are **6 different positions** where adding a single obstacle would trap the guard in a loop.

## Implementation Notes

This solution demonstrates several key features of the ascii-world library:

### 1. **Grid Parsing**
```haskell
charMap '#' = Just (MaskIndex "obstacles")
charMap '^' = Just (PointsIndex "guard")
```

### 2. **Collision Detection with Bitwise Operations**
```haskell
if isPointOverlappingMask width nextPos obstacles
  then go visited' (pos, turnRight dir)  -- Turn right
  else go visited' (nextPos, dir)        -- Move forward
```

The `isPointOverlappingMask` function uses O(1) bitwise AND to check collisions.

### 3. **State Tracking for Cycle Detection**
```haskell
type State = (Point, Direction)

simulatePatrol :: Int -> Int -> Mask -> State -> (S.Set State, Bool)
simulatePatrol width height obstacles = go S.empty
  where
    go visited state
      | state `S.member` visited = (visited, False)  -- Loop!
```

By tracking both position AND direction, we can detect when the guard returns to an exact previous state, indicating a loop.

### 4. **Dynamic Obstacle Testing**
```haskell
let newObstacles = bitwiseOr obstacles (pointToMask width pos)
let (_, exited) = simulatePatrol width height newObstacles startState
```

For Part 2, we test each candidate position by adding an obstacle using bitwise OR and re-simulating.

## Key Functions

### `simulatePatrol`
Simulates the guard's patrol path.

**Parameters**:
- `width`, `height`: Grid dimensions
- `obstacles`: Bitmask of obstacle positions
- `startState`: Initial (position, direction)

**Returns**:
- Set of visited states
- Boolean indicating if guard exited (True) or looped (False)

### `turnRight`
Rotates direction 90 degrees clockwise.

```haskell
turnRight (0, 1)   = (1, 0)   -- Up -> Right
turnRight (1, 0)   = (0, -1)  -- Right -> Down
turnRight (0, -1)  = (-1, 0)  -- Down -> Left
turnRight (-1, 0)  = (0, 1)   -- Left -> Up
```

### `findLoopCausingPositions`
Finds all positions where adding an obstacle causes a loop.

**Strategy**:
1. Run normal simulation to get all visited positions
2. For each visited position (except start), test if adding obstacle there causes loop
3. Return set of positions that cause loops

## Performance Characteristics

### Part 1
- **Time**: O(n * m) where n×m is grid size (worst case: visit every cell)
- **Space**: O(states) where states = positions × 4 directions

### Part 2
- **Time**: O(k * n * m) where k is number of visited positions
  - For each candidate position, run full simulation
- **Space**: O(states) for each simulation

### Optimizations Used

1. **Bitwise collision detection**: O(1) obstacle checking
2. **Set-based cycle detection**: O(log s) state lookup
3. **Early termination**: Stop simulation on cycle or exit
4. **Candidate pruning**: Only test positions the guard actually visits

## Usage

```bash
# Run with stack
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package split-0.2.3.5 -- day06.hs

# Or compile for better performance
stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 -- day06.hs -O2
./day06
```

## Expected Output

```
=== Day 6: Guard Gallivant ===

Grid: 10x10
Guard starting at: (4, 3)
Obstacles: 8 positions

Part 1: Distinct positions visited: 41
Part 2: Positions causing loops: 6

Sample of visited positions (first 10):
  (4, 3)
  (4, 4)
  (4, 5)
  (4, 6)
  ...
```

## Integration with ascii-world Library

This example showcases:

✅ **Grid parsing** with character-to-layer mapping
✅ **Bitwise collision detection** for efficient obstacle checking
✅ **Movement simulation** with direction changes
✅ **Cycle detection** using state tracking
✅ **Dynamic mask modification** for Part 2 testing

The combination of bitwise operations (O(1) collision checks) and efficient state tracking (Set-based cycle detection) makes this solution both elegant and performant.

## Related Examples

- **[Day 10: Hoof It](day10_README.md)** - Pathfinding with constraints
- **[Day 12: Garden Groups](test/day12.hs)** - Region detection
- **[Tutorial 03: Bitwise Operations](../examples/03-bitwise-operations.hs)** - Mask manipulation patterns

---

**Challenge**: Can you modify this solution to handle guards that turn left instead of right? Or multiple guards on the same map?
