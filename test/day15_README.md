# Day 15: Warehouse Woes - Solution

## Problem Description

A robot (`@`) is moving through a warehouse filled with boxes (`O`) and walls (`#`). The robot follows a sequence of movement commands.

**Movement Rules**:
1. If the robot moves into an **empty space** (`.`), it moves successfully
2. If the robot moves into a **wall** (`#`), the move fails (robot stays in place)
3. If the robot moves into a **box** (`O`), it tries to push the box:
   - If there's empty space beyond the box(es), push succeeds
   - If there's a wall beyond the box(es), push fails
   - Multiple boxes in a line can be pushed together

### Part 1: Simple Box Pushing

Calculate the sum of all boxes' GPS coordinates after executing all moves.

**GPS Coordinate**: `100 × distance_from_top + distance_from_left`

## Example Input

```
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
```

**Grid**:
- `#`: Walls (immovable)
- `O`: Boxes (pushable)
- `@`: Robot (starting position)
- `.`: Empty space

**Moves**:
- `<`: Move left
- `>`: Move right
- `^`: Move up
- `v`: Move down

### Example Solution

Starting with robot at position (2, 5):

```
Initial:
########
#..O.O.#
##@.O..#    ← Robot at @
#...O..#
#.#.O..#
#...O..#
#......#
########
```

After moves `<^^>>>vv<v>>v<<`:

```
Final:
########
#....OO#
##.....#
#.....O#
#.#O@..#    ← Robot moved here
#...O..#
#...O..#
########
```

**Part 1 Answer**: Sum of GPS coordinates = **2028**

## Algorithm Overview

### Movement Simulation

**Basic Move**:
```haskell
tryMove :: Grid -> Point -> Direction -> (Grid, Point)
tryMove grid pos dir =
    let nextPos = move pos dir
        nextCell = grid ! nextPos
    in case nextCell of
        '.' -> (grid, nextPos)           -- Empty: move
        '#' -> (grid, pos)               -- Wall: stay
        'O' -> tryPushBox grid pos dir   -- Box: try to push
```

### Box Pushing Logic

**Single Box**:
```
Before:    After (if space):
@ O .  →   . @ O
```

**Multiple Boxes**:
```
Before:        After (if space):
@ O O O .  →   . @ O O O
```

**Blocked Push**:
```
Before:        After (blocked):
@ O O #    →   @ O O #  (no change)
```

**Algorithm**:
1. Find all consecutive boxes in the push direction
2. Check what's beyond the last box
3. If empty space: Move all boxes forward, robot moves into first box's spot
4. If wall: Nothing moves

### GPS Calculation

For each box at position (x, y):
```
GPS = 100 × (rows - y) + x
```

Where `rows` is the total number of rows (distance from top).

**Example**:
- Box at (4, 6) in 8-row grid: `100 × (8 - 6) + 4 = 204`
- Box at (1, 3) in 8-row grid: `100 × (8 - 3) + 1 = 501`

## Implementation Notes

This solution demonstrates stateful simulation with object interaction:

### 1. **Cascade Detection**

Find all boxes that would move together:

```haskell
takeWhileBox :: Grid -> Point -> Direction -> [Point]
takeWhileBox grid pos dir =
    case grid ! pos of
        'O' -> pos : takeWhileBox grid (move pos dir) dir
        _   -> []
```

**Example**:
```
@ O O O .
  ^^^^^^^
  These boxes form a chain
```

Returns `[(2,5), (3,5), (4,5)]` - all boxes in the push path.

### 2. **Atomic Updates**

All boxes in a push move simultaneously:

```haskell
tryPushBox grid robotPos dir =
    let boxChain = takeWhileBox grid nextPos dir
        afterBoxes = last boxChain + dir
    in if grid ! afterBoxes == '.'
       then -- Update all: new box pos, clear old first box pos
            let grid' = M.insert afterBoxes 'O' grid
                grid'' = M.insert nextPos '.' grid'
            in (grid'', nextPos)
       else (grid, robotPos)  -- Nothing moves
```

**Key insight**: Only the first and last positions change!
- First box position → empty (robot fills it)
- Position after last box → new box

### 3. **State Tracking**

Separate robot position from grid:

```haskell
type State = (Grid, Point)  -- (warehouse state, robot position)

simulateAll :: Grid -> Point -> [Char] -> State
simulateAll grid pos [] = (grid, pos)
simulateAll grid pos (move:rest) =
    let (grid', pos') = tryMove grid pos (charToDir move)
    in simulateAll grid' pos' rest
```

Robot position tracked explicitly rather than stored in grid.

### 4. **Collision Resolution**

Three-way decision for each move:

```haskell
case nextCell of
    '.' -> move succeeds
    '#' -> move fails
    'O' -> recursive check (push boxes)
```

Clean pattern matching handles all cases.

## Performance Characteristics

### Time Complexity

**Per Move**:
- Check next cell: O(1)
- Find box chain: O(k) where k is chain length
- Update grid: O(1) for first and last positions
- **Total per move**: O(k)

**Full Simulation**:
- n moves, average chain length k̄
- **Total**: O(n × k̄)
- Typical: k̄ ≈ 1-5, so effectively O(n)

### Space Complexity

- Grid storage: O(width × height)
- Box chain list: O(k)
- **Total**: O(width × height)

### Optimization Opportunities

**Current Implementation**:
```haskell
-- Finds all boxes then checks end
boxChain = takeWhileBox grid pos dir
valid = grid ! (afterBoxes boxChain) == '.'
```

**Optimized Version**:
```haskell
-- Early termination on wall
findPushable grid pos dir =
    case grid ! pos of
        '.' -> Just []
        '#' -> Nothing
        'O' -> (pos :) <$> findPushable grid (move pos dir) dir
```

Returns `Nothing` immediately on wall, avoiding unnecessary list building.

## Example Walkthrough

### Move Sequence

Starting position:
```
##@.O..#
```

**Move 1: `<` (left)**
```
##@.O..#  →  #@..O..#
  ↑            ↑
  Wall blocks, stay in place
```

**Move 2: `>` (right)**
```
#@..O..#  →  #.@.O..#
```

**Move 3: `>` (right)**
```
#.@.O..#  →  #..@O..#
   Push box →
```

**Move 4: `>` (right)**
```
#..@O..#  →  #...@O.#
    Push box →
```

**Move 5: `>` (right)**
```
#...@O.#  →  #....@O#
     Push box →
```

**Move 6: `>` (right)**
```
#....@O#  →  #....@O#
          #   Wall blocks box
          Entire push fails
```

### Cascade Example

Multiple boxes:
```
Before:     Move >     After:
@ O O .  →  →  →  →  . @ O O
```

Steps:
1. Check next cell: `O` (box)
2. Find chain: `[box1, box2]`
3. Check after chain: `.` (empty)
4. Move box2 to empty space
5. Clear box1's old position
6. Robot moves to box1's position

## Debugging Tips

### Common Issues

**1. Off-by-one in GPS calculation**
```haskell
-- Wrong: Using y directly
gps (x, y) = 100 * y + x

-- Correct: Distance from top
gps (x, y) = 100 * (maxY - y) + x
```

**2. Forgetting to clear first box position**
```haskell
-- Wrong: Only adds new box
grid' = M.insert afterBoxes 'O' grid

-- Correct: Clear and add
grid' = M.insert afterBoxes 'O' grid
grid'' = M.insert firstBox '.' grid'
```

**3. Moving robot before checking validity**
```haskell
-- Wrong: Move then check
newPos = move robotPos dir
if canPush grid newPos dir then ...

-- Correct: Check then move
if canPush grid (move robotPos dir) dir
then (grid', move robotPos dir)
else (grid, robotPos)
```

## Usage

```bash
# Run with stack
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package split-0.2.3.5 -- test/day15.hs

# Or compile
stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 -- test/day15.hs -O2
./day15
```

## Expected Output

```
=== Day 15: Warehouse Woes ===

Grid: 8 rows
Moves: 15 commands

Initial state:
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

Final state:
########
#....OO#
##.....#
#.....O#
#.#O@..#
#...O..#
#...O..#
########

Part 1: Sum of GPS coordinates: 2028
```

## Comparison with Other Solutions

### Simulation Complexity

| Solution | Object Interaction | State Changes |
|----------|-------------------|---------------|
| **Day 6** | Simple collision | Robot state only |
| **Day 14** | No interaction | Position updates |
| **Day 15** | Complex cascade | Grid + Robot state |

### Movement Types

| Solution | Movement Pattern |
|----------|-----------------|
| **Day 6** | Rule-based (turn on obstacle) |
| **Day 14** | Continuous (velocity-based) |
| **Day 15** | Discrete (command-based with physics) |

## Real-World Applications

This type of simulation applies to:

1. **Robotics**: Motion planning with movable obstacles
2. **Puzzle games**: Sokoban, block-pushing games
3. **Physics engines**: Chain reactions, domino effects
4. **Manufacturing**: Conveyor belt simulations
5. **Traffic systems**: Vehicle pushing scenarios

## Extensions and Challenges

**Easy**:
- Count total boxes pushed
- Find shortest command sequence
- Detect if any boxes are stuck (unreachable spaces)

**Medium**:
- Part 2: Handle wide boxes (2×1 objects)
- Multiple robots pushing simultaneously
- Boxes with different weights (harder to push)

**Hard**:
- 3D warehouse with box stacking
- Slippery floors (boxes slide until hitting obstacles)
- Rotating boxes with orientation

## Related Examples

- **[Day 4: Ceres Search](day04_README.md)** - Grid pattern matching
- **[Day 6: Guard Gallivant](day06_README.md)** - Single entity movement
- **[Day 14: Restroom Redoubt](day14_README.md)** - Multiple independent entities
- **[Tutorial 01: Basic Grid](../examples/01-basic-grid.hs)** - Grid fundamentals

---

**Key Takeaway**: Object interaction requires careful state management. Separate concerns (robot position vs grid state), use pattern matching for clear case handling, and remember that cascade effects often involve only boundary changes (first and last objects) rather than moving every object individually.
