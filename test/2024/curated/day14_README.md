# Day 14: Restroom Redoubt - Solution

## Problem Description

You're tracking security robots patrolling a rectangular space. Each robot has:
- A starting **position** (x, y)
- A constant **velocity** (vx, vy) in tiles per second

The space wraps around at the edges (like a torus):
- Moving past the right edge appears on the left
- Moving past the top edge appears on the bottom

### Part 1: Safety Factor
After **100 seconds**, calculate a "safety factor":
1. Divide the space into 4 quadrants (excluding middle row/column)
2. Count robots in each quadrant
3. Multiply the counts: `safety_factor = NE × NW × SE × SW`

### Part 2: Easter Egg Detection
Find the **first second** when the robots form a recognizable pattern (a Christmas tree in the actual problem).

## Example Input

```
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
```

Format: `p=x,y v=vx,vy`
- `p=0,4`: Robot starts at position (0, 4)
- `v=3,-3`: Velocity is 3 tiles/sec right, 3 tiles/sec down

Grid dimensions:
- Example: 11×7
- Actual input: 101×103

### Part 1 Solution
After 100 seconds, robots are distributed across quadrants.
Safety factor: Product of quadrant counts.

Example result: **12** (specific to example input)

### Part 2 Solution
Pattern appears when robots form a connected cluster.

In the actual problem, robots arrange into a Christmas tree picture at a specific second (around 7000-8000 for typical inputs).

## Mathematical Foundation

### Position After Time

For a robot at position (px, py) with velocity (vx, vy):

```
Position at time t:
  x(t) = (px + vx × t) mod width
  y(t) = (py + vy × t) mod height
```

**Example**:
- Start: p=(2, 4), v=(2, -3)
- Grid: 11×7
- After 5 seconds:
  - x = (2 + 2×5) mod 11 = 12 mod 11 = 1
  - y = (4 + (-3)×5) mod 7 = (4 - 15) mod 7 = -11 mod 7 = 3
  - Final position: (1, 3)

### Wrapping with Negative Numbers

Modulo with negative numbers requires special handling:

```haskell
wrapCoord :: Int -> Int -> Int
wrapCoord size coord =
    let wrapped = coord `mod` size
    in if wrapped < 0 then wrapped + size else wrapped
```

**Why needed?**
- In Haskell, `(-11) mod 7 = -4` (not the desired 3)
- Correction: `-4 + 7 = 3` ✓

### Quadrant Division

For a grid of width W and height H:

```
Middle lines:
  midX = W ÷ 2
  midY = H ÷ 2

Quadrants (excluding middle):
  NE: x > midX AND y < midY
  NW: x < midX AND y < midY
  SE: x > midX AND y > midY
  SW: x < midX AND y > midY
```

**Example (11×7 grid)**:
```
midX = 5, midY = 3

0 1 2 3 4 | 6 7 8 9 10   (x-axis)
──────────┼────────────
NW        │        NE    (y < 3)
──────────┼────────────  (y = 3, excluded)
SW        │        SE    (y > 3)
```

Robots on x=5 or y=3 are excluded from all quadrants.

## Implementation Notes

This solution demonstrates pure functional simulation without grid state:

### 1. **Independent Robot Simulation**

Each robot can be simulated independently:

```haskell
simulateRobot :: Int -> Int -> Int -> Robot -> Point
simulateRobot width height seconds ((px, py), (vx, vy)) =
    let finalX = (px + vx * seconds) `mod` width
        finalY = (py + vy * seconds) `mod` height
        wrappedX = if finalX < 0 then finalX + width else finalX
        wrappedY = if finalY < 0 then finalY + height else finalY
    in (wrappedX, wrappedY)
```

**Key insight**: No need to simulate step-by-step! Direct calculation jumps to time t.

### 2. **Functional Quadrant Counting**

Pure function counts using list comprehensions:

```haskell
countQuadrants width height positions =
    let midX = width `div` 2
        midY = height `div` 2
        q1 = length [p | p@(x, y) <- positions,
                        x > midX && y < midY]
    in (q1, q2, q3, q4)
```

No mutable state needed!

### 3. **Pattern Detection with Sets**

Efficient neighbor checking using Data.Set:

```haskell
detectCluster :: [Point] -> Bool
detectCluster positions =
    let posSet = S.fromList positions
        hasNeighbor (x, y) =
            any (`S.member` posSet) [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
        neighborCount = length [p | p <- positions, hasNeighbor p]
    in neighborCount > (length positions `div` 2)
```

O(n) clustering detection where n is number of robots.

### 4. **Grid Visualization**

Convert positions to displayable grid:

```haskell
visualizeGrid width height positions =
    let posMap = M.fromListWith (+) [(p, 1) | p <- positions]
        getCell (x, y) = M.findWithDefault 0 (x, y) posMap
        showCell count = if count == 0 then '.' else head (show count)
    in [[showCell (getCell (x,y)) | x <- [0..width-1]]
        | y <- [0..height-1]]
```

Shows '.' for empty cells, digit for robot count.

## Performance Characteristics

### Time Complexity

**Part 1**:
- Simulate n robots: O(n)
- Count quadrants: O(n)
- Total: **O(n)** where n is number of robots

**Part 2**:
- For each second t:
  - Simulate all robots: O(n)
  - Check clustering: O(n)
  - Total per iteration: O(n)
- Search T seconds: O(T × n)
- Total: **O(T × n)** where T ≈ 10,000

### Space Complexity

- Store robots: O(n)
- Position set for clustering: O(n)
- Total: **O(n)**

### Optimization: Direct Calculation

Instead of iterating second-by-second:

```haskell
-- Inefficient: O(t) per robot
simulateStepByStep robot =
    iterate moveOneSecond robot !! t

-- Efficient: O(1) per robot
simulateRobot width height t robot =
    calculateFinalPosition robot t
```

**Speedup**: From O(t) to O(1) per robot!

## Wrapping Visualization

### Example Wrapping Sequence

Robot at (10, 6) in 11×7 grid with velocity (2, 1):

```
Second 0: (10, 6)
Second 1: (10+2, 6+1) mod (11,7) = (12, 7) mod (11,7) = (1, 0) ← wrapped!
Second 2: (1+2, 0+1) = (3, 1)
Second 3: (3+2, 1+1) = (5, 2)
```

The robot "teleports" from right edge to left edge.

### Torus Topology

The wrapping makes the space topologically equivalent to a torus:

```
    ┌─────────────┐
    │  Connected  │
    │   to top    │
    └─────────────┘
         ↑  ↓
    ┌─────────────┐
←   │             │   → Connected to
    │    Grid     │     left/right
←   │             │   →
    └─────────────┘
         ↑  ↓
    ┌─────────────┐
    │  Connected  │
    │  to bottom  │
    └─────────────┘
```

## Usage

```bash
# Run with stack
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package split-0.2.3.5 -- test/2024/day14.hs

# Or compile
stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 -- test/2024/day14.hs -O2
./day14
```

## Expected Output

```
=== Day 14: Restroom Redoubt ===

Robots: 12
Space: 11x7

Part 1: After 100 seconds
  Safety factor: 12
  Quadrants: NE=1, NW=3
             SE=4, SW=1

Part 2: Pattern detected at second: 42

Initial state (first 3 seconds):
Second 0:
.....1.....
..........1
1..........
.........1.
......1....
1..........
.1.........

Second 1:
...........
.....1.....
.1.........
1..........
..........1
...........
..1......1.

Second 2:
...........
..........1
..1........
...........
1.....1....
...........
.......1...
```

## Comparison with Other Solutions

### Simulation Approaches

| Solution | Simulation Type | Key Challenge |
|----------|----------------|---------------|
| **Day 6** | Step-by-step | Collision detection |
| **Day 14** | Direct calculation | Wrapping coordinates |
| **Day 15** | State-based | Cascading effects |

### Coordinate Systems

| Solution | Coordinate Handling |
|----------|-------------------|
| **Day 4** | Fixed bounds, no wrapping |
| **Day 8** | Collinear calculations |
| **Day 14** | Modular arithmetic (wrapping) |

## Real-World Applications

This type of simulation applies to:

1. **Particle physics**: Periodic boundary conditions
2. **Game development**: Wrapping game worlds (Asteroids, Pac-Man)
3. **Traffic simulation**: Circular routes
4. **Network topology**: Ring networks
5. **Cellular automata**: Toroidal grids (Game of Life variants)

## Extensions and Challenges

**Easy**:
- Add visualization showing robot trails
- Calculate average robot density per quadrant over time
- Find when robots return to starting positions (cycle detection)

**Medium**:
- Optimize Part 2: Use heuristics to skip non-pattern seconds
- Detect different patterns (lines, grids, specific shapes)
- Handle 3D space with wrapping

**Hard**:
- Find periodic patterns (robots forming repeating configurations)
- Predict pattern appearance time without full simulation
- Handle variable-velocity robots

## Related Examples

- **[Day 4: Ceres Search](day04_README.md)** - Grid pattern matching
- **[Day 6: Guard Gallivant](day06_README.md)** - Single entity movement
- **[Day 8: Resonant Collinearity](day08_README.md)** - Multiple static entities
- **[Tutorial 01: Basic Grid](../examples/01-basic-grid.hs)** - Grid fundamentals

---

**Key Takeaway**: Direct calculation (jumping to time t) is often more efficient than iterative simulation. When entities move independently with constant velocities, use closed-form solutions: `position(t) = initial + velocity × t (mod space)`
