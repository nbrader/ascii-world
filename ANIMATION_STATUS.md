# Animation Status Report

This document categorizes all Advent of Code animations in this repository by type.

## Animation Types

- **Progress Bar**: Simple horizontal progress bar showing completion through the input
- **Real Animation**: Sophisticated algorithm visualization with custom rendering logic

---

## 2022 Animations (25/25 complete)

### Real Animations (5 days)

| Day | Title | Animation Type |
|-----|-------|----------------|
| 09 | Rope Bridge | Rope physics simulation (head and tail following) |
| 12 | Hill Climbing Algorithm | BFS pathfinding on heightmap |
| 14 | Regolith Reservoir | Falling sand simulation |
| 23 | Unstable Diffusion | Elf spreading simulation |
| 25 | Full of Hot Air | SNAFU number conversion (digit-by-digit visualization) |

### Progress Bar Only (20 days)

Days: 01, 02, 03, 04, 05, 06, 07, 08, 10, 11, 13, 15, 16, 17, 18, 19, 20, 21, 22, 24

---

## 2023 Animations (21/25 complete)

### Real Animations (5 days)

| Day | Title | Animation Type |
|-----|-------|----------------|
| 01 | Trebuchet! | Character-by-character scanner detecting digit words |
| 10 | Pipe Maze | Pipe loop traversal |
| 14 | Parabolic Reflector Dish | Rocks rolling on tilted platform |
| 16 | The Floor Will Be Lava | Light beam tracing through mirrors/splitters |
| 21 | Step Counter | BFS expansion showing reachable garden plots |

### Progress Bar Only (16 days)

Days: 02, 03, 04, 05, 06, 07, 08, 09, 11, 12, 13, 15, 17, 18, 19, 20

### No Animation (4 days)

Days: 22, 23, 24, 25

---

## 2024 Animations

### Curated Animations (5 days) - All Real Animations

Located in `test/2024/curated/`

| Day | Title | Animation Type |
|-----|-------|----------------|
| 06 | Guard Gallivant | Guard patrolling with cycle detection (~169 lines) |
| 10 | Hoof It | BFS wavefront expansion on heightmap |
| 16 | Reindeer Maze | Dijkstra's pathfinding with directional state |
| 18 | RAM Run | Bytes falling with dynamic path recalculation |
| 20 | Race Condition | Race track with cheat detection |

### Standard Animations (12 days) - All Progress Bar

Located in `test/2024/dayNN/animated/`

Days: 01, 02, 03, 04, 05, 07, 08, 09, 11, 12, 13, 14

### Placeholders (4 days)

Days: 06, 10, 15, 16 (in standard directory - real animations are in curated)

### No Animation

Days: 17-25

---

## Summary Statistics

| Category | Count | Percentage |
|----------|-------|------------|
| **Progress Bar Animations** | 36 | 54% |
| **Real Algorithm Visualizations** | 15 | 22% |
| **2024 Standard (Progress Bar)** | 12 | 18% |
| **2024 Placeholders** | 4 | 6% |
| **TOTAL Animation Files** | 67 | 100% |

---

## Technical Implementation Details

### Progress Bar Pattern
- Uses AsciiWorld library
- Simple pattern: `[(i, 0) | i <- [0..progress]]`
- ~100 lines of code
- Shows completion through input processing

### Real Animation Pattern
- May use AsciiWorld library OR custom ANSI terminal control
- Has `data Frame` structure with simulation state
- Custom `renderFrame` logic
- ~150-230 lines of code
- Shows step-by-step algorithm execution

### Curated Animations (2024 only)
- Hand-coded ANSI terminal control
- No AsciiWorld dependency
- Most sophisticated visualizations
- Educational algorithm demonstrations

---

*Last updated: 2025-11-16*
