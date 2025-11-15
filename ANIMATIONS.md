# 🎬 Animation Guide

This project includes several animated visualizations of Advent of Code 2024 solutions!

## 🚀 Quick Start (Windows)

Pull the latest changes first:
```cmd
git pull
```

Then simply **double-click** the main batch file:

- **`test/2024/curated/run_all_animations.bat`** - Interactive menu with ALL animations (17 total!)

### 🆕 Input Selection Feature

All animations now support **multiple input data sources**:
- **Example data** - Small test data from problem statements (default)
- **Actual puzzle data** - Your full puzzle input
- **Additional examples** - Some days have example2, example3, etc.

When you run an animation through the batch file, you'll be prompted to choose which input to use!

### Recommended: Interactive Menu

For the best experience, double-click:
```
test/2024/curated/run_all_animations.bat
```

This gives you a menu to:
1. Choose which animation to watch (17 available!)
2. Select which input data to use (example, actual data, or alternatives)
3. Watch the algorithm visualized step-by-step!

## 🐧 Linux/macOS

Make scripts executable and run with optional input selection:

```bash
chmod +x test/2024/curated/*_animated.hs

# Run with example data (default)
./test/2024/curated/day16_animated.hs

# Run with actual puzzle data
./test/2024/curated/day16_animated.hs data

# Run with alternative example
./test/2024/curated/day16_animated.hs example2
```

Or use Stack with input selection:
```bash
# With example data (default)
stack --resolver lts-21.22 runghc \
  --package containers-0.6.7 \
  --package ansi-terminal-0.11.5 \
  test/2024/curated/day16_animated.hs

# With actual puzzle data
stack --resolver lts-21.22 runghc \
  --package containers-0.6.7 \
  --package ansi-terminal-0.11.5 \
  test/2024/curated/day16_animated.hs data
```

**Available input options**: `example` (default), `data`, `example2`, `example3`

## 📋 Available Animations

All animations now support choosing between example and actual puzzle data!

### Standard Animations
- **Day 01**: Historian Hysteria - List pairing and distance calculation
- **Day 02**: Red-Nosed Reports - Safety report validation
- **Day 03**: Mull It Over - Finding and evaluating mul instructions
- **Day 04**: Ceres Search - Word search puzzle visualization
- **Day 05**: Print Queue - Page ordering validation
- **Day 07**: Bridge Repair - Equation solving with operators
- **Day 08**: Resonant Collinearity - Antenna antinode detection
- **Day 09**: Disk Fragmenter - Disk block reorganization
- **Day 11**: Plutonian Pebbles - Stone transformation rules
- **Day 12**: Garden Groups - Region detection and perimeter
- **Day 13**: Claw Contraption - Prize machine optimization
- **Day 14**: Restroom Redoubt - Robot motion simulation

### Curated Animations (Optimized)

### Day 6: Guard Gallivant 💂
- Shows a guard patrolling a grid
- Demonstrates turning on obstacles
- Visualizes cycle detection for loops
- **Algorithm**: State-based simulation
- **Part 1 focus**: The `[Part 1]` status text follows the guard until they leave the mapped area (unique position counting).
- **Part 2 cues**: When the status switches to `[Part 2] Looping`, you’ve entered the infinite-loop scenario explored in Part 2 after placing an extra obstacle.

### Day 10: Hoof It 🥾
- Height-based pathfinding visualization
- Shows wavefront expansion by height levels
- Demonstrates flood-fill technique
- **Algorithm**: BFS with height constraints
- **Part 1 focus**: The expanding wave mirrors how Part 1 counts trailhead scores (number of peaks reached).
- **Part 2 link**: The same `[Part 2]` wave information is reused for trailhead ratings, so every frame highlights how both parts share the BFS frontier.

### Day 16: Reindeer Maze 🦌
- Dijkstra's pathfinding with directional state
- Shows exploration with cost tracking
- Displays current direction (>, <, ^, v)
- **Algorithm**: Dijkstra's with rotation costs
- **Part 1 focus**: Cost readouts show the minimum score pursuit all the way to the exit.
- **Part 2 link**: The `[Part 2] Optimal tiles` annotation counts every tile that participates in any optimal route.

### Day 18: RAM Run 💾
- Bytes falling into memory space
- Path recalculation after each byte
- Shows when path becomes blocked
- **Algorithm**: BFS with dynamic obstacles
- **Part 1 focus**: Frames reporting a path length correspond to the Part 1 shortest-path requirement.
- **Part 2 cue**: The first `[Part 2] No path to exit` frame marks the blocking byte you need to report.

### Day 20: Race Condition 🏁
- Race track path visualization
- Shows cheat shortcuts through walls
- Demonstrates Manhattan distance optimization
- **Algorithm**: BFS + cheat detection
- **Part 1 focus**: Short cheats (≤2 picoseconds) animate first, matching Part 1’s requirement.
- **Part 2 focus**: Longer `[Part 2]` cheats (≤20 picoseconds) then appear to illustrate the second puzzle.

## Advent of Code Archives

- [`test/2022`](test/2022/README.md): Full AoC 2022 import with prompts, standard solutions, and animation stubs.
- [`test/2023`](test/2023/README.md): Same structure for AoC 2023.
- [`test/2024/archive`](test/2024/archive/README.md): Complete AoC 2024 data set alongside the curated solutions already under `test/2024/`.

Every archive folder contains per-day `standard/` and `animated/` directories so you can drop visualizations in as they are developed.

## 💡 Tips for Best Experience

1. **Maximize your terminal/console** - Animations need space!
2. **Zoom out if needed** - Use `Ctrl + -` or `Ctrl + Mouse Wheel`
3. **Use a modern terminal**:
   - Windows: Windows Terminal (recommended)
   - Linux: GNOME Terminal, Konsole
   - macOS: iTerm2 or Terminal.app
4. **Let animations complete** - They pause automatically when done

## ⚙️ Prerequisites

You need **Stack** (Haskell build tool) installed:

### Windows
Download from: https://docs.haskellstack.org/en/stable/install_and_upgrade/

Or use Chocolatey:
```cmd
choco install haskell-stack
```

### Linux
```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Or package manager:
```bash
# Ubuntu/Debian
sudo apt install haskell-stack

# Arch Linux
sudo pacman -S stack
```

### macOS
```bash
brew install haskell-stack
```

## 🔧 Troubleshooting

### "stack: command not found"
- Install Stack using the instructions above
- Restart your terminal after installation

### "Permission denied" (Linux/macOS)
```bash
chmod +x test/*_animated.hs
```

### Animation looks garbled
- Use Windows Terminal on Windows
- Make sure your terminal supports ANSI escape codes

### Characters don't display correctly
- The animations now use ASCII characters (>, <, ^, v)
- Should work on all terminals without encoding issues

### Screen too small
- Maximize your terminal window
- Zoom out: `Ctrl + -` or `Ctrl + Mouse Wheel`

## 📖 Running Solutions (Non-Animated)

To run the actual solutions that compute answers:

```bash
# Windows
stack --resolver lts-21.22 runghc --package containers-0.6.7 test/2024/day16.hs

# Linux/macOS
./test/2024/day16.hs
```

## 🎯 What You'll See

Each animation uses **ANSI control codes** to update the terminal in place, creating smooth visualizations:

- **Visited cells** are marked with dots (·)
- **Current position** is highlighted with [R], [S], etc.
- **Obstacles** are shown as blocks (███ or ###)
- **Paths** show the route taken
- **Status information** updates in real-time

The animations demonstrate the algorithms at work, making it easier to understand how grid-based pathfinding and search algorithms operate!

## 📚 More Information

- **[README.md](README.md)** - Project overview
- **[USAGE.md](USAGE.md)** - Library usage guide
- **[test/2024/day16_README.md](test/2024/day16_README.md)** - Day 16 details
- **[test/2024/day18_README.md](test/2024/day18_README.md)** - Day 18 details
- **[test/2024/day20_README.md](test/2024/day20_README.md)** - Day 20 details

---

Enjoy watching the algorithms come to life! 🎄✨
