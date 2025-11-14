# 🎬 Animation Guide

This project includes several animated visualizations of Advent of Code 2024 solutions!

## 🚀 Quick Start (Windows)

Pull the latest changes first:
```cmd
git pull
```

Then simply **double-click** any batch file in the `test/2024/` directory:

- **`test/2024/run_all_animations.bat`** - Interactive menu to choose animations
- **`test/2024/run_day06_animated.bat`** - Day 6: Guard Gallivant (Part 1 path + Part 2 loop cues)
- **`test/2024/run_day10_animated.bat`** - Day 10: Hoof It (Parts 1 & 2 BFS wave)
- **`test/2024/run_day16_animated.bat`** - Day 16: Reindeer Maze (Parts 1 & 2 optimal paths)
- **`test/2024/run_day18_animated.bat`** - Day 18: RAM Run (Part 1 shortest paths, Part 2 blocker)
- **`test/2024/run_day20_animated.bat`** - Day 20: Race Condition (Part 1 short cheats + Part 2 long cheats)

### Recommended: Interactive Menu

For the best experience, double-click:
```
test/2024/run_all_animations.bat
```

This gives you a menu to select which animation to watch!

## 🐧 Linux/macOS

Make scripts executable and run directly:

```bash
chmod +x test/*_animated.hs
./test/2024/day16_animated.hs
```

Or use Stack:
```bash
stack --resolver lts-21.22 runghc \
  --package containers-0.6.7 \
  --package ansi-terminal-0.11.5 \
  test/2024/day16_animated.hs
```

## 📋 Available Animations

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

## Advent of Code 2022 Stubs

Every Advent of Code 2022 puzzle has been imported under [`test/2022`](test/2022/README.md) with its original prompt and Haskell solution.  
Each day includes an `animated/` folder containing a README and a placeholder `dayXX_animated.hs`. Use these as starting points when you are ready to build full visualizations for the 2022 archive.

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
