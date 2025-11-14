# 🎬 Animation Guide

This project includes several animated visualizations of Advent of Code 2024 solutions!

## 🚀 Quick Start (Windows)

Pull the latest changes first:
```cmd
git pull
```

Then simply **double-click** any batch file in the `test/` directory:

- **`run_all_animations.bat`** - Interactive menu to choose animations
- **`run_day06_animated.bat`** - Day 6: Guard Gallivant
- **`run_day10_animated.bat`** - Day 10: Hoof It
- **`run_day16_animated.bat`** - Day 16: Reindeer Maze
- **`run_day18_animated.bat`** - Day 18: RAM Run
- **`run_day20_animated.bat`** - Day 20: Race Condition

### Recommended: Interactive Menu

For the best experience, double-click:
```
test/run_all_animations.bat
```

This gives you a menu to select which animation to watch!

## 🐧 Linux/macOS

Make scripts executable and run directly:

```bash
chmod +x test/*_animated.hs
./test/day16_animated.hs
```

Or use Stack:
```bash
stack --resolver lts-21.22 runghc \
  --package containers-0.6.7 \
  --package ansi-terminal-0.11.5 \
  test/day16_animated.hs
```

## 📋 Available Animations

### Day 6: Guard Gallivant 💂
- Shows a guard patrolling a grid
- Demonstrates turning on obstacles
- Visualizes cycle detection for loops
- **Algorithm**: State-based simulation

### Day 10: Hoof It 🥾
- Height-based pathfinding visualization
- Shows wavefront expansion by height levels
- Demonstrates flood-fill technique
- **Algorithm**: BFS with height constraints

### Day 16: Reindeer Maze 🦌
- Dijkstra's pathfinding with directional state
- Shows exploration with cost tracking
- Displays current direction (>, <, ^, v)
- **Algorithm**: Dijkstra's with rotation costs

### Day 18: RAM Run 💾
- Bytes falling into memory space
- Path recalculation after each byte
- Shows when path becomes blocked
- **Algorithm**: BFS with dynamic obstacles

### Day 20: Race Condition 🏁
- Race track path visualization
- Shows cheat shortcuts through walls
- Demonstrates Manhattan distance optimization
- **Algorithm**: BFS + cheat detection

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
stack --resolver lts-21.22 runghc --package containers-0.6.7 test/day16.hs

# Linux/macOS
./test/day16.hs
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
- **[test/day16_README.md](test/day16_README.md)** - Day 16 details
- **[test/day18_README.md](test/day18_README.md)** - Day 18 details
- **[test/day20_README.md](test/day20_README.md)** - Day 20 details

---

Enjoy watching the algorithms come to life! 🎄✨
