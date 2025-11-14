@echo off
cd /d "%~dp0..\.."
echo Running Day 16 Animation: Reindeer Maze (Part 1 min-cost + Part 2 optimal tiles)
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/day16_animated.hs
pause
