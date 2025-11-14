@echo off
cd /d "%~dp0.."
echo Running Day 16 Animation: Reindeer Maze
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 test/day16_animated.hs
pause
