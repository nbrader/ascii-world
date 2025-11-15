@echo off
chcp 65001 >nul
cd /d "%~dp0..\.."
echo Running Day 6 Animation: Guard Gallivant (Part 1 path + Part 2 loop cues)
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/day06_animated.hs
pause
