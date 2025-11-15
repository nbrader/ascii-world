@echo off
chcp 65001 >nul
cd /d "%~dp0..\.."
echo Running Day 20 Animation: Race Condition (Part 1 short cheats + Part 2 long cheats)
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/day20_animated.hs
pause
