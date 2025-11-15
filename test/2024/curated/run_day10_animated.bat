@echo off
chcp 65001 >nul
cd /d "%~dp0..\.."
echo Running Day 10 Animation: Hoof It (Parts 1 + 2 shared BFS wave)
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 --package array test/2024/day10_animated.hs
pause
