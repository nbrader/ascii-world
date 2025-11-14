@echo off
cd /d "%~dp0..\.."
echo Running Day 18 Animation: RAM Run (Part 1 path + Part 2 blocking byte)
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/day18_animated.hs
pause
