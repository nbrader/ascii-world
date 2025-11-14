@echo off
echo Running Day 20 Animation: Race Condition
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 test/day20_animated.hs
pause
