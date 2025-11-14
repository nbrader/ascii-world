@echo off
echo Running Day 18 Animation: RAM Run
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 test/day18_animated.hs
pause
