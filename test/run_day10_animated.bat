@echo off
cd /d "%~dp0.."
echo Running Day 10 Animation: Hoof It
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 --package array test/day10_animated.hs
pause
