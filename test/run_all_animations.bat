@echo off
echo ========================================
echo   Advent of Code 2024 - Animations
echo ========================================
echo.
echo Available animations:
echo   1. Day 6:  Guard Gallivant
echo   2. Day 10: Hoof It (height-based pathfinding)
echo   3. Day 16: Reindeer Maze (Dijkstra's algorithm)
echo   4. Day 18: RAM Run (bytes falling, BFS)
echo   5. Day 20: Race Condition (cheat detection)
echo.
echo   0. Exit
echo.
set /p choice="Enter your choice (0-5): "

if "%choice%"=="1" goto day06
if "%choice%"=="2" goto day10
if "%choice%"=="3" goto day16
if "%choice%"=="4" goto day18
if "%choice%"=="5" goto day20
if "%choice%"=="0" goto end
echo Invalid choice!
pause
goto menu

:day06
echo.
echo Running Day 6: Guard Gallivant...
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 test/day06_animated.hs
pause
goto menu

:day10
echo.
echo Running Day 10: Hoof It...
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 --package array test/day10_animated.hs
pause
goto menu

:day16
echo.
echo Running Day 16: Reindeer Maze...
echo (Make sure you've pulled latest changes: git pull)
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 test/day16_animated.hs
pause
goto menu

:day18
echo.
echo Running Day 18: RAM Run...
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 test/day18_animated.hs
pause
goto menu

:day20
echo.
echo Running Day 20: Race Condition...
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 test/day20_animated.hs
pause
goto menu

:menu
cls
echo ========================================
echo   Advent of Code 2024 - Animations
echo ========================================
echo.
echo Available animations:
echo   1. Day 6:  Guard Gallivant
echo   2. Day 10: Hoof It (height-based pathfinding)
echo   3. Day 16: Reindeer Maze (Dijkstra's algorithm)
echo   4. Day 18: RAM Run (bytes falling, BFS)
echo   5. Day 20: Race Condition (cheat detection)
echo.
echo   0. Exit
echo.
set /p choice="Enter your choice (0-5): "

if "%choice%"=="1" goto day06
if "%choice%"=="2" goto day10
if "%choice%"=="3" goto day16
if "%choice%"=="4" goto day18
if "%choice%"=="5" goto day20
if "%choice%"=="0" goto end
echo Invalid choice!
pause
goto menu

:end
echo.
echo Thanks for watching the animations!
echo.
