@echo off
cd /d "%~dp0..\.."

:menu
cls
echo ========================================
echo   Advent of Code 2024 - Animations
echo ========================================
echo.
echo Standard Animations:
echo   1. Day 01: Historian Hysteria
echo   2. Day 02: Red-Nosed Reports
echo   3. Day 03: Mull It Over
echo   4. Day 04: Ceres Search
echo   5. Day 05: Print Queue
echo   7. Day 07: Bridge Repair
echo   8. Day 08: Resonant Collinearity
echo   9. Day 09: Disk Fragmenter
echo  11. Day 11: Plutonian Pebbles
echo  12. Day 12: Garden Groups
echo  13. Day 13: Claw Contraption
echo  14. Day 14: Restroom Redoubt
echo.
echo Curated Animations (optimized):
echo   6. Day 06: Guard Gallivant
echo  10. Day 10: Hoof It
echo  16. Day 16: Reindeer Maze
echo  18. Day 18: RAM Run
echo  20. Day 20: Race Condition
echo.
echo   0. Exit
echo.
set /p choice="Enter day number (0-20): "

if "%choice%"=="0" goto end
if "%choice%"=="1" goto day01
if "%choice%"=="2" goto day02
if "%choice%"=="3" goto day03
if "%choice%"=="4" goto day04
if "%choice%"=="5" goto day05
if "%choice%"=="6" goto day06
if "%choice%"=="7" goto day07
if "%choice%"=="8" goto day08
if "%choice%"=="9" goto day09
if "%choice%"=="10" goto day10
if "%choice%"=="11" goto day11
if "%choice%"=="12" goto day12
if "%choice%"=="13" goto day13
if "%choice%"=="14" goto day14
if "%choice%"=="16" goto day16
if "%choice%"=="18" goto day18
if "%choice%"=="20" goto day20
echo Invalid choice!
pause
goto menu

:input_select
echo.
echo Select input data:
echo   1. Example data (default)
echo   2. Actual puzzle data
echo   3. Example 2 (if available)
echo.
set /p input_choice="Enter choice (1-3, default 1): "
if "%input_choice%"=="" set input_choice=1
if "%input_choice%"=="1" set INPUT_TYPE=example
if "%input_choice%"=="2" set INPUT_TYPE=data
if "%input_choice%"=="3" set INPUT_TYPE=example2
goto :eof

:day01
call :input_select
echo.
echo Running Day 01: Historian Hysteria with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc --package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/day01/animated/day01_animated.hs %INPUT_TYPE%
pause
goto menu

:day02
call :input_select
echo.
echo Running Day 02: Red-Nosed Reports with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc --package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/day02/animated/day02_animated.hs %INPUT_TYPE%
pause
goto menu

:day03
call :input_select
echo.
echo Running Day 03: Mull It Over with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc --package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/day03/animated/day03_animated.hs %INPUT_TYPE%
pause
goto menu

:day04
call :input_select
echo.
echo Running Day 04: Ceres Search with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc --package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/day04/animated/day04_animated.hs %INPUT_TYPE%
pause
goto menu

:day05
call :input_select
echo.
echo Running Day 05: Print Queue with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc --package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/day05/animated/day05_animated.hs %INPUT_TYPE%
pause
goto menu

:day06
call :input_select
echo.
echo Running Day 06: Guard Gallivant with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/curated/day06_animated.hs %INPUT_TYPE%
pause
goto menu

:day07
call :input_select
echo.
echo Running Day 07: Bridge Repair with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc --package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/day07/animated/day07_animated.hs %INPUT_TYPE%
pause
goto menu

:day08
call :input_select
echo.
echo Running Day 08: Resonant Collinearity with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc --package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/day08/animated/day08_animated.hs %INPUT_TYPE%
pause
goto menu

:day09
call :input_select
echo.
echo Running Day 09: Disk Fragmenter with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc --package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/day09/animated/day09_animated.hs %INPUT_TYPE%
pause
goto menu

:day10
call :input_select
echo.
echo Running Day 10: Hoof It with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 --package array test/2024/curated/day10_animated.hs %INPUT_TYPE%
pause
goto menu

:day11
call :input_select
echo.
echo Running Day 11: Plutonian Pebbles with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc --package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/day11/animated/day11_animated.hs %INPUT_TYPE%
pause
goto menu

:day12
call :input_select
echo.
echo Running Day 12: Garden Groups with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc --package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/day12/animated/day12_animated.hs %INPUT_TYPE%
pause
goto menu

:day13
call :input_select
echo.
echo Running Day 13: Claw Contraption with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc --package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/day13/animated/day13_animated.hs %INPUT_TYPE%
pause
goto menu

:day14
call :input_select
echo.
echo Running Day 14: Restroom Redoubt with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc --package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/day14/animated/day14_animated.hs %INPUT_TYPE%
pause
goto menu

:day16
call :input_select
echo.
echo Running Day 16: Reindeer Maze with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/curated/day16_animated.hs %INPUT_TYPE%
pause
goto menu

:day18
call :input_select
echo.
echo Running Day 18: RAM Run with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/curated/day18_animated.hs %INPUT_TYPE%
pause
goto menu

:day20
call :input_select
echo.
echo Running Day 20: Race Condition with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc --package containers-0.6.7 --package ansi-terminal-0.11.5 test/2024/curated/day20_animated.hs %INPUT_TYPE%
pause
goto menu

:end
echo.
echo Thanks for watching the animations!
echo.
