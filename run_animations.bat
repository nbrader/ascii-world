@echo off
setlocal enabledelayedexpansion

:main_menu
cls
echo ========================================
echo   Advent of Code - All Animations
echo ========================================
echo.
echo Select Year:
echo   1. 2024 Animations (17 available)
echo   2. 2023 Animations
echo   3. 2022 Animations
echo   0. Exit
echo.
set /p year_choice="Enter choice (0-3): "

if "%year_choice%"=="0" goto end
if "%year_choice%"=="1" goto year2024
if "%year_choice%"=="2" goto year2023
if "%year_choice%"=="3" goto year2022
echo Invalid choice!
pause
goto main_menu

:year2024
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
echo   0. Back to main menu
echo.
set /p day_choice="Enter day number (0-20): "

if "%day_choice%"=="0" goto main_menu
if "%day_choice%"=="1" set DAY=01 & set ANIM_PATH=test\2024\day01\animated\day01_animated.hs & set PACKAGES=--package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 & goto run_2024
if "%day_choice%"=="2" set DAY=02 & set ANIM_PATH=test\2024\day02\animated\day02_animated.hs & set PACKAGES=--package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 & goto run_2024
if "%day_choice%"=="3" set DAY=03 & set ANIM_PATH=test\2024\day03\animated\day03_animated.hs & set PACKAGES=--package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 & goto run_2024
if "%day_choice%"=="4" set DAY=04 & set ANIM_PATH=test\2024\day04\animated\day04_animated.hs & set PACKAGES=--package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 & goto run_2024
if "%day_choice%"=="5" set DAY=05 & set ANIM_PATH=test\2024\day05\animated\day05_animated.hs & set PACKAGES=--package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 & goto run_2024
if "%day_choice%"=="6" set DAY=06 & set ANIM_PATH=test\2024\curated\day06_animated.hs & set PACKAGES=--package containers-0.6.7 --package ansi-terminal-0.11.5 & goto run_2024
if "%day_choice%"=="7" set DAY=07 & set ANIM_PATH=test\2024\day07\animated\day07_animated.hs & set PACKAGES=--package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 & goto run_2024
if "%day_choice%"=="8" set DAY=08 & set ANIM_PATH=test\2024\day08\animated\day08_animated.hs & set PACKAGES=--package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 & goto run_2024
if "%day_choice%"=="9" set DAY=09 & set ANIM_PATH=test\2024\day09\animated\day09_animated.hs & set PACKAGES=--package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 & goto run_2024
if "%day_choice%"=="10" set DAY=10 & set ANIM_PATH=test\2024\curated\day10_animated.hs & set PACKAGES=--package containers-0.6.7 --package ansi-terminal-0.11.5 --package array & goto run_2024
if "%day_choice%"=="11" set DAY=11 & set ANIM_PATH=test\2024\day11\animated\day11_animated.hs & set PACKAGES=--package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 & goto run_2024
if "%day_choice%"=="12" set DAY=12 & set ANIM_PATH=test\2024\day12\animated\day12_animated.hs & set PACKAGES=--package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 & goto run_2024
if "%day_choice%"=="13" set DAY=13 & set ANIM_PATH=test\2024\day13\animated\day13_animated.hs & set PACKAGES=--package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 & goto run_2024
if "%day_choice%"=="14" set DAY=14 & set ANIM_PATH=test\2024\day14\animated\day14_animated.hs & set PACKAGES=--package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5 & goto run_2024
if "%day_choice%"=="16" set DAY=16 & set ANIM_PATH=test\2024\curated\day16_animated.hs & set PACKAGES=--package containers-0.6.7 --package ansi-terminal-0.11.5 & goto run_2024
if "%day_choice%"=="18" set DAY=18 & set ANIM_PATH=test\2024\curated\day18_animated.hs & set PACKAGES=--package containers-0.6.7 --package ansi-terminal-0.11.5 & goto run_2024
if "%day_choice%"=="20" set DAY=20 & set ANIM_PATH=test\2024\curated\day20_animated.hs & set PACKAGES=--package containers-0.6.7 --package ansi-terminal-0.11.5 & goto run_2024
echo Invalid choice!
pause
goto year2024

:run_2024
call :input_select
echo.
echo Running Day %DAY% with %INPUT_TYPE% data...
echo.
echo Executing: stack --resolver lts-21.22 runghc %PACKAGES% %ANIM_PATH% %INPUT_TYPE%
echo.
stack --resolver lts-21.22 runghc %PACKAGES% %ANIM_PATH% %INPUT_TYPE%
pause
goto year2024

:year2023
cls
echo ========================================
echo   Advent of Code 2023 - Animations
echo ========================================
echo.
echo 2023 animations are placeholders.
echo They can be implemented using the same pattern as 2024.
echo.
pause
goto main_menu

:year2022
cls
echo ========================================
echo   Advent of Code 2022 - Animations
echo ========================================
echo.
echo 2022 animations are placeholders.
echo They can be implemented using the same pattern as 2024.
echo.
pause
goto main_menu

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

:end
echo.
echo Thanks for watching the animations!
echo.
endlocal
