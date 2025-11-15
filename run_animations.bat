@echo off
chcp 65001 >nul
setlocal enabledelayedexpansion

:main_menu
cls
echo ========================================
echo   Advent of Code - All Animations
echo ========================================
echo.
echo Select Year:
echo   1. 2024 Animations (17 available)
echo   2. 2023 Animations (21 available)
echo   3. 2022 Animations (25 available)
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
echo Enter day number (1-25) or 0 to go back
echo.
set /p day_choice="Day: "

if "%day_choice%"=="0" goto main_menu
set DAY=%day_choice%
if %DAY% LSS 10 set DAY=0%day_choice%
set ANIM_PATH=test\2024\day%DAY%\animated\day%DAY%_animated.hs
set PACKAGES=--package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5

REM Check for curated animations
if "%day_choice%"=="6" set ANIM_PATH=test\2024\curated\day06_animated.hs & set PACKAGES=--package containers-0.6.7 --package ansi-terminal-0.11.5
if "%day_choice%"=="10" set ANIM_PATH=test\2024\curated\day10_animated.hs & set PACKAGES=--package containers-0.6.7 --package ansi-terminal-0.11.5 --package array
if "%day_choice%"=="16" set ANIM_PATH=test\2024\curated\day16_animated.hs & set PACKAGES=--package containers-0.6.7 --package ansi-terminal-0.11.5
if "%day_choice%"=="18" set ANIM_PATH=test\2024\curated\day18_animated.hs & set PACKAGES=--package containers-0.6.7 --package ansi-terminal-0.11.5
if "%day_choice%"=="20" set ANIM_PATH=test\2024\curated\day20_animated.hs & set PACKAGES=--package containers-0.6.7 --package ansi-terminal-0.11.5

call :input_select
echo.
echo Running 2024 Day %DAY% with %INPUT_TYPE% data...
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
echo Enter day number (1-21) or 0 to go back
echo.
set /p day_choice="Day: "

if "%day_choice%"=="0" goto main_menu
set DAY=%day_choice%
if %DAY% LSS 10 set DAY=0%day_choice%
set ANIM_PATH=test\2023\day%DAY%\animated\day%DAY%_animated.hs
set PACKAGES=--package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5

call :input_select
echo.
echo Running 2023 Day %DAY% with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc %PACKAGES% %ANIM_PATH% %INPUT_TYPE%
pause
goto year2023

:year2022
cls
echo ========================================
echo   Advent of Code 2022 - Animations
echo ========================================
echo.
echo Enter day number (1-25) or 0 to go back
echo.
set /p day_choice="Day: "

if "%day_choice%"=="0" goto main_menu
set DAY=%day_choice%
if %DAY% LSS 10 set DAY=0%day_choice%
set ANIM_PATH=test\2022\day%DAY%\animated\day%DAY%_animated.hs
set PACKAGES=--package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5

call :input_select
echo.
echo Running 2022 Day %DAY% with %INPUT_TYPE% data...
echo.
stack --resolver lts-21.22 runghc %PACKAGES% %ANIM_PATH% %INPUT_TYPE%
pause
goto year2022

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
