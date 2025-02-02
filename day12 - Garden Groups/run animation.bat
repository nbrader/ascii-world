@echo off
setlocal

set EXECUTABLE=WalkableWorld.exe
set SOURCE=WalkableWorld.hs

:compile
rem Compile using stack
stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3 --package ansi-terminal-0.11.5 -- "%SOURCE%" -O2

rem Check if compilation was successful
if %ERRORLEVEL% neq 0 (
    echo Compilation failed.
    exit /b %ERRORLEVEL%
)

rem Ensure the executable exists before running it
if not exist "%EXECUTABLE%" (
    echo Executable not found.
    exit /b 1
)

:run
set /p RESTART="Make sure to zoom out using Ctrl + Scroll Wheel for better visibility. Press Enter when you are ready to start playing..."
echo Running %EXECUTABLE%...
.\%EXECUTABLE%

set /p RESTART="Do you want to restart? (y/n): "
if /I "%RESTART%" == "y" goto run

endlocal
