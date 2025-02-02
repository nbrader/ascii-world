@echo off
setlocal

set "EXECUTABLE=WalkableWorld.exe"
set "SOURCE=WalkableWorld.hs"

:initial
if not exist "%EXECUTABLE%" (
    echo WARNING: The executable does not exist and will be built.
    set /p dummy="Press Enter to compile the executable..."
)

:main_loop
echo Rebuilding executable...
rem Always rebuild the executable before playing
stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3 --package ansi-terminal-0.11.5 -- "%SOURCE%" -O2

rem Check if compilation was successful
if %ERRORLEVEL% neq 0 (
    echo Compilation failed.
    exit /b %ERRORLEVEL%
)

rem Verify that the executable was produced
if not exist "%EXECUTABLE%" (
    echo Compilation did not produce the expected executable.
    exit /b 1
)

set /p dummy="Make sure to zoom out using Ctrl + Scroll Wheel for better visibility. Press Enter when you are ready to start playing..."
echo Running %EXECUTABLE%...
.\%EXECUTABLE%

set /p dummy="Press Enter to restart (this will also rebuild the executable)..."
goto main_loop

endlocal
