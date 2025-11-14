@echo off
setlocal enabledelayedexpansion

:: Capture current code page so we can restore it later
set "CHCP_CMD=%SystemRoot%\System32\chcp.com"
if not exist "%CHCP_CMD%" (
    echo Could not find chcp utility at %SystemRoot%\System32\chcp.com
    exit /b 1
)

for /f "tokens=2 delims=:" %%a in ('"%CHCP_CMD%"') do set "_ORIG_CP=%%a"
set "_ORIG_CP=!_ORIG_CP: =!"

echo Switching console to UTF-8 (65001) for stack output...
"%CHCP_CMD%" 65001 >nul
if errorlevel 1 (
    echo Failed to switch code page. Aborting.
    exit /b 1
)

echo Running stack test...
stack test
set "EXIT_CODE=%ERRORLEVEL%"

if defined _ORIG_CP (
    echo Restoring console code page !_ORIG_CP! ...
    "%CHCP_CMD%" !_ORIG_CP! >nul
)

exit /b %EXIT_CODE%
