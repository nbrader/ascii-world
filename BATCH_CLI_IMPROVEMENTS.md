# Batch CLI Improvements for run_animations.bat

This document proposes improvements to `run_animations.bat` based on analysis of its current implementation and the animation test results.

## Current Strengths

The current batch file has several good features:
- ✓ Clear hierarchical menu structure (Year → Day)
- ✓ Shows available animation counts per year
- ✓ Input type selection (example/data/example2)
- ✓ Intelligent routing to curated animations for specific days
- ✓ Consistent package management
- ✓ Clear visual separators and prompts

## Proposed Improvements

### 1. Fix Incorrect Animation Count for 2024

**Issue:** Line 11 shows "17 available" but there are actually 18 unique days with animations.

**Fix:**
```batch
REM Current (line 11):
echo   1. 2024 Animations (17 available)

REM Proposed:
echo   1. 2024 Animations (18 available)
```

**Explanation:** Days 1-16 have regular animations, plus days 18 and 20 have curated-only animations.

### 2. Add Input Validation for Day Numbers

**Issue:** No validation when user enters invalid day numbers (e.g., day 99, day -1).

**Fix:** Add validation after day input for each year:
```batch
:year2024
cls
echo ========================================
echo   Advent of Code 2024 - Animations
echo ========================================
echo.
echo Enter day number (1-16, 18, 20) or 0 to go back
echo.
set /p day_choice="Day: "

if "%day_choice%"=="0" goto main_menu

REM Add validation
if %day_choice% LSS 1 (
    echo Invalid day number! Please enter a valid day.
    pause
    goto year2024
)
if %day_choice% GTR 25 (
    echo Invalid day number! Please enter a valid day.
    pause
    goto year2024
)

REM Continue with existing logic...
```

### 3. Add File Existence Check

**Issue:** Batch file doesn't check if animation file exists before running, leading to confusing stack errors.

**Fix:** Add check before running stack command:
```batch
if not exist "%ANIM_PATH%" (
    echo.
    echo ERROR: Animation not found at: %ANIM_PATH%
    echo This animation may not be available yet.
    echo.
    pause
    goto year2024
)

echo Running 2024 Day %DAY% with %INPUT_TYPE% data...
```

### 4. Update Day Range Prompts

**Issue:** Prompts claim all days are available, but some years have fewer animations.

**Fix:**
```batch
REM 2024 (line 32):
echo Enter day number (1-16, 18, 20) or 0 to go back

REM 2023 (line 63): [Already correct]
echo Enter day number (1-21) or 0 to go back

REM 2022 (line 87): [Already correct]
echo Enter day number (1-25) or 0 to go back
```

### 5. Show When Curated Version is Used

**Issue:** Users don't know when a curated vs regular animation will run.

**Fix:** Add informational messages:
```batch
if "%day_choice%"=="6" (
    set ANIM_PATH=test\2024\curated\day06_animated.hs
    set PACKAGES=--package containers-0.6.7 --package ansi-terminal-0.11.5
    echo [Using curated animation for Day 06]
)
if "%day_choice%"=="10" (
    set ANIM_PATH=test\2024\curated\day10_animated.hs
    set PACKAGES=--package containers-0.6.7 --package ansi-terminal-0.11.5 --package array
    echo [Using curated animation for Day 10]
)
```

### 6. Add Error Handling After Stack Command

**Issue:** If stack fails, script continues without indicating failure clearly.

**Fix:**
```batch
stack --resolver lts-21.22 runghc %PACKAGES% %ANIM_PATH% %INPUT_TYPE%

if %ERRORLEVEL% NEQ 0 (
    echo.
    echo Animation exited with error code %ERRORLEVEL%
    echo See error messages above for details.
    echo.
)
pause
goto year2024
```

### 7. Reduce Package Specification Duplication

**Issue:** Package lists are repeated throughout the file.

**Fix:** Define common packages once:
```batch
@echo off
setlocal enabledelayedexpansion

REM Define common package sets
set BASE_PACKAGES=--package containers-0.6.7 --package ansi-terminal-0.11.5
set ASCII_PACKAGES=--package ascii-world %BASE_PACKAGES%

REM Then use:
set PACKAGES=%ASCII_PACKAGES%

REM Or for curated:
set PACKAGES=%BASE_PACKAGES%
```

### 8. Add "Run All" Batch Mode

**Issue:** No easy way to run all animations sequentially for testing.

**Fix:** Add new menu option:
```batch
:main_menu
cls
echo ========================================
echo   Advent of Code - All Animations
echo ========================================
echo.
echo Select Year:
echo   1. 2024 Animations (18 available)
echo   2. 2023 Animations (21 available)
echo   3. 2022 Animations (25 available)
echo   4. Run All Animations (Test Mode)
echo   0. Exit
echo.
set /p year_choice="Enter choice (0-4): "

if "%year_choice%"=="4" goto run_all_mode
```

Then implement `run_all_mode` label to loop through all animations.

### 9. Add Timing Information

**Issue:** Users don't know how long animations take.

**Fix:** Use PowerShell to measure time:
```batch
echo Running 2024 Day %DAY% with %INPUT_TYPE% data...
echo.

powershell -Command "$start = Get-Date; & { stack --resolver lts-21.22 runghc %PACKAGES% '%ANIM_PATH%' '%INPUT_TYPE%' }; $duration = (Get-Date) - $start; Write-Host \"`nCompleted in $($duration.TotalSeconds.ToString('0.0')) seconds\" -ForegroundColor Green"

pause
```

### 10. Add Input File Availability Check

**Issue:** Users don't know which input files (example/data/example2) exist before selecting.

**Fix:** Show available inputs:
```batch
:input_select
echo.
echo Select input data:

if exist "test\%YEAR%\day%DAY%\day%DAY% (example).csv" (
    echo   1. Example data [Available]
) else (
    echo   1. Example data [Not found]
)

if exist "test\%YEAR%\day%DAY%\day%DAY% (data).csv" (
    echo   2. Actual puzzle data [Available]
) else (
    echo   2. Actual puzzle data [Not found]
)

if exist "test\%YEAR%\day%DAY%\day%DAY% (example 2).csv" (
    echo   3. Example 2 [Available]
) else (
    echo   3. Example 2 [Not found]
)

echo.
set /p input_choice="Enter choice (1-3, default 1): "
```

## Cross-Platform Support

### Issue
The `.bat` file only works on Windows, limiting accessibility for Linux/Mac users.

### Solutions

#### Option A: Create Shell Script Version
Create `run_animations.sh` for Linux/Mac with identical functionality:

```bash
#!/bin/bash

# Main menu
show_main_menu() {
    clear
    echo "========================================"
    echo "  Advent of Code - All Animations"
    echo "========================================"
    echo ""
    echo "Select Year:"
    echo "  1. 2024 Animations (18 available)"
    echo "  2. 2023 Animations (21 available)"
    echo "  3. 2022 Animations (25 available)"
    echo "  0. Exit"
    echo ""
    read -p "Enter choice (0-3): " year_choice
    # ... implementation
}
```

#### Option B: Create PowerShell Version
Create `run_animations.ps1` for cross-platform PowerShell:

```powershell
function Show-MainMenu {
    Clear-Host
    Write-Host "========================================"
    Write-Host "  Advent of Code - All Animations"
    Write-Host "========================================"
    # ... implementation
}
```

#### Option C: Create Haskell Launcher
Create a Haskell-based launcher that works everywhere:

```haskell
-- run_animations.hs
-- Platform-agnostic animation launcher
```

**Recommendation:** Create both `.sh` and `.ps1` versions to maximize accessibility.

## Enhanced User Experience

### Suggested Additions

1. **Color Coding** - Use ANSI escape codes in batch (Windows 10+):
```batch
echo [92mAnimation completed successfully![0m
echo [91mAnimation failed![0m
```

2. **Progress Indicators** - Show progress when running multiple animations:
```batch
echo Running animation 15/63...
```

3. **Save Output Option** - Allow saving animation output:
```batch
echo Would you like to save output to a file? (y/n)
set /p save_output=
if /i "%save_output%"=="y" (
    stack ... %ANIM_PATH% %INPUT_TYPE% > output_%DAY%.txt 2>&1
)
```

4. **Recently Run Menu** - Track and offer quick access to recent animations:
```batch
echo Recently run:
echo   - 2024 Day 06 (curated, example data)
echo   - 2023 Day 10 (regular, puzzle data)
```

5. **Animation Descriptions** - Show what each day's animation does:
```batch
if "%day_choice%"=="1" echo Description: Historian Hysteria - List pairing and sorting
if "%day_choice%"=="6" echo Description: Guard Gallivant - Pathfinding with obstacles
```

## Implementation Priority

### High Priority (Quick Wins)
1. ✓ Fix animation count (1 line change)
2. ✓ Add file existence check (4-5 lines)
3. ✓ Update day range prompts (3 line changes)
4. ✓ Add error handling (3-4 lines)

### Medium Priority (Valuable Features)
5. ✓ Show curated selection info (6-8 lines)
6. ✓ Add input validation (8-10 lines)
7. ✓ Reduce package duplication (refactoring)

### Low Priority (Nice to Have)
8. ✓ Add "Run All" mode (significant addition)
9. ✓ Add timing information (requires PowerShell)
10. ✓ Show input availability (complex logic)

### Future Enhancements
- Cross-platform versions (.sh, .ps1)
- Color coding
- Recently run menu
- Animation descriptions

## Testing Recommendations

After implementing improvements:

1. Test on clean Windows installation
2. Verify all menu paths work correctly
3. Test error conditions (missing files, invalid input)
4. Ensure backward compatibility
5. Test with both regular and curated animations
6. Verify all three years (2022, 2023, 2024)

## Related Documents

- `FINAL_TEST_RESULTS.md` - Animation test results showing which animations work
- `README_TEST_RESULTS.md` - Quick summary of test findings
- `test_animations.sh` - Automated testing script for all animations

## Conclusion

These improvements will make `run_animations.bat` more robust, user-friendly, and informative. The high-priority changes can be implemented quickly with minimal risk, while medium and low priority enhancements can be added incrementally based on user feedback.

The most impactful improvements are:
1. File existence checks (prevents confusing errors)
2. Error handling (clearer feedback on failures)
3. Input validation (prevents invalid selections)
4. Curated version indicators (user awareness)
5. Cross-platform support (accessibility)
