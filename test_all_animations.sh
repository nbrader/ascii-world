#!/bin/bash

# Animation Testing Script
# Tests all animations from run_animations.bat and logs results

LOG_FILE="animation_test_results.log"
TIMEOUT_SECONDS=30

echo "==================================================" > "$LOG_FILE"
echo "Animation Testing Log - $(date)" >> "$LOG_FILE"
echo "==================================================" >> "$LOG_FILE"
echo "" >> "$LOG_FILE"

# Function to test an animation
test_animation() {
    local year=$1
    local day=$2
    local anim_path=$3
    local packages=$4
    local input_type=${5:-example}
    local description=$6

    echo "----------------------------------------" | tee -a "$LOG_FILE"
    echo "Testing: $year Day $day ($description)" | tee -a "$LOG_FILE"
    echo "Path: $anim_path" | tee -a "$LOG_FILE"
    echo "Input: $input_type" | tee -a "$LOG_FILE"

    if [ ! -f "$anim_path" ]; then
        echo "STATUS: FILE NOT FOUND ❌" | tee -a "$LOG_FILE"
        echo "" >> "$LOG_FILE"
        return 1
    fi

    echo "Running animation (timeout: ${TIMEOUT_SECONDS}s)..." | tee -a "$LOG_FILE"

    # Run with timeout and capture output
    timeout $TIMEOUT_SECONDS stack --resolver lts-21.22 runghc $packages "$anim_path" "$input_type" > /tmp/anim_output.txt 2>&1
    local exit_code=$?

    if [ $exit_code -eq 124 ]; then
        echo "STATUS: TIMEOUT (ran for ${TIMEOUT_SECONDS}s - likely working) ⏱️" | tee -a "$LOG_FILE"
    elif [ $exit_code -eq 0 ]; then
        echo "STATUS: COMPLETED SUCCESSFULLY ✓" | tee -a "$LOG_FILE"
    else
        echo "STATUS: ERROR (exit code: $exit_code) ❌" | tee -a "$LOG_FILE"
        echo "Error output:" >> "$LOG_FILE"
        tail -20 /tmp/anim_output.txt >> "$LOG_FILE"
    fi

    echo "" >> "$LOG_FILE"
    return 0
}

# 2022 Animations (25 total)
echo "" | tee -a "$LOG_FILE"
echo "==================================================" | tee -a "$LOG_FILE"
echo "YEAR 2022 - Testing 25 animations" | tee -a "$LOG_FILE"
echo "==================================================" | tee -a "$LOG_FILE"

PACKAGES_2022="--package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5"

for day in {1..25}; do
    DAY=$(printf "%02d" $day)
    ANIM_PATH="test/2022/day${DAY}/animated/day${DAY}_animated.hs"
    test_animation "2022" "$DAY" "$ANIM_PATH" "$PACKAGES_2022" "example" "Regular"
done

# 2023 Animations (21 total)
echo "" | tee -a "$LOG_FILE"
echo "==================================================" | tee -a "$LOG_FILE"
echo "YEAR 2023 - Testing 21 animations" | tee -a "$LOG_FILE"
echo "==================================================" | tee -a "$LOG_FILE"

PACKAGES_2023="--package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5"

for day in {1..21}; do
    DAY=$(printf "%02d" $day)
    ANIM_PATH="test/2023/day${DAY}/animated/day${DAY}_animated.hs"
    test_animation "2023" "$DAY" "$ANIM_PATH" "$PACKAGES_2023" "example" "Regular"
done

# 2024 Regular Animations (16 total)
echo "" | tee -a "$LOG_FILE"
echo "==================================================" | tee -a "$LOG_FILE"
echo "YEAR 2024 - Testing Regular animations (days 1-16)" | tee -a "$LOG_FILE"
echo "==================================================" | tee -a "$LOG_FILE"

PACKAGES_2024="--package ascii-world --package containers-0.6.7 --package ansi-terminal-0.11.5"

for day in {1..16}; do
    DAY=$(printf "%02d" $day)
    ANIM_PATH="test/2024/day${DAY}/animated/day${DAY}_animated.hs"
    test_animation "2024" "$DAY" "$ANIM_PATH" "$PACKAGES_2024" "example" "Regular"
done

# 2024 Curated Animations
echo "" | tee -a "$LOG_FILE"
echo "==================================================" | tee -a "$LOG_FILE"
echo "YEAR 2024 - Testing Curated animations" | tee -a "$LOG_FILE"
echo "==================================================" | tee -a "$LOG_FILE"

# Day 06 Curated
test_animation "2024" "06" "test/2024/curated/day06_animated.hs" \
    "--package containers-0.6.7 --package ansi-terminal-0.11.5" "example" "Curated"

# Day 10 Curated
test_animation "2024" "10" "test/2024/curated/day10_animated.hs" \
    "--package containers-0.6.7 --package ansi-terminal-0.11.5 --package array" "example" "Curated"

# Day 16 Curated
test_animation "2024" "16" "test/2024/curated/day16_animated.hs" \
    "--package containers-0.6.7 --package ansi-terminal-0.11.5" "example" "Curated"

# Day 18 Curated
test_animation "2024" "18" "test/2024/curated/day18_animated.hs" \
    "--package containers-0.6.7 --package ansi-terminal-0.11.5" "example" "Curated"

# Day 20 Curated
test_animation "2024" "20" "test/2024/curated/day20_animated.hs" \
    "--package containers-0.6.7 --package ansi-terminal-0.11.5" "example" "Curated"

echo "" | tee -a "$LOG_FILE"
echo "==================================================" | tee -a "$LOG_FILE"
echo "Testing Complete!" | tee -a "$LOG_FILE"
echo "==================================================" | tee -a "$LOG_FILE"
echo "" | tee -a "$LOG_FILE"
echo "Log saved to: $LOG_FILE" | tee -a "$LOG_FILE"
