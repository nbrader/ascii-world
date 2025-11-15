# Day 01 Animation - Trebuchet!

Animated calibration walkthrough for Advent of Code 2023 Day 01, rendered with
the [`ascii-world`](../../../../src/AsciiWorld.hs) grid library.

## Running

```bash
stack --resolver lts-21.22 runghc \
  --package containers-0.6.7 \
  --package ansi-terminal-0.11.5 \
  test/2023/day01/animated/day01_animated.hs
```

The script defaults to the example input from the standard solution folder.
Every animation frame is drawn inside an `AsciiWorld`:

- Row 1 shows the scan cursor, Part 1 first/last digit markers, and `+`
  indicators for brand-new Part 2 matches.
- Row 2 shows the raw calibration text.
- Row 3 accumulates the Part 2 digits discovered so far.

Frame details below the grid call out newly detected digits and keep a running
tally for both puzzle parts.
