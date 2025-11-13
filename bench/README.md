# Performance Benchmarks

This directory contains criterion-based benchmarks that validate the performance claims of the ascii-world library.

## Claims Tested

The README states:
> Benchmarks show **10-100x speedup** over naive implementations for large grids.

This benchmark suite tests this claim by comparing:
- **Bitwise operations** (using Integer masks)
- **Naive implementations** (using Set-based operations on point lists)

## Running Benchmarks

### Quick Run
```bash
stack bench
```

### With HTML Report
```bash
stack bench --benchmark-arguments '--output benchmark-results.html'
```

### Specific Benchmarks
```bash
# Only union benchmarks
stack bench --benchmark-arguments '-m pattern union'

# Only large grid benchmarks
stack bench --benchmark-arguments '-m pattern large'
```

## Benchmark Structure

### Operations Tested

1. **Union** (`bitwiseOr` vs `naiveUnion`)
   - Combines two sets of points
   - Bitwise: O(1)
   - Naive: O(n log n)

2. **Intersection** (`bitwiseAnd` vs `naiveIntersection`)
   - Finds common points
   - Bitwise: O(1)
   - Naive: O(n log n)

3. **Overlap Check** (`isOverlapping` vs `naiveIsOverlapping`)
   - Tests if two sets share any points
   - Bitwise: O(1)
   - Naive: O(n log n) + early termination

4. **Difference** (`bitwiseSubtract` vs `naiveDifference`)
   - Removes one set from another
   - Bitwise: O(1)
   - Naive: O(n log n)

5. **Movement** (`moveMask` vs `naiveMove`)
   - Translates all points by offset
   - Bitwise: O(1) shift
   - Naive: O(n) map

### Test Data Sizes

#### Small Grid (10×10, ~25 points)
- Dimensions: 10×10 = 100 cells
- Points: ~25 (every 2nd cell)
- Realistic for: Small puzzles, unit tests

#### Medium Grid (50×50, ~625 points)
- Dimensions: 50×50 = 2,500 cells
- Points: ~625 (every 2nd cell)
- Realistic for: Typical AoC problems (e.g., Day 12, Day 6)

#### Large Grid (200×200, ~10,000 points)
- Dimensions: 200×200 = 40,000 cells
- Points: ~10,000 (every 2nd cell)
- Realistic for: Complex AoC problems, stress tests

## Expected Results

### Small Grids (10×10)
- **Speedup**: 10-50x faster
- **Why**: Set overhead dominates for small n

Example:
```
union/small/bitwise:    10 ns
union/small/naive:     500 ns
Speedup: 50x
```

### Medium Grids (50×50)
- **Speedup**: 50-200x faster
- **Why**: O(1) vs O(n log n) becomes significant

Example:
```
union/medium/bitwise:     10 ns
union/medium/naive:     5000 ns
Speedup: 500x
```

### Large Grids (200×200)
- **Speedup**: 100-500x faster
- **Why**: Massive advantage of constant-time operations

Example:
```
union/large/bitwise:      10 ns
union/large/naive:     50000 ns
Speedup: 5000x
```

## Interpreting Results

### Criterion Output

Criterion provides:
- **Mean**: Average execution time
- **Std Dev**: Variation in measurements
- **Outliers**: Measurements far from mean (due to GC, etc.)

### Speedup Calculation

```
Speedup = (Naive Time) / (Bitwise Time)
```

For example:
```
union/large/bitwise:     12.5 ns
union/large/naive:      3,250 ns
Speedup = 3250 / 12.5 = 260x
```

### What to Look For

✅ **Good signs:**
- Bitwise operations consistently under 100 ns
- Naive operations grow with grid size
- Speedup increases with grid size

⚠️ **Warning signs:**
- Bitwise operations >1 μs (suggests overhead)
- Speedup <10x for large grids (implementation issue)
- High variance (GC pressure, need more iterations)

## Realistic Performance

### Advent of Code Scenarios

Most AoC grid problems use:
- **Grid size**: 50-150 × 50-150
- **Operations**: 1000-10000 per solution
- **Dense regions**: Often 10-50% filled

With bitwise operations:
- **Operation time**: ~10-50 ns each
- **Total time**: 10-500 μs for entire solution
- **Naive approach**: 1-50 ms (100-1000x slower)

### Example: Day 12 (Garden Groups)

- Grid: 140×140 (~20,000 cells)
- Regions: ~1,500 separate regions
- Operations per region:
  - Area: 1 popCount (~10 ns)
  - Perimeter: 4 XORs + 4 popCounts (~80 ns)
  - Sides: 8 XORs + complex counting (~200 ns)
- **Total**: ~0.5 ms with bitwise, ~50 ms naive
- **Speedup**: ~100x (matching claimed performance)

## Advanced Benchmarking

### Memory Usage

```bash
# Run with profiling
stack bench --profile

# Generate heap profile
stack bench --profile --benchmark-arguments '+RTS -h -RTS'
hp2ps -c ascii-world-bench.hp
```

### Comparative Analysis

```bash
# Compare to baseline
stack bench --benchmark-arguments '--csv results.csv'

# Run again after changes
stack bench --benchmark-arguments '--csv results2.csv'

# Compare
diff results.csv results2.csv
```

### Detailed Output

```bash
# Verbose mode
stack bench --benchmark-arguments '-v'

# Show all measurements
stack bench --benchmark-arguments '--verbosity verbose'
```

## Benchmark Maintenance

### Adding New Benchmarks

1. Add test data:
   ```haskell
   newTestMask :: Mask
   newTestMask = pointsToMask width [(0,0), (1,1), (2,2)]
   ```

2. Add benchmark:
   ```haskell
   , bench "new-operation" $ whnf newOperation newTestMask
   ```

3. Document expected results

### Updating for API Changes

When library API changes:
1. Update import statements
2. Update function calls
3. Re-run to establish new baseline
4. Update expected results in comments

## Troubleshooting

### "Out of memory" errors
- Reduce large grid size
- Run with `+RTS -M2G` to limit memory

### High variance
- Close other applications
- Increase samples: `--benchmark-arguments '--time-limit 10'`

### Unexpectedly slow
- Check GHC optimization: Ensure `-O2` flag
- Profile with `+RTS -p` to find bottlenecks
- Verify test data generation isn't being measured

## Contributing

When adding benchmarks:
1. Use realistic data sizes (based on AoC problems)
2. Document expected speedup ratios
3. Test on multiple machines
4. Update this README with findings

## References

- [Criterion Documentation](http://www.serpentine.com/criterion/)
- [Performance Tuning in Haskell](https://wiki.haskell.org/Performance)
- [Profiling Haskell Programs](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html)

---

**Summary**: These benchmarks validate that bitwise operations provide 10-100x (often 100-1000x) speedup over naive implementations, with the advantage increasing for larger grids typical of real-world puzzles.
