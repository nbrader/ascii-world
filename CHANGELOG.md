# Changelog

All notable changes to the ascii-world library will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Comprehensive test suite with 90+ tests
  - 40+ unit tests for Mask module operations
  - 20+ QuickCheck property-based tests
  - 30+ integration tests for WalkableWorld module (end-to-end functionality)
    - Flood-fill and region detection tests
    - Edge and perimeter counting verification
    - Side counting (connected edges) tests
    - Real-world AoC example validation
    - Coordinate system verification
    - Bitwise operations on regions
    - Edge case handling
- Complete usage documentation (USAGE.md)
  - Module guides for Mask.hs, AsciiWorld.hs, WalkableWorld.hs
  - 5 common usage patterns with working code examples
  - Performance tips and FAQ section
- Professional README.md with badges and examples
- CHANGELOG.md for tracking version history
- Example programs and tutorials:
  - Day 4 solution (word search with multi-directional pattern matching)
  - Day 6 solution (guard patrol with movement simulation and cycle detection)
  - Day 8 solution (resonant collinearity with antinode calculations)
  - Day 10 solution (topographic pathfinding)
  - Day 12 solution (region detection and perimeter counting)
  - Day 14 solution (robot simulation with wrapping coordinates and pattern detection)
  - Step-by-step tutorial examples in examples/ directory:
    - 01-basic-grid.hs: Grid creation and coordinate system
    - 02-region-detection.hs: Connected component analysis
    - 03-bitwise-operations.hs: Mask manipulation and collision detection
    - 04-complete-solution.hs: Full AoC problem workflow
  - Comprehensive examples/README.md with learning paths and expected outputs
- Comprehensive Haddock documentation
  - Module-level documentation with usage examples
  - Function-level documentation with examples and complexity notes
  - All exported functions documented with clear examples
  - Doctest-compatible examples throughout
  - Added maskToPoints utility function with documentation
- Performance benchmark suite (bench/)
  - Criterion-based benchmarks comparing bitwise vs naive implementations
  - Tests for union, intersection, overlap check, difference, and movement operations
  - Three data sizes: small (10×10), medium (50×50), large (200×200)
  - Validates "10-100x speedup" claim from README
  - Comprehensive benchmark documentation and usage guide
- Developer documentation
  - CONTRIBUTING.md with setup, coding standards, and PR process
  - GitHub issue templates (bug report, feature request, question)
  - Pull request template with comprehensive checklist
  - Clear contribution guidelines and code of conduct
- Better error handling throughout library
  - Replaced unsafe `fromJust` calls with safe alternatives
  - Added `lookupMaskOrError` helper with informative error messages
  - All critical functions now provide context in error messages

### Changed
- **API Compatibility**: Updated from old API to current
  - `WorldKey` → `MaskOrPointsIndex`
  - `WKMask` → `MaskIndex`
  - `WKPoints` → `PointsIndex`
  - `maskKeys` → `maskIndices`
  - `filterMaskKeysInWW` → `filterMaskIndicesInWW`
  - `nameZOrder` → `indexZOrder`
- Improved error messages in `applyMask` function
- Added `Show mk` constraint to `applyMask` for better error reporting
- Enhanced test suite structure with modular test files

### Deprecated
- `modifyAsAsciiWorld` function in WalkableWorld.hs
  - Function has incorrect key mapping logic
  - Added deprecation pragma with clear warning message
  - Documented the problem and provided alternative approaches
  - Function kept for backward compatibility but will be removed in v1.0.0

### Fixed
- Fixed API compatibility issues in test files
  - test/day12.hs now compiles with current API
  - test/Spec.hs uses correct MaskOrPointsIndex types
- Replaced 13 unsafe `fromJust` calls in WalkableWorld.hs
  - `totalHorizontalEdgesOverPoints`
  - `totalVerticalEdgesOverPoints`
  - `totalConnectedHorizontalEdges`
  - `totalConnectedVerticalEdges`
  - `totalConnectedOneSidedHorizontalEdges`
  - `totalConnectedOneSidedVerticalEdges`
  - `totalPoints`
  - `partitionMaskByReachableLRDU` (multiple locations)
- Improved error handling in AsciiWorld.hs `applyMask` function

## [0.1.0.0] - 2024-11-13

### Added
- Initial library implementation
- Core modules:
  - `Mask.hs` - Bitwise operations on Integer grids
  - `AsciiWorld.hs` - Grid management with named layers
  - `WalkableWorld.hs` - High-level algorithms (flood-fill, region detection)
  - `Util.hs` - Utility functions (directions, helpers)
- Basic functionality:
  - Point-Index conversion
  - Bitwise mask operations (AND, OR, XOR, subtract)
  - Overlap detection
  - Mask movement and transformation
  - Grid reading and display
  - Connected component detection via flood-fill
  - Edge and perimeter counting
  - Side counting (connected one-sided edges)
- Example solutions for Advent of Code 2024:
  - Day 12 (Garden Groups) - region detection
  - Basic test infrastructure

### Known Issues
- `modifyAsAsciiWorld` function has incorrect behavior
- Limited test coverage (only 2 basic tests)
- No property-based testing
- Error messages not always informative (unsafe `fromJust` usage)
- API inconsistency (old WorldKey vs new MaskOrPointsIndex)

## Version History

### Current Development Status

The library is actively developed for solving Advent of Code 2024 puzzles. Current focus:
- Improving test coverage to 80%+
- Adding comprehensive documentation
- Fixing API compatibility issues
- Enhancing error handling

### Compatibility Notes

**Breaking Changes from 0.1.0.0 to Unreleased:**
- API types changed (`WorldKey` → `MaskOrPointsIndex`)
- Function names changed (`maskKeys` → `maskIndices`)
- Some unexported functions may have different signatures

If you're upgrading from 0.1.0.0:
1. Update all `WorldKey` usages to `MaskOrPointsIndex`
2. Replace `WKMask` with `MaskIndex` and `WKPoints` with `PointsIndex`
3. Update function names (`maskKeys` → `maskIndices`, etc.)
4. Review error handling (errors now include more context)

### Migration Guide

#### From 0.1.0.0 to Current

```haskell
-- OLD (0.1.0.0)
import WalkableWorld (WorldKey(..))
charMap 'X' = Just (WKMask "wall")
let keys = maskKeys world

-- NEW (Current)
import WalkableWorld (MaskOrPointsIndex(..))
charMap 'X' = Just (MaskIndex "wall")
let keys = maskIndices world
```

## Future Plans

### Planned for v0.2.0.0
- [ ] Remove deprecated `modifyAsAsciiWorld` function
- [ ] Add benchmarking suite
- [ ] Optimize flood-fill algorithm
- [ ] Add more usage examples
- [ ] Improve documentation with more diagrams

### Planned for v1.0.0
- [ ] Stabilize API
- [ ] Publish to Hackage
- [ ] Add comprehensive Haddock documentation
- [ ] Performance optimization pass
- [ ] Support for non-rectangular grids
- [ ] 3D grid support (experimental)

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on how to contribute.

## Links

- [GitHub Repository](https://github.com/nbrader/ascii-world)
- [Issue Tracker](https://github.com/nbrader/ascii-world/issues)
- [Usage Documentation](USAGE.md)
- [API Reference](src/)
