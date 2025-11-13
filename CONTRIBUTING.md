# Contributing to ascii-world

Thank you for your interest in contributing to ascii-world! This document provides guidelines and instructions for contributing to the project.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [Project Structure](#project-structure)
- [Coding Standards](#coding-standards)
- [Testing Requirements](#testing-requirements)
- [Submission Guidelines](#submission-guidelines)
- [Documentation](#documentation)

## Code of Conduct

This project follows a simple code of conduct:

- Be respectful and considerate in all interactions
- Provide constructive feedback
- Focus on what is best for the project and community
- Show empathy towards other community members

## Getting Started

### Prerequisites

- **Haskell Stack**: Install from [haskellstack.org](https://www.haskellstack.org/)
- **GHC**: Version 9.2.5 or compatible (managed by Stack)
- **Git**: For version control

### Development Setup

1. **Fork and Clone**:
   ```bash
   git fork https://github.com/nbrader/ascii-world
   git clone https://github.com/YOUR-USERNAME/ascii-world
   cd ascii-world
   ```

2. **Build the Project**:
   ```bash
   stack build
   ```

3. **Run Tests**:
   ```bash
   stack test
   ```

4. **Run Examples**:
   ```bash
   # Run Day 12 solution
   stack --resolver lts-21.22 runghc --package containers --package split -- test/day12.hs

   # Run tutorial examples
   stack --resolver lts-21.22 runghc --package containers --package split -- examples/01-basic-grid.hs
   ```

## Project Structure

```
ascii-world/
├── src/                    # Library source code
│   ├── Mask.hs            # Low-level bitwise operations
│   ├── AsciiWorld.hs      # Mid-level grid management
│   ├── WalkableWorld.hs   # High-level algorithms
│   └── Util.hs            # Utility functions
├── test/                   # Test suite
│   ├── Spec.hs            # Main test entry point
│   ├── MaskSpec.hs        # Unit tests for Mask module
│   ├── PropertyTests.hs   # QuickCheck property tests
│   ├── WalkableWorldSpec.hs  # Integration tests
│   ├── day10.hs           # AoC Day 10 solution
│   └── day12.hs           # AoC Day 12 solution
├── examples/               # Tutorial examples
│   ├── README.md          # Learning path guide
│   ├── 01-basic-grid.hs   # Basic grid operations
│   ├── 02-region-detection.hs  # Connected components
│   ├── 03-bitwise-operations.hs  # Mask manipulation
│   └── 04-complete-solution.hs  # Full AoC workflow
├── README.md              # Project overview
├── USAGE.md               # Comprehensive usage guide
├── CHANGELOG.md           # Version history
└── CONTRIBUTING.md        # This file
```

### Module Architecture

The library has a three-layer architecture:

1. **Mask.hs** (Foundation): Bitwise operations on Integer types
2. **AsciiWorld.hs** (Middle): Grid management with named layers
3. **WalkableWorld.hs** (Top): High-level algorithms (flood-fill, region detection)

Each layer builds on the one below it.

## Coding Standards

### Haskell Style Guide

- **Indentation**: Use 2 or 4 spaces consistently (no tabs)
- **Line Length**: Aim for 80-100 characters maximum
- **Naming Conventions**:
  - Functions and variables: `camelCase`
  - Types and constructors: `PascalCase`
  - Constants: `camelCase`

### Code Organization

```haskell
-- Module header with Haddock documentation
{-|
Module      : ModuleName
Description : Brief description
Copyright   : (c) 2024
License     : BSD3
-}

module ModuleName
  ( -- * Main Types
    SomeType
    -- * Core Functions
  , someFunction
  ) where

-- Imports (grouped: base, containers, project modules)
import Data.Bits
import qualified Data.Map as M
import Mask

-- Type definitions
type Point = (Int, Int)

-- Functions with Haddock documentation
-- | Brief description
--
-- Detailed description with examples
--
-- ==== __Examples__
--
-- >>> someFunction 42
-- 84
someFunction :: Int -> Int
someFunction x = x * 2
```

### Documentation Requirements

All exported functions must have:

1. **Haddock comment** with description
2. **Type signature** with constraints documented
3. **Examples** using doctest syntax (where applicable)
4. **Complexity notes** for non-obvious performance characteristics

Example:

```haskell
-- | Finds all connected regions in a mask using flood-fill.
--
-- This function partitions a mask into separate connected components,
-- where connectivity is defined by the 4-directional adjacency (LRDU).
--
-- ==== __Examples__
--
-- >>> let mask = bitwiseOr (pointToMask 5 (0,0)) (pointToMask 5 (1,0))
-- >>> length (partitionMaskByReachableLRDU mask world)
-- 1  -- Single connected region
--
-- Complexity: O(n) where n is the number of set bits
partitionMaskByReachableLRDU :: Mask -> WalkableWorld mk pk -> [Mask]
```

## Testing Requirements

### Test Coverage

All contributions must maintain or improve test coverage. The project currently has:

- **90+ tests** total
- **40+ unit tests** (Mask operations)
- **20+ property tests** (QuickCheck)
- **30+ integration tests** (end-to-end)

### Writing Tests

#### Unit Tests (HSpec)

Located in `test/MaskSpec.hs`, `test/WalkableWorldSpec.hs`:

```haskell
describe "functionName" $ do
  it "does something specific" $ do
    let result = functionName input
    result `shouldBe` expected

  it "handles edge case" $ do
    functionName edgeInput `shouldBe` edgeExpected
```

#### Property Tests (QuickCheck)

Located in `test/PropertyTests.hs`:

```haskell
it "satisfies mathematical property" $
  property $ \width index ->
    width > 0 ==>
      pointToIndex width (indexToPoint width index) == index
```

#### Integration Tests

Located in `test/WalkableWorldSpec.hs`:

```haskell
it "solves real AoC problem correctly" $ do
  let input = unlines ["AAAA", "BBCD", "BBCC", "EEEC"]
  let (_, world) = readWorld charMap input
  let result = solve world
  result `shouldBe` 140
```

### Running Tests

```bash
# Run all tests
stack test

# Run with coverage report
stack test --coverage

# Run specific test file
stack test --test-arguments "-m pattern"

# Run with verbose output
stack test --test-arguments "--verbose"
```

### Test Requirements for PRs

- All existing tests must pass
- New features must include tests
- Bug fixes must include regression tests
- Aim for >80% code coverage

## Submission Guidelines

### Before Submitting

1. **Run tests**: Ensure all tests pass
   ```bash
   stack test
   ```

2. **Check formatting**: Ensure code follows style guide

3. **Update documentation**:
   - Add Haddock comments for new functions
   - Update USAGE.md if adding new patterns
   - Update CHANGELOG.md with your changes
   - Add examples if introducing new features

4. **Test examples**: Ensure tutorial examples still work
   ```bash
   stack runghc --package containers --package split -- examples/01-basic-grid.hs
   ```

### Pull Request Process

1. **Create a feature branch**:
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes**:
   - Write code following style guide
   - Add tests for new functionality
   - Update documentation

3. **Commit with clear messages**:
   ```bash
   git commit -m "Add: Brief description of feature

   - Detailed point 1
   - Detailed point 2
   - Fixes #issue-number (if applicable)"
   ```

4. **Push to your fork**:
   ```bash
   git push origin feature/your-feature-name
   ```

5. **Open a Pull Request**:
   - Provide clear title and description
   - Reference related issues
   - Explain what changed and why
   - Include example usage if applicable

### Commit Message Guidelines

Format:
```
Type: Brief summary (50 chars or less)

More detailed explanation if needed (wrap at 72 chars).
Include motivation for the change and contrast with previous behavior.

- Bullet points are okay
- Use present tense: "Add feature" not "Added feature"
- Reference issues: Fixes #123, Closes #456
```

Types:
- `Add`: New feature or functionality
- `Fix`: Bug fix
- `Update`: Improvement to existing feature
- `Refactor`: Code restructuring without behavior change
- `Docs`: Documentation changes
- `Test`: Adding or updating tests
- `Chore`: Maintenance tasks

### Code Review Process

1. Maintainer reviews PR
2. Feedback provided via comments
3. Contributor addresses feedback
4. Approval and merge when ready

Expect:
- Response within 1-2 weeks
- Constructive feedback
- Iterative improvement process

## Documentation

### Types of Documentation

1. **README.md**: Project overview and quick start
2. **USAGE.md**: Comprehensive usage guide with patterns
3. **CHANGELOG.md**: Version history and breaking changes
4. **examples/**: Step-by-step tutorials
5. **Haddock comments**: API documentation in source code

### Writing Documentation

- **Be clear and concise**
- **Include working examples**
- **Explain the "why" not just the "what"**
- **Use consistent terminology**
- **Test all code examples**

### Generating Haddock Documentation

```bash
# Generate HTML documentation
stack haddock

# View generated docs
# Open .stack-work/install/.../doc/index.html
```

## Getting Help

- **Questions**: Open a GitHub Discussion
- **Bugs**: Open a GitHub Issue
- **Features**: Open a GitHub Issue with [Feature Request] tag
- **Security**: Email maintainer directly (see README)

## Recognition

Contributors will be:
- Listed in CHANGELOG.md for their contributions
- Acknowledged in release notes
- Added to a CONTRIBUTORS file (coming soon)

## License

By contributing, you agree that your contributions will be licensed under the BSD-3-Clause License.

---

**Thank you for contributing to ascii-world!** 🎉

Every contribution, no matter how small, helps make this library better for everyone.
