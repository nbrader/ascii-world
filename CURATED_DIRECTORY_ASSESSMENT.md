# Assessment: 2024 Curated Directory Structure

## Current Situation

### Directory Structure

**Standard Animations** (`test/2024/dayNN/animated/`)
- Days 01-05, 07-09, 11-14: Full animations using AsciiWorld library (98-177 lines)
- Days 06, 10, 15, 16: Placeholder files (5 lines each, not implemented)

**Curated Directory** (`test/2024/curated/`)
- Animated files: Days 06, 10, 16, 18, 20 (all custom ANSI, no AsciiWorld)
- Solution files: Days 04, 06, 08, 10, 12, 14, 15, 16, 18, 20
- README files for the solutions
- Input data files for some days
- Batch files for running animations

### Key Differences

| Aspect | Standard Animations | Curated Animations |
|--------|---------------------|-------------------|
| **Technology** | AsciiWorld library | Custom ANSI terminal codes |
| **Complexity** | Simpler (mostly progress bars) | Complex algorithm visualizations |
| **File Size** | ~100-180 lines | ~170-230 lines |
| **Dependencies** | Requires ascii-world package | Only ansi-terminal |
| **Quality** | Generated/templated | Hand-crafted, polished |

---

## Problems with Current Structure

### 1. **Confusing Organization**
- Days 06, 10, 16 have **duplicate entries**:
  - Placeholder in `test/2024/dayNN/animated/`
  - Real animation in `test/2024/curated/`
- Violates DRY principle and user expectations

### 2. **Discovery Problem**
- Users naturally look in `test/2024/day06/animated/` for day 06's animation
- They find a placeholder saying "not yet implemented"
- May not discover the real animation exists in `curated/`
- No clear pointer from placeholder to curated version

### 3. **Inconsistent with 2022/2023**
- 2022: All animations in `test/2022/dayNN/animated/`
- 2023: All animations in `test/2023/dayNN/animated/`
- 2024: Animations split between standard and curated
- Breaking the established pattern

### 4. **Mixed Concerns**
- The `curated/` directory contains:
  - Animations (5 files)
  - Solutions (10 files)
  - Input data
  - Batch scripts
- Unclear if it's a "showcase", "examples", or "high-quality animations"

### 5. **Maintenance Burden**
- Two locations to check for animations
- Placeholders need to be kept in sync
- Batch files duplicated (old `run_all_animations.bat` vs root `run_animations.bat`)

---

## Recommendations

### Option A: Move Curated Animations to Standard Locations ⭐ **RECOMMENDED**

**Action:**
1. Move `curated/day06_animated.hs` → `test/2024/day06/animated/day06_animated.hs`
2. Move `curated/day10_animated.hs` → `test/2024/day10/animated/day10_animated.hs`
3. Move `curated/day16_animated.hs` → `test/2024/day16/animated/day16_animated.hs`
4. Move `curated/day18_animated.hs` → `test/2024/day18/animated/day18_animated.hs` (create directory)
5. Move `curated/day20_animated.hs` → `test/2024/day20/animated/day20_animated.hs` (create directory)
6. Delete placeholder files for days 06, 10, 15, 16
7. Repurpose `curated/` as a solutions showcase (without animations)

**Pros:**
- ✅ Consistent with 2022/2023 structure
- ✅ Single source of truth per day
- ✅ Easy discovery - users look in `dayNN/animated/` and find it
- ✅ Simpler mental model
- ✅ Easier maintenance

**Cons:**
- ❌ Loses clear distinction between "basic" and "advanced" animations
- ❌ Mixing AsciiWorld-based and ANSI-based animations in same directory tree

---

### Option B: Keep Curated, Add Better Signposting

**Action:**
1. Update placeholder READMEs to clearly point to curated versions
2. Update placeholder .hs files to say "See test/2024/curated/day06_animated.hs"
3. Create a `test/2024/curated/README.md` explaining its purpose
4. Clean up curated directory - separate solutions from animations

**Pros:**
- ✅ Maintains distinction between animation quality levels
- ✅ Keeps custom ANSI animations separate from AsciiWorld ones
- ✅ Less file movement required

**Cons:**
- ❌ Still confusing for users
- ❌ Requires users to look in two places
- ❌ Violates principle of least surprise
- ❌ Inconsistent with 2022/2023

---

### Option C: Flatten Everything into Curated

**Action:**
1. Move ALL 2024 animations into `test/2024/curated/`
2. Rename to `test/2024/animations/`
3. Update all references

**Pros:**
- ✅ Single location for all 2024 animations
- ✅ Can clearly showcase best animations

**Cons:**
- ❌ Breaks the `test/YEAR/dayNN/animated/` pattern
- ❌ Major restructuring required
- ❌ Inconsistent with 2022/2023
- ❌ Makes automated tooling harder

---

## Final Recommendation

**Adopt Option A: Move Curated Animations to Standard Locations**

### Rationale:

1. **User Experience**: Users expect animations at `test/2024/day06/animated/day06_animated.hs`
2. **Consistency**: Matches 2022 and 2023 structure
3. **Simplicity**: One animation per day, in the obvious location
4. **Discoverability**: No hidden animations in unexpected places
5. **Maintainability**: Fewer places to look, less duplication

### Implementation Plan:

1. Create missing animated directories (day17-day20)
2. Move curated animations to their respective day directories
3. Remove placeholder files
4. Update `ANIMATION_STATUS.md` to note which animations use custom ANSI vs AsciiWorld
5. Repurpose `curated/` directory:
   - Keep as a solutions showcase (non-animation files)
   - OR: Delete if not needed
   - OR: Rename to `examples/` or `showcase/` if keeping solutions

### Quality Distinction Without Separate Directory:

We can still document animation quality in `ANIMATION_STATUS.md`:
- Add "Implementation" column: "AsciiWorld" vs "Custom ANSI"
- Add "Quality" column: "Basic" vs "Advanced"
- The file size and technology used already distinguish them

---

## Decision Needed

Do you want to:
- **A)** Move curated animations to standard locations (recommended)
- **B)** Keep current structure with better documentation
- **C)** Other approach?

The current structure causes confusion and should be addressed.
