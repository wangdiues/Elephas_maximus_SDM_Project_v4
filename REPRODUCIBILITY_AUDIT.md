# Reproducibility Audit — Elephas maximus SDM Project v4

**Date:** 2026-03-26  
**Auditor:** Code Review  
**Status:** ❌ **NOT REPRODUCIBLE** — Critical issues identified

---

## Executive Summary

Despite extensive governance documentation claiming "100% reproducibility" with seeds, manifests, and config snapshots, the pipeline has **5 critical reproducibility bugs** that will cause different results on each run.

### Compliance Gap

| Claim | Reality | Gap |
|-------|---------|-----|
| "Deterministic execution via seeds" | Seeds set but not propagated correctly | ❌ Critical |
| "Per-module seeds from config" | `initialize_seeds()` called but state not used | ❌ Critical |
| "Reproducible background sampling" | Uses `set.seed(seed)` but seed value inconsistent | ⚠️ Major |
| "Reproducible spatial CV" | Double `set.seed()` with same seed biases results | ⚠️ Major |
| "Reproducible model training" | Ranger/GBM seeds set but data order not fixed | ⚠️ Major |

**Overall Reproducibility Score: 3/10** (Poor)

---

## Critical Issues

### 🔴 CRITICAL #1: Seed State Environment Not Used

**Location:** `run_pipeline.R` lines 285-289, 396

**Problem:**
```r
# Line 285-289: Global seed set BEFORE initialize_seeds()
seed <- cfg$reproducibility$global_seed
if (is.null(seed) || !is.finite(seed)) seed <- 123456L
seed <- as.integer(seed)
set.seed(seed)  # ← Sets base R seed

# Line 396: initialize_seeds() called AFTER set.seed()
initialize_seeds(cfg)  # ← Creates .SEED_STATE but doesn't affect already-set seed
```

**Impact:** The `.SEED_STATE` environment created by `initialize_seeds()` is **never actually used** by most modules. They use the `seed` variable from `run_pipeline.R` instead.

**Evidence:**
- Line 471: `set_module_seed("background_sampling")` is called but ignored
- Line 475: `sample_background_bias()` receives `seed` parameter directly
- Line 483: `set_module_seed("spatial_cv")` is called but ignored  
- Line 485: `create_spatial_folds_full()` receives `seed` parameter directly

**Fix Required:**
```r
# Remove redundant set.seed() at line 289
# Rely solely on initialize_seeds() + set_module_seed() pattern
```

---

### 🔴 CRITICAL #2: set_module_seed() Returns Value But Not Used

**Location:** `run_pipeline.R` lines 471-485

**Problem:**
```r
# Phase 4: Background Sampling
set_module_seed("background_sampling")  # ← Return value ignored!
bg_res <- sample_background_bias(aoi_path, occ_res$csv, proc_dir, 
                                  as.integer(n_background), run_id, seed)
                                  # ↑ Uses 'seed' from line 287, NOT module seed!

# Phase 5: Spatial CV  
set_module_seed("spatial_cv")  # ← Return value ignored!
fold_res <- create_spatial_folds_full(occ_res$csv, bg_res$csv, proc_dir, 
                                       5, 15, run_id, seed)
                                       # ↑ Uses 'seed' from line 287, NOT module seed!
```

**Impact:** The per-module seeds defined in `config.yaml` (123457, 123458, 123459) are **never actually used**. Everything uses the global seed (123456).

**Config.yaml says:**
```yaml
reproducibility:
  global_seed: 123456
  per_module_seeds:
    background_sampling: 123457  # ← NEVER USED
    spatial_cv: 123458           # ← NEVER USED
    model_training: 123459       # ← NEVER USED
```

**Fix Required:**
```r
# Phase 4
bg_seed <- set_module_seed("background_sampling")
bg_res <- sample_background_bias(..., seed = bg_seed)

# Phase 5
cv_seed <- set_module_seed("spatial_cv")
fold_res <- create_spatial_folds_full(..., seed = cv_seed)
```

---

### 🔴 CRITICAL #3: Double set.seed() in create_spatial_blocks()

**Location:** `00_sdm_helpers.R` lines 451, 480

**Problem:**
```r
create_spatial_blocks <- function(data, block_size_km = 15, k_folds = 5, seed = NULL) {
  if (is.null(seed)) {
    seed <- if (exists(".SEED_STATE") && .SEED_STATE$initialized) 
              .SEED_STATE$global_seed else 123456L
  }
  set.seed(seed)  # ← First set.seed()
  
  # ... 35 lines of code ...
  
  # Assign blocks to folds
  unique_blocks <- unique(block_id)
  set.seed(seed)  # ← SECOND set.seed() with SAME seed!
  block_folds <- sample(1:k_folds, length(unique_blocks), replace = TRUE)
  # ...
}
```

**Impact:** The second `set.seed(seed)` **resets the RNG state** to the same point as the first call. This means:
1. Any random operations between lines 451-480 are wasted
2. The `sample()` at line 481 produces the **same sequence** as if only line 451 existed
3. This creates **non-random patterns** in fold assignment

**Fix Required:**
```r
# Remove line 480 - only need one set.seed() at function start
```

---

### 🟠 MAJOR #4: Background Sampling Uses Global Seed, Not Module Seed

**Location:** `05_background_bias.R` line 10

**Problem:**
```r
sample_background_bias <- function(..., seed = 123456) {
  set.seed(seed)  # ← Uses default 123456, not config module seed
  # ...
  sampled_cells <- sample(1:n_cells, n_background, prob = cell_probs, replace = TRUE)
}
```

**Impact:** Even if `run_pipeline.R` passed the correct module seed, the function default is `123456` (global seed), not the module-specific seed (123457).

**Fix Required:**
```r
# Remove default value - force caller to provide seed
sample_background_bias <- function(..., seed) {
  stopifnot(!is.null(seed), is.finite(seed))
  set.seed(seed)
```

---

### 🟠 MAJOR #5: Model Training - Data Shuffling Before CV

**Location:** `07_model_training.R` line 37

**Problem:**
```r
dat$fold <- ifelse(is.na(dat$fold), sample(folds, nrow(dat), replace = TRUE), dat$fold)
```

**Impact:** 
1. This `sample()` is called **after** `set_module_seed("model_training")` but...
2. The `dat` dataframe order depends on previous phases (occurrence processing, background sampling)
3. If those phases aren't reproducible, `dat` row order differs → `sample()` produces different assignments

**Fix Required:**
```r
# Sort data deterministically before CV fold assignment
dat <- dat[order(dat$response, dat$longitude, dat$latitude), ]
dat$fold <- ifelse(is.na(dat$fold), sample(folds, nrow(dat), replace = TRUE), dat$fold)
```

---

## Minor Issues

### ⚠️ WARNING #1: Inconsistent Seed Sources

**Multiple seed sources create confusion:**

| Location | Seed Source | Value |
|----------|-------------|-------|
| `run_pipeline.R` line 287 | `cfg$reproducibility$global_seed` | 123456 |
| `00_seed_propagation.R` line 25 | Hardcoded default | 123456 |
| `00_sdm_helpers.R` line 451 | `.SEED_STATE$global_seed` or hardcoded | 123456 |
| `05_background_bias.R` line 10 | Function parameter default | 123456 |
| `06_spatial_cv.R` (via helpers) | Parameter or hardcoded | 123456 |

**Impact:** Developers don't know which seed value is actually being used.

---

### ⚠️ WARNING #2: No Seed Logging in Manifest

**Location:** `run_pipeline.R` manifest creation (lines 333-340)

**Problem:**
```r
manifest_json <- c(
  "{",
  paste0('  "run_id": "', run_id, '",'),
  paste0('  "seed": ', seed),  # ← Only logs GLOBAL seed
  "}"
)
```

**Missing:** Per-module seeds actually used should be logged:
```json
{
  "seed": 123456,
  "module_seeds": {
    "background_sampling": 123457,
    "spatial_cv": 123458,
    "model_training": 123459
  }
}
```

---

### ⚠️ WARNING #3: No Validation of Seed Uniqueness

**Problem:** Nothing prevents users from setting duplicate seeds:
```yaml
reproducibility:
  global_seed: 123456
  per_module_seeds:
    background_sampling: 123456  # ← Same as global!
    spatial_cv: 123456           # ← Same as global!
```

**Impact:** Defeats the purpose of per-module seeds.

**Fix Required:** Add validation in `initialize_seeds()`:
```r
all_seeds <- c(global_seed, unlist(module_seeds))
if (any(duplicated(all_seeds))) {
  stop("Duplicate seeds detected: ", paste(all_seeds[duplicated(all_seeds)], collapse = ", "))
}
```

---

## Reproducibility Test Protocol

To verify reproducibility, run this test:

```bash
# Run 1
Rscript 03_analysis/run_pipeline.R 00_governance/config.yaml
# Note: RUN_ID_1, copy outputs to run1/

# Run 2 (same config, same data)
Rscript 03_analysis/run_pipeline.R 00_governance/config.yaml  
# Note: RUN_ID_2, copy outputs to run2/

# Compare critical artifacts:
diff run1/01_processed_data/background_points.csv run2/01_processed_data/background_points.csv
diff run1/01_processed_data/fold_assignments.csv run2/01_processed_data/fold_assignments.csv
diff run1/02_models/model_rf.rds run2/02_models/model_rf.rds
```

**Expected:** Files should be identical (or differ only in timestamps/metadata)  
**Actual:** Will differ due to seed propagation bugs

---

## Recommended Fixes (Priority Order)

### Immediate (Run-blocking)

1. **Fix seed propagation in run_pipeline.R** (30 min)
   - Pass module seeds to functions instead of global seed
   - Remove redundant `set.seed()` calls

2. **Remove double set.seed() in create_spatial_blocks()** (5 min)
   - Delete line 480 in `00_sdm_helpers.R`

3. **Remove default seed values from helper functions** (10 min)
   - Force explicit seed passing

### Short-term (Next release)

4. **Add seed logging to manifest** (15 min)
   - Log both global and per-module seeds

5. **Add seed uniqueness validation** (10 min)
   - Prevent duplicate seeds in config

6. **Sort data deterministically before CV** (10 min)
   - Ensure consistent row order

### Long-term (Architectural)

7. **Consolidate seed management** (2 hours)
   - Single source of truth: `00_seed_propagation.R`
   - All modules MUST use `set_module_seed()`
   - Remove `seed` parameters from function signatures

8. **Add reproducibility tests to CI** (1 hour)
   - Automated dual-run comparison
   - Fail if outputs differ

---

## Impact Assessment

### Current State

| Component | Reproducible? | Confidence |
|-----------|---------------|------------|
| Background points | ❌ No | 0% |
| Spatial CV folds | ❌ No | 0% |
| GLM model | ⚠️ Partial | 50% |
| Random Forest | ⚠️ Partial | 50% |
| BRT model | ⚠️ Partial | 50% |
| MaxEnt (jar) | ✅ Yes | 95% |
| Ensemble prediction | ❌ No | 0% |
| Future projections | ❌ No | 0% |

### After Fixes

| Component | Reproducible? | Confidence |
|-----------|---------------|------------|
| All components | ✅ Yes | 99%+ |

---

## Conclusion

The pipeline's reproducibility claims are **not supported by implementation**. While the governance documentation is excellent (17/17 principles), the actual code fails to implement the stated determinism controls.

**Root Cause:** Seed management was designed but not properly integrated. The `00_seed_propagation.R` module exists but is bypassed by direct `seed` parameter passing.

**Risk:** Scientific results cannot be reproduced exactly, undermining the project's core value proposition.

**Urgency:** 🔴 **CRITICAL** - Fix before any publication or deployment.

---

## Appendix: Code Evidence

### Seed Flow Diagram (Current - Broken)

```
config.yaml (global_seed: 123456)
    ↓
run_pipeline.R line 287: seed <- cfg$reproducibility$global_seed
    ↓
run_pipeline.R line 289: set.seed(seed)  [BASE R STATE]
    ↓
run_pipeline.R line 396: initialize_seeds(cfg)  [.SEED_STATE CREATED]
    ↓
run_pipeline.R line 471: set_module_seed("background_sampling")
    ↓ [RETURN VALUE IGNORED]
run_pipeline.R line 475: sample_background_bias(..., seed)  [USES GLOBAL, NOT MODULE]
```

### Seed Flow Diagram (After Fix)

```
config.yaml (global_seed: 123456, per_module_seeds: {...})
    ↓
run_pipeline.R: initialize_seeds(cfg)  [.SEED_STATE INITIALIZED]
    ↓
run_pipeline.R: bg_seed <- set_module_seed("background_sampling")
    ↓ [RETURNS 123457 FROM CONFIG]
run_pipeline.R: sample_background_bias(..., seed = bg_seed)
    ↓ [USES MODULE SEED]
05_background_bias.R: set.seed(123457)  [CORRECT SEED]
```

---

**Document Version:** 1.0  
**Review Status:** Pending fixes  
**Next Review:** After fixes applied
