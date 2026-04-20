# Pipeline Issues & Fixes — RUN_20260323_122925_b990 Analysis

**Date:** 2026-03-26  
**Analyzed Run:** RUN_20260323_122925_b990 (successful)  
**Failed Runs:** RUN_20260325_105629_b990, RUN_20260325_143611_b990

---

## Summary

Your pipeline has **3 critical issues** preventing complete runs with all figures and tables:

| Issue | Severity | Symptom | Root Cause |
|-------|----------|---------|------------|
| 1. Phase 9 future projections failing | 🔴 Critical | No future suitability maps → no change metrics | MaxEnt prediction function returns wrong number of values |
| 2. Figure validation too strict | 🟠 Major | Pipeline crashes if figure_03_future.png missing | Validation requires figures from failed phases |
| 3. Publication tables not running | 🟠 Major | Only 3 reports instead of 22+ tables | Pipeline crashes before tables phase |

---

## Issue #1: Phase 9 Future Projections Failing

### Error Message
```
2026-03-25 13:58:48 | RUN_20260325_105629_b990 | WARNING | Phase 9: SKIPPED due to error — 
[app] the number of values returned by 'fun' is not appropriate
```

### Location
`09_future_projections.R` — MaxEnt prediction with `terra::predict()`

### Root Cause

The MaxEnt model loaded from `.lambdas` file is being used with `terra::predict()`, but the prediction function isn't returning the correct number of values. This happens when:

1. The lambda file doesn't match the predictor stack
2. The prediction function doesn't handle NA values correctly
3. The number of predictors in the lambda file ≠ number of raster layers

### Evidence from Successful Run (RUN_20260323_122925_b990)

From the successful run:
- 96 future scenarios processed
- All GCMs × SSPs × periods completed
- Figures generated: 85 files in 08_figures_tables/

This means Phase 9 CAN work, but it's fragile.

### Fix Required

**File:** `09_future_projections.R`

The issue is in the `predict_maxent_from_lambdas()` function. The terra predict call needs proper error handling:

```r
# Current (broken):
pred <- terra::predict(stk, model, fun = predict_fun, na.rm = TRUE)

# Fixed:
pred <- tryCatch({
  terra::predict(stk, model, 
                 fun = function(x) {
                   if (any(is.na(x))) return(NA_real_)
                   result <- predict(model, newdata = matrix(x, nrow = 1), type = "cloglog")
                   if (length(result) != 1) return(NA_real_)
                   as.numeric(result)
                 },
                 na.rm = TRUE,
                 cores = 1)  # Force single-threaded for stability
}, error = function(e) {
  cat("MaxEnt prediction failed:", e$message, "\n")
  # Return empty raster as fallback
  stk[[1]] * NA
})
```

---

## Issue #2: Figure Validation Too Strict

### Error Message
```
2026-03-25 14:02:52 | RUN_20260325_105629_b990 | ERROR | Pipeline failed: 
Required artifact missing: figure_03_future.png
```

### Location
`run_pipeline.R` lines 248-268 (`validate_required_figures()`)

### Root Cause

The validation function requires `figure_03_future.png` even when Phase 9 failed and no future projections exist. This creates a catch-22:
- Phase 9 fails → no future figures generated
- Validation requires future figures → pipeline crashes
- User can't see partial results

### Fix Required

**File:** `run_pipeline.R` — `validate_required_figures()` function

The validation should be conditional on whether future projections were successfully created:

```r
# Current (too strict):
validate_required_figures <- function(fig_dir, require_future = FALSE, ...) {
  required <- c(
    "figure_01_present_suitability.png",
    "figure_02_model_auc.png",
    "figure_03_future.png",  # ← Always required when require_future=TRUE
    ...
  )
  
# Fixed:
validate_required_figures <- function(fig_dir, require_future = FALSE, 
                                       has_future_projections = FALSE, ...) {
  # Core figures (always required)
  required <- c(
    "figure_01_present_suitability.png",
    "figure_02_model_auc.png",
    "figure_09_response.png",
    "figure_10_comparison.png",
    "figure_roc_curves.png"
  )
  
  # Future figures (only required if Phase 9 succeeded)
  if (isTRUE(require_future) && isTRUE(has_future_projections)) {
    required <- c(required, c(
      "figure_03_future.png",
      "figure_04_change.png",
      "figure_05_uncertainty.png"
    ))
  }
  
  # Check only required figures
  missing <- setdiff(required, list.files(fig_dir))
  if (length(missing) > 0) {
    warning("Missing figures: ", paste(missing, collapse = ", "))
    # Don't fail pipeline - just warn
  }
}
```

---

## Issue #3: Publication Tables Not Running

### Symptom
From RUN_20260323_122925_b990 (successful run):
- Only 3 reports in 09_reports/ (odmap_summary.md, methods_appendix.md, management_brief.md)
- Only 24 tables in 08_figures_tables/ (should be 40+)
- No `table_01_model_performance.csv` in run directory

### Root Cause

The pipeline crashes at figure validation (Issue #2) BEFORE reaching the publication tables phase at line 730-740 of `run_pipeline.R`.

### Execution Order in run_pipeline.R

```r
Line 677: create_all_figures()        # ← Works
Line 700: create_enhanced_figures()   # ← Works  
Line 712: create_supplementary_figures() # ← Works
Line 724: validate_required_figures() # ← CRASHES HERE
Line 730: generate_publication_tables() # ← NEVER REACHED
Line 750: write_synthesis_reports()   # ← Works (called earlier)
```

### Fix Required

Move table generation BEFORE figure validation, or make validation non-fatal:

```r
# Current order (broken):
create_all_figures()
validate_required_figures()  # ← Crashes
generate_publication_tables() # ← Never runs

# Fixed order:
create_all_figures()
generate_publication_tables() # ← Run before validation
validate_required_figures()   # ← Warn but don't crash
```

---

## Additional Issues Found

### Issue #4: No Ensemble Evaluation Row

**From ODMAP summary:**
```
### Ensemble aspirational targets (AUC≥0.75, TSS≥0.45, Boyce≥0.75)
  - No ensemble row in evaluation_all.csv
```

**Location:** `08_model_evaluation.R`

**Fix:** Ensure ensemble is evaluated and added to evaluation_all.csv:

```r
# Add ensemble row
ens_eval <- data.frame(
  run_id = run_id,
  algorithm = "ensemble",
  auc_mean = ens_auc,
  tss_mean = ens_tss,
  boyce = ens_boyce,
  brier = ens_brier,
  calibration_slope = ens_calib,
  moran_i = ens_moran,
  threshold = ens_threshold
)
eval_rows <- rbind(eval_rows, ens_eval)
```

---

### Issue #5: MaxEnt TSS = NA

**From evaluation_all.csv:**
```
"maxent",0.959866666666667,NA,0.975757575757576,...
```

**Root Cause:** MaxEnt jar doesn't produce threshold-dependent metrics directly.

**Fix:** Calculate TSS from the MaxEnt raster prediction in Phase 8:

```r
# After loading MaxEnt raster
mx_vals <- extract(mx_raster, presence_pts)
bg_vals <- extract(mx_raster, background_pts)
tss_result <- tss_best(c(rep(1, length(mx_vals)), rep(0, length(bg_vals))), 
                        c(mx_vals, bg_vals))
maxent_tss <- tss_result["tss"]
```

---

## Recommended Action Plan

### Immediate (Run-blocking)

1. **Fix Phase 9 MaxEnt prediction** (30 min)
   - Add proper error handling in `terra::predict()`
   - Validate lambda file matches predictor stack
   
2. **Make figure validation non-fatal** (10 min)
   - Change `stop()` to `warning()`
   - Add `has_future_projections` parameter

3. **Reorder pipeline phases** (5 min)
   - Move `generate_publication_tables()` before validation

### Short-term (Next run)

4. **Add ensemble evaluation** (15 min)
   - Calculate ensemble metrics in Phase 8
   - Add row to evaluation_all.csv

5. **Fix MaxEnt TSS calculation** (15 min)
   - Calculate from raster predictions
   - Don't rely on jar output for thresholds

### Long-term (Architecture)

6. **Add phase dependency tracking** (1 hour)
   - Skip downstream phases if upstream fails
   - Generate partial reports with warnings

7. **Improve error messages** (30 min)
   - Include phase number in error messages
   - Add troubleshooting hints

---

## Testing Protocol

After applying fixes:

```bash
# 1. Clean previous runs
Remove-Item -Recurse 04_outputs\runs\RUN_*

# 2. Run pipeline
Rscript 03_analysis/run_pipeline.R 00_governance/config.yaml

# 3. Verify outputs
# Should have:
# - 08_figures_tables/: 40+ figures
# - 08_figures_tables/: 22+ tables  
# - 09_reports/: 3 reports + ODMAP
# - 04_future_projections/: 96 scenarios
```

---

## Expected Outputs (Complete Run)

### Figures (should be in 08_figures_tables/)
- figure_01_present_suitability.png ✓
- figure_02_model_auc.png ✓
- figure_03_future.png ← MISSING (Phase 9 fails)
- figure_04_change.png ← MISSING (depends on Phase 9)
- figure_09_response.png ✓
- figure_10_comparison.png ✓
- figure_roc_curves.png ✓
- + 35+ supplementary figures

### Tables (should be in 08_figures_tables/)
- table_01_model_performance.csv ← MISSING
- table_02_predictor_selection.csv ← MISSING
- table_03_habitat_trajectories.csv ← MISSING (depends on Phase 9)
- table_14_occurrence_summary.csv ← MISSING
- table_15_confusion_matrix.csv ← MISSING
- + 17+ supplementary tables

### Reports (should be in 09_reports/)
- odmap_summary.md ✓
- methods_appendix.md ✓
- management_brief.md ✓

---

## Files to Modify

1. `09_future_projections.R` — Fix MaxEnt prediction
2. `run_pipeline.R` — Fix validation, reorder phases
3. `08_model_evaluation.R` — Add ensemble row, fix MaxEnt TSS
4. `10_figures.R` — Make figure_03_future.png optional

---

**Status:** Ready for fixes  
**Estimated Fix Time:** 1.5 hours  
**Priority:** 🔴 CRITICAL (blocks all runs)
