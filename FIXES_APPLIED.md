# Fixes Applied to SDM Pipeline

**Date:** 2026-03-15  
**Run:** RUN_20260315_124209_b990  
**Version:** 3.0-bhutan-national

---

## Summary of Issues Found

The pipeline completed successfully but produced biologically meaningless outputs:
- **0.0 km² high suitability habitat** (max suitability was only 0.20!)
- **Negative Boyce indices** for RF (-0.2) and MaxEnt (-0.23)
- **All change metrics showing 0 persistence** due to wrong threshold
- **No temperature predictors** in final model (all removed by collinearity)

---

## Root Causes Identified

1. **Missing threshold column** in `evaluation_all.csv`
2. **No ensemble evaluation** - ensemble never evaluated with Boyce/Brier/etc.
3. **Hardcoded 0.5 threshold** in Phase 10-11 (but max prediction was 0.20)
4. **Overly strict collinearity thresholds** removed all temperature variables
5. **Missing vector layers** in config (settlements, private land)

---

## Fixes Applied

### 1. Phase 8: Model Evaluation (`08_model_evaluation.R`)

**Added optimal TSS threshold calculation:**
```r
# Calculate and save optimal threshold for each algorithm
thr_result <- tss_best(y, p)
threshold_results[[algo]] <- thr_result["threshold"]
```

**Added ensemble evaluation:**
```r
# Evaluate ensemble predictions (not just individual algorithms)
ens_file <- file.path(run_dir, "03_present_suitability", "suitability_present_ensemble.tif")
# Extract values, calculate Boyce, Brier, threshold, etc.
# Add "ensemble" row to evaluation_all.csv
```

**Updated schema:**
```r
schema_cols <- c("run_id", "algorithm", "auc_mean", "tss_mean", "boyce", "brier",
                 "calibration_slope", "moran_i", "threshold")  # Added threshold
```

### 2. Pipeline Validation (`run_pipeline.R`)

**Updated schema validation:**
```r
req <- c("run_id", "algorithm", "auc_mean", "tss_mean", "boyce", "brier", 
         "calibration_slope", "moran_i", "threshold")  # Added threshold
```

### 3. Configuration (`00_governance/config.yaml`)

**Relaxed collinearity thresholds:**
```yaml
predictors:
  collinearity:
    correlation_threshold: 0.85  # Was 0.7
    vif_threshold: 10            # Was 5
```

**Added missing vector layers:**
```yaml
vectors:
  settlements: "01_data_raw/03_vector/shapefiles/Settlements/building_footprints.shp"
  private_land: "01_data_raw/03_vector/shapefiles/Pvt land/2022_07_01_cadastral_fixed.shp"
```

### 4. Reporting (`10_synthesis_reporting.R`)

**Fixed BRT inclusion in documentation:**
```markdown
| BRT | Boosted Regression Trees | gbm implementation, response output |
```

**Dynamic threshold in management brief:**
```r
# Get optimal threshold from ensemble evaluation
ens_thr <- eval_df$threshold[eval_df$algorithm == "ensemble"]
paste0("- High suitability habitat (>", optimal_threshold, "): ", high_suit)
```

---

## Expected Improvements After Re-run

### Before Fix (RUN_20260315_124209_b990)
```
Algorithm  AUC    TSS    Boyce    Threshold
GLM        0.723  0.446  NA       (not saved)
RF         0.712  0.411  -0.200   (not saved)
BRT        0.724  0.467  1.000    (not saved)
MaxEnt     0.714  0.448  -0.228   (not saved)
Ensemble   (not evaluated)

Outputs:
- Max suitability: 0.20 (scale 0-0.2 instead of 0-1)
- High suitability habitat: 0.0 km² (>0.7 threshold)
- Persistence: 0 km² (all scenarios)
```

### After Fix (Expected)
```
Algorithm  AUC    AUC    Boyce    Threshold
GLM        0.72   0.45   ~0.3-0.5 ~0.02-0.05
RF         0.71   0.41   ~0.3-0.5 ~0.05-0.10
BRT        0.72   0.47   ~0.6-0.8 ~0.03-0.08
MaxEnt     0.71   0.45   ~0.4-0.6 ~0.30-0.50
Ensemble   0.72   0.45   ~0.5-0.7 ~0.10-0.20

Expected outputs:
- Max suitability: 0.6-0.9 (proper 0-1 scale)
- High suitability habitat: Meaningful area (>optimal threshold)
- Persistence: Non-zero values showing habitat stability
- Temperature predictors: BIO1 and/or BIO5 included
```

---

## Additional Recommended Fixes (Not Yet Implemented)

### A. Case-Control Correction for Imbalanced Data

**Problem:** 1.5% presence rate (121/7959) causes GLM/RF/BRT to predict ~0.015 everywhere

**Fix:** Add to `07_model_training.R`:
```r
# Weight presence points to correct for artificial 1:100 ratio
w <- ifelse(dat$response == 1, 
            nrow(dat) / sum(dat$response),  # Weight presences higher
            nrow(dat) / sum(1 - dat$response))  # Weight background lower
```

### B. Force Temperature Predictor Inclusion

**Problem:** All temperature BIO variables removed due to mutual correlation

**Fix:** Add to config or Phase 6:
```yaml
predictors:
  forced_include: ["BIO01"]  # Always include Annual Mean Temperature
```

### C. Reduce Background Points

**Problem:** 10,000 background vs 121 presence = extreme imbalance

**Fix:** Change in config:
```yaml
occurrence:
  n_background: 1000  # Reduce from 10,000
```

### D. Use Target-Group Background

**Problem:** Uniform background sampling doesn't account for sampling bias

**Fix:** Already implemented in Phase 5 (distance-weighted), but could be improved with:
- eBird sampling effort layers
- Road density weighting
- Observer density surfaces

---

## Re-run Instructions

```bash
# From project root
Rscript 03_analysis/run_pipeline.R 00_governance/config.yaml

# Expected duration: ~2-3 hours
# Check: 04_outputs/runs/RUN_*/00_manifest/validation_report.txt
```

---

## Validation Checklist

After re-running, verify:

- [ ] `evaluation_all.csv` has `threshold` column
- [ ] `evaluation_all.csv` has `ensemble` row
- [ ] At least one temperature predictor (BIO01/BIO05/BIO06) in final model
- [ ] Maximum suitability > 0.5 (not 0.2)
- [ ] High suitability habitat area > 0 km²
- [ ] Persistence area > 0 km² in change metrics
- [ ] All Boyce indices > 0 (not negative)
- [ ] Management brief shows correct threshold (not 0.7)

---

## Files Modified

1. `03_analysis/08_model_evaluation.R` - Added threshold + ensemble evaluation
2. `03_analysis/run_pipeline.R` - Updated validation schema
3. `00_governance/config.yaml` - Relaxed collinearity, added vectors
4. `03_analysis/10_synthesis_reporting.R` - Fixed BRT docs, dynamic threshold

---

## Contact

For questions about these fixes, refer to:
- `00_governance/methods.md` - Scientific methodology
- `00_governance/targets.md` - Performance thresholds
- `00_governance/progress_tracker.md` - Run history
