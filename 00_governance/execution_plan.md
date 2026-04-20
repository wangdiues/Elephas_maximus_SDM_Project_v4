# Execution Plan — Elephas maximus SDM Project v4

Version: 3.0-bhutan-national
Updated: 2026-03-15

---

## Run Strategy: Four Gates

Run the pipeline in four sequential gates rather than one full jump. Each gate must pass before the next starts.

### Gate 1 — Validation only (~2 min)
```
Rscript 03_analysis/00_data_validation.R 00_governance/config.yaml
```
Pass criteria:
- occurrence_bhutan.csv loads; 123 records with decimalLongitude/decimalLatitude
- bhutan.shp loads; CRS is not NA
- All non-empty predictor paths resolve to existing files
- BIO raster glob finds ≥ 19 files
- AOI geometry validity check passes

### Gate 2 — Phases 1–6 (~30 min)
```powershell
# Run via PowerShell (not MSYS2 bash — ggsave segfaults under MSYS2)
Rscript 03_analysis/run_pipeline.R 00_governance/config.yaml
# Stop after Phase 6 by setting execution.max_phase: 6 in config
```
Pass criteria:
- m_area_vector.gpkg created; contains Bhutan boundary in EPSG:32645
- occurrence_clean.csv present; n_presences ≥ 30
- background_points.gpkg present; n_background = 10,000
- cv_folds.gpkg present; 5 folds
- predictor_stack.tif non-NA over > 0% of AOI
- predictor_manifest.csv lists ≥ 5 predictors

### Gate 3 — Phases 7–8 (~20 min)
Pass criteria:
- All 4 model objects saved (.rds)
- AUC ≥ 0.65 for each included algorithm
- Calibration slope within [0.5, 2.0]
- Ensemble raster non-NA over > 0% of AOI
- evaluation_all.csv written

### Gate 4 — Phases 9–13 (~75 min for full future, ~15 min for reduced)
For initial Bhutan run, set:
```yaml
execution:
  run_future_projections: true
  max_future_scenarios: 8   # 1 GCM x 4 SSPs x 2 periods for quick test
```
Pass criteria:
- Future projection index CSV written
- PA overlay statistics written
- ODMAP report and management brief generated
- Reports reference "Bhutan" (not "South Central Bhutan")

---

## Resource Estimates

| Gate | Wall time | RAM peak | Notes |
|------|-----------|----------|-------|
| 1 | ~2 min | <1 GB | Validation only |
| 2 | ~30 min | ~4 GB | Bhutan extent; large raster crop |
| 3 | ~20 min | ~6 GB | 4 algorithms × 5 CV folds |
| 4 (reduced) | ~15 min | ~4 GB | 8 scenarios |
| 4 (full) | ~75 min | ~8 GB | 96 scenarios |

---

## Monitoring

```bash
# Follow pipeline log
tail -f 06_logs/pipeline.log

# Check error log
tail -f 06_logs/errors.log

# Latest run status
tail -1 00_registry/run_registry.csv
```

---

## Rollback

If a gate fails:
1. Read `06_logs/errors.log` for the FATAL message.
2. Fix the root cause (do not retry without a fix).
3. Re-run from Gate 1 (validation) to confirm the fix.
4. Proceed to the failed gate.

Previous run directories in `04_outputs/runs/` are retained. Do not delete them — they provide the comparison baseline.
