# Acceptance Criteria — Elephas maximus SDM Project v4

Version: 3.0-bhutan-national
Updated: 2026-03-15

A run is considered successful when ALL of the following criteria are met.

---

## 1. Data & Spatial Integrity

| Criterion | Requirement | Test |
|-----------|-------------|------|
| Occurrence file loads | 1,089 records, longitude/latitude columns present | Phase 1 log |
| Coordinates in bounds | All points within Bhutan (lon 88.5–92.5°E, lat 26.5–28.5°N) | Phase 3 output |
| Occurrence post-thinning | ≥ 30 unique presence locations | Phase 3 log |
| AOI CRS | EPSG:32645 (not NA) after loading + reprojection | Phase 2 log |
| AOI raster overlap | m_mask.tif has > 0% non-zero cells | Phase 2 output |
| BIO rasters found | Exactly 19 BIO files match glob | Phase 0b check |
| All enabled vector paths | Files exist on disk | validation gate |

## 2. Model Performance

| Algorithm | AUC | Calibration slope | Boyce |
|-----------|-----|------------------|-------|
| GLM | ≥ 0.65 | 0.5 – 2.0 | ≥ 0.10 |
| RF | ≥ 0.65 | 0.5 – 2.0 | ≥ 0.10 |
| BRT | ≥ 0.65 | 0.5 – 2.0 | ≥ 0.10 |
| MaxEnt | ≥ 0.65 | 0.5 – 2.0 | ≥ 0.10 |

At least 2 algorithms must pass to form an ensemble. If fewer than 2 pass, the run is FAILED.

## 3. Ensemble

| Criterion | Requirement |
|-----------|-------------|
| Ensemble raster non-NA | > 0% of AOI cells |
| Ensemble members | ≥ 2 algorithms |
| Ensemble weights logged | Written to run manifest |

## 4. Outputs

| Criterion | Requirement |
|-----------|-------------|
| Present suitability raster | ensemble_suitability_present.tif exists and is non-NA |
| Binary suitability raster | binary_suitability_present.tif exists |
| Evaluation CSV | evaluation_all.csv has rows for each algorithm |
| Run registry updated | run_registry.csv has new row with success = TRUE |
| ODMAP report | odmap_report.md exists in 09_reports/ |
| Management brief | management_brief.md exists in 09_reports/ |

## 5. Governance & Reproducibility

| Criterion | Requirement |
|-----------|-------------|
| Run manifest | run_manifest.json written to 00_manifest/ |
| Config snapshot | config_snapshot.yaml written to 00_manifest/ |
| Predictor manifest | predictor_manifest.csv copied to 00_manifest/ |
| Package versions | package_versions.csv written to 00_manifest/ |
| Reports say "Bhutan" | No "South Central Bhutan" in output reports |

## 6. Tests

| Suite | Requirement |
|-------|-------------|
| test-governance.R | 100% pass |
| test-core-functions.R | 100% pass |
| test-pipeline-integration.R | 100% pass |

---

## Failure Definitions

**FATAL** — run stops immediately; no outputs committed to registry:
- Any required file missing at pipeline start
- AOI CRS = NA
- m_mask all zeros
- Fewer than 2 algorithms pass quality gates

**WARNING** — run continues; noted in registry:
- Boyce between 0.00 and 0.10 for any algorithm
- Moran's I ≥ 0.10 in residuals
- Conflict layer missing (expected for Bhutan run)
- Settlements / private land disabled (expected)
