# Performance Targets — Elephas maximus SDM Project v4

Version: 3.0-bhutan-national
Updated: 2026-03-15

---

## Per-Algorithm Quality Gates (FATAL if not met)

| Metric | Threshold | Applies to | Consequence |
|--------|-----------|-----------|-------------|
| AUC (ROC) | ≥ 0.65 | GLM, RF, BRT, MaxEnt | FATAL — algorithm excluded from ensemble |
| Calibration slope | 0.5 – 2.0 | GLM, RF, BRT, MaxEnt | FATAL — algorithm excluded |
| Boyce index | ≥ 0.10 | GLM, RF, BRT, MaxEnt | WARNING if < 0.10; FATAL if < 0.00 |

## Per-Algorithm Aspirational Targets

| Metric | Target | Notes |
|--------|--------|-------|
| AUC | ≥ 0.75 | Good discrimination |
| TSS | ≥ 0.30 | Acceptable skill |
| Boyce | ≥ 0.60 | Strong habitat association |
| Moran's I (residuals) | < 0.10 | Low spatial autocorrelation |

## Ensemble Quality Gates

| Metric | Threshold | Notes |
|--------|-----------|-------|
| Ensemble AUC | ≥ 0.80 | Aspirational; run proceeds even if not met |
| Ensemble TSS | ≥ 0.50 | Aspirational |
| Min algorithms in ensemble | ≥ 2 | At least 2 must pass individual gates |

## Spatial Integrity Targets

| Check | Requirement |
|-------|-------------|
| Output CRS | EPSG:32645 for all rasters and vectors |
| AOI CRS validity | Must not be NA; pipeline halts if missing |
| Raster non-NA coverage | > 0% of AOI cells must be non-NA after masking |

## Data Quality Targets

| Check | Requirement |
|-------|-------------|
| Occurrence records (post-thinning) | ≥ 30 unique locations |
| BIO rasters found | = 19 |
| Predictor collinearity | VIF ≤ 5, r ≤ 0.7 |
| Max predictors in models | ≤ 25 |

---

## Rationale

- AUC ≥ 0.65: standard minimum for discriminatory SDMs (Swets 1988; Fielding & Bell 1997).
- Boyce floor 0.10: chosen to avoid excluding models with minor binning artefacts while still catching genuinely uninformative predictions.
- Calibration slope 0.5–2.0: captures gross over- and under-fitting without being overly strict for ecological data.
- TSS ≥ 0.30: "fair" agreement by Landis & Koch (1977) scale, broadly accepted for SDM (Allouche et al. 2006).
