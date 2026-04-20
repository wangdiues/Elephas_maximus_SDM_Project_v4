# Decision Log — Elephas maximus SDM Project v4

Chronological record of key modelling and governance decisions.

---

## 2026-04-20: Occurrence dataset authority reconciled to elephant_PA_data.csv

**Decision**: Treat `01_data_raw/01_occurrences/elephant_PA_data.csv` as the authoritative operational occurrence input for the repository.

**Rationale**: This is the occurrence file that is actually present in the repository and referenced by the active `config.yaml`. It contains 1,089 station-level records with `longitude` / `latitude` columns and binary `presence` values, which matches the current operational pipeline configuration.

**Consequences**:
- Governance and registry files must name `elephant_PA_data.csv` as the active occurrence input.
- `occurrence_bhutan.csv` remains a historical planning reference only; it is not the packaged operational input in this repository.
- Validation criteria, execution gates, and documentation should use the `longitude` / `latitude` schema and the station-level record counts from `elephant_PA_data.csv`.

---

## 2026-03-15: Study area migration to Bhutan-wide (v3.0)

**Decision**: Change study area from South Central Bhutan to all of Bhutan.

**Rationale**: New occurrence data (`occurrence_bhutan.csv`, 123 records) covers the full country. The SCB boundary was an operational constraint of the original collar deployment, not a biological boundary. Bhutan-wide modelling provides more ecologically meaningful habitat predictions and better informs national conservation planning.

**Consequences**:
- `aoi_south_central_bhutan` config key replaced by `aoi_bhutan` pointing to `bhutan.shp`.
- `occurrence.primary_input` updated to `occurrence_bhutan.csv`.
- `occurrence.required_columns` updated to `["decimalLongitude", "decimalLatitude"]` to match new file format.
- SCB-only predictor layers (settlements `MS_Building_Foot_Prints_SCB`, private land `Private_Public_Registered_Lands_SCB`) disabled (empty string in config) pending Bhutan-wide replacements.
- `primary_landcover_source` switched from `local_lulc` (SCB-only raster) to `esa` (Bhutan-wide ESA landcover).
- `lulc2020.shp` retained as auxiliary local vector; `lulc_south_central_bhutan.tif` no longer used.
- Version bumped to 3.0-bhutan-national.

---

## 2026-03-12: Critical CRS bug fix (all rasters were NA)

**Decision**: Fix CRS inheritance from bioclim rasters.

**Root cause**: Bioclim rasters carry a custom TM CRS (lon_0=90) rather than EPSG:32645. The template grid inherited this, causing AOI reprojection to produce non-overlapping extents. m_mask was all zeros → all predictions masked to NA.

**Fix**:
- `get_unified_template()` now explicitly projects ref raster to EPSG:32645.
- `config.yaml` declares `spatial.target_crs_epsg: 32645`.
- `02_accessible_area_M.R` defaults to EPSG:32645 instead of inheriting from template.

---

## 2026-03-10: Calibration slope and quality gate hardening

**Decision**: Add calibration slope FATAL gate and Moran's I WARNING gate to `run_pipeline.R`.

**Rationale**: GLM and RF had calibration slopes > 13 in test runs but the pipeline did not catch them. Logit-scale calibration regression is now used (not identity scale, which was a pre-existing bug).

---

## 2026-03-09: Phase 9 crash — model library loading

**Decision**: Add explicit `library()` calls for ranger, gbm, maxnet inside `09_future_projections.R`.

**Rationale**: Predict calls failed when packages were not loaded in the future-projection environment even though models were trained in a different session.

---

## 2026-03-08: 10/10 quality sprint — multiple bug fixes

**Decision**: Fix BRT ensemble regex, aes_string deprecation, Boyce floor alignment, VIF implementation.

See session memory for full list. Key: Boyce floor aligned to 0.10 (was inconsistent between targets.md and code).

---

## BRT excluded from ensemble — expected behaviour

**Observation**: BRT Boyce = -0.866 in reference run.

**Assessment**: BRT exclusion from ensemble is correct behaviour when BRT Boyce < 0.10. This is not a bug. BRT predictions still written to disk for diagnostics.

---

## Accessible area (M) definition for Bhutan-wide modelling

**Decision**: Use Bhutan national boundary as M.

**Rationale**: For a first stable Bhutan-wide run, the national boundary is a conservative, reproducible M. A dispersal-based or ecological accessibility surface would require additional movement data not currently available. This decision should be revisited after initial model evaluation.
