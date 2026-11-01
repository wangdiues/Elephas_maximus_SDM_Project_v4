# Decision Log — Elephas maximus SDM Project v4

Chronological record of key modelling and governance decisions.

---

## 2026-11-01: Reduce the GCM ensemble from 8 to 5 independent model families

**Decision**: Project with ACCESS-CM2, CNRM-CM6-1, INM-CM5-0, MIROC6 and MPI-ESM1-2-LR: 5 GCMs × 4 SSPs × 3 periods = 60 individual projections and 12 SSP-period summaries. Remove CNRM-ESM2-1, INM-CM4-8, MRI-ESM2-0 and the never-contracted MIROC-ES2L from `01_data_raw/02_rasters/future/`.

**Rationale**:
- Cut projection cost by about 40%.
- Use one model per family, so ensemble members are not near-duplicates. CNRM-ESM2-1 shares the CNRM-CM6-1 atmosphere and ocean; INM-CM4-8 shares the INM-CM5-0 core (Knutti et al. 2017, GRL 44:1909).
- Span low to high equilibrium climate sensitivity rather than clustering at the extremes. Approximate ECS: INM-CM5-0 ~1.9 °C, MIROC6 ~2.6, MPI-ESM1-2-LR ~3.0, ACCESS-CM2 ~4.7, CNRM-CM6-1 ~4.8. Verify these against Meehl et al. 2020 (Sci. Adv. 6:eaba1981) before citing.
- This responds to the "hot model" concern (Hausfather et al. 2022, Nature 605:26).

**Consequences**: refugia denominator = 60; core = 60/60; high stability ≥ 45/60 (`ceiling(0.75 × 60)`). This supersedes the 96/96 and ≥72/96 thresholds of the 2026-10-18 entry. The manuscript must state the selection criteria (family independence and ECS spread) and acknowledge that 5 GCMs under-sample structural uncertainty relative to the full CMIP6 ensemble.

---

## 2026-10-18: Submission-readiness analytical and publication contracts

**Decision**: Implement the reviewed submission-readiness plan as fail-fast code contracts without running or replacing the current analysis outputs.

**Locked defaults**:
- Every production run writes to a new `04_outputs/runs/RUN_*` directory and refuses reuse.
- Production mode requires a clean Git worktree, configuration snapshot, input hashes, and run-specific manifests.
- Spatial CV uses whole-block deterministic assignment, five folds, a primary 15-km block size, a minimum of 10 presences per fold, and composition-only sensitivity checks at 10–30 km.
- The primary ensemble uses equal weights among at least two algorithms that have complete spatial OOF predictions and pass the prespecified AUC and TSS gates.
- MaxEnt remains a full-model sensitivity product but is excluded from primary evaluation and ensembling until genuine governed spatial OOF predictions exist for both presence and reference records.
- The classification threshold is the TSS optimum from the spatial OOF ensemble; algorithm-specific thresholds are not averaged.
- Future processing requires exactly 8 GCMs × 4 SSPs × 3 periods = 96 individual projections and 12 SSP-period summaries. Present and future products use the same locked algorithm set and weights.
- Primary refugia use present-suitable cells retained across the 96 individual GCM projections; core means 96/96 and high stability means at least 72/96.
- The human-pressure overlay is labelled a potential interaction-exposure index, not conflict risk, unless a fitted and independently validated conflict model is later supplied.
- Label permutation without complete model refitting is not reported as a null-model test.

**Implementation note**: Code parsing and lightweight contract tests are permitted before analysis. No scientific result is updated until a clean production run passes all gates.

### Model Selection

Candidate algorithms are screened using spatial out-of-fold predictions only. The primary ensemble requires at least two eligible algorithms and uses equal weights by default. MaxEnt remains a sensitivity model unless complete, non-leaking out-of-fold predictions are implemented and pass the same gates.

### Manual Overrides

No manual model inclusion, scenario omission, threshold substitution, or refugia-denominator override is permitted in production mode. Any future exception must be recorded here before execution, justified scientifically, and represented in the run manifest.

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
