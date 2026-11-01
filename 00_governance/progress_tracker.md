# Progress Tracker — Elephas maximus SDM Project v4

Study area: Bhutan (national) | Version: 3.0-bhutan-national
Last updated: 2026-10-18

---

## Pipeline Phase Status

Status legend: **READY** = configured but not necessarily executed; **CODE READY / NOT RUN** = implementation and static checks complete, scientific outputs pending a clean production run; **COMPLETE** = production output validated; **WARNING** or **BLOCKED** = intervention required.

| Phase | Name | Status | Timestamp | Notes |
|-------|------|--------|-----------|-------|
| 0 | Config + seed init | READY | 2026-10-18 | Updated for Bhutan |
| 0b | Governance validation | READY | 2026-10-18 | Governance files created |
| 1 | Data ingest & harmonise | READY | 2026-10-18 | elephant_PA_data.csv |
| 2 | Accessible area (M) | READY | 2026-10-18 | bhutan.shp, EPSG:32645 |
| 3 | Occurrence processing | READY | 2026-10-18 | decimalLon/Lat columns |
| 4 | Background sampling | READY | 2026-10-18 | n=10,000 |
| 5 | Background bias | READY | 2026-10-18 | — |
| 6 | Spatial CV | CODE READY / NOT RUN | 2026-10-18 | Whole-block assignment, composition gates, block-size sensitivity report |
| 7 | Model training | CODE READY / NOT RUN | 2026-10-18 | Quality-gated equal-weight ensemble; MaxEnt sensitivity-only until valid OOF predictions exist |
| 8 | Model evaluation | CODE READY / NOT RUN | 2026-10-18 | Spatial OOF metrics and fold-level uncertainty; full-raster training-site evaluation prohibited |
| 9 | Future projections | CODE READY / NOT RUN | 2026-10-18 | Exactly 96 contracted scenarios; fail-fast completeness |
| 10 | Change metrics + refugia | CODE READY / NOT RUN | 2026-10-18 | 12 SSP-period summaries; refugia denominator fixed at 96 individual GCM projections |
| 11 | Uncertainty | CODE READY / NOT RUN | 2026-10-18 | Required when future projections are enabled |
| 12 | Conservation overlays | READY | 2026-10-18 | PA overlay; settlements/pvt disabled |
| 13 | Synthesis reporting | CODE READY / NOT RUN | 2026-10-18 | Includes final submission-contract validation |

---

## Migration Checklist (South Central Bhutan → Bhutan)

### Config & Registry
- [x] config.yaml: occurrence.primary_input → elephant_PA_data.csv
- [x] config.yaml: vectors.aoi_bhutan added; aoi_south_central_bhutan removed
- [x] config.yaml: settlements disabled (SCB-only)
- [x] config.yaml: private_land disabled (SCB-only)
- [x] config.yaml: primary_landcover_source → esa
- [x] config.yaml: project.study_area_name + study_area_id added
- [x] config.yaml: per_module_seeds added
- [x] config.yaml: version → 3.0-bhutan-national
- [x] data_registry.csv: occ_raw_004 (elephant_PA_data.csv) added
- [x] data_registry.csv: aoi_002 (bhutan.shp) added
- [x] DESCRIPTION: version 3.0.0; R >= 4.3.0; description updated

### Scripts
- [x] 02_accessible_area_M.R: aoi_bhutan key; subtitle updated
- [x] 03_occurrence_processing.R: default fallback path updated
- [x] 07_model_training.R: AOI mask comment updated
- [x] 10_figures.R: load_aoi_for_figures path updated
- [x] 10_synthesis_reporting.R: region from config, not hardcoded
- [x] 00_data_validation.R: aoi_bhutan key (2 locations)
- [x] setup.R: occurrence + AOI paths updated
- [x] run_pipeline.R: aoi_bhutan key (2 locations)
- [x] CRS check script: paths updated to bhutan.shp (script since removed)

### Tests
- [x] test-pipeline-integration.R: vector_keys updated; settlements/private_land removed

### Governance
- [x] governance.md created
- [x] methods.md created
- [x] targets.md created
- [x] memory_log.md created
- [x] progress_tracker.md created
- [x] execution_plan.md created
- [x] acceptance_criteria.md created
- [x] data_context_specification.md created

### Legal
- [x] LICENSE (MIT)
- [x] CITATION.cff
- [x] CONTRIBUTING.md
- [x] CODE_OF_CONDUCT.md

---

## Pending (Post-Migration Gates)

### Gate 1: Validation only
- [ ] Run `00_data_validation.R` — expect clean pass with bhutan.shp + elephant_PA_data.csv

### Gate 2: Phases 1–6
- [ ] AOI loads, CRS = EPSG:32645
- [ ] 1,089 occurrence records loaded, `longitude` / `latitude` columns detected
- [ ] Background sampling within Bhutan boundary succeeds
- [ ] Predictor extraction over Bhutan extent completes

### Gate 3: Phases 7–8
- [ ] GLM, RF, and BRT candidate fits complete; MaxEnt sensitivity fit is optional
- [ ] GLM AUC ≥ 0.65
- [ ] RF AUC ≥ 0.65
- [ ] BRT AUC ≥ 0.65
- [ ] At least two algorithms also pass TSS ≥ 0.30 with complete spatial OOF predictions
- [ ] Ensemble created with ≥ 2 eligible members

### Gate 4: Phases 9–13
- [ ] All 96 contracted future projections and 12 SSP-period summaries complete
- [ ] Conservation overlays (PA only; settlements/pvt skipped)
- [ ] Reports say "Bhutan" throughout
