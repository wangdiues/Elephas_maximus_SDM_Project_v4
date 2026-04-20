# Progress Tracker — Elephas maximus SDM Project v4

Study area: Bhutan (national) | Version: 3.0-bhutan-national
Last updated: 2026-03-15

---

## Pipeline Phase Status

| Phase | Name | Status | Notes |
|-------|------|--------|-------|
| 0 | Config + seed init | READY | Updated for Bhutan |
| 0b | Governance validation | READY | Governance files created |
| 1 | Data ingest & harmonise | READY | occurrence_bhutan.csv |
| 2 | Accessible area (M) | READY | bhutan.shp, EPSG:32645 |
| 3 | Occurrence processing | READY | decimalLon/Lat columns |
| 4 | Background sampling | READY | n=10,000 |
| 5 | Background bias | READY | |
| 6 | Spatial CV | READY | |
| 7 | Model training | READY | GLM, RF, BRT, MaxEnt |
| 8 | Model evaluation | READY | AUC, TSS, Boyce, calibration |
| 9 | Future projections | OPTIONAL | 96 scenarios; ~75 min |
| 10 | Change metrics | OPTIONAL | depends on Phase 9 |
| 11 | Uncertainty | OPTIONAL | depends on Phase 9 |
| 12 | Conservation overlays | READY | PA overlay; settlements/pvt disabled |
| 13 | Synthesis reporting | READY | study_area_name from config |

---

## Migration Checklist (South Central Bhutan → Bhutan)

### Config & Registry
- [x] config.yaml: occurrence.primary_input → occurrence_bhutan.csv
- [x] config.yaml: vectors.aoi_bhutan added; aoi_south_central_bhutan removed
- [x] config.yaml: settlements disabled (SCB-only)
- [x] config.yaml: private_land disabled (SCB-only)
- [x] config.yaml: primary_landcover_source → esa
- [x] config.yaml: project.study_area_name + study_area_id added
- [x] config.yaml: per_module_seeds added
- [x] config.yaml: version → 3.0-bhutan-national
- [x] data_registry.csv: occ_raw_003 (occurrence_bhutan.csv) added
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
- [x] _check_crs.R: paths updated to bhutan.shp

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
- [ ] Run `00_data_validation.R` — expect clean pass with bhutan.shp + occurrence_bhutan.csv

### Gate 2: Phases 1–6
- [ ] AOI loads, CRS = EPSG:32645
- [ ] 123 occurrence records loaded, coordinate columns detected
- [ ] Background sampling within Bhutan boundary succeeds
- [ ] Predictor extraction over Bhutan extent completes

### Gate 3: Phases 7–8
- [ ] All 4 algorithms train successfully
- [ ] GLM AUC ≥ 0.65
- [ ] RF AUC ≥ 0.65
- [ ] BRT AUC ≥ 0.65
- [ ] MaxEnt AUC ≥ 0.65
- [ ] Ensemble created with ≥ 2 members

### Gate 4: Phases 9–13
- [ ] Future projections (reduced set first)
- [ ] Conservation overlays (PA only; settlements/pvt skipped)
- [ ] Reports say "Bhutan" throughout
