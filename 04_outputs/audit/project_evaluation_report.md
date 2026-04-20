# Project Evaluation Report (Improved)

- Repo root: `E:\Elephas_maximus_SDM_Project_v4`
- Generated: `2026-02-24T13:28:15`

## Repository Discovery
- Top-level folders: `-p, .github, 00_governance, 00_registry, 01_data_raw, 03_analysis, 04_outputs, 05_manuscripts, 06_logs, tests`
- `00_governance`: FOUND
- `00_registry`: FOUND
- `01_data_raw`: FOUND
- `02_data_intermediate`: MISSING
- `03_analysis`: FOUND
- `04_outputs`: FOUND
- `05_validation`: MISSING
- `06_logs`: FOUND

## Criteria Docs
- `analytics.md`: FOUND (`E:\Elephas_maximus_SDM_Project_v4\00_governance\analytics.md`)
- `methods.md`: FOUND (`E:\Elephas_maximus_SDM_Project_v4\00_governance\methods.md`)
- `targets.md`: FOUND (`E:\Elephas_maximus_SDM_Project_v4\00_governance\targets.md`)
- `title.md`: FOUND (`E:\Elephas_maximus_SDM_Project_v4\00_governance\title.md`)

## Run Inventory
- `RUN_20260224_113154_b990`
- `RUN_20260224_113641_b990`
- `RUN_20260224_113826_b990`
- `RUN_20260224_114149_b990`
- `RUN_20260224_114314_b990`
- `RUN_20260224_114444_b990`
- `RUN_20260224_114910_b990`
- `RUN_20260224_115145_b990`
- `RUN_20260224_115714_b990`
- `RUN_20260224_115946_b990`
- `RUN_20260224_120251_b990`

## Run Scoreboard
| run_id | A | B | C | D | E | F | overall | classification |
|---|---:|---:|---:|---:|---:|---:|---:|---|
| RUN_20260224_113154_b990 | 5.00 | 10.00 | 2.00 | 2.50 | 0.00 | 0.00 | 3.52 | NOT READY |
| RUN_20260224_113641_b990 | 5.00 | 2.00 | 0.00 | 0.00 | 0.00 | 0.00 | 1.15 | NOT READY |
| RUN_20260224_113826_b990 | 5.00 | 6.00 | 0.00 | 0.00 | 0.00 | 0.00 | 1.95 | NOT READY |
| RUN_20260224_114149_b990 | 5.00 | 6.00 | 0.00 | 0.00 | 0.00 | 0.00 | 1.95 | NOT READY |
| RUN_20260224_114314_b990 | 5.00 | 6.00 | 0.00 | 0.00 | 0.00 | 0.00 | 1.95 | NOT READY |
| RUN_20260224_114444_b990 | 5.00 | 6.00 | 0.00 | 0.00 | 0.00 | 0.00 | 1.95 | NOT READY |
| RUN_20260224_114910_b990 | 5.00 | 6.00 | 0.00 | 0.00 | 0.00 | 0.00 | 1.95 | NOT READY |
| RUN_20260224_115145_b990 | 5.00 | 6.00 | 0.00 | 0.00 | 0.00 | 0.00 | 1.95 | NOT READY |
| RUN_20260224_115714_b990 | 5.00 | 10.00 | 2.00 | 0.00 | 0.00 | 0.00 | 3.15 | NOT READY |
| RUN_20260224_115946_b990 | 5.00 | 10.00 | 2.00 | 0.00 | 0.00 | 0.00 | 3.15 | NOT READY |
| RUN_20260224_120251_b990 | 5.00 | 10.00 | 2.00 | 2.50 | 0.00 | 0.00 | 3.52 | NOT READY |

## RUN_20260224_113154_b990
- Structural evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_113154_b990\00_manifest\config_snapshot.yaml`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_113154_b990\00_manifest\run_manifest.json`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_113154_b990\00_manifest\validation_report.txt`
- Performance evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_113154_b990\02_models\evaluation_all.csv`
- Metrics row: algorithm=glm, auc_mean=0.355588573133388, tss_mean=0.108292691309393
- Metrics row: algorithm=rf, auc_mean=0.639971751257577, tss_mean=0.368408143657987
- Metrics row: algorithm=maxent, auc_mean=0.354405723758976, tss_mean=0.117675103694664
- Climate evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_113154_b990\04_future_projections`
- Figure quality gate: Manual verification required (scale bar, north arrow, CRS annotation, inset, fixed scales).
- Fatal failures:
  - AUC<0.65 (min observed 0.354406)
  - TSS<0.3 (min observed 0.108293)
  - Configured partial scenario execution: max_future_scenarios=6 (targets require full GCM×SSP×period outputs)
- Warnings:
  - Missing one or more required metrics artifacts: Boyce/Brier/Calibration/Moran
- Remediation:
  - Generate `00_manifest/inputs_hashes.csv` in pipeline startup.
  - Write Boyce/Brier/Calibration/Moran outputs in Phase 8 and include them in run artifacts.
  - Write `04_future_projections/gcm_completeness_report.csv` and `future_gcm_ensemble_<ssp>_<period>.tif` in Phase 9.
  - Write `extrapolation_fraction_<ssp>_<period>.csv` in Phase 9.
  - Generate `06_uncertainty/` outputs in Phase 11.
  - Generate `07_overlays/` indicator tables and rasters in Phase 12.

## RUN_20260224_113641_b990
- Structural evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_113641_b990\00_manifest\config_snapshot.yaml`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_113641_b990\00_manifest\run_manifest.json`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_113641_b990\00_manifest\validation_report.txt`
- Performance evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_113641_b990\02_models\evaluation_all.csv`
- I cannot confirm this.
[MISSING INPUTS] `02_models/evaluation_all.csv`
- Climate evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_113641_b990\04_future_projections`
- Figure quality gate: Manual verification required (scale bar, north arrow, CRS annotation, inset, fixed scales).
- Fatal failures:
  - Configured partial scenario execution: max_future_scenarios=6 (targets require full GCM×SSP×period outputs)
- Warnings:
  - Missing one or more required metrics artifacts: Boyce/Brier/Calibration/Moran
- Remediation:
  - Generate `00_manifest/inputs_hashes.csv` in pipeline startup.
  - Write Boyce/Brier/Calibration/Moran outputs in Phase 8 and include them in run artifacts.
  - Write `04_future_projections/gcm_completeness_report.csv` and `future_gcm_ensemble_<ssp>_<period>.tif` in Phase 9.
  - Write `extrapolation_fraction_<ssp>_<period>.csv` in Phase 9.
  - Generate `06_uncertainty/` outputs in Phase 11.
  - Generate `07_overlays/` indicator tables and rasters in Phase 12.

## RUN_20260224_113826_b990
- Structural evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_113826_b990\00_manifest\config_snapshot.yaml`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_113826_b990\00_manifest\run_manifest.json`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_113826_b990\00_manifest\validation_report.txt`
- Performance evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_113826_b990\02_models\evaluation_all.csv`
- I cannot confirm this.
[MISSING INPUTS] `02_models/evaluation_all.csv`
- Climate evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_113826_b990\04_future_projections`
- Figure quality gate: Manual verification required (scale bar, north arrow, CRS annotation, inset, fixed scales).
- Fatal failures:
  - Configured partial scenario execution: max_future_scenarios=6 (targets require full GCM×SSP×period outputs)
- Warnings:
  - Missing one or more required metrics artifacts: Boyce/Brier/Calibration/Moran
- Remediation:
  - Generate `00_manifest/inputs_hashes.csv` in pipeline startup.
  - Write Boyce/Brier/Calibration/Moran outputs in Phase 8 and include them in run artifacts.
  - Write `04_future_projections/gcm_completeness_report.csv` and `future_gcm_ensemble_<ssp>_<period>.tif` in Phase 9.
  - Write `extrapolation_fraction_<ssp>_<period>.csv` in Phase 9.
  - Generate `06_uncertainty/` outputs in Phase 11.
  - Generate `07_overlays/` indicator tables and rasters in Phase 12.

## RUN_20260224_114149_b990
- Structural evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114149_b990\00_manifest\config_snapshot.yaml`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114149_b990\00_manifest\run_manifest.json`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114149_b990\00_manifest\validation_report.txt`
- Performance evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114149_b990\02_models\evaluation_all.csv`
- I cannot confirm this.
[MISSING INPUTS] `02_models/evaluation_all.csv`
- Climate evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114149_b990\04_future_projections`
- Figure quality gate: Manual verification required (scale bar, north arrow, CRS annotation, inset, fixed scales).
- Fatal failures:
  - Configured partial scenario execution: max_future_scenarios=6 (targets require full GCM×SSP×period outputs)
- Warnings:
  - Missing one or more required metrics artifacts: Boyce/Brier/Calibration/Moran
- Remediation:
  - Generate `00_manifest/inputs_hashes.csv` in pipeline startup.
  - Write Boyce/Brier/Calibration/Moran outputs in Phase 8 and include them in run artifacts.
  - Write `04_future_projections/gcm_completeness_report.csv` and `future_gcm_ensemble_<ssp>_<period>.tif` in Phase 9.
  - Write `extrapolation_fraction_<ssp>_<period>.csv` in Phase 9.
  - Generate `06_uncertainty/` outputs in Phase 11.
  - Generate `07_overlays/` indicator tables and rasters in Phase 12.

## RUN_20260224_114314_b990
- Structural evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114314_b990\00_manifest\config_snapshot.yaml`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114314_b990\00_manifest\run_manifest.json`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114314_b990\00_manifest\validation_report.txt`
- Performance evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114314_b990\02_models\evaluation_all.csv`
- I cannot confirm this.
[MISSING INPUTS] `02_models/evaluation_all.csv`
- Climate evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114314_b990\04_future_projections`
- Figure quality gate: Manual verification required (scale bar, north arrow, CRS annotation, inset, fixed scales).
- Fatal failures:
  - Configured partial scenario execution: max_future_scenarios=6 (targets require full GCM×SSP×period outputs)
- Warnings:
  - Missing one or more required metrics artifacts: Boyce/Brier/Calibration/Moran
- Remediation:
  - Generate `00_manifest/inputs_hashes.csv` in pipeline startup.
  - Write Boyce/Brier/Calibration/Moran outputs in Phase 8 and include them in run artifacts.
  - Write `04_future_projections/gcm_completeness_report.csv` and `future_gcm_ensemble_<ssp>_<period>.tif` in Phase 9.
  - Write `extrapolation_fraction_<ssp>_<period>.csv` in Phase 9.
  - Generate `06_uncertainty/` outputs in Phase 11.
  - Generate `07_overlays/` indicator tables and rasters in Phase 12.

## RUN_20260224_114444_b990
- Structural evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114444_b990\00_manifest\config_snapshot.yaml`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114444_b990\00_manifest\run_manifest.json`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114444_b990\00_manifest\validation_report.txt`
- Performance evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114444_b990\02_models\evaluation_all.csv`
- I cannot confirm this.
[MISSING INPUTS] `02_models/evaluation_all.csv`
- Climate evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114444_b990\04_future_projections`
- Figure quality gate: Manual verification required (scale bar, north arrow, CRS annotation, inset, fixed scales).
- Fatal failures:
  - Configured partial scenario execution: max_future_scenarios=6 (targets require full GCM×SSP×period outputs)
- Warnings:
  - Missing one or more required metrics artifacts: Boyce/Brier/Calibration/Moran
- Remediation:
  - Generate `00_manifest/inputs_hashes.csv` in pipeline startup.
  - Write Boyce/Brier/Calibration/Moran outputs in Phase 8 and include them in run artifacts.
  - Write `04_future_projections/gcm_completeness_report.csv` and `future_gcm_ensemble_<ssp>_<period>.tif` in Phase 9.
  - Write `extrapolation_fraction_<ssp>_<period>.csv` in Phase 9.
  - Generate `06_uncertainty/` outputs in Phase 11.
  - Generate `07_overlays/` indicator tables and rasters in Phase 12.

## RUN_20260224_114910_b990
- Structural evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114910_b990\00_manifest\config_snapshot.yaml`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114910_b990\00_manifest\run_manifest.json`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114910_b990\00_manifest\validation_report.txt`
- Performance evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114910_b990\02_models\evaluation_all.csv`
- I cannot confirm this.
[MISSING INPUTS] `02_models/evaluation_all.csv`
- Climate evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_114910_b990\04_future_projections`
- Figure quality gate: Manual verification required (scale bar, north arrow, CRS annotation, inset, fixed scales).
- Fatal failures:
  - Configured partial scenario execution: max_future_scenarios=6 (targets require full GCM×SSP×period outputs)
- Warnings:
  - Missing one or more required metrics artifacts: Boyce/Brier/Calibration/Moran
- Remediation:
  - Generate `00_manifest/inputs_hashes.csv` in pipeline startup.
  - Write Boyce/Brier/Calibration/Moran outputs in Phase 8 and include them in run artifacts.
  - Write `04_future_projections/gcm_completeness_report.csv` and `future_gcm_ensemble_<ssp>_<period>.tif` in Phase 9.
  - Write `extrapolation_fraction_<ssp>_<period>.csv` in Phase 9.
  - Generate `06_uncertainty/` outputs in Phase 11.
  - Generate `07_overlays/` indicator tables and rasters in Phase 12.

## RUN_20260224_115145_b990
- Structural evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_115145_b990\00_manifest\config_snapshot.yaml`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_115145_b990\00_manifest\run_manifest.json`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_115145_b990\00_manifest\validation_report.txt`
- Performance evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_115145_b990\02_models\evaluation_all.csv`
- I cannot confirm this.
[MISSING INPUTS] `02_models/evaluation_all.csv`
- Climate evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_115145_b990\04_future_projections`
- Figure quality gate: Manual verification required (scale bar, north arrow, CRS annotation, inset, fixed scales).
- Fatal failures:
  - Configured partial scenario execution: max_future_scenarios=6 (targets require full GCM×SSP×period outputs)
- Warnings:
  - Missing one or more required metrics artifacts: Boyce/Brier/Calibration/Moran
- Remediation:
  - Generate `00_manifest/inputs_hashes.csv` in pipeline startup.
  - Write Boyce/Brier/Calibration/Moran outputs in Phase 8 and include them in run artifacts.
  - Write `04_future_projections/gcm_completeness_report.csv` and `future_gcm_ensemble_<ssp>_<period>.tif` in Phase 9.
  - Write `extrapolation_fraction_<ssp>_<period>.csv` in Phase 9.
  - Generate `06_uncertainty/` outputs in Phase 11.
  - Generate `07_overlays/` indicator tables and rasters in Phase 12.

## RUN_20260224_115714_b990
- Structural evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_115714_b990\00_manifest\config_snapshot.yaml`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_115714_b990\00_manifest\run_manifest.json`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_115714_b990\00_manifest\validation_report.txt`
- Performance evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_115714_b990\02_models\evaluation_all.csv`
- Metrics row: algorithm=glm, auc_mean=0.417911427674228, tss_mean=0.125576582059535
- Metrics row: algorithm=rf, auc_mean=0.730813121265319, tss_mean=0.518219413060606
- Metrics row: algorithm=maxent, auc_mean=0.443900836537275, tss_mean=0.149835077646763
- Climate evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_115714_b990\04_future_projections`
- Figure quality gate: Manual verification required (scale bar, north arrow, CRS annotation, inset, fixed scales).
- Fatal failures:
  - AUC<0.65 (min observed 0.417911)
  - TSS<0.3 (min observed 0.125577)
  - Configured partial scenario execution: max_future_scenarios=6 (targets require full GCM×SSP×period outputs)
- Warnings:
  - Missing one or more required metrics artifacts: Boyce/Brier/Calibration/Moran
- Remediation:
  - Generate `00_manifest/inputs_hashes.csv` in pipeline startup.
  - Write Boyce/Brier/Calibration/Moran outputs in Phase 8 and include them in run artifacts.
  - Write `04_future_projections/gcm_completeness_report.csv` and `future_gcm_ensemble_<ssp>_<period>.tif` in Phase 9.
  - Write `extrapolation_fraction_<ssp>_<period>.csv` in Phase 9.
  - Generate `06_uncertainty/` outputs in Phase 11.
  - Generate `07_overlays/` indicator tables and rasters in Phase 12.

## RUN_20260224_115946_b990
- Structural evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_115946_b990\00_manifest\config_snapshot.yaml`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_115946_b990\00_manifest\run_manifest.json`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_115946_b990\00_manifest\validation_report.txt`
- Performance evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_115946_b990\02_models\evaluation_all.csv`
- Metrics row: algorithm=glm, auc_mean=0.417911427674228, tss_mean=0.125576582059535
- Metrics row: algorithm=rf, auc_mean=0.730813121265319, tss_mean=0.518219413060606
- Metrics row: algorithm=maxent, auc_mean=0.443900836537275, tss_mean=0.149835077646763
- Climate evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_115946_b990\04_future_projections`
- Figure quality gate: Manual verification required (scale bar, north arrow, CRS annotation, inset, fixed scales).
- Fatal failures:
  - AUC<0.65 (min observed 0.417911)
  - TSS<0.3 (min observed 0.125577)
  - Configured partial scenario execution: max_future_scenarios=6 (targets require full GCM×SSP×period outputs)
- Warnings:
  - Missing one or more required metrics artifacts: Boyce/Brier/Calibration/Moran
- Remediation:
  - Generate `00_manifest/inputs_hashes.csv` in pipeline startup.
  - Write Boyce/Brier/Calibration/Moran outputs in Phase 8 and include them in run artifacts.
  - Write `04_future_projections/gcm_completeness_report.csv` and `future_gcm_ensemble_<ssp>_<period>.tif` in Phase 9.
  - Write `extrapolation_fraction_<ssp>_<period>.csv` in Phase 9.
  - Generate `06_uncertainty/` outputs in Phase 11.
  - Generate `07_overlays/` indicator tables and rasters in Phase 12.

## RUN_20260224_120251_b990
- Structural evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_120251_b990\00_manifest\config_snapshot.yaml`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_120251_b990\00_manifest\run_manifest.json`, `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_120251_b990\00_manifest\validation_report.txt`
- Performance evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_120251_b990\02_models\evaluation_all.csv`
- Metrics row: algorithm=glm, auc_mean=0.417911427674228, tss_mean=0.125576582059535
- Metrics row: algorithm=rf, auc_mean=0.730813121265319, tss_mean=0.518219413060606
- Metrics row: algorithm=maxent, auc_mean=0.443900836537275, tss_mean=0.149835077646763
- Climate evidence: `E:\Elephas_maximus_SDM_Project_v4\04_outputs\runs\RUN_20260224_120251_b990\04_future_projections`
- Figure quality gate: Manual verification required (scale bar, north arrow, CRS annotation, inset, fixed scales).
- Fatal failures:
  - AUC<0.65 (min observed 0.417911)
  - TSS<0.3 (min observed 0.125577)
  - Configured partial scenario execution: max_future_scenarios=6 (targets require full GCM×SSP×period outputs)
- Warnings:
  - Missing one or more required metrics artifacts: Boyce/Brier/Calibration/Moran
- Remediation:
  - Generate `00_manifest/inputs_hashes.csv` in pipeline startup.
  - Write Boyce/Brier/Calibration/Moran outputs in Phase 8 and include them in run artifacts.
  - Write `04_future_projections/gcm_completeness_report.csv` and `future_gcm_ensemble_<ssp>_<period>.tif` in Phase 9.
  - Write `extrapolation_fraction_<ssp>_<period>.csv` in Phase 9.
  - Generate `06_uncertainty/` outputs in Phase 11.
  - Generate `07_overlays/` indicator tables and rasters in Phase 12.

## Executive Decision
- Best observed run: `RUN_20260224_113154_b990` (overall `3.52`, class `NOT READY`)
- Portfolio status: `NOT READY`

## [MISSING INPUTS]
- `E:\Elephas_maximus_SDM_Project_v4\02_data_intermediate`
- `E:\Elephas_maximus_SDM_Project_v4\05_validation`
