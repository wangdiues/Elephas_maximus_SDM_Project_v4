# Elephas maximus SDM Project v2.1

**Species Distribution Modeling for Asian Elephants in South Central Bhutan**

[![R](https://img.shields.io/badge/R-4.3+-blue)](https://cran.r-project.org)
[![License](https://img.shields.io/badge/license-MIT-green)](LICENSE)

---

## Project Overview

This project implements a governance-compliant Species Distribution Modeling (SDM) pipeline for Asian elephants (*Elephas maximus*) in South Central Bhutan under CMIP6 climate change scenarios.

This GitHub repository is the code and metadata layer of the project. Large
raw data, generated run directories, logs, and working manuscript files are
kept out of version control and should be published separately only after
reviewing size, licensing, and telemetry sensitivity constraints.

### Key Features

- **14-Phase Analytics Pipeline** orchestrated by `run_pipeline.R`
- **4-Algorithm Ensemble**: GLM, Random Forest, BRT, MaxEnt (AUC-weighted)
- **CMIP6 Future Projections**: 8 GCMs x 4 SSPs x 3 time periods = 96 scenarios
- **Uncertainty Quantification**: GCM SD, algorithm SD, combined, agreement maps
- **Spatial Cross-Validation**: 15 km block CV with 5 folds
- **MESS Extrapolation Diagnostics** for all future scenarios
- **Evaluation Metrics**: AUC, TSS, Boyce continuous index, Brier score, calibration slope, Moran's I
- **Conservation Overlays**: PA analysis, conflict zones, proximity to infrastructure
- **ODMAP-Compliant Documentation**
- **Docker + CI/CD** (GitHub Actions)

### Study Area & Data

- **CRS**: EPSG:32645 (WGS 84 / UTM Zone 45N)
- **Occurrence Data**: 150 unique GPS collar telemetry locations
- **Background Points**: ~10,000 bias-corrected
- **Predictors**: 19 bioclimatic + DEM + HFI + NDVI + EVI + distance layers (25 total)

---

## Repository Structure

```
Elephas_maximus_SDM_Project_v4/
├── 00_governance/          # Governance documents + config.yaml
├── 00_registry/            # Data/model/run registries (CSV)
├── 01_data_raw/            # Input data (immutable)
├── 03_analysis/            # R pipeline scripts
│   ├── run_pipeline.R      # CANONICAL RUNNER (entry point)
│   ├── 00_sdm_helpers.R    # Core metric functions
│   ├── 00_spatial_alignment.R  # Raster alignment utilities
│   ├── 00_contract_helpers.R   # Path/validation utilities
│   ├── 00_logging.R        # Structured logging
│   ├── 00_seed_propagation.R   # Centralized seed management
│   ├── 00_data_validation.R    # Pre-flight validation
│   ├── 01_data_ingest_harmonize.R
│   ├── 02_accessible_area_M.R
│   ├── 03_occurrence_processing.R
│   ├── 04_predictor_engine.R
│   ├── 05_background_bias.R
│   ├── 06_spatial_cv.R
│   ├── 07_model_training.R
│   ├── 08_model_evaluation.R
│   ├── 09_future_projections.R
│   ├── 10_change_metrics.R
│   ├── 10_figures.R
│   ├── 11_uncertainty.R
│   ├── 12_conservation_overlays.R
│   └── 13_synthesis_reporting.R
├── 04_outputs/runs/        # Run outputs (RUN_<timestamp>_<hash>/)
├── 06_logs/                # pipeline.log, errors.log, warnings.log
├── tests/testthat/         # Unit + integration tests
├── Dockerfile
├── docker-compose.yml
└── Makefile
```

---

## Quick Start

### Prerequisites

- R >= 4.3 with packages: `terra`, `sf`, `yaml`, `ggplot2`, `viridis`, `ranger`, `maxnet`, `gbm`, `rlang`
- On Windows, run via PowerShell (not MSYS2 bash) to avoid `ggsave()` segfaults

### Install Dependencies

```r
# From R console:
source("03_analysis/setup.R")
```

### Run Pipeline

```powershell
# Windows PowerShell (recommended):
powershell.exe -Command "& 'C:\Program Files\R\R-4.4.0\bin\Rscript.exe' 03_analysis/run_pipeline.R 00_governance/config.yaml"

# Or via the convenience script:
.\rerun.ps1
```

```bash
# Linux / macOS:
Rscript 03_analysis/run_pipeline.R 00_governance/config.yaml
```

### Run Tests

```bash
make test
# or directly:
Rscript -e "testthat::test_dir('tests/testthat')"
```

### Docker

```bash
docker build -t elephas-sdm:2.1 .
docker run -v ${PWD}:/app elephas-sdm:2.1
```

---

## Pipeline Phases

| Phase | Module | Description |
|-------|--------|-------------|
| 0     | Bootstrap + Validation | Run ID, config snapshot, pre-flight checks |
| 0b    | Vector CRS Fix | Assign EPSG:32645 to shapefiles lacking valid CRS |
| 1     | Data Ingest | Harmonize rasters to unified template grid |
| 2     | Accessible Area (M) | Generate M mask from AOI polygon |
| 3     | Occurrence Processing | Clean, thin, and spatially validate presence records |
| 4     | Background Sampling | Bias-corrected background point generation |
| 5     | Spatial CV | 15 km block assignment, 5-fold spatial cross-validation |
| 6     | Predictor Engine | Distance rasters, collinearity diagnostics, predictor selection |
| 7     | Model Training | GLM + RF + BRT + MaxEnt with 5-fold CV; ensemble prediction |
| 8     | Model Evaluation | AUC, TSS, Boyce, Brier, calibration slope, Moran's I |
| 9     | Future Projections | CMIP6 multi-GCM projections with MESS diagnostics (optional) |
| 10    | Change Metrics | Delta suitability, gain/loss/persistence maps |
| 11    | Uncertainty | GCM SD, algorithm SD, combined uncertainty, agreement maps |
| 12    | Conservation Overlays | PA analysis, conflict zones, proximity indicators |
| 13    | Figures + Reports | 13 publication figures, ODMAP summary, management brief |

---

## Performance Thresholds (FATAL floors)

| Metric | FATAL Floor | Target |
|--------|-------------|--------|
| AUC-ROC (per algorithm) | < 0.65 | >= 0.70 |
| TSS (per algorithm) | < 0.30 | >= 0.40 |
| Boyce Index (per algorithm) | < 0.10 | >= 0.70 |
| Brier Score (per algorithm) | > 0.25 | < 0.20 |
| Ensemble AUC | — (aspirational) | >= 0.75 |

---

## Reproducibility

- **Global seed**: Configurable via `config.yaml` (`reproducibility.global_seed`, default 123456)
- **Per-module seeds**: Centralized via `00_seed_propagation.R` for background sampling, spatial CV, model training
- **Package versions**: Snapshotted to `package_versions.csv` in each run manifest
- **Input hashes**: MD5 checksums of all input files recorded per run
- **Session info**: Full `sessionInfo()` captured in each run manifest
- **Config snapshot**: config.yaml copied into run directory at start

---

## Testing

The test suite (`tests/testthat/`) includes:

- **test-core-functions.R**: 40+ unit tests for AUC, TSS, Brier, Boyce, MESS, VIF, calibration, Moran's I, logging
- **test-pipeline-integration.R**: Structural integration tests (config validation, source graph, function availability, phase numbering)
- **test-governance.R**: Governance compliance checks

```bash
make test            # Run all tests
make lint            # Code linting with lintr
make check-governance  # Verify governance files exist
make all             # lint + test + governance check
```

---

## Documentation

| Document | Purpose |
|----------|---------|
| [00_governance/config.yaml](00_governance/config.yaml) | Single source of truth for all parameters |
| [00_governance/governance.md](00_governance/governance.md) | Constitutional governance rules |
| [00_governance/methods.md](00_governance/methods.md) | Scientific methodology |
| [00_governance/targets.md](00_governance/targets.md) | Performance targets and FATAL thresholds |

---

## Citation

If you use this framework, please cite:

```
Zurell, D., et al. (2020). A standard protocol for reporting species distribution models.
Ecography, 43(9), 1261-1277. https://doi.org/10.1111/ecog.04960

O'Neill, B.C., et al. (2016). The Scenario Model Intercomparison Project (ScenarioMIP)
for CMIP6. Geoscientific Model Development, 9, 3461-3482.
```

---

## License

MIT License - see [LICENSE](LICENSE) file.

---

**Last Updated:** 2026-03-08
**Version:** 2.1-ensemble
