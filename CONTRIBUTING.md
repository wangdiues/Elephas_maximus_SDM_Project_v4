# Contributing to Elephas maximus SDM Project v4

Thank you for your interest in contributing.

## How to Contribute

### Reporting Issues
Open an issue on GitHub with:
- A clear description of the problem
- The run ID (from `00_registry/run_registry.csv`) if it is a pipeline failure
- The relevant lines from `06_logs/errors.log`
- Your R version and OS

### Proposing Changes

1. Fork the repository and create a branch from `main`.
2. Make your changes. Follow the conventions below.
3. Run the test suite: `testthat::test_dir("tests/testthat")` — all tests must pass.
4. Run the data validator: `Rscript 03_analysis/00_data_validation.R 00_governance/config.yaml`
5. Open a pull request with a clear description of what changed and why.

## Code Conventions

- **R style**: snake_case for variables and functions, 80-character line limit.
- **No hard-coded place names**: study area name must come from `config$project$study_area_name`.
- **No hard-coded seeds**: all stochastic operations use the seed system in `00_seed_propagation.R`.
- **Phase scripts are sourced, not modified**: the runner (`run_pipeline.R`) sources phase scripts. Do not add runner logic inside phase scripts.
- **Helper functions**: SDM metrics live in `00_sdm_helpers.R`; pipeline utilities in `00_contract_helpers.R`. Do not duplicate.
- **Governance**: any change that affects performance thresholds must update `00_governance/targets.md` and be logged in `00_governance/memory_log.md`.
- **Predictor coverage**: any new predictor layer must cover the full study area or be documented as optional with explicit justification.

## Testing

Run the full test suite before submitting a PR:

```r
testthat::test_dir("tests/testthat")
```

Unit tests live in `tests/testthat/test-core-functions.R`.
Integration tests live in `tests/testthat/test-pipeline-integration.R`.
Governance tests live in `tests/testthat/test-governance.R`.

## Data Policy

Do not commit raw occurrence data, raster files, or shapefiles to the repository. The `01_data_raw/` directory is excluded from version control. Keep data paths relative and registered in `00_registry/data_registry.csv`.
