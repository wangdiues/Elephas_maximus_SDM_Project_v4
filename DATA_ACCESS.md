# Data Access

This GitHub repository is intended to store the project's code, configuration,
tests, and governance metadata.

The following content is intentionally excluded from version control:

- `01_data_raw/` raw occurrence, raster, and vector inputs
- `02_data_intermediate/` generated intermediate products
- `04_outputs/runs/` full model runs and derived rasters
- `04_outputs/figures/` generated figure exports
- `05_manuscripts/` working manuscript documents
- `06_logs/` runtime logs

Reasons for exclusion:

- raw and derived geospatial layers are too large for a normal GitHub repository
- occurrence inputs may contain sensitive telemetry-derived locations
- generated outputs and logs are reproducible artifacts, not source files

Recommended publication pattern:

- GitHub: code, documentation, tests, workflow files, and lightweight metadata
- Zenodo or OSF: release archives and any approved public data package
- Private or institutional storage: restricted raw data and full run outputs

Before publishing any occurrence data, confirm the sharing policy, licensing,
and any required spatial generalization or embargo conditions.
