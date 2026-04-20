# Governance Constitution — Elephas maximus SDM Project v4

Version: 3.0-bhutan-national
Effective: 2026-03-15
Study area: Bhutan (national)

---

## Principles

1. **Reproducibility** — Every run is identified by a unique run ID, seeded deterministically, and archived with a manifest recording config hash, code version, and package versions.
2. **Transparency** — All model inclusion/exclusion decisions are rule-based, logged, and traceable to `targets.md` thresholds.
3. **Data provenance** — All inputs are registered in `data_registry.csv` with type, CRS, and notes. Deprecated datasets are marked, not deleted.
4. **Spatial integrity** — All outputs use EPSG:32645 (WGS 84 / UTM Zone 45N). CRS is validated before any spatial operation.
5. **Predictor coverage** — Every predictor layer must cover the full study area AOI or be explicitly excluded with justification.
6. **Quality gates** — Runs failing AUC < 0.65 (per algorithm) or calibration slope outside [0.5, 2.0] are flagged FATAL and do not proceed to ensemble.
7. **Ensemble transparency** — Ensemble weights and inclusion criteria are written to the run manifest.
8. **Separation of concerns** — Phase scripts are sourced, not modified, by the runner. Helper functions live in `00_sdm_helpers.R` and `00_contract_helpers.R`.
9. **No silent failures** — All phase errors are caught and logged; fatal errors halt the pipeline with a clear message.
10. **Seed traceability** — Global and per-module seeds are declared in `config.yaml` and applied before stochastic operations.
11. **Registry completeness** — `run_registry.csv` and `model_registry.csv` are appended on every run, successful or not.
12. **Documentation currency** — `README.md`, `QUICKSTART.md`, and `FEATURES.md` must reflect the active study area and pipeline version.
13. **Test coverage** — Core functions, governance files, and pipeline integration are covered by `tests/testthat/`.
14. **Dependency minimalism** — `DESCRIPTION` Imports lists only packages required at runtime; heavy packages are Suggests.
15. **Study area genericity** — Study area name and ID are read from config; no script may hard-code a place name in logic or output filenames.
16. **SCB-only layer policy** — Layers that cover only South Central Bhutan are marked as deprecated in `data_registry.csv` and disabled (empty string) in `config.yaml` until Bhutan-wide replacements are available.
17. **Audit trail** — The predictor manifest and config snapshot are copied to `04_outputs/runs/RUN_*/00_manifest/` on every run.

---

## Compliance Scorecard

| Principle | Status | Notes |
|-----------|--------|-------|
| Reproducibility | PASS | run_id + seeding in place |
| Transparency | PASS | threshold gates logged |
| Data provenance | PASS | data_registry.csv updated |
| Spatial integrity | PASS | CRS fix applied 2026-03-12 |
| Predictor coverage | IN PROGRESS | SCB-only layers disabled |
| Quality gates | PASS | AUC + calibration gates in runner |
| Ensemble transparency | PASS | weights written to manifest |
| Separation of concerns | PASS | phase scripts isolated |
| No silent failures | PASS | tryCatch + logging throughout |
| Seed traceability | PASS | per_module_seeds added |
| Registry completeness | PASS | append on every run |
| Documentation currency | IN PROGRESS | README/QUICKSTART update pending |
| Test coverage | PASS | 40+ unit + 8 integration tests |
| Dependency minimalism | PASS | DESCRIPTION trimmed |
| Study area genericity | PASS | config-driven as of v3.0 |
| SCB-only layer policy | PASS | settlements + private_land disabled |
| Audit trail | PASS | manifest snapshot per run |
