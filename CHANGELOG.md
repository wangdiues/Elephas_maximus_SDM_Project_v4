# Changelog

Detailed historical fix notes from March–April 2026 were removed on 2026-11-01. Retrieve them from git history if needed (`git log --diff-filter=D -- FIXES_APPLIED.md`).

## 2026-11-01: Submission production run

- Phase 9 keeps only the contracted 8 GCMs × 4 SSPs × 3 periods (96 projections). The extra `miroc_es2l` folder and the `mri_esm2_0` 1986–2015 historical baseline are excluded. Earlier runs counted that baseline, which produced "97 scenarios / 13 summaries".
- `%||%` no longer errors on character vectors longer than one element.
- The supplementary-figure standalone block no longer runs when the pipeline sources the file.
- The log summary counts entries for the current run only.
- Removed one-off diagnostic, publication and partial re-run scripts superseded by `14_tables.R`, `15_submission_validation.R` and `16_submission_figures.R`.

## 2026-10-18: Submission-readiness contracts

See `00_governance/submission_readiness_plan.md` and `00_governance/memory_log.md`. Changes:

- Per-run isolated output directories that refuse reuse.
- Production mode requires a clean Git worktree.
- Spatial out-of-fold evaluation for every algorithm.
- Equal-weight ensemble of the algorithms that pass the gates, with MaxEnt kept as a sensitivity model.
- Threshold set at the TSS optimum of the OOF ensemble predictions.
- Refugia defined over all 96 GCM projections.
- The human-pressure overlay is relabelled as an interaction-exposure index.
- Final submission validation.

## March–April 2026: Bhutan national pipeline

- Study area expanded from south-central Bhutan to the national extent.
- CRS inheritance fixed for bioclim rasters.
- Calibration-slope and Moran's I gates added.
- Seed propagation and reproducibility fixes.
- MaxEnt future prediction and figure-validation fixes.
