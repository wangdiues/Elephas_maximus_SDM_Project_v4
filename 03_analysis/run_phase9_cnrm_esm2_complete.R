#!/usr/bin/env Rscript
# Complete the missing cnrm_esm2_1 scenarios (2071_2100 period for all 4 SSPs)
# Skip already-existing output files.

repo_root <- "E:/Elephas_maximus_SDM_Project_v4"
run_dir   <- "E:/Elephas_maximus_SDM_Project_v4/04_outputs/runs/RUN_20260317_203608_b990"

setwd(repo_root)

suppressPackageStartupMessages({
  library(terra)
  library(ranger)
  library(gbm)
  library(sf)
})

source(file.path(repo_root, "03_analysis", "00_spatial_alignment.R"))
source(file.path(repo_root, "03_analysis", "00_sdm_helpers.R"))
source(file.path(repo_root, "03_analysis", "00_contract_helpers.R"))
source(file.path(repo_root, "03_analysis", "09_future_projections.R"))

cat("=== Phase 9: CNRM-ESM2-1 completion (missing 2071_2100 scenarios) ===\n")
cat(sprintf("Start: %s\n\n", Sys.time()))

res <- project_future_with_mess(
  repo_root     = repo_root,
  run_dir       = run_dir,
  gcm_filter    = "cnrm_esm2_1",
  skip_existing = TRUE
)

cat(sprintf("\n=== Complete: %d scenarios processed ===\n", res$n_scenarios))
cat(sprintf("End: %s\n", Sys.time()))
