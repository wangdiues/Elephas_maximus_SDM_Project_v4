#!/usr/bin/env Rscript
# Complete the missing cnrm_esm2_1 scenarios (2071_2100 period for all 4 SSPs)
# Skip already-existing output files.

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_file) == 0) {
  frame_files <- vapply(sys.frames(), function(env) if (is.null(env$ofile)) "" else as.character(env$ofile), character(1))
  frame_files <- frame_files[nzchar(frame_files)]
  if (length(frame_files) > 0) script_file <- frame_files[[length(frame_files)]]
}
script_dir <- if (length(script_file) > 0) dirname(normalizePath(sub("^--file=", "", script_file[1]), winslash = "/", mustWork = FALSE)) else normalizePath("03_analysis", winslash = "/", mustWork = FALSE)
source(file.path(script_dir, "00_repo_paths.R"))

repo_root <- find_repo_root()
args <- commandArgs(trailingOnly = TRUE)
run_dir <- resolve_run_dir(if (length(args) >= 1) args[[1]] else NULL, repo_root = repo_root)

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
