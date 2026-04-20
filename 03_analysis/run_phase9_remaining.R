#!/usr/bin/env Rscript
# Run Phase 9 (future projections) for the 5 remaining GCMs only.
# Skip the 3 already-completed GCMs (acces_cm2, cnrm_cm6_1, cnrm_esm2_1).

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

remaining_gcms <- c("inm_cm4_8", "inm_cm5_0", "miroc6", "miroc_es2l", "mpi_esm1_2_lr")

cat("=== Phase 9: Future Projections (remaining 5 GCMs) ===\n")
cat(sprintf("Run dir: %s\n", run_dir))
cat(sprintf("GCMs: %s\n", paste(remaining_gcms, collapse = ", ")))
cat(sprintf("Start: %s\n\n", Sys.time()))

res <- project_future_with_mess(
  repo_root     = repo_root,
  run_dir       = run_dir,
  gcm_filter    = remaining_gcms,
  skip_existing = FALSE
)

cat(sprintf("\n=== Phase 9 complete: %d scenarios processed ===\n", res$n_scenarios))
cat(sprintf("End: %s\n", Sys.time()))
