#!/usr/bin/env Rscript
# STD-HARDENING: 2026-02-24
# role: phase_or_helper
# note: normalized script metadata for deterministic maintenance

if (!exists("write_synthesis_reports", mode = "function")) {
  # Resolve path relative to this script's location
  this_dir <- if (exists("repo_root")) file.path(repo_root, "03_analysis") else "03_analysis"
  source(file.path(this_dir, "10_synthesis_reporting.R"), local = TRUE)
}

module_13_synthesis_reporting <- function(run_dir, run_id) {
  write_synthesis_reports(run_dir = run_dir, run_id = run_id)
}

