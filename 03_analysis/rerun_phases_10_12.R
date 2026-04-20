#!/usr/bin/env Rscript
# =============================================================================
# rerun_phases_10_12.R — Standalone re-execution of Phases 10-12 + figures
#
# Applies fixes and re-runs only the post-projection phases against an
# existing run directory. Does NOT re-run Phases 1-9 (no model re-training).
#
# Usage (PowerShell — ggsave segfaults under MSYS2 bash):
#   Rscript 03_analysis/rerun_phases_10_12.R [run_dir] [config_path]
#
# Defaults:
#   run_dir     = 04_outputs/runs/<latest>
#   config_path = 00_governance/config.yaml
# =============================================================================

suppressPackageStartupMessages({ library(terra); library(sf) })

args        <- commandArgs(trailingOnly = TRUE)
config_rel  <- if (length(args) >= 2) args[[2]] else "00_governance/config.yaml"
config_path <- normalizePath(config_rel, winslash = "/", mustWork = FALSE)

# Resolve repo root relative to this script's location
script_dir  <- tryCatch(
  normalizePath(dirname(sys.frame(1)$ofile), winslash = "/"),
  error = function(e) normalizePath("03_analysis", winslash = "/", mustWork = FALSE)
)
repo_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)

# Determine run_dir
if (length(args) >= 1) {
  run_dir <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
} else {
  runs_root <- file.path(repo_root, "04_outputs", "runs")
  run_dirs  <- sort(list.dirs(runs_root, recursive = FALSE, full.names = TRUE))
  if (length(run_dirs) == 0) stop("No run directories found in ", runs_root)
  run_dir   <- run_dirs[length(run_dirs)]  # latest
  cat(sprintf("Using latest run: %s\n", basename(run_dir)))
}
run_id <- basename(run_dir)

cat(sprintf("\n=== Standalone Phase 10-12 + Figures re-run ===\n"))
cat(sprintf("Run dir:    %s\n", run_dir))
cat(sprintf("Config:     %s\n", config_path))
cat(sprintf("Repo root:  %s\n\n", repo_root))

# Source all required modules
for (src in c("00_contract_helpers.R", "00_sdm_helpers.R", "00_spatial_alignment.R",
              "10_change_metrics.R", "11_uncertainty.R",
              "12_conservation_overlays.R", "10_figures.R", "10_figures_enhanced.R")) {
  f <- file.path(repo_root, "03_analysis", src)
  if (file.exists(f)) {
    tryCatch(source(f, local = FALSE),
             error = function(e) cat(sprintf("  Warning: could not source %s: %s\n", src, conditionMessage(e))))
  }
}

# ── Phase 10: Change Metrics ──────────────────────────────────────────────────
cat("--- Phase 10: Change Metrics ---\n")
chg_res <- tryCatch(
  module_10_change_metrics(run_dir, config_path = config_path),
  error = function(e) {
    cat(sprintf("Phase 10 error: %s\n", conditionMessage(e)))
    list(n_maps = 0)
  }
)
cat(sprintf("Phase 10 complete: %d delta maps\n\n", chg_res$n_maps))

# ── Phase 11: Uncertainty ─────────────────────────────────────────────────────
cat("--- Phase 11: Uncertainty ---\n")
unc_res <- tryCatch(
  compute_uncertainty(run_dir),
  error = function(e) {
    cat(sprintf("Phase 11 error: %s\n", conditionMessage(e)))
    list(n_groups = 0)
  }
)
cat(sprintf("Phase 11 complete: %d uncertainty groups\n\n", unc_res$n_groups))

# ── Phase 12: Conservation Overlays ──────────────────────────────────────────
cat("--- Phase 12: Conservation Overlays ---\n")
ov_res <- tryCatch(
  module_12_conservation_overlays(run_dir, config_path),
  error = function(e) {
    cat(sprintf("Phase 12 error: %s\n", conditionMessage(e)))
    list(n_layers = 0)
  }
)
cat(sprintf("Phase 12 complete: %d layers\n\n", ov_res$n_layers))

# ── Figures: pipeline figures (figure_01 ... figure_10) ───────────────────────
cat("--- Pipeline Figures (10_figures.R) ---\n")
fig_res <- tryCatch(
  create_all_figures(run_dir, run_id),
  error = function(e) {
    cat(sprintf("Pipeline figures error: %s\n", conditionMessage(e)))
    list(n_figures = 0)
  }
)
cat(sprintf("Pipeline figures: %d generated\n\n", fig_res$n_figures))

# ── Enhanced Figures (10_figures_enhanced.R) ─────────────────────────────────
cat("--- Enhanced Figures (10_figures_enhanced.R) ---\n")
enh_res <- tryCatch(
  create_enhanced_figures(run_dir),
  error = function(e) {
    cat(sprintf("Enhanced figures error: %s\n", conditionMessage(e)))
    list(n_figures = 0)
  }
)
cat(sprintf("Enhanced figures: %d generated\n\n", enh_res$n_figures))

# ── Summary ───────────────────────────────────────────────────────────────────
cat(sprintf("=== Re-run complete ===\n"))
cat(sprintf("  Phase 10 delta maps:      %d\n", chg_res$n_maps))
cat(sprintf("  Phase 11 uncertainty:     %d groups\n", unc_res$n_groups))
cat(sprintf("  Phase 12 overlay layers:  %d\n", ov_res$n_layers))
cat(sprintf("  Pipeline figures:         %d\n", fig_res$n_figures))
cat(sprintf("  Enhanced figures:         %d\n", enh_res$n_figures))
cat(sprintf("  Output dir: %s\n", file.path(run_dir, "08_figures_tables")))
