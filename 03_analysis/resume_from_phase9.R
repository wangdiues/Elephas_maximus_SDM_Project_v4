#!/usr/bin/env Rscript
# =============================================================================
# resume_from_phase9.R — Resume pipeline from Phase 9 using existing run
# =============================================================================
# Usage: Rscript 03_analysis/resume_from_phase9.R [RUN_ID]
#   If RUN_ID omitted, uses most recent run directory.
# =============================================================================

suppressWarnings({ options(stringsAsFactors = FALSE) })

args <- commandArgs(trailingOnly = TRUE)
config_rel <- "00_governance/config.yaml"
if (!file.exists(config_rel)) stop("Config not found: ", config_rel)

cfg <- yaml::read_yaml(config_rel)
repo_root <- cfg$paths$repo_root
if (is.null(repo_root) || !nzchar(repo_root)) repo_root <- "."
repo_root <- normalizePath(repo_root, winslash = "/", mustWork = FALSE)

source(file.path(repo_root, "03_analysis", "00_contract_helpers.R"), local = TRUE)

# Locate run directory
if (length(args) >= 1) {
  run_id <- args[1]
  run_dir <- locate_run_dir(repo_root, cfg, run_id)
} else {
  run_dir <- find_latest_run_dir(repo_root, cfg)
  run_id <- read_run_id_from_manifest(run_dir)
}
if (!dir.exists(run_dir)) stop("Run directory not found: ", run_dir)
cat(sprintf("=== RESUMING from Phase 9 ===\nRun ID:  %s\nRun dir: %s\n\n", run_id, run_dir))

# Verify Phases 1-8 artifacts exist
stopifnot(
  "Missing models dir" = dir.exists(file.path(run_dir, "02_models")),
  "Missing evaluation" = file.exists(file.path(run_dir, "02_models", "evaluation_all.csv")),
  "Missing template"   = file.exists(file.path(run_dir, "02_data_intermediate", "template_grid.tif"))
)

# Setup logs
logs_root <- file.path(repo_root, cfg$paths$logs_root %||% "06_logs")
dir.create(logs_root, recursive = TRUE, showWarnings = FALSE)
pipeline_log <- file.path(logs_root, "pipeline.log")
errors_log   <- file.path(logs_root, "errors.log")
warnings_log <- file.path(logs_root, "warnings.log")

append_log <- function(path, level, run_id, msg) {
  cat(paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), run_id, level, msg, sep = " | "),
      "\n", file = path, append = TRUE, sep = "")
}

is_fatal_warning <- function(msg) FALSE

seed <- cfg$reproducibility$global_seed
if (is.null(seed) || !is.finite(seed)) seed <- 123456L
seed <- as.integer(seed)
set.seed(seed)

# Source all modules
cat("Sourcing modules...\n")
source(file.path(repo_root, "03_analysis", "00_contract_helpers.R"), local = TRUE)
source(file.path(repo_root, "03_analysis", "00_logging.R"), local = TRUE)
source(file.path(repo_root, "03_analysis", "00_sdm_helpers.R"), local = TRUE)
source(file.path(repo_root, "03_analysis", "00_spatial_alignment.R"), local = TRUE)
source(file.path(repo_root, "03_analysis", "00_seed_propagation.R"), local = TRUE)
source(file.path(repo_root, "03_analysis", "00_data_validation.R"), local = TRUE)
source(file.path(repo_root, "03_analysis", "09_future_projections.R"), local = TRUE)
source(file.path(repo_root, "03_analysis", "10_change_metrics.R"), local = TRUE)
source(file.path(repo_root, "03_analysis", "10_figures.R"), local = TRUE)
source(file.path(repo_root, "03_analysis", "11_uncertainty.R"), local = TRUE)
source(file.path(repo_root, "03_analysis", "12_conservation_overlays.R"), local = TRUE)
source(file.path(repo_root, "03_analysis", "13_synthesis_reporting.R"), local = TRUE)

initialize_seeds(cfg)
initialize_logs(run_id, logs_root)

started <- Sys.time()
append_log(pipeline_log, "INFO", run_id, "=== RESUME from Phase 9 ===")

run_future <- isTRUE(cfg$execution$run_future_projections)

# Phase 9: Future Projections
if (run_future) {
  append_log(pipeline_log, "INFO", run_id, "Phase 9: Future projections start")
  tryCatch({
    max_future <- cfg$execution$max_future_scenarios
    if (is.null(max_future) || !is.finite(max_future)) max_future <- 12L
    fut_res <- project_future_with_mess(
      repo_root,
      run_dir,
      as.integer(max_future),
      skip_existing = TRUE
    )
    append_log(pipeline_log, "INFO", run_id,
               paste("Phase 9: Complete (scenarios =", fut_res$n_scenarios, ")"))
  }, error = function(e9) {
    append_log(warnings_log, "WARNING", run_id, paste("Phase 9 failed:", conditionMessage(e9)))
    append_log(pipeline_log, "WARNING", run_id,
               paste("Phase 9: SKIPPED due to error —", conditionMessage(e9)))
  })
} else {
  cat("Phase 9 skipped (run_future_projections = false in config)\n")
}

# Phase 10: Change Metrics
append_log(pipeline_log, "INFO", run_id, "Phase 10: Change metrics start")
chg_res <- tryCatch(
  module_10_change_metrics(run_dir, config_path = config_rel),
  error = function(e) {
    append_log(pipeline_log, "WARNING", run_id, paste("Phase 10 non-fatal error:", conditionMessage(e)))
    list(n_maps = 0)
  }
)
append_log(pipeline_log, "INFO", run_id,
  paste("Phase 10: Complete (scenarios =", chg_res$n_maps, ")"))

# Phase 11: Uncertainty
append_log(pipeline_log, "INFO", run_id, "Phase 11: Uncertainty start")
unc_res <- tryCatch(
  compute_uncertainty(run_dir),
  error = function(e) {
    append_log(pipeline_log, "WARNING", run_id, paste("Phase 11 non-fatal error:", conditionMessage(e)))
    list(n_groups = 0)
  }
)
append_log(pipeline_log, "INFO", run_id, paste("Phase 11: Complete (groups =", unc_res$n_groups, ")"))

# Phase 12: Conservation Overlays
append_log(pipeline_log, "INFO", run_id, "Phase 12: Conservation overlays start")
ov_res <- tryCatch(
  module_12_conservation_overlays(run_dir, config_rel),
  error = function(e) {
    append_log(pipeline_log, "WARNING", run_id, paste("Phase 12 non-fatal error:", conditionMessage(e)))
    list(n_layers = 0)
  }
)
append_log(pipeline_log, "INFO", run_id, paste("Phase 12: Complete (layers =", ov_res$n_layers, ")"))

# Figures
append_log(pipeline_log, "INFO", run_id, "Figures: start")
fig_res <- tryCatch(
  create_all_figures(run_dir, run_id),
  error = function(e) {
    append_log(pipeline_log, "WARNING", run_id, paste("Figures error:", conditionMessage(e)))
    list(n_figures = 0)
  }
)
append_log(pipeline_log, "INFO", run_id, paste("Figures: Complete (n =", fig_res$n_figures, ")"))

# Phase 13: Reports
append_log(pipeline_log, "INFO", run_id, "Phase 13: Reports start")
tryCatch({
  write_synthesis_reports(run_dir, run_id)
  append_log(pipeline_log, "INFO", run_id, "Phase 13: Complete")
}, error = function(e) {
  append_log(pipeline_log, "WARNING", run_id, paste("Phase 13 error:", conditionMessage(e)))
})

duration_min <- round(as.numeric(difftime(Sys.time(), started, units = "mins")), 3)
append_log(pipeline_log, "INFO", run_id,
  sprintf("=== RESUME COMPLETE (%.2f min) ===", duration_min))

finalize_logs()
cat(sprintf("\n=== RESUME COMPLETE ===\nRun ID: %s\nDuration: %.2f min\n", run_id, duration_min))
