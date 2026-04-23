#!/usr/bin/env Rscript
# Regenerate figures + reports for an existing run without re-running the full pipeline
# Usage: Rscript rerun_figures.R [run_dir] [config_path]

args <- commandArgs(trailingOnly = TRUE)
repo_root <- normalizePath(".", winslash = "/")

# Source helpers
for (f in c("00_logging.R", "00_sdm_helpers.R", "00_contract_helpers.R",
            "00_spatial_alignment.R", "00_seed_propagation.R",
            "10_figures.R")) {
  src <- file.path(repo_root, "03_analysis", f)
  if (file.exists(src)) tryCatch(source(src, local = TRUE),
    error = function(e) message("Could not source ", f, ": ", e$message))
}
if (file.exists(file.path(repo_root, "03_analysis", "10_figures_enhanced.R")))
  tryCatch(source(file.path(repo_root, "03_analysis", "10_figures_enhanced.R"), local = TRUE),
    error = function(e) message("Could not source 10_figures_enhanced.R: ", e$message))
if (file.exists(file.path(repo_root, "03_analysis", "10_figures_supplementary.R")))
  tryCatch(source(file.path(repo_root, "03_analysis", "10_figures_supplementary.R"), local = TRUE),
    error = function(e) message("Could not source 10_figures_supplementary.R: ", e$message))

run_dir     <- if (length(args) >= 1) args[[1]] else {
  find_latest_run_dir(repo_root)
}
config_path <- if (length(args) >= 2) args[[2]] else "00_governance/config.yaml"
run_id      <- read_run_id_from_manifest(run_dir)
cat(sprintf("Regenerating figures for: %s\n", run_id))

# Basic figures
cat("Running create_all_figures...\n")
fig_res <- tryCatch(
  create_all_figures(run_dir, run_id),
  error = function(e) { message("Basic figures error: ", e$message); list(n_figures = 0) }
)
cat(sprintf("Basic figures done: %d\n", fig_res$n_figures))

# Enhanced figures
if (exists("create_enhanced_figures", mode = "function")) {
  cat("Running create_enhanced_figures...\n")
  enh_res <- tryCatch(
    create_enhanced_figures(run_dir),
    error = function(e) { message("Enhanced figures error: ", e$message); list(n_figures = 0) }
  )
  cat(sprintf("Enhanced figures done: %d\n", enh_res$n_figures))
}

# Supplementary figures
if (exists("create_supplementary_figures", mode = "function")) {
  cat("Running create_supplementary_figures...\n")
  supp_res <- tryCatch(
    create_supplementary_figures(run_dir, run_id),
    error = function(e) { message("Supplementary figures error: ", e$message); list(n_figures = 0) }
  )
  cat(sprintf("Supplementary figures done: %d\n", supp_res$n_figures))
}

# Publication tables
tbl_src <- file.path(repo_root, "03_analysis", "14_tables.R")
if (file.exists(tbl_src)) {
  tryCatch(source(tbl_src, local = TRUE),
    error = function(e) message("Could not source 14_tables.R: ", e$message))
}
if (exists("generate_publication_tables", mode = "function")) {
  cat("Running generate_publication_tables...\n")
  tbl_res <- tryCatch(
    generate_publication_tables(run_dir, config_path),
    error = function(e) { message("Tables error: ", e$message); list(n_tables = 0) }
  )
  cat(sprintf("Tables done: %d\n", tbl_res$n_tables))
}

# Reports
rpt_src <- file.path(repo_root, "03_analysis", "13_synthesis_reporting.R")
if (file.exists(rpt_src)) {
  source(rpt_src, local = TRUE)
  tryCatch(write_synthesis_reports(run_dir, run_id),
    error = function(e) message("Reports error: ", e$message))
}

cat(sprintf("\nOutputs written to: %s/08_figures_tables\n", run_dir))
