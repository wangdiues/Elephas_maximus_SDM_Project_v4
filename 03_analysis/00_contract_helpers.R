#!/usr/bin/env Rscript
# =============================================================================
# 00_contract_helpers.R — Config, Path, Manifest & Validation Utilities
# =============================================================================
# Purpose: Config loading, path resolution, run structure, file validation.
#          Metric functions (auc_score, tss_best, brier_score, boyce, MESS,
#          collinearity, spatial blocking, calibration, Moran's I) live in
#          00_sdm_helpers.R — do NOT duplicate them here.
# =============================================================================

suppressPackageStartupMessages({
  library(yaml)
  library(terra)
  library(sf)
})

# =============================================================================
# CONFIG LOADING
# =============================================================================

#' Load and validate project configuration
#' @param config_path Path to config.yaml
#' @return Validated config list
load_contract_config <- function(config_path = "00_governance/config.yaml") {
  if (!file.exists(config_path)) stop("Config file not found: ", config_path)
  config <- yaml::read_yaml(config_path)
  required_sections <- c("project", "paths", "occurrence", "predictors", "reproducibility")
  missing <- setdiff(required_sections, names(config))
  if (length(missing) > 0)
    stop("Missing required config sections: ", paste(missing, collapse = ", "))
  if (!is.null(config$reproducibility$global_seed))
    set.seed(config$reproducibility$global_seed)
  config
}

# =============================================================================
# PATH MANAGEMENT
# =============================================================================

#' Resolve path relative to repo root
#' @param path Relative or absolute path
#' @param repo_root Repository root (optional)
#' @return Absolute path
resolve_path <- function(path, repo_root = NULL) {
  if (is.na(path) || !nzchar(path)) return(NA_character_)
  if (grepl("^[A-Za-z]:[/\\\\]", path) || startsWith(path, "/"))
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  if (!is.null(repo_root))
    return(normalizePath(file.path(repo_root, path), winslash = "/", mustWork = FALSE))
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' Create run directory structure
#' @param runs_root Base runs directory
#' @param run_id Run identifier
#' @return List of directory paths
create_run_structure <- function(runs_root, run_id) {
  run_dir <- file.path(runs_root, run_id)
  dirs <- c("00_manifest", "01_processed_data", "02_models",
            "03_present_suitability", "04_future_projections",
            "05_change_metrics", "06_uncertainty", "07_overlays",
            "08_figures_tables", "09_reports")
  for (d in dirs) dir.create(file.path(run_dir, d), recursive = TRUE, showWarnings = FALSE)
  setNames(
    as.list(file.path(run_dir, c("", dirs))),
    c("run_dir", "manifest_dir", "processed_dir", "model_dir", "present_dir",
      "future_dir", "change_dir", "uncertainty_dir", "overlay_dir",
      "figure_dir", "report_dir")
  )
}

# =============================================================================
# MANIFEST CREATION
# =============================================================================

#' Create run manifest (JSON + config snapshot + session info)
#' @param run_id Run ID
#' @param config_path Config path
#' @param output_dir Manifest directory
#' @return Manifest path
create_run_manifest <- function(run_id, config_path, output_dir) {
  manifest_path <- file.path(output_dir, "run_manifest.json")
  json_lines <- c(
    "{",
    sprintf('  "run_id": "%s",', run_id),
    sprintf('  "timestamp": "%s",', format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    sprintf('  "config_hash": "%s",', unname(tools::md5sum(config_path))),
    '  "code_version": "2.1-ensemble",',
    sprintf('  "r_version": "%s.%s",', R.version$major, R.version$minor),
    sprintf('  "platform": "%s"', R.version$platform),
    "}"
  )
  writeLines(json_lines, manifest_path)
  file.copy(config_path, file.path(output_dir, "config_snapshot.yaml"), overwrite = TRUE)
  tryCatch({
    si_path <- file.path(output_dir, "session_info.txt")
    sink(si_path)
    on.exit(if (sink.number() > 0) sink(), add = TRUE)
    print(sessionInfo())
    sink()
  }, error = function(e) {
    try(sink(), silent = TRUE)
  })
  manifest_path
}

# =============================================================================
# FILE VALIDATION
# =============================================================================

#' Check if file exists and is valid
#' @param path File path
#' @param type File type ("raster", "vector", "table")
#' @param template Template raster for geometry comparison (optional)
#' @return List with valid (bool) and error (string or NULL)
validate_file <- function(path, type = "raster", template = NULL) {
  result <- list(valid = FALSE, error = NULL)
  if (!file.exists(path))     { result$error <- "File does not exist"; return(result) }
  if (file.info(path)$size == 0) { result$error <- "File is empty";     return(result) }
  if (type == "raster") {
    r <- try(rast(path), silent = TRUE)
    if (inherits(r, "try-error"))   { result$error <- "Cannot read raster"; return(result) }
    if (all(is.na(values(r))))      { result$error <- "All values are NA";  return(result) }
    if (var(values(r), na.rm = TRUE) == 0) { result$error <- "Zero variance"; return(result) }
    if (!is.null(template) && !isTRUE(compareGeom(r, template, stopOnError = FALSE)))
      { result$error <- "Geometry mismatch with template"; return(result) }
  } else if (type == "vector") {
    v <- try(vect(path), silent = TRUE)
    if (inherits(v, "try-error")) { result$error <- "Cannot read vector"; return(result) }
    if (nrow(v) == 0)             { result$error <- "Vector is empty";    return(result) }
  } else if (type == "table") {
    df <- try(read.csv(path), silent = TRUE)
    if (inherits(df, "try-error")) { result$error <- "Cannot read CSV"; return(result) }
    if (nrow(df) == 0)             { result$error <- "Table is empty";  return(result) }
  }
  result$valid <- TRUE
  result
}
