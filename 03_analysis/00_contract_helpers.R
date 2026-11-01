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

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x) || (is.character(x) && !nzchar(x))) y else x
}

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
  # Preserve POSIX absolute paths verbatim when tests or containers invoke
  # this helper from Windows; normalizePath() would otherwise prepend the
  # current Windows drive and silently change the path's meaning.
  if (startsWith(path, "/")) return(gsub("\\\\", "/", path))
  if (grepl("^[A-Za-z]:[/\\\\]", path))
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  if (!is.null(repo_root))
    return(normalizePath(file.path(repo_root, path), winslash = "/", mustWork = FALSE))
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' Create an immutable per-run output directory structure
#' @param output_root Runs root (e.g. 04_outputs/runs/)
#' @param run_id Unique run identifier
#' @param require_empty Refuse to reuse a pre-existing non-empty run directory
#' @return List of directory paths
create_run_structure <- function(output_root, run_id, require_empty = TRUE) {
  if (is.null(run_id) || !nzchar(run_id) || !grepl("^RUN_[A-Za-z0-9_-]+$", run_id)) {
    stop("run_id must be a non-empty identifier beginning with RUN_")
  }
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  run_dir <- file.path(output_root, run_id)
  if (dir.exists(run_dir) && isTRUE(require_empty)) {
    existing <- list.files(run_dir, all.files = TRUE, no.. = TRUE)
    if (length(existing) > 0) stop("Refusing to reuse non-empty run directory: ", run_dir)
  }
  dirs <- c("00_manifest", "01_processed_data", "02_data_intermediate", "02_models",
            "03_present_suitability", "04_future_projections",
            "05_change_metrics", "06_uncertainty", "07_overlays",
            "08_figures_tables", "09_reports")
  for (d in dirs) dir.create(file.path(run_dir, d), recursive = TRUE, showWarnings = FALSE)
  setNames(
    as.list(file.path(run_dir, c("", dirs))),
    c("run_dir", "manifest_dir", "processed_dir", "intermediate_dir", "model_dir", "present_dir",
      "future_dir", "change_dir", "uncertainty_dir", "overlay_dir",
      "figure_dir", "report_dir")
  )
}

#' Read run_id from manifest JSON written by run_pipeline.R
#' @param run_dir Output directory (may be 04_outputs/ or legacy RUN_* dir)
#' @return run_id string, or "current" if manifest not found
read_run_id_from_manifest <- function(run_dir) {
  manifest_file <- file.path(run_dir, "00_manifest", "run_manifest.json")
  if (!file.exists(manifest_file)) return("current")
  mlines <- readLines(manifest_file, warn = FALSE)
  m <- regmatches(mlines, regexpr('"run_id":[[:space:]]*"[^"]+"', mlines))
  if (length(m) == 0) return("current")
  sub('"run_id":[[:space:]]*"([^"]+)"', "\\1", m[1])
}

#' Discover run roots from config, with compatibility for legacy layouts
#' @param repo_root Repository root
#' @param cfg Optional loaded config
#' @return Character vector of candidate run roots
discover_run_roots <- function(repo_root, cfg = NULL) {
  paths <- if (!is.null(cfg) && !is.null(cfg$paths)) cfg$paths else list()
  roots <- c(paths$runs_root, paths$outputs_root, "04_outputs")
  roots <- roots[!is.na(roots) & nzchar(roots)]
  roots <- unique(vapply(roots, function(root) {
    normalizePath(file.path(repo_root, root), winslash = "/", mustWork = FALSE)
  }, character(1)))
  roots
}

#' List all run directories, newest first
#' @param repo_root Repository root
#' @param cfg Optional loaded config
#' @return Character vector of RUN_* directories
list_run_dirs <- function(repo_root, cfg = NULL) {
  roots <- discover_run_roots(repo_root, cfg)
  out <- character()

  for (root in roots) {
    if (!dir.exists(root)) next

    candidates <- list.dirs(root, recursive = FALSE, full.names = TRUE)
    candidates <- candidates[grepl("^RUN_", basename(candidates))]

    legacy_root <- file.path(root, "runs")
    if (basename(root) == "04_outputs" && dir.exists(legacy_root)) {
      legacy_candidates <- list.dirs(legacy_root, recursive = FALSE, full.names = TRUE)
      legacy_candidates <- legacy_candidates[grepl("^RUN_", basename(legacy_candidates))]
      candidates <- c(candidates, legacy_candidates)
    }

    out <- c(out, candidates)
  }

  out <- unique(out[dir.exists(out)])
  if (length(out) == 0) return(out)
  out[order(file.info(out)$mtime, decreasing = TRUE)]
}

#' Return the newest immutable run directory
#' @param repo_root Repository root
#' @param cfg Optional loaded config
#' @return Absolute path to the output root directory
find_latest_run_dir <- function(repo_root, cfg = NULL) {
  runs <- list_run_dirs(repo_root, cfg)
  if (length(runs) == 0) stop("No immutable RUN_* directory found")
  runs[[1]]
}

#' Resolve an immutable run directory by exact run ID
#' @param repo_root Repository root
#' @param cfg Optional loaded config
#' @param run_id Run identifier (used only for legacy RUN_* lookup)
#' @return Absolute path to the run directory
locate_run_dir <- function(repo_root, cfg = NULL, run_id) {
  roots <- discover_run_roots(repo_root, cfg)
  for (root in roots) {
    candidate <- file.path(root, run_id)
    if (dir.exists(candidate)) return(candidate)
    legacy_candidate <- file.path(root, "runs", run_id)
    if (dir.exists(legacy_candidate)) return(legacy_candidate)
  }
  stop("Run directory not found for run_id: ", run_id)
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
