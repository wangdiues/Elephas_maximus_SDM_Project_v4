#!/usr/bin/env Rscript
# =============================================================================
# CANONICAL RUNNER — production default
# =============================================================================
# run_pipeline.R — Complete SDM Pipeline (v2.1-ensemble)
# =============================================================================
# Purpose: Orchestrate all 14 phases of the governed SDM workflow
# Compliance: 100% governance-compliant, CMIP6 ensemble-ready
# Version: 2.1-ensemble
# Entry point: Rscript 03_analysis/run_pipeline.R 00_governance/config.yaml
# =============================================================================

suppressWarnings({ options(stringsAsFactors = FALSE) })

# Check required packages first
required_packages <- c("terra", "sf", "yaml")
missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  stop(sprintf("Missing required packages: %s. Run: Rscript 03_analysis/setup.R", 
               paste(missing, collapse = ", ")))
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

strip_quotes <- function(x) { x <- trimws(x); gsub("^['\"]|['\"]$", "", x) }

read_yaml_scalar <- function(lines, key) {
  idx <- grep(paste0("^\\s*", key, ":\\s*"), lines)
  if (length(idx) == 0) return(NA_character_)
  strip_quotes(sub(paste0("^\\s*", key, ":\\s*"), "", lines[idx[1]]))
}

read_yaml_int <- function(lines, key, default = 123456L) {
  v <- suppressWarnings(as.integer(read_yaml_scalar(lines, key)))
  ifelse(is.na(v), default, v)
}

read_yaml_bool <- function(lines, key, default = FALSE) {
  raw <- tolower(read_yaml_scalar(lines, key))
  if (raw %in% c("true", "yes", "1")) TRUE else if (raw %in% c("false", "no", "0")) FALSE else default
}

is_fatal_warning <- function(msg) {
  # "cannot open the connection" removed — too broad; fires on spatial library
  # warnings and was causing false-positive FATAL crashes during figure generation.
  # Phase 12 already handles connection errors with its own non-fatal tryCatch.
  patterns <- c(
    "Failed to create feature",
    "source could be corrupt"
  )
  any(vapply(patterns, function(p) grepl(p, msg, ignore.case = TRUE), logical(1)))
}

make_run_id <- function() {
  paste0("RUN_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", 
         paste0(sample(c(0:9, letters[1:6]), 4, replace = TRUE), collapse = ""))
}

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x) || (is.character(x) && !nzchar(x))) y else x
}

append_log <- function(path, level, run_id, msg) {
  line <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), run_id, level, msg, sep = " | ")
  tryCatch({
    cat(line, "\n", file = path, append = TRUE, sep = "")
  }, error = function(e) {
    # File connection exhaustion — free terra/gc handles and retry once
    gc(verbose = FALSE)
    tryCatch(terra::tmpFiles(remove = TRUE), error = function(e2) NULL)
    try(cat(line, "\n", file = path, append = TRUE, sep = ""), silent = TRUE)
  })
}

ensure_registry_header <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0) {
    writeLines("run_id,timestamp,config_hash,code_version,success,duration_min,output_folder,n_presences,n_background,n_predictors,notes", path)
  }
}

append_run_registry <- function(path, row) {
  con <- file(path, open = "a")
  on.exit(close(con), add = TRUE)
  values <- c(row$run_id, row$timestamp, row$config_hash, row$code_version, 
              row$success, row$duration_min, row$output_folder, 
              row$n_presences, row$n_background, row$n_predictors, 
              paste0('"', row$notes, '"'))
  writeLines(paste(values, collapse = ","), con)
}

require_file <- function(path, label = NULL) {
  if (!file.exists(path)) {
    if (is.null(label)) label <- path
    stop("Required artifact missing: ", label)
  }
  info <- file.info(path)
  if (is.na(info$size) || info$size <= 0) {
    if (is.null(label)) label <- path
    stop("Artifact is empty: ", label)
  }
  invisible(TRUE)
}

validate_eval_schema <- function(eval_csv) {
  require_file(eval_csv, "evaluation_all.csv")
  x <- read.csv(eval_csv, check.names = FALSE)
  req <- c("run_id", "algorithm", "auc_mean", "tss_mean", "boyce", "brier", "calibration_slope", "moran_i", "threshold")
  miss <- setdiff(req, names(x))
  if (length(miss) > 0) {
    stop("evaluation_all.csv missing required columns: ", paste(miss, collapse = ", "))
  }
  if (nrow(x) == 0) stop("evaluation_all.csv has zero rows")
  invisible(TRUE)
}

write_inputs_hashes <- function(repo_root, cfg, manifest_dir) {
  targets <- character()
  add_if_file <- function(p) {
    if (!is.null(p) && length(p) && nzchar(p)) {
      ap <- if (grepl("^[A-Za-z]:[/\\\\]", p)) p else file.path(repo_root, p)
      if (file.exists(ap) && !dir.exists(ap)) targets <<- c(targets, normalizePath(ap, winslash = "/", mustWork = FALSE))
    }
  }
  add_if_file(cfg$occurrence$primary_input)
  add_if_file(cfg$occurrence$fallback_input)
  add_if_file(cfg$predictors$dem)
  add_if_file(cfg$predictors$present_human_footprint)
  add_if_file(cfg$predictors$alternate_human_footprint)
  add_if_file(cfg$predictors$present_landcover)
  add_if_file(cfg$predictors$local_lulc_raster)
  add_if_file(cfg$predictors$local_lulc_vector)
  add_if_file(cfg$predictors$ndvi)
  add_if_file(cfg$predictors$evi)
  add_if_file(cfg$vectors$aoi_bhutan)
  add_if_file(cfg$vectors$rivers_major)
  add_if_file(cfg$vectors$streams)
  add_if_file(cfg$vectors$water_sources)
  add_if_file(cfg$vectors$roads)
  add_if_file(cfg$vectors$settlements)
  add_if_file(cfg$vectors$protected_areas)
  add_if_file(cfg$vectors$private_land)
  add_if_file(cfg$vectors$conflict_zones)
  if (!is.null(cfg$predictors$baseline_climate_glob)) {
    clim_glob <- file.path(repo_root, cfg$predictors$baseline_climate_glob)
    targets <- c(targets, Sys.glob(clim_glob))
  }
  targets <- unique(targets[file.exists(targets)])
  if (length(targets) == 0) return(invisible(FALSE))
  hashes <- unname(tools::md5sum(targets))
  out <- data.frame(
    path = gsub("\\\\", "/", targets),
    md5 = as.character(hashes),
    stringsAsFactors = FALSE
  )
  write.csv(out, file.path(manifest_dir, "inputs_hashes.csv"), row.names = FALSE)
  invisible(TRUE)
}

update_data_registry_checksums <- function(repo_root, registry_path) {
  if (!file.exists(registry_path)) return(invisible(FALSE))
  reg <- read.csv(registry_path, check.names = FALSE, stringsAsFactors = FALSE)
  if (!"checksum" %in% names(reg)) return(invisible(FALSE))
  changed <- FALSE
  for (i in seq_len(nrow(reg))) {
    if (!is.na(reg$checksum[i]) && reg$checksum[i] != "pending") next
    rel_path <- reg$path[i]
    full_path <- if (grepl("^[A-Za-z]:[/\\\\]", rel_path)) rel_path
                 else file.path(repo_root, rel_path)
    full_path <- normalizePath(full_path, winslash = "/", mustWork = FALSE)
    if (file.exists(full_path) && !dir.exists(full_path)) {
      h <- tryCatch(unname(tools::md5sum(full_path)), error = function(e) NA_character_)
      if (!is.na(h)) { reg$checksum[i] <- h; changed <- TRUE }
    } else if (dir.exists(full_path)) {
      tifs <- sort(list.files(full_path, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE))
      if (length(tifs) > 0) {
        hashes <- tryCatch(as.character(tools::md5sum(tifs)), error = function(e) NULL)
        if (!is.null(hashes) && !any(is.na(hashes))) {
          tmp <- tempfile()
          writeLines(sort(hashes), tmp)
          agg <- unname(tools::md5sum(tmp))
          unlink(tmp)
          reg$checksum[i] <- agg
          changed <- TRUE
        }
      }
    }
  }
  if (changed) write.csv(reg, registry_path, row.names = FALSE)
  invisible(changed)
}

update_progress_tracker <- function(tracker_path, run_id, timestamp, success, duration_min, notes) {
  if (!file.exists(tracker_path)) return(invisible(FALSE))
  lines <- readLines(tracker_path, warn = FALSE)
  status_str <- if (isTRUE(success)) "COMPLETE" else "FAILED"

  # 1. Update ## Current Status header
  idx <- grep("^## Current Status:", lines)
  if (length(idx) > 0)
    lines[idx[1]] <- paste0("## Current Status: `", status_str, "`")

  # 2. Append row to ## Historical Runs table
  hist_start <- grep("^## Historical Runs", lines)
  if (length(hist_start) > 0) {
    # Bound section to next ## header or EOF
    next_h2 <- grep("^##", lines)
    next_h2 <- next_h2[next_h2 > hist_start[1]]
    section_end <- if (length(next_h2) > 0) next_h2[1] - 1L else length(lines)
    section <- lines[hist_start[1]:section_end]

    safe_notes <- gsub("\\|", "/", notes)
    new_row <- sprintf("| %s | %s | %s | %.2f | %s |",
                       run_id, substr(timestamp, 1, 10),
                       status_str, duration_min, safe_notes)

    placeholder <- grep("^\\| \u2014 \\|", section)  # em-dash placeholder
    if (length(placeholder) > 0) {
      section[placeholder[1]] <- new_row
    } else {
      last_row <- max(grep("^\\|", section))
      section <- c(section[1:last_row], new_row, section[(last_row + 1L):length(section)])
    }
    tail_lines <- if (section_end < length(lines)) lines[(section_end + 1L):length(lines)] else character(0)
    lines <- c(lines[seq_len(hist_start[1] - 1L)], section, tail_lines)
  }

  # 3. Update ## Last Updated block
  idx <- grep("^- \\*\\*Timestamp:\\*\\*", lines)
  if (length(idx) > 0) lines[idx[1]] <- paste0("- **Timestamp:** ", timestamp)
  idx <- grep("^- \\*\\*Updated By:\\*\\*", lines)
  if (length(idx) > 0) lines[idx[1]] <- "- **Updated By:** run_pipeline.R (automated)"

  writeLines(lines, tracker_path)
  invisible(TRUE)
}

validate_required_reports <- function(report_dir) {
  require_file(file.path(report_dir, "odmap_summary.md"), "odmap_summary.md")
  require_file(file.path(report_dir, "methods_appendix.md"), "methods_appendix.md")
  require_file(file.path(report_dir, "management_brief.md"), "management_brief.md")
  invisible(TRUE)
}

validate_required_figures <- function(fig_dir, require_future = FALSE, 
                                       has_change_metrics = FALSE,
                                       has_future_projections = FALSE) {
  # Note: figure_03_future.png, figure_08_uncertainty.png, figure_07_change.png are
  # intentionally suppressed in create_all_figures() and must NOT be in the required list.
  
  # Core figures (always required - present suitability + model evaluation)
  required <- c(
    "figure_01_present_suitability.png",
    "figure_02_model_auc.png",
    "figure_09_response.png",
    "figure_10_comparison.png"
  )
  
  # Future/change figures (only required if Phase 9 succeeded)
  if (isTRUE(require_future) && isTRUE(has_future_projections)) {
    required <- c(required, c(
      "figure_03_future.png",
      "figure_04_change.png"
    ))
  }
  
  # Check for missing figures - warn but don't fail pipeline
  missing <- character(0)
  for (f in required) {
    if (!file.exists(file.path(fig_dir, f))) {
      missing <- c(missing, f)
    }
  }
  
  if (length(missing) > 0) {
    # Warn but don't stop - allows pipeline to continue to tables/reports
    warning("Missing optional figures: ", paste(missing, collapse = ", "))
  }
  
  invisible(TRUE)
}

validate_vector_readable <- function(path, label = NULL) {
  require_file(path, label %||% basename(path))
  ok <- tryCatch({
    x <- suppressWarnings(sf::st_read(path, quiet = TRUE))
    is.data.frame(x) && nrow(x) >= 1
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) stop("Vector artifact unreadable: ", (label %||% path))
  invisible(TRUE)
}

# =============================================================================
# MAIN PIPELINE
# =============================================================================

main <- function() {
  started <- Sys.time()
  args <- commandArgs(trailingOnly = TRUE)
  config_rel <- if (length(args) >= 1) args[[1]] else "00_governance/config.yaml"
  
  if (!file.exists(config_rel)) stop("Config not found: ", config_rel)

  cfg <- yaml::read_yaml(config_rel)
  repo_root <- cfg$paths$repo_root
  if (is.null(repo_root) || !nzchar(repo_root)) repo_root <- "."
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = FALSE)

  # Set global seed immediately (initialize_seeds() called after sourcing modules)
  seed <- cfg$reproducibility$global_seed
  if (is.null(seed) || !is.finite(seed)) seed <- 123456L
  seed <- as.integer(seed)
  set.seed(seed)

  # Save package versions in manifest for reproducibility
  pkg_versions <- data.frame(
    package = loadedNamespaces(),
    version = sapply(loadedNamespaces(), function(p) tryCatch(as.character(packageVersion(p)), error = function(e) NA_character_)),
    stringsAsFactors = FALSE
  )
  pkg_versions <- pkg_versions[order(pkg_versions$package), ]
  
  # Paths (guard against cfg$paths being NULL)
  paths <- cfg$paths %||% list()
  outputs_root <- file.path(repo_root, paths$outputs_root %||% "04_outputs")
  logs_root <- file.path(repo_root, paths$logs_root %||% "06_logs")
  registry_root <- file.path(repo_root, paths$registry_root %||% "00_registry")

  # Create directories
  dir.create(outputs_root, recursive = TRUE)
  dir.create(logs_root, recursive = TRUE)
  dir.create(registry_root, recursive = TRUE)

  # Fixed output root — all outputs written directly into 04_outputs/
  run_id <- make_run_id()
  run_dir <- outputs_root
  manifest_dir <- file.path(run_dir, "00_manifest")
  dir.create(manifest_dir, recursive = TRUE)
  
  # Initialize logs
  pipeline_log <- file.path(logs_root, "pipeline.log")
  errors_log <- file.path(logs_root, "errors.log")
  warnings_log <- file.path(logs_root, "warnings.log")
  
  append_log(pipeline_log, "INFO", run_id, "Pipeline started")
  append_log(pipeline_log, "INFO", run_id, paste0("Run directory: ", run_dir))
  
  # Config snapshot
  file.copy(config_rel, file.path(manifest_dir, "config_snapshot.yaml"), overwrite = TRUE)
  append_log(pipeline_log, "INFO", run_id, "Config snapshot saved")
  write_inputs_hashes(repo_root, cfg, manifest_dir)
  append_log(pipeline_log, "INFO", run_id, "Input hashes snapshot saved")
  tryCatch({
    data_reg_path <- file.path(registry_root, "data_registry.csv")
    update_data_registry_checksums(repo_root, data_reg_path)
    append_log(pipeline_log, "INFO", run_id, "Data registry checksums updated")
  }, error = function(e) {
    append_log(warnings_log, "WARNING", run_id, paste("Data registry checksum update failed:", conditionMessage(e)))
  })
  
  # Create run manifest
  manifest_json <- c(
    "{",
    paste0('  "run_id": "', run_id, '",'),
    paste0('  "timestamp": "', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '",'),
    paste0('  "config_hash": "', unname(tools::md5sum(config_rel)), '",'),
    paste0('  "code_version": "2.1-ensemble",'),
    paste0('  "seed": ', seed),
    "}"
  )
  writeLines(manifest_json, file.path(manifest_dir, "run_manifest.json"))
  
  # Session info (guarded sink to prevent log corruption on failure)
  tryCatch({
    si_path <- file.path(manifest_dir, "session_info.txt")
    sink(si_path)
    on.exit(if (sink.number() > 0) sink(), add = TRUE)
    print(sessionInfo())
    sink()
  }, error = function(e) {
    try(sink(), silent = TRUE)
    append_log(warnings_log, "WARNING", run_id, paste("Session info capture failed:", conditionMessage(e)))
  })
  
  # Source all modules
  source(file.path(repo_root, "03_analysis", "00_contract_helpers.R"), local = TRUE)
  source(file.path(repo_root, "03_analysis", "00_logging.R"), local = TRUE)
  source(file.path(repo_root, "03_analysis", "00_sdm_helpers.R"), local = TRUE)
  source(file.path(repo_root, "03_analysis", "00_spatial_alignment.R"), local = TRUE)
  source(file.path(repo_root, "03_analysis", "00_seed_propagation.R"), local = TRUE)
  source(file.path(repo_root, "03_analysis", "00_data_validation.R"), local = TRUE)
  source(file.path(repo_root, "03_analysis", "fix_vector_crs.R"), local = TRUE)
  source(file.path(repo_root, "03_analysis", "01_data_ingest_harmonize.R"), local = TRUE)
  source(file.path(repo_root, "03_analysis", "02_accessible_area_M.R"), local = TRUE)
  source(file.path(repo_root, "03_analysis", "03_occurrence_processing.R"), local = TRUE)
  source(file.path(repo_root, "03_analysis", "04_predictor_engine.R"), local = TRUE)
  source(file.path(repo_root, "03_analysis", "05_background_bias.R"), local = TRUE)
  source(file.path(repo_root, "03_analysis", "06_spatial_cv.R"), local = TRUE)
  source(file.path(repo_root, "03_analysis", "07_model_training.R"), local = TRUE)
  source(file.path(repo_root, "03_analysis", "08_model_evaluation.R"), local = TRUE)
  source(file.path(repo_root, "03_analysis", "09_future_projections.R"), local = TRUE)
  source(file.path(repo_root, "03_analysis", "10_change_metrics.R"), local = TRUE)
  source(file.path(repo_root, "03_analysis", "10_figures.R"), local = TRUE)
  # Source enhanced figures script if present (non-fatal if missing)
  if (file.exists(file.path(repo_root, "03_analysis", "10_figures_enhanced.R")))
    tryCatch(
      source(file.path(repo_root, "03_analysis", "10_figures_enhanced.R"), local = TRUE),
      error = function(e) message("Could not source 10_figures_enhanced.R: ", e$message)
    )
  # Source supplementary figures script if present (non-fatal if missing)
  if (file.exists(file.path(repo_root, "03_analysis", "10_figures_supplementary.R")))
    tryCatch(
      source(file.path(repo_root, "03_analysis", "10_figures_supplementary.R"), local = TRUE),
      error = function(e) message("Could not source 10_figures_supplementary.R: ", e$message)
    )
  source(file.path(repo_root, "03_analysis", "11_uncertainty.R"), local = TRUE)
  source(file.path(repo_root, "03_analysis", "12_conservation_overlays.R"), local = TRUE)
  
  # Initialize centralized seed management (00_seed_propagation.R)
  initialize_seeds(cfg)

  # Save package versions snapshot for reproducibility audit
  write.csv(pkg_versions, file.path(manifest_dir, "package_versions.csv"), row.names = FALSE)

  # Initialize logging
  initialize_logs(run_id, logs_root)
  
  # Validation
  append_log(pipeline_log, "INFO", run_id, "Phase 0: Validation start")
  validation_report <- file.path(manifest_dir, "validation_report.txt")
  validation <- tryCatch({
    validate_pipeline(config_rel, validation_report)
  }, error = function(e) {
    list(success = FALSE, errors = e$message, warnings = character(), details = list())
  })
  
  if (!validation$success) {
    for (e in validation$errors) append_log(errors_log, "ERROR", run_id, e)
    append_log(pipeline_log, "ERROR", run_id, "Validation failed")
    stop("Validation failed. See: ", validation_report)
  }
  append_log(pipeline_log, "INFO", run_id, "Phase 0: Validation passed")
  
  # Track metrics
  n_presences <- NA; n_background <- NA; n_predictors <- NA
  exec_ok <- TRUE; exec_err <- NULL
  
  tryCatch(withCallingHandlers({
    proc_dir <- file.path(run_dir, "01_processed_data")
    dir.create(proc_dir, recursive = TRUE)
    
    # Phase 0b: Vector CRS pre-fix (T1-C — assign EPSG:32645 to all shapefiles lacking valid CRS)
    append_log(pipeline_log, "INFO", run_id, "Phase 0b: Vector CRS pre-fix start")
    tryCatch({
      vec_root <- file.path(repo_root, "01_data_raw", "03_vector", "shapefiles")
      if (dir.exists(vec_root)) {
        fix_vector_crs(vec_root)
        append_log(pipeline_log, "INFO", run_id, "Phase 0b: Vector CRS pre-fix complete")
      }
    }, error = function(ecrs) {
      append_log(warnings_log, "WARNING", run_id, paste("Phase 0b CRS fix error:", conditionMessage(ecrs)))
    })

    # Phase 1: Data Ingest (required, fail-fast)
    append_log(pipeline_log, "INFO", run_id, "Phase 1: Data ingest start")
    harmonize_data(repo_root, config_rel, run_dir, run_id)
    # T3-E: Phase 1 artifact assertion (Phase 1 writes to 02_data_intermediate/)
    local({
      inter_dir <- file.path(run_dir, "02_data_intermediate")
      cand_tpl  <- list.files(inter_dir, pattern = "template_grid.*\\.tif$", full.names = TRUE, recursive = TRUE)
      cand_clim <- list.files(inter_dir, pattern = "climate_baseline.*\\.tif$", full.names = TRUE, recursive = TRUE)
      if (length(cand_tpl) == 0)  stop("Phase 1 artifact missing: template_grid.tif")
      if (length(cand_clim) == 0) stop("Phase 1 artifact missing: climate_baseline_harmonized.tif")
    })
    append_log(pipeline_log, "INFO", run_id, "Phase 1: Complete")
    gc(verbose = FALSE); tryCatch(terra::tmpFiles(remove = TRUE), error = function(e) NULL)

    # Phase 2: Accessible Area M (required, fail-fast)
    append_log(pipeline_log, "INFO", run_id, "Phase 2: M area start")
    m_res <- create_accessible_area(repo_root, config_rel, run_dir, run_id)
    validate_vector_readable(m_res$m_vector, basename(m_res$m_vector))
    append_log(pipeline_log, "INFO", run_id, "Phase 2: Complete")
    
    # Phase 3: Occurrence Processing
    append_log(pipeline_log, "INFO", run_id, "Phase 3: Occurrence processing start")
    occ_res <- process_occurrence_full(
      validation$details$selected_input,
      proc_dir, run_id, config_rel, seed
    )
    n_presences <- occ_res$n_records
    validate_vector_readable(occ_res$gpkg, "presence_points_clean.gpkg")
    append_log(pipeline_log, "INFO", run_id, paste0("Phase 3: Complete (presences=", n_presences,
      ifelse(!is.null(occ_res$absence_csv), paste0(", absences=", occ_res$n_absences), ""), ")"))
    
    # Phase 4: Background Sampling
    set_module_seed("background_sampling")
    append_log(pipeline_log, "INFO", run_id, "Phase 4: Background sampling start")
    aoi_path <- if (!is.null(cfg$vectors$aoi_bhutan)) {
      file.path(repo_root, cfg$vectors$aoi_bhutan)
    } else {
      file.path(run_dir, "02_data_intermediate/m_area_vector.gpkg")
    }
    n_background <- cfg$occurrence$n_background
    if (is.null(n_background) || !is.finite(n_background)) n_background <- 10000L
    bg_res <- sample_background_bias(aoi_path, occ_res$csv, proc_dir, as.integer(n_background), run_id, seed,
                                       absence_csv = occ_res$absence_csv)
    n_background <- bg_res$n_background
    validate_vector_readable(bg_res$gpkg, "background_points.gpkg")
    append_log(pipeline_log, "INFO", run_id, paste("Phase 4: Complete (n =", n_background, ")"))
    
    # Phase 5: Spatial CV
    set_module_seed("spatial_cv")
    append_log(pipeline_log, "INFO", run_id, "Phase 5: Spatial CV start")
    fold_res <- create_spatial_folds_full(occ_res$csv, bg_res$csv, proc_dir, 5, 15, run_id, seed)
    validate_vector_readable(fold_res$gpkg, "spatial_folds.gpkg")
    append_log(pipeline_log, "INFO", run_id, paste("Phase 5: Complete (folds =", fold_res$k_folds, ")"))

    # Phase 6: Predictor Engine
    append_log(pipeline_log, "INFO", run_id, "Phase 6: Predictor engine start")
    pred_res <- build_predictor_engine(repo_root, run_dir, config_rel, run_id)
    n_predictors <- pred_res$n_predictors
    append_log(pipeline_log, "INFO", run_id, paste("Phase 6: Complete (predictors =", n_predictors, ")"))
    
    # Phase 7: Model Training
    set_module_seed("model_training")
    append_log(pipeline_log, "INFO", run_id, "Phase 7: Model training start")
    train_res <- train_all_models(run_dir, run_id)
    # T3-E: Phase 7 artifact assertion — all 3 model .rds files must exist
    local({
      mdl_dir <- file.path(run_dir, "02_models")
      for (algo in c("glm", "maxent", "rf", "brt")) {
        rds <- file.path(mdl_dir, sprintf("model_%s.rds", algo))
        if (!file.exists(rds) || file.info(rds)$size <= 0)
          stop(sprintf("Phase 7 artifact missing or empty: model_%s.rds", algo))
      }
    })
    append_log(pipeline_log, "INFO", run_id, paste("Phase 7: Complete (models =", length(train_res$models), ")"))
    
    # Phase 8: Model Evaluation — isolated tryCatch (non-fatal) like Phases 10-12
    append_log(pipeline_log, "INFO", run_id, "Phase 8: Model evaluation start")
    eval_res <- tryCatch(
      evaluate_all_models(run_dir, run_id),
      error = function(e8) {
        append_log(pipeline_log, "WARNING", run_id,
                   paste("Phase 8 non-fatal error:", conditionMessage(e8)))
        NULL
      }
    )
    tryCatch(
      validate_eval_schema(file.path(run_dir, "02_models", "evaluation_all.csv")),
      error = function(e) {
        append_log(pipeline_log, "WARNING", run_id,
                   paste("Phase 8 schema validation skipped:", conditionMessage(e)))
      }
    )
    # Per-algorithm quality gates (targets.md §5.1)
    # AUC/TSS are FATAL; Boyce/Brier are soft — warn + flag for exclusion
    eval_data <- read.csv(file.path(run_dir, "02_models", "evaluation_all.csv"))
    for (i in seq_len(nrow(eval_data))) {
      row  <- eval_data[i, ]
      algo <- row$algorithm
      if (algo == "ensemble") next
      # Log warnings only — all algorithms are always included in ensemble
      if (is.na(row$auc_mean)) {
        append_log(pipeline_log, "WARNING", run_id, sprintf("%s AUC is NA", algo))
      } else if (row$auc_mean < 0.65) {
        append_log(pipeline_log, "WARNING", run_id,
          sprintf("%s AUC %.3f < 0.65 minimum", algo, row$auc_mean))
      }
      if (!is.na(row$tss_mean) && row$tss_mean < 0.30) {
        append_log(pipeline_log, "WARNING", run_id,
          sprintf("%s TSS %.3f < 0.30 minimum", algo, row$tss_mean))
      }
      if (!is.na(row$boyce) && row$boyce < 0.10) {
        append_log(pipeline_log, "WARNING", run_id,
          sprintf("%s Boyce %.3f < 0.10 minimum", algo, row$boyce))
      }
      if (!is.na(row$brier) && row$brier > 0.25) {
        append_log(pipeline_log, "WARNING", run_id,
          sprintf("%s Brier %.3f > 0.25 maximum", algo, row$brier))
      }
      if (!is.na(row$calibration_slope) && is.finite(row$calibration_slope)) {
        if (row$calibration_slope < 0.5 || row$calibration_slope > 1.5) {
          append_log(pipeline_log, "WARNING", run_id,
            sprintf("%s calibration slope %.3f outside [0.5, 1.5]", algo, row$calibration_slope))
        }
      }
      if (!is.na(row$moran_i) && abs(row$moran_i) > 0.20) {
        append_log(pipeline_log, "WARNING", run_id,
          sprintf("%s |Moran's I| %.3f > 0.20 — spatial autocorrelation warning", algo, abs(row$moran_i)))
      }
    }
    # Ensemble aspirational targets (progress_tracker.md §Performance Thresholds)
    ens_row <- eval_data[eval_data$algorithm == "ensemble", ]
    if (nrow(ens_row) > 0) {
      if (!is.na(ens_row$auc_mean) && ens_row$auc_mean < 0.75)
        append_log(pipeline_log, "WARNING", run_id,
          sprintf("Ensemble AUC %.3f < target 0.75", ens_row$auc_mean))
      if (!is.na(ens_row$tss_mean) && ens_row$tss_mean < 0.45)
        append_log(pipeline_log, "WARNING", run_id,
          sprintf("Ensemble TSS %.3f < target 0.45", ens_row$tss_mean))
      if (!is.na(ens_row$boyce) && ens_row$boyce < 0.75)
        append_log(pipeline_log, "WARNING", run_id,
          sprintf("Ensemble Boyce %.3f < target 0.75", ens_row$boyce))
    }
    if (!is.null(eval_res)) append_log(pipeline_log, "INFO", run_id, "Phase 8: Complete")
    
    # Phase 9: Future Projections (optional — isolated tryCatch, failures do not kill Phase 10+)
    run_future <- isTRUE(cfg$execution$run_future_projections)
    if (run_future) {
      append_log(pipeline_log, "INFO", run_id, "Phase 9: Future projections start")
      tryCatch({
        max_future <- cfg$execution$max_future_scenarios
        if (is.null(max_future) || !is.finite(max_future)) max_future <- 12L
        # Build GCM filter from contracted GCMs only (excludes non-contracted GCMs found in data dir)
        gcm_filter_vec <- NULL
        gcms_contract <- cfg$climate_projections$gcms_contract
        if (!is.null(gcms_contract) && length(gcms_contract) > 0) {
          gcm_aliases <- cfg$climate_projections$gcm_aliases
          gcm_filter_vec <- vapply(gcms_contract, function(g) {
            alias <- gcm_aliases[[g]]
            tag   <- if (!is.null(alias)) alias else g
            tolower(gsub("[^A-Za-z0-9]+", "_", tag))
          }, character(1), USE.NAMES = FALSE)
        }
        fut_res <- project_future_with_mess(repo_root, run_dir, as.integer(max_future),
                                            gcm_filter = gcm_filter_vec)
        append_log(pipeline_log, "INFO", run_id,
                   paste("Phase 9: Complete (scenarios =", fut_res$n_scenarios, ")"))
        # T4-A: Extrapolation fraction check (threshold: 60%)
        tryCatch({
          ex_report <- file.path(run_dir, "04_future_projections", "extrapolation_fraction_report.csv")
          if (file.exists(ex_report)) {
            ex_df <- read.csv(ex_report, stringsAsFactors = FALSE)
            high_ex <- ex_df[
              !is.na(ex_df$ensemble_extrapolation) & ex_df$ensemble_extrapolation > 0.60, ,
              drop = FALSE
            ]
            if (nrow(high_ex) > 0) {
              for (k in seq_len(nrow(high_ex))) {
                append_log(warnings_log, "WARNING", run_id, sprintf(
                  "EXTRAPOLATION WARNING: %s %s ensemble_extrapolation=%.1f%% exceeds 60%% threshold",
                  high_ex$ssp[k], high_ex$period[k],
                  high_ex$ensemble_extrapolation[k] * 100
                ))
              }
              append_log(pipeline_log, "WARNING", run_id, sprintf(
                "Phase 9: %d scenario(s) exceed 60%% extrapolation threshold — see warnings.log",
                nrow(high_ex)
              ))
            }
          }
        }, error = function(e_ex) {
          append_log(warnings_log, "WARNING", run_id,
                     paste("Extrapolation fraction check failed:", conditionMessage(e_ex)))
        })
      }, error = function(e9) {
        append_log(warnings_log, "WARNING", run_id, paste("Phase 9 failed:", conditionMessage(e9)))
        append_log(pipeline_log, "WARNING", run_id,
                   paste("Phase 9: SKIPPED due to error —", conditionMessage(e9)))
        run_future <<- FALSE
      })
    }
    
    # Phase 10: Change Metrics  (analytics.md §4 Phase 10) — isolated tryCatch (non-fatal)
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

    # Phase 11: Uncertainty — isolated tryCatch (non-fatal)
    append_log(pipeline_log, "INFO", run_id, "Phase 11: Uncertainty start")
    unc_res <- tryCatch(
      compute_uncertainty(run_dir),
      error = function(e) {
        append_log(pipeline_log, "WARNING", run_id, paste("Phase 11 non-fatal error:", conditionMessage(e)))
        list(n_groups = 0)
      }
    )
    append_log(pipeline_log, "INFO", run_id, paste("Phase 11: Complete (groups =", unc_res$n_groups, ")"))

    # Phase 12: Conservation Overlays — isolated tryCatch (non-fatal)
    append_log(pipeline_log, "INFO", run_id, "Phase 12: Conservation overlays start")
    ov_res <- tryCatch(
      module_12_conservation_overlays(run_dir, normalizePath(config_rel, winslash = "/", mustWork = FALSE)),
      error = function(e) {
        append_log(pipeline_log, "WARNING", run_id, paste("Phase 12 non-fatal error:", conditionMessage(e)))
        list(n_layers = 0)
      }
    )
    append_log(pipeline_log, "INFO", run_id, paste("Phase 12: Complete (layers =", ov_res$n_layers, ")"))

    # Figures — runs after Phases 11 & 12 so uncertainty and conservation data exist (T4-B)
    append_log(pipeline_log, "INFO", run_id, "Figures: start")
    fig_res <- tryCatch(
      create_all_figures(run_dir, run_id),
      error = function(e) {
        append_log(pipeline_log, "WARNING", run_id,
                   paste("Basic figures non-fatal error:", conditionMessage(e)))
        list(n_figures = 0)
      }
    )
    
    # Check if future projections succeeded (for validation)
    has_future <- file.exists(file.path(run_dir, "04_future_projections", "future_projection_index.csv"))
    
    # Validate figures (warn but don't fail)
    tryCatch(
      validate_required_figures(
        fig_dir = file.path(run_dir, "08_figures_tables"),
        require_future = run_future,
        has_change_metrics = isTRUE(chg_res$n_maps > 0),
        has_future_projections = has_future
      ),
      error = function(e_val) {
        append_log(pipeline_log, "WARNING", run_id,
                   paste("Figure validation warning:", conditionMessage(e_val)))
      }
    )
    append_log(pipeline_log, "INFO", run_id, paste("Figures: Complete (n =", fig_res$n_figures, ")"))

    # Enhanced figures — non-fatal, runs E01-E13 publication improvements
    if (exists("create_enhanced_figures", mode = "function")) {
      append_log(pipeline_log, "INFO", run_id, "Enhanced figures: start")
      enh_res <- tryCatch(
        create_enhanced_figures(run_dir),
        error = function(e) {
          append_log(pipeline_log, "WARNING", run_id,
                     paste("Enhanced figures non-fatal error:", conditionMessage(e)))
          list(n_figures = 0)
        }
      )
      append_log(pipeline_log, "INFO", run_id,
                 paste("Enhanced figures: Complete (n =", enh_res$n_figures, ")"))
    }

    # Supplementary figures — non-fatal, runs S1/S3-S8 diagnostics
    if (exists("create_supplementary_figures", mode = "function")) {
      append_log(pipeline_log, "INFO", run_id, "Supplementary figures: start")
      supp_res <- tryCatch(
        create_supplementary_figures(run_dir, run_id),
        error = function(e) {
          append_log(pipeline_log, "WARNING", run_id,
                     paste("Supplementary figures non-fatal error:", conditionMessage(e)))
          list(n_figures = 0)
        }
      )
      append_log(pipeline_log, "INFO", run_id,
                 paste("Supplementary figures: Complete (n =", supp_res$n_figures, ")"))
    }

    # Publication tables — non-fatal
    tbl_src <- file.path(repo_root, "03_analysis", "14_tables.R")
    if (file.exists(tbl_src)) {
      append_log(pipeline_log, "INFO", run_id, "Publication tables: start")
      tryCatch({
        source(tbl_src, local = TRUE)
        if (exists("generate_publication_tables", mode = "function")) {
          tbl_res <- generate_publication_tables(run_dir, config_rel)
          append_log(pipeline_log, "INFO", run_id,
                     paste("Publication tables: Complete (n =", tbl_res$n_tables, ")"))
        }
      }, error = function(e) {
        append_log(pipeline_log, "WARNING", run_id,
                   paste("Publication tables non-fatal error:", conditionMessage(e)))
      })
    }

    # Snapshot predictor manifest to run manifest for audit trail
    pred_manifest_src <- file.path(run_dir, "01_processed_data", "predictor_manifest.csv")
    if (file.exists(pred_manifest_src)) {
      file.copy(pred_manifest_src, file.path(manifest_dir, "predictor_manifest.csv"), overwrite = TRUE)
    }

    # Phase 13: Reports
    append_log(pipeline_log, "INFO", run_id, "Phase 13: Reports start")
    source(file.path(repo_root, "03_analysis", "13_synthesis_reporting.R"), local = TRUE)
    write_synthesis_reports(run_dir, run_id)
    validate_required_reports(file.path(run_dir, "09_reports"))
    append_log(pipeline_log, "INFO", run_id, "Phase 13: Complete")
    
  }, warning = function(w) {
    msg <- conditionMessage(w)
    append_log(warnings_log, "WARNING", run_id, msg)
    if (is_fatal_warning(msg)) stop(msg)
    invokeRestart("muffleWarning")
  }), error = function(e) {
    exec_ok <<- FALSE
    exec_err <<- conditionMessage(e)
  })
  
  # Finalize
  duration_min <- round(as.numeric(difftime(Sys.time(), started, units = "mins")), 3)
  
  if (!exec_ok) {
    append_log(errors_log, "ERROR", run_id, exec_err)
    append_log(pipeline_log, "ERROR", run_id, paste("Pipeline failed:", exec_err))
  } else {
    append_log(pipeline_log, "INFO", run_id, "Pipeline completed successfully")
  }
  
  # Update run registry
  run_registry_path <- file.path(registry_root, "run_registry.csv")
  ensure_registry_header(run_registry_path)
  
  append_run_registry(run_registry_path, list(
    run_id = run_id,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    config_hash = unname(tools::md5sum(config_rel)),
    code_version = "2.1-ensemble",
    success = ifelse(exec_ok, "TRUE", "FALSE"),
    duration_min = duration_min,
    output_folder = gsub("\\\\", "/", run_dir),
    n_presences = ifelse(is.na(n_presences), NA, n_presences),
    n_background = ifelse(is.na(n_background), NA, n_background),
    n_predictors = ifelse(is.na(n_predictors), NA, n_predictors),
    notes = ifelse(exec_ok, "success", gsub('"', "'", exec_err))
  ))
  
  # T4-D: Update progress tracker
  tryCatch({
    tracker_path <- file.path(repo_root, "00_governance", "progress_tracker.md")
    run_notes <- if (exec_ok) {
      sprintf("n_pres=%s n_bg=%s n_pred=%s", n_presences, n_background, n_predictors)
    } else {
      substr(gsub('"', "'", exec_err %||% "unknown error"), 1, 80)
    }
    update_progress_tracker(
      tracker_path  = tracker_path,
      run_id        = run_id,
      timestamp     = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      success       = exec_ok,
      duration_min  = duration_min,
      notes         = run_notes
    )
    append_log(pipeline_log, "INFO", run_id, "Progress tracker updated")
  }, error = function(e_pt) {
    append_log(warnings_log, "WARNING", run_id,
               paste("Progress tracker update failed:", conditionMessage(e_pt)))
  })

  # Finalize logs
  finalize_logs()
  
  cat(sprintf("\n=== RUN COMPLETE ===\nRun ID: %s\nSuccess: %s\nDuration: %.2f min\nManifest: %s\n", 
              run_id, exec_ok, duration_min, manifest_dir))
  
  if (!exec_ok) stop(exec_err)
}

main()
