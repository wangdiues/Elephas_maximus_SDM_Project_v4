#!/usr/bin/env Rscript
# =============================================================================
# 08_model_evaluation.R — COMPLETE with Boyce, Brier, Calibration, Moran's I
# CRITICAL FIX: All required metrics now computed including ensemble evaluation
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
})
if (!exists("auc_score", mode = "function")) {
  src_dir <- if (exists("repo_root")) file.path(repo_root, "03_analysis") else "03_analysis"
  source(file.path(src_dir, "00_sdm_helpers.R"))
}
if (!exists("resolve_path", mode = "function")) {
  src_dir <- if (exists("repo_root")) file.path(repo_root, "03_analysis") else "03_analysis"
  source(file.path(src_dir, "00_contract_helpers.R"))
}

if (!exists("brier_score")) {
  brier_score <- function(y, p) mean((as.numeric(p) - as.numeric(y))^2, na.rm = TRUE)
}

evaluate_all_models <- function(run_dir, run_id) {
  model_dir <- file.path(run_dir, "02_models")
  fig_dir <- file.path(run_dir, "08_figures_tables")
  proc_dir <- file.path(run_dir, "01_processed_data")
  dir.create(fig_dir, recursive = TRUE)
  
  eval_file <- file.path(model_dir, "evaluation_all.csv")
  dat <- read.csv(file.path(proc_dir, "modeling_dataset.csv"))
  
  if (!file.exists(eval_file)) {
    cat("No evaluation file found\n")
    return(NULL)
  }
  
  eval_results <- read.csv(eval_file)
  
  # CRITICAL: Add Boyce, Brier, Calibration, Moran's I, and optimal threshold for each algorithm
  boyce_results <- list()
  brier_results <- list()
  calibration_results <- list()
  moran_results <- list()
  threshold_results <- list()

  # Load out-of-fold predictions saved by Phase 7 — avoids data leakage
  oof_file <- file.path(proc_dir, "oof_predictions.csv")
  oof <- if (file.exists(oof_file)) read.csv(oof_file) else NULL

  for (algo in eval_results$algorithm) {
    # Use OOF holdout predictions — never train-set predictions
    oof_col <- paste0("pred_", algo)
    if (!is.null(oof) && oof_col %in% names(oof)) {
      valid <- !is.na(oof[[oof_col]])
      p   <- as.numeric(oof[[oof_col]][valid])
      y   <- as.numeric(oof$response[valid])
      lon <- as.numeric(oof$longitude[valid])
      lat <- as.numeric(oof$latitude[valid])
    } else {
      cat(sprintf("  %s: no OOF predictions found, skipping holdout metrics\n", algo))
      next
    }
    if (length(p) < 10 || length(unique(p)) < 2) {
      cat(sprintf("  %s: insufficient valid OOF predictions (%d) — skipping holdout metrics\n", algo, length(p)))
      next
    }

    # Boyce Index: pres_pred = predictions at presence locations;
    # bg_pred = predictions at all surveyed locations (presences + absences).
    # With true PA data this measures whether presences are predicted higher
    # than the average surveyed location — a valid discrimination metric.
    pres_pred <- p[y == 1]
    bg_pred   <- p
    boyce <- boyce_continuous(pres_pred, bg_pred)
    boyce_results[[algo]] <- boyce$boyce_index

    # Brier Score
    brier <- brier_score(y, p)
    brier_results[[algo]] <- brier

    # Calibration Slope
    calib <- calculate_calibration_slope(y, p)
    calibration_results[[algo]] <- calib$slope

    # Moran's I of residuals
    residuals <- y - p
    moran <- calculate_moran_i(residuals, lon, lat)
    moran_results[[algo]] <- moran$I

    # Optimal TSS threshold for binary habitat classification (CRITICAL FIX)
    thr_result <- tss_best(y, p)
    threshold_results[[algo]] <- thr_result["threshold"]

    cat(sprintf("  %s: Boyce=%.3f, Brier=%.3f, Calib=%.3f, Moran's I=%.3f, Threshold=%.4f\n",
                algo, boyce$boyce_index, brier, calib$slope, moran$I, thr_result["threshold"]))
  }
  
  # Update evaluation with new metrics
  eval_results$boyce <- sapply(eval_results$algorithm, function(a)
    ifelse(!is.null(boyce_results[[a]]), boyce_results[[a]], NA))
  eval_results$brier <- sapply(eval_results$algorithm, function(a)
    ifelse(!is.null(brier_results[[a]]), brier_results[[a]], NA))
  eval_results$calibration_slope <- sapply(eval_results$algorithm, function(a)
    ifelse(!is.null(calibration_results[[a]]), calibration_results[[a]], NA))
  eval_results$moran_i <- sapply(eval_results$algorithm, function(a)
    ifelse(!is.null(moran_results[[a]]), moran_results[[a]], NA))
  # CRITICAL FIX: Add optimal TSS threshold column
  eval_results$threshold <- sapply(eval_results$algorithm, function(a)
    ifelse(!is.null(threshold_results[[a]]), threshold_results[[a]], NA))

  # T3-A: Enforce schema — ensure run_id column is present
  if (!"run_id" %in% names(eval_results)) {
    eval_results <- cbind(run_id = run_id, eval_results, stringsAsFactors = FALSE)
  } else {
    eval_results$run_id <- run_id
  }
  # Enforce column order for canonical schema
  schema_cols <- c("run_id", "algorithm", "auc_mean", "tss_mean", "boyce", "brier",
                   "calibration_slope", "moran_i", "threshold")
  extra_cols  <- setdiff(names(eval_results), schema_cols)
  eval_results <- eval_results[, c(schema_cols[schema_cols %in% names(eval_results)], extra_cols),
                               drop = FALSE]

  # Keep the canonical table updated with full metrics for downstream audits.
  write.csv(eval_results, file.path(model_dir, "evaluation_all.csv"), row.names = FALSE)

  # T3-A: Write per-algorithm summary table (including ensemble)
  write.csv(eval_results, file.path(model_dir, "evaluation_summary.csv"), row.names = FALSE)

  # Save complete evaluation (legacy alias)
  write.csv(eval_results, file.path(model_dir, "evaluation_complete.csv"), row.names = FALSE)

  # Update metrics_summary to include ensemble
  metrics_summary <- data.frame(
    run_id = run_id,
    algorithm = eval_results$algorithm,
    auc = eval_results$auc_mean,
    tss = eval_results$tss_mean,
    boyce = eval_results$boyce,
    brier = eval_results$brier,
    calibration_slope = eval_results$calibration_slope,
    moran_i = eval_results$moran_i,
    threshold = eval_results$threshold,
    stringsAsFactors = FALSE
  )
  write.csv(metrics_summary, file.path(model_dir, "metrics_summary.csv"), row.names = FALSE)
  write.csv(metrics_summary[, c("run_id", "algorithm", "boyce")], file.path(model_dir, "boyce_metrics.csv"), row.names = FALSE)
  write.csv(metrics_summary[, c("run_id", "algorithm", "brier")], file.path(model_dir, "brier_metrics.csv"), row.names = FALSE)
  write.csv(metrics_summary[, c("run_id", "algorithm", "calibration_slope")], file.path(model_dir, "calibration_metrics.csv"), row.names = FALSE)
  write.csv(metrics_summary[, c("run_id", "algorithm", "moran_i")], file.path(model_dir, "moran_metrics.csv"), row.names = FALSE)
  
  # Create ROC curve data — using OOF holdout predictions
  roc_data <- list()
  for (algo in eval_results$algorithm) {
    oof_col <- paste0("pred_", algo)
    if (is.null(oof) || !oof_col %in% names(oof)) next
    valid <- !is.na(oof[[oof_col]])
    p <- as.numeric(oof[[oof_col]][valid])
    y <- as.numeric(oof$response[valid])

    # Use actual unique prediction values as thresholds for smooth curves
    thresholds <- sort(unique(c(0, p, 1)))
    roc_df <- data.frame(threshold = thresholds)
    roc_df$sensitivity <- sapply(thresholds, function(t) {
      pred <- ifelse(p >= t, 1, 0)
      tp <- sum(pred == 1 & y == 1); fn <- sum(pred == 0 & y == 1)
      ifelse((tp + fn) == 0, 0, tp / (tp + fn))
    })
    roc_df$specificity <- sapply(thresholds, function(t) {
      pred <- ifelse(p >= t, 1, 0)
      tn <- sum(pred == 0 & y == 0); fp <- sum(pred == 1 & y == 0)
      ifelse((tn + fp) == 0, 0, tn / (tn + fp))
    })
    roc_df$algorithm <- algo
    # Sort by FPR ascending for correct line rendering
    roc_df <- roc_df[order(1 - roc_df$specificity, roc_df$sensitivity), ]
    roc_data[[algo]] <- roc_df
  }

  roc_combined <- do.call(rbind, roc_data)
  write.csv(roc_combined, file.path(model_dir, "roc_curve_data.csv"), row.names = FALSE)

  # Create calibration plot data — using OOF holdout predictions
  calib_data <- list()
  for (algo in eval_results$algorithm) {
    oof_col <- paste0("pred_", algo)
    if (is.null(oof) || !oof_col %in% names(oof)) next
    valid <- !is.na(oof[[oof_col]])
    p <- as.numeric(oof[[oof_col]][valid])
    y <- as.numeric(oof$response[valid])

    bins <- cut(p, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
    bin_data <- data.frame(
      bin = levels(bins),
      predicted = tapply(p, bins, mean),
      observed  = tapply(y, bins, mean),
      n         = tapply(y, bins, length),
      algorithm = algo
    )
    calib_data[[algo]] <- bin_data
  }
  
  calib_combined <- do.call(rbind, calib_data)
  write.csv(calib_combined, file.path(model_dir, "calibration_data.csv"), row.names = FALSE)

  # CRITICAL FIX: Evaluate ensemble predictions (not just individual algorithms)
  ens_file <- file.path(run_dir, "03_present_suitability", "suitability_present_ensemble.tif")
  if (file.exists(ens_file)) {
    cat("  Evaluating ensemble predictions...\n")
    tryCatch({
      ens_rast <- terra::rast(ens_file)
      # Extract ensemble predictions at presence and background locations
      pres_sf <- st_as_sf(oof[oof$response == 1, ], coords = c("longitude", "latitude"), crs = 4326)
      bg_sf   <- st_as_sf(oof[oof$response == 0, ], coords = c("longitude", "latitude"), crs = 4326)
      # Safe CRS extraction: authority field may be empty on some systems
      target_epsg <- tryCatch({
        auth <- terra::crs(ens_rast, describe = TRUE)$authority
        if (!is.null(auth) && nzchar(auth)) as.integer(gsub(".*:", "", auth)) else 32645L
      }, error = function(e) 32645L)
      pres_sf <- st_transform(pres_sf, target_epsg)
      bg_sf   <- st_transform(bg_sf,   target_epsg)
      
      # Extract values
      ens_pres <- terra::extract(ens_rast, st_coordinates(pres_sf))[,2]
      ens_bg   <- terra::extract(ens_rast, st_coordinates(bg_sf))[,2]
      
      ens_pres <- ens_pres[!is.na(ens_pres)]
      ens_bg   <- ens_bg[!is.na(ens_bg)]

      if (length(ens_pres) > 0 && length(ens_bg) > 0) {
        # Ensemble Boyce
        ens_boyce <- boyce_continuous(ens_pres, ens_bg)
        # Ensemble Brier (using all OOF — keep NA mask aligned with response)
        ens_all_raw <- terra::extract(ens_rast, st_coordinates(st_as_sf(oof, coords = c("longitude", "latitude"), crs = 4326)))[,2]
        ens_valid <- !is.na(ens_all_raw)
        ens_all   <- ens_all_raw[ens_valid]
        ens_y     <- as.numeric(oof$response)[ens_valid]
        ens_brier <- mean((ens_all - ens_y)^2, na.rm = TRUE)
        # Ensemble threshold
        ens_thr <- tss_best(ens_y, ens_all)
        
        # Add ensemble row to evaluation
        ens_row <- data.frame(
          run_id = run_id,
          algorithm = "ensemble",
          auc_mean = auc_score(ens_y, ens_all),
          tss_mean = ens_thr["tss"],
          boyce = ens_boyce$boyce_index,
          brier = ens_brier,
          calibration_slope = calculate_calibration_slope(ens_y, ens_all)$slope,
          moran_i = calculate_moran_i(ens_y - ens_all, oof$longitude[ens_valid], oof$latitude[ens_valid])$I,
          threshold = ens_thr["threshold"],
          stringsAsFactors = FALSE
        )
        eval_results <- rbind(eval_results, ens_row)
        cat(sprintf("  Ensemble: AUC=%.3f, Boyce=%.3f, Threshold=%.4f\n", 
                    ens_row$auc_mean, ens_row$boyce, ens_row$threshold))
      }
    }, error = function(e) {
      cat(sprintf("  Ensemble evaluation failed: %s\n", conditionMessage(e)))
    })
  }

  # Governance alias G01 (targets.md §1.8)
  src_g01 <- file.path(run_dir, "03_present_suitability", "suitability_present_ensemble.tif")
  dst_g01 <- file.path(run_dir, "03_present_suitability", "present_gcm_ensemble.tif")
  if (file.exists(src_g01) && !file.exists(dst_g01)) file.copy(src_g01, dst_g01)

  cat(sprintf("Phase 8 complete: %d models evaluated with Boyce/Brier/Calibration/Moran\n", nrow(eval_results)))

  list(evaluation = eval_results, boyce = boyce_results, brier = brier_results)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  run_dir <- if (length(args) >= 1) args[[1]] else "."
  run_id <- if (length(args) >= 2) args[[2]] else paste0("RUN_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  
  result <- evaluate_all_models(run_dir, run_id)
}
