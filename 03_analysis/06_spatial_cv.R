#!/usr/bin/env Rscript
# STD-HARDENING: 2026-02-24
# role: phase_or_helper
# note: normalized script metadata for deterministic maintenance
# 06_spatial_cv.R — Spatial blocking for cross-validation

suppressPackageStartupMessages({ library(sf) })
if (!exists("create_spatial_blocks", mode = "function")) {
  src_dir <- if (exists("repo_root")) file.path(repo_root, "03_analysis") else "03_analysis"
  source(file.path(src_dir, "00_sdm_helpers.R"))
}

create_spatial_folds_full <- function(presence_csv, background_csv, out_dir, k_folds = 5, block_size_km = 15, run_id, seed = 123456) {
  dir.create(out_dir, recursive = TRUE)
  
  p <- read.csv(presence_csv)
  b <- read.csv(background_csv)
  
  p$type <- "presence"; b$type <- "background"
  p$id <- seq_len(nrow(p)); b$id <- seq_len(nrow(b))
  
  df <- rbind(p[, c("id", "type", "longitude", "latitude")], b[, c("id", "type", "longitude", "latitude")])
  
  # Spatial blocking
  folds <- create_spatial_blocks(df, block_size_km, k_folds, seed)
  df$fold <- folds
  
  # Validate min presences per fold
  pres_per_fold <- table(df$fold[df$type == "presence"])
  min_pres <- min(pres_per_fold)
  
  if (min_pres < 3) {
    warning(sprintf("Fold %d has only %d presences", which.min(pres_per_fold), min_pres))
  }
  
  # Save
  write.csv(df, file.path(out_dir, "fold_assignments.csv"), row.names = FALSE)
  
  sfobj <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  fold_vec <- file.path(out_dir, "spatial_folds.gpkg")
  write_ok <- tryCatch({
    st_write(sfobj, fold_vec, delete_dsn = TRUE, quiet = TRUE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(write_ok) || !file.exists(fold_vec)) {
    fold_vec <- file.path(out_dir, "spatial_folds.geojson")
    sf::st_write(sfobj, fold_vec, delete_dsn = TRUE, quiet = TRUE, driver = "GeoJSON")
  }
  
  # Report
  bg_label <- if (file.exists(gsub("presence_points_clean", "absence_points_clean", presence_csv))) "n_absence" else "n_background"
  report <- c("SPATIAL FOLDS REPORT", paste0("run_id: ", run_id), paste0("k_folds: ", k_folds),
              paste0("n_presence: ", nrow(p)), paste0(bg_label, ": ", nrow(b)),
              "presence_per_fold:", paste(paste0("fold_", names(pres_per_fold), "=", as.integer(pres_per_fold)), collapse = ", "))
  writeLines(report, file.path(out_dir, "spatial_folds_report.txt"))
  
  list(csv = file.path(out_dir, "fold_assignments.csv"), gpkg = fold_vec, k_folds = k_folds, min_presences = min_pres)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  presence_csv <- if (length(args) >= 1) args[[1]] else "01_processed_data/presence_points_clean.csv"
  background_csv <- if (length(args) >= 2) args[[2]] else "01_processed_data/background_points.csv"
  out_dir <- if (length(args) >= 3) args[[3]] else "01_processed_data"
  run_id <- if (length(args) >= 4) args[[4]] else paste0("RUN_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  
  result <- create_spatial_folds_full(presence_csv, background_csv, out_dir, run_id = run_id)
  cat(sprintf("Phase 6 complete: %d folds, min presences = %d\n", result$k_folds, result$min_presences))
}

