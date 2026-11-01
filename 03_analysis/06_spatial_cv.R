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

create_spatial_folds_full <- function(presence_csv, background_csv, out_dir, k_folds = 5,
                                      block_size_km = 15, run_id, seed = 123456,
                                      min_presences_per_fold = 10,
                                      candidate_block_sizes_km = c(10, 15, 20, 25, 30)) {
  dir.create(out_dir, recursive = TRUE)
  
  p <- read.csv(presence_csv)
  b <- read.csv(background_csv)
  
  p$type <- "presence"; b$type <- "background"
  p$id <- seq_len(nrow(p)); b$id <- seq_len(nrow(b))
  
  df <- rbind(p[, c("id", "type", "longitude", "latitude")], b[, c("id", "type", "longitude", "latitude")])

  # Design sensitivity is assessed from fold composition only, before any model
  # performance is inspected. The configured block size remains the primary design.
  sensitivity <- do.call(rbind, lapply(unique(as.numeric(candidate_block_sizes_km)), function(bs) {
    tryCatch({
      f <- create_spatial_blocks(df, bs, k_folds, seed)
      pp <- tabulate(f[df$type == "presence"], nbins = k_folds)
      nn <- tabulate(f[df$type != "presence"], nbins = k_folds)
      data.frame(block_size_km = bs, valid = all(pp > 0 & nn > 0),
                 min_presences = min(pp), max_presences = max(pp),
                 min_reference = min(nn), max_reference = max(nn),
                 reason = "", stringsAsFactors = FALSE)
    }, error = function(e) {
      data.frame(block_size_km = bs, valid = FALSE, min_presences = NA_integer_,
                 max_presences = NA_integer_, min_reference = NA_integer_,
                 max_reference = NA_integer_, reason = conditionMessage(e),
                 stringsAsFactors = FALSE)
    })
  }))
  sensitivity$selected_primary <- sensitivity$block_size_km == block_size_km
  write.csv(sensitivity, file.path(out_dir, "spatial_block_design_sensitivity.csv"), row.names = FALSE)
  
  # Spatial blocking
  folds <- create_spatial_blocks(df, block_size_km, k_folds, seed)
  block_id <- attr(folds, "block_id")
  x_breaks <- attr(folds, "x_breaks")
  y_breaks <- attr(folds, "y_breaks")
  df$fold <- folds
  df$block_id <- block_id
  df$run_id <- run_id
  
  # Validate min presences per fold
  pres_per_fold <- table(df$fold[df$type == "presence"])
  ref_per_fold <- table(df$fold[df$type != "presence"])
  min_pres <- min(pres_per_fold)
  
  if (length(pres_per_fold) != k_folds || any(!seq_len(k_folds) %in% as.integer(names(pres_per_fold)))) {
    stop("Spatial CV did not assign presences to every configured fold")
  }
  if (length(ref_per_fold) != k_folds || any(!seq_len(k_folds) %in% as.integer(names(ref_per_fold)))) {
    stop("Spatial CV did not assign reference records to every configured fold")
  }
  if (min_pres < min_presences_per_fold) {
    stop(sprintf("Spatial CV failed: a fold has %d presences; configured minimum is %d",
                 min_pres, min_presences_per_fold))
  }
  
  # Save
  write.csv(df, file.path(out_dir, "fold_assignments.csv"), row.names = FALSE)

  fold_composition <- do.call(rbind, lapply(seq_len(k_folds), function(k) {
    z <- df[df$fold == k, , drop = FALSE]
    data.frame(
      run_id = run_id,
      fold = k,
      n_presence = sum(z$type == "presence"),
      n_reference = sum(z$type != "presence"),
      n_total = nrow(z),
      n_blocks = length(unique(z$block_id)),
      stringsAsFactors = FALSE
    )
  }))
  write.csv(fold_composition, file.path(out_dir, "fold_composition.csv"), row.names = FALSE)
  
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

  # Save the actual block polygons, not only the points coloured by fold.
  block_rows <- unique(df[, c("block_id", "fold")])
  polygons <- list()
  metadata <- list()
  idx <- 1L
  for (iy in seq_len(length(y_breaks) - 1L)) {
    for (ix in seq_len(length(x_breaks) - 1L)) {
      bid <- (iy - 1L) * (length(x_breaks) - 1L) + ix
      hit <- block_rows[block_rows$block_id == bid, , drop = FALSE]
      if (nrow(hit) == 0) next
      ring <- matrix(c(
        x_breaks[ix], y_breaks[iy],
        x_breaks[ix + 1L], y_breaks[iy],
        x_breaks[ix + 1L], y_breaks[iy + 1L],
        x_breaks[ix], y_breaks[iy + 1L],
        x_breaks[ix], y_breaks[iy]
      ), ncol = 2, byrow = TRUE)
      polygons[[idx]] <- st_polygon(list(ring))
      metadata[[idx]] <- data.frame(block_id = bid, fold = hit$fold[[1]])
      idx <- idx + 1L
    }
  }
  block_sf <- st_sf(do.call(rbind, metadata), geometry = st_sfc(polygons, crs = 32645))
  st_write(block_sf, file.path(out_dir, "spatial_blocks.gpkg"), delete_dsn = TRUE, quiet = TRUE)
  
  # Report
  bg_label <- if (file.exists(gsub("presence_points_clean", "absence_points_clean", presence_csv))) "n_absence" else "n_background"
  report <- c("SPATIAL FOLDS REPORT", paste0("run_id: ", run_id), paste0("k_folds: ", k_folds),
              paste0("n_presence: ", nrow(p)), paste0(bg_label, ": ", nrow(b)),
              paste0("block_size_km: ", block_size_km),
              paste0("min_presences_required: ", min_presences_per_fold),
              "presence_per_fold:", paste(paste0("fold_", names(pres_per_fold), "=", as.integer(pres_per_fold)), collapse = ", "))
  writeLines(report, file.path(out_dir, "spatial_folds_report.txt"))
  
  list(csv = file.path(out_dir, "fold_assignments.csv"), gpkg = fold_vec,
       blocks = file.path(out_dir, "spatial_blocks.gpkg"),
       composition = file.path(out_dir, "fold_composition.csv"),
       sensitivity = file.path(out_dir, "spatial_block_design_sensitivity.csv"),
       k_folds = k_folds, min_presences = min_pres)
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

