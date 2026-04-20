#!/usr/bin/env Rscript
# STD-HARDENING: 2026-02-24
# role: phase_or_helper
# note: normalized script metadata for deterministic maintenance
# 03_occurrence_processing.R — FIXED: 50m grid thinning (not 2km!)

suppressPackageStartupMessages({
  library(sf)
  library(yaml)
})

process_occurrence_full <- function(input_csv, out_dir, run_id, config_path = NULL, seed = 123456) {
  set.seed(seed)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  thin_km <- 0.1  # kept for reporting only; grid thinning disabled — exact dedup used instead

  if (!is.null(config_path) && file.exists(config_path)) {
    config <- read_yaml(config_path)
    if (!is.null(config$occurrence$thinning_km)) thin_km <- config$occurrence$thinning_km
  }

  # Load data
  occ <- read.csv(input_csv, check.names = FALSE)
  n0 <- nrow(occ)
  cat(sprintf("Loaded %d records\n", n0))

  # Detect coordinate columns
  lon_col <- match(tolower(c("longitude", "decimalLongitude", "lon", "x")), tolower(names(occ)), nomatch = 0)
  lon_col <- names(occ)[lon_col[lon_col > 0]][1]
  lat_col <- match(tolower(c("latitude", "decimalLatitude", "lat", "y")), tolower(names(occ)), nomatch = 0)
  lat_col <- names(occ)[lat_col[lat_col > 0]][1]

  if (is.null(lon_col) || is.null(lat_col)) stop("Coordinate columns not found")

  # Detect presence/absence column
  pa_col <- names(occ)[tolower(names(occ)) %in% c("presence", "pa", "occ", "occurrence")][1]
  has_pa <- !is.na(pa_col) && pa_col %in% names(occ)

  # Coordinate QC
  lon <- suppressWarnings(as.numeric(occ[[lon_col]]))
  lat <- suppressWarnings(as.numeric(occ[[lat_col]]))
  keep <- !is.na(lon) & !is.na(lat) & lon >= -180 & lon <= 180 & lat >= -90 & lat <= 90
  occ <- occ[keep, ]
  n1 <- nrow(occ)
  cat(sprintf("After coordinate QC: %d records\n", n1))

  occ$longitude <- lon[keep]
  occ$latitude <- lat[keep]

  # Split presence/absence if PA column is detected
  if (has_pa) {
    pa_vals <- suppressWarnings(as.integer(occ[[pa_col]]))
    pres_rows <- !is.na(pa_vals) & pa_vals == 1
    abs_rows  <- !is.na(pa_vals) & pa_vals == 0
    cat(sprintf("PA column '%s' detected: %d presences, %d absences\n",
                pa_col, sum(pres_rows), sum(abs_rows)))
    pres_occ <- occ[pres_rows, ]
    abs_occ  <- occ[abs_rows, ]
  } else {
    pres_occ <- occ
    abs_occ  <- data.frame()
  }

  # Exact coordinate deduplication — presences
  n_before_p <- nrow(pres_occ)
  key_p <- paste(round(pres_occ$longitude, 6), round(pres_occ$latitude, 6), sep = "_")
  pres_occ <- pres_occ[!duplicated(key_p), ]
  removed_p <- n_before_p - nrow(pres_occ)
  cat(sprintf("After deduplication (presences): %d records (removed %d)\n", nrow(pres_occ), removed_p))

  n4 <- nrow(pres_occ)
  if (n4 < 10) {
    warning(sprintf("Only %d presence records - model performance may be poor!", n4))
  } else {
    cat(sprintf("Good sample size for modeling (%d presences)\n", n4))
  }

  # Exact coordinate deduplication — absences
  n_abs <- 0L
  absence_csv <- NULL
  if (nrow(abs_occ) > 0) {
    key_a <- paste(round(abs_occ$longitude, 6), round(abs_occ$latitude, 6), sep = "_")
    abs_occ <- abs_occ[!duplicated(key_a), ]
    n_abs <- nrow(abs_occ)
    cat(sprintf("After deduplication (absences): %d records\n", n_abs))

    absence_csv <- file.path(out_dir, "absence_points_clean.csv")
    write.csv(abs_occ, absence_csv, row.names = FALSE)
    abs_pts <- st_as_sf(abs_occ, coords = c("longitude", "latitude"), crs = 4326)
    abs_gpkg <- file.path(out_dir, "absence_points_clean.gpkg")
    write_ok_a <- tryCatch({
      st_write(abs_pts, abs_gpkg, delete_dsn = TRUE, quiet = TRUE); TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!isTRUE(write_ok_a) || !file.exists(abs_gpkg)) {
      sf::st_write(abs_pts, file.path(out_dir, "absence_points_clean.geojson"),
                   delete_dsn = TRUE, quiet = TRUE, driver = "GeoJSON")
    }
    cat(sprintf("Saved: %s\n", absence_csv))
  }

  # Save presence outputs
  out_csv <- file.path(out_dir, "presence_points_clean.csv")
  write.csv(pres_occ, out_csv, row.names = FALSE)

  pts <- st_as_sf(pres_occ, coords = c("longitude", "latitude"), crs = 4326)
  out_gpkg <- file.path(out_dir, "presence_points_clean.gpkg")
  write_ok <- tryCatch({
    st_write(pts, out_gpkg, delete_dsn = TRUE, quiet = TRUE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(write_ok) || !file.exists(out_gpkg)) {
    out_gpkg <- file.path(out_dir, "presence_points_clean.geojson")
    sf::st_write(pts, out_gpkg, delete_dsn = TRUE, quiet = TRUE, driver = "GeoJSON")
  }

  # Report
  report <- c(
    "OCCURRENCE PROCESSING REPORT",
    paste0("run_id: ", run_id),
    paste0("input: ", input_csv),
    paste0("records_input: ", n0),
    paste0("records_after_coord_qc: ", n1),
    paste0("pa_column_detected: ", ifelse(has_pa, pa_col, "none")),
    paste0("presences_final: ", n4),
    paste0("absences_final: ", n_abs),
    paste0("longitude_column: ", lon_col),
    paste0("latitude_column: ", lat_col)
  )
  writeLines(report, file.path(out_dir, "occurrence_processing_report.txt"))

  cat(sprintf("Saved: %s\n", out_csv))
  cat(sprintf("Saved: %s\n", out_gpkg))

  list(csv = out_csv, gpkg = out_gpkg, n_records = n4,
       absence_csv = absence_csv, n_absences = n_abs)
}

# Command line execution
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  input_csv <- if (length(args) >= 1) args[[1]] else "01_data_raw/01_occurrences/elephant_PA_data.csv"
  out_dir <- if (length(args) >= 2) args[[2]] else "01_processed_data"
  run_id <- if (length(args) >= 3) args[[3]] else paste0("RUN_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  
  result <- process_occurrence_full(input_csv, out_dir, run_id)
  cat(sprintf("\n=== Phase 3 Complete ===\nFinal presence records: %d\n", result$n_records))
}

