#!/usr/bin/env Rscript
# STD-HARDENING: 2026-02-24
# role: phase_or_helper
# note: normalized script metadata for deterministic maintenance
# 05_background_bias.R — Target-group bias background sampling

suppressPackageStartupMessages({ library(sf); library(terra); library(yaml) })

sample_background_bias <- function(aoi_path, presence_csv, out_dir, n_background = 10000, run_id, seed = 123456, absence_csv = NULL) {
  set.seed(seed)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # If true absence points exist, use them directly (no pseudo-absence generation needed)
  if (!is.null(absence_csv) && file.exists(absence_csv)) {
    abs_data <- read.csv(absence_csv, check.names = FALSE)
    abs_data <- abs_data[is.finite(abs_data$longitude) & is.finite(abs_data$latitude), , drop = FALSE]
    if (nrow(abs_data) > 0) {
      cat(sprintf("Using %d true absence points as background (skipping pseudo-absence generation)\n", nrow(abs_data)))
      bg <- data.frame(
        id        = seq_len(nrow(abs_data)),
        longitude = abs_data$longitude,
        latitude  = abs_data$latitude,
        weight    = 1 / nrow(abs_data),
        strata    = NA
      )
      bg_sf_ll <- st_as_sf(bg, coords = c("longitude", "latitude"), crs = 4326)
      out_csv <- file.path(out_dir, "background_points.csv")
      write.csv(bg, out_csv, row.names = FALSE)
      bg_gpkg <- file.path(out_dir, "background_points.gpkg")
      write_ok <- tryCatch({
        st_write(bg_sf_ll, bg_gpkg, delete_dsn = TRUE, quiet = TRUE); TRUE
      }, error = function(e) FALSE, warning = function(w) FALSE)
      if (!isTRUE(write_ok) || !file.exists(bg_gpkg)) {
        bg_gpkg <- file.path(out_dir, "background_points.geojson")
        sf::st_write(bg_sf_ll, bg_gpkg, delete_dsn = TRUE, quiet = TRUE, driver = "GeoJSON")
      }
      report <- c("BACKGROUND SAMPLING REPORT", paste0("run_id: ", run_id),
                  paste0("method: true_absences"), paste0("n_background: ", nrow(bg)),
                  paste0("source: ", absence_csv))
      writeLines(report, file.path(out_dir, "background_sampling_report.txt"))
      return(list(csv = out_csv, gpkg = bg_gpkg, n_background = nrow(bg)))
    }
  }

  # Fallback: generate bias-corrected pseudo-absences from AOI
  # Load AOI
  aoi <- st_read(aoi_path, quiet = TRUE)
  aoi <- st_make_valid(st_union(aoi))
  aoi <- st_sf(id = 1L, geometry = st_sfc(aoi, crs = st_crs(st_read(aoi_path, quiet = TRUE))))

  # Load presences for bias surface
  pres <- read.csv(presence_csv)
  pres <- pres[is.finite(pres$longitude) & is.finite(pres$latitude), , drop = FALSE]
  if (nrow(pres) == 0) stop("No valid presence points for background sampling")

  # Build bias surface in projected CRS.
  crs_utm <- 32645
  pres_sf <- st_as_sf(pres, coords = c("longitude", "latitude"), crs = 4326)
  pres_proj <- st_transform(pres_sf, crs_utm)
  aoi_proj <- st_transform(aoi, crs_utm)

  template <- rast(vect(aoi_proj), resolution = 1000)
  dist_rast <- distance(template, vect(pres_proj))
  bias_vals <- values(dist_rast)
  bias_vals <- 1 / (1 + bias_vals)
  bias_vals[!is.finite(bias_vals)] <- 0
  max_b <- suppressWarnings(max(bias_vals, na.rm = TRUE))
  if (!is.finite(max_b) || max_b <= 0) max_b <- 1
  bias_vals <- bias_vals / max_b
  bias_rast <- dist_rast
  values(bias_rast) <- bias_vals

  # Sample with bias weighting
  n_cells <- ncell(template)
  cell_probs <- values(bias_rast)
  cell_probs[is.na(cell_probs)] <- 0
  sum_probs <- sum(cell_probs)
  if (!is.finite(sum_probs) || sum_probs <= 0) stop("Invalid background sampling weights")
  cell_probs <- cell_probs / sum_probs

  sampled_cells <- sample(1:n_cells, n_background, prob = cell_probs, replace = TRUE)
  sampled_coords <- xyFromCell(template, sampled_cells)

  # Create background points
  bg <- data.frame(
    id = 1:n_background,
    longitude = sampled_coords[, 1],
    latitude = sampled_coords[, 2],
    weight = cell_probs[sampled_cells],
    strata = NA
  )

  # Filter to AOI and convert back to WGS84.
  bg_sf <- st_as_sf(bg, coords = c("longitude", "latitude"), crs = crs_utm)
  inside <- lengths(st_intersects(bg_sf, aoi_proj)) > 0
  bg <- bg[inside, ]
  bg_sf <- bg_sf[inside, ]
  bg_sf_ll <- st_transform(bg_sf, 4326)
  xy_ll <- st_coordinates(bg_sf_ll)
  bg$longitude <- xy_ll[, 1]
  bg$latitude <- xy_ll[, 2]

  # Save
  out_csv <- file.path(out_dir, "background_points.csv")
  write.csv(bg, out_csv, row.names = FALSE)
  bg_gpkg <- file.path(out_dir, "background_points.gpkg")
  if (file.exists(bg_gpkg)) suppressWarnings(file.remove(bg_gpkg))
  write_ok <- tryCatch({
    st_write(bg_sf_ll, bg_gpkg, delete_dsn = TRUE, quiet = TRUE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(write_ok) || !file.exists(bg_gpkg)) {
    bg_gpkg <- file.path(out_dir, "background_points.geojson")
    sf::st_write(bg_sf_ll, bg_gpkg, delete_dsn = TRUE, quiet = TRUE, driver = "GeoJSON")
  }

  # Report
  report <- c("BACKGROUND SAMPLING REPORT", paste0("run_id: ", run_id),
               paste0("method: pseudo_absence_bias_corrected"),
               paste0("n_background: ", nrow(bg)), paste0("seed: ", seed))
  writeLines(report, file.path(out_dir, "background_sampling_report.txt"))

  list(csv = out_csv, gpkg = bg_gpkg, n_background = nrow(bg))
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  aoi_path <- if (length(args) >= 1) args[[1]] else "02_data_intermediate/m_area_vector.gpkg"
  presence_csv <- if (length(args) >= 2) args[[2]] else "01_processed_data/presence_points_clean.csv"
  out_dir <- if (length(args) >= 3) args[[3]] else "01_processed_data"
  run_id <- if (length(args) >= 4) args[[4]] else paste0("RUN_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  absence_csv_arg <- if (length(args) >= 5) args[[5]] else {
    candidate <- file.path(out_dir, "absence_points_clean.csv")
    if (file.exists(candidate)) candidate else NULL
  }

  result <- sample_background_bias(aoi_path, presence_csv, out_dir, run_id = run_id, absence_csv = absence_csv_arg)
  cat(sprintf("Phase 5 complete: %d background points\n", result$n_background))
}

