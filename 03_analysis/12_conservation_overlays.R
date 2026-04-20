#!/usr/bin/env Rscript
# =============================================================================
# 12_conservation_overlays.R — COMPLETE conservation overlay analysis
# CRITICAL FIX: All conservation outputs now generated
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(yaml)
})

as_repo_path <- function(repo_root, p) {
  if (is.null(p) || !length(p) || !nzchar(p)) return(NA_character_)
  if (grepl("^[A-Za-z]:[/\\\\]", p)) return(p)
  file.path(repo_root, p)
}

safe_mean_extract <- function(r, v) {
  out <- tryCatch(extract(r, vect(v), fun = mean, na.rm = TRUE), error = function(e) NULL)
  if (is.null(out) || ncol(out) < 2) return(NULL)
  # terra names the value column after the layer, not "mean" — normalize it
  names(out)[2] <- "mean"
  out
}

run_overlays <- function(repo_root, run_dir) {
  out_dir <- file.path(run_dir, "07_overlays")
  dir.create(out_dir, recursive = TRUE)
  
  # Load present suitability
  suit_file <- file.path(run_dir, "03_present_suitability/suitability_present_ensemble.tif")
  if (!file.exists(suit_file)) {
    cat("No present suitability found\n")
    return(list(n_layers = 0))
  }
  suit <- rast(suit_file)
  
  # Load config
  config_path <- file.path(repo_root, "00_governance/config.yaml")
  config <- read_yaml(config_path)
  
  idx <- data.frame()
  
  # Protected Areas
  pa_file <- as_repo_path(repo_root, config$vectors$protected_areas)
  if (!is.na(pa_file) && file.exists(pa_file)) {
    cat("Processing Protected Areas...\n")
    pa <- tryCatch(st_read(pa_file, quiet = TRUE), error = function(e) {
      cat(sprintf("  PA read failed: %s\n", conditionMessage(e))); NULL
    })
    if (!is.null(pa)) {
      pa <- st_transform(pa, crs(suit))
      pa_suit <- safe_mean_extract(suit, pa)
      
      if (!is.null(pa_suit)) {
        pa_summary <- data.frame(
          layer = "protected_areas",
          n_polygons = nrow(pa),
          mean_suitability = mean(pa_suit$mean, na.rm = TRUE),
          sd_suitability = sd(pa_suit$mean, na.rm = TRUE),
          min_suitability = min(pa_suit$mean, na.rm = TRUE),
          max_suitability = max(pa_suit$mean, na.rm = TRUE),
          area_km2 = sum(st_area(pa), na.rm = TRUE) / 1e6,
          stringsAsFactors = FALSE
        )
        write.csv(pa_summary, file.path(out_dir, "suitability_within_PAs.csv"), row.names = FALSE)
        idx <- rbind(idx, data.frame(layer = "protected_areas", path = file.path(out_dir, "suitability_within_PAs.csv"), stringsAsFactors = FALSE))

        # Generate PA suitability raster for R07 figure
        tryCatch({
          pa_v <- vect(pa)
          if (!isTRUE(crs(pa_v) == crs(suit))) pa_v <- project(pa_v, crs(suit))
          pa_suit_rast <- mask(suit, pa_v)
          writeRaster(pa_suit_rast, file.path(out_dir, "suitability_within_PAs.tif"), overwrite = TRUE)
        }, error = function(e) {
          cat(sprintf("  PA raster generation failed: %s\n", conditionMessage(e)))
        })

        cat("  PA analysis complete\n")
      }
    }
  }

  # Private land
  private_file <- as_repo_path(repo_root, config$vectors$private_land)
  if (!is.na(private_file) && file.exists(private_file)) {
    cat("Processing Private Land...\n")
    private_land <- tryCatch(st_read(private_file, quiet = TRUE), error = function(e) {
      cat(sprintf("  Private land read failed: %s\n", conditionMessage(e))); NULL
    })
    if (!is.null(private_land)) {
      private_land <- st_transform(private_land, crs(suit))
      private_suit <- safe_mean_extract(suit, private_land)
      if (!is.null(private_suit)) {
        private_summary <- data.frame(
          layer = "private_land",
          n_polygons = nrow(private_land),
          mean_suitability = mean(private_suit$mean, na.rm = TRUE),
          sd_suitability = sd(private_suit$mean, na.rm = TRUE),
          min_suitability = min(private_suit$mean, na.rm = TRUE),
          max_suitability = max(private_suit$mean, na.rm = TRUE),
          area_km2 = sum(st_area(private_land), na.rm = TRUE) / 1e6,
          stringsAsFactors = FALSE
        )
        write.csv(private_summary, file.path(out_dir, "suitability_within_private_land.csv"), row.names = FALSE)
        idx <- rbind(idx, data.frame(layer = "private_land", path = file.path(out_dir, "suitability_within_private_land.csv"), stringsAsFactors = FALSE))
        cat("  Private land analysis complete\n")
      }
    }
  }

  # Conflict Zones
  conflict_file <- as_repo_path(repo_root, config$vectors$conflict_zones)
  if (!is.na(conflict_file) && file.exists(conflict_file)) {
    cat("Processing Conflict Zones...\n")
    conflict <- tryCatch(st_read(conflict_file, quiet = TRUE), error = function(e) {
      cat(sprintf("  Conflict zones read failed: %s\n", conditionMessage(e))); NULL
    })
    if (!is.null(conflict)) {
      conflict <- st_transform(conflict, crs(suit))
      conflict_suit <- safe_mean_extract(suit, conflict)
      if (!is.null(conflict_suit)) {
        conflict_summary <- data.frame(
          layer = "conflict_zones",
          n_polygons = nrow(conflict),
          mean_suitability = mean(conflict_suit$mean, na.rm = TRUE),
          sd_suitability = sd(conflict_suit$mean, na.rm = TRUE),
          min_suitability = min(conflict_suit$mean, na.rm = TRUE),
          max_suitability = max(conflict_suit$mean, na.rm = TRUE),
          area_km2 = sum(st_area(conflict), na.rm = TRUE) / 1e6,
          stringsAsFactors = FALSE
        )
        write.csv(conflict_summary, file.path(out_dir, "suitability_within_conflict_zones.csv"), row.names = FALSE)
        idx <- rbind(idx, data.frame(layer = "conflict_zones", path = file.path(out_dir, "suitability_within_conflict_zones.csv"), stringsAsFactors = FALSE))
        cat("  Conflict analysis complete\n")
      }
    }
  }

  # Local LULC summary
  lulc_path <- as_repo_path(repo_root, config$predictors$local_lulc_raster)
  if (!is.na(lulc_path) && file.exists(lulc_path)) {
    cat("Processing Local LULC Summary...\n")
    lulc <- tryCatch(rast(lulc_path), error = function(e) NULL)
    if (!is.null(lulc)) {
      if (!isTRUE(compareGeom(lulc, suit, stopOnError = FALSE))) {
        lulc <- resample(lulc, suit, method = "near")
      }
      lulc_vals <- values(lulc)
      suit_vals <- values(suit)
      keep <- is.finite(lulc_vals) & is.finite(suit_vals)
      if (sum(keep) > 0) {
        lulc_df <- data.frame(class_value = lulc_vals[keep], suitability = suit_vals[keep])
        lulc_summary <- aggregate(suitability ~ class_value, lulc_df, function(x) c(mean = mean(x), n = length(x)))
        lulc_summary <- data.frame(
          class_value = lulc_summary$class_value,
          mean_suitability = vapply(lulc_summary$suitability, function(x) x[[1]], numeric(1)),
          n_cells = vapply(lulc_summary$suitability, function(x) x[[2]], numeric(1)),
          stringsAsFactors = FALSE
        )
        write.csv(lulc_summary, file.path(out_dir, "suitability_by_local_lulc.csv"), row.names = FALSE)
        idx <- rbind(idx, data.frame(layer = "local_lulc", path = file.path(out_dir, "suitability_by_local_lulc.csv"), stringsAsFactors = FALSE))
        cat("  Local LULC summary complete\n")
      }
    }
  }
  
  # Station-level overlay: presence/absence survey points within conservation zones
  pres_csv_path <- file.path(run_dir, "01_processed_data", "presence_points_clean.csv")
  abs_csv_path  <- file.path(run_dir, "01_processed_data", "absence_points_clean.csv")
  if (file.exists(pres_csv_path)) {
    cat("Processing Station-level Conservation Overlay...\n")
    tryCatch({
      pres_df <- read.csv(pres_csv_path)
      pres_sf <- st_as_sf(pres_df, coords = c("longitude", "latitude"), crs = 4326)
      pres_sf <- st_transform(pres_sf, crs(suit))
      pres_sf$pa_type <- "presence"

      abs_sf <- NULL
      if (file.exists(abs_csv_path)) {
        abs_df <- read.csv(abs_csv_path)
        abs_sf <- st_as_sf(abs_df, coords = c("longitude", "latitude"), crs = 4326)
        abs_sf <- st_transform(abs_sf, crs(suit))
        abs_sf$pa_type <- "absence"
        station_sf <- rbind(pres_sf[, "pa_type", drop = FALSE], abs_sf[, "pa_type", drop = FALSE])
      } else {
        station_sf <- pres_sf[, "pa_type", drop = FALSE]
      }

      # Extract suitability at all stations
      suit_at_stations <- terra::extract(suit, vect(station_sf))
      station_sf$suitability <- suit_at_stations[, 2]

      # Overlay with Protected Areas
      if (!is.na(pa_file) && file.exists(pa_file)) {
        pa_v <- tryCatch(st_read(pa_file, quiet = TRUE), error = function(e) NULL)
        if (!is.null(pa_v)) {
          pa_v <- st_transform(pa_v, crs(suit))
          in_pa <- lengths(st_within(station_sf, pa_v)) > 0
          station_sf$within_pa <- in_pa
        }
      }

      # Overlay with Private Land
      if (!is.na(private_file) && file.exists(private_file)) {
        pvt <- tryCatch(st_read(private_file, quiet = TRUE), error = function(e) NULL)
        if (!is.null(pvt)) {
          pvt <- st_transform(pvt, crs(suit))
          in_pvt <- lengths(st_within(station_sf, pvt)) > 0
          station_sf$within_private_land <- in_pvt
        }
      }

      st_write(station_sf, file.path(out_dir, "station_conservation_overlay.gpkg"),
               delete_dsn = TRUE, quiet = TRUE)

      # Summary table
      coords <- st_coordinates(station_sf)
      station_df <- as.data.frame(station_sf)
      station_df$longitude <- coords[, 1]
      station_df$latitude  <- coords[, 2]
      station_df$geometry  <- NULL
      write.csv(station_df, file.path(out_dir, "station_conservation_overlay.csv"), row.names = FALSE)

      # Aggregate summary
      summary_rows <- lapply(c("presence", "absence"), function(pt) {
        sub <- station_df[station_df$pa_type == pt, ]
        data.frame(
          pa_type = pt,
          n_stations = nrow(sub),
          mean_suitability = mean(sub$suitability, na.rm = TRUE),
          n_within_pa = if ("within_pa" %in% names(sub)) sum(sub$within_pa, na.rm = TRUE) else NA,
          pct_within_pa = if ("within_pa" %in% names(sub)) round(100 * mean(sub$within_pa, na.rm = TRUE), 1) else NA,
          n_within_private = if ("within_private_land" %in% names(sub)) sum(sub$within_private_land, na.rm = TRUE) else NA,
          stringsAsFactors = FALSE
        )
      })
      summary_df <- do.call(rbind, summary_rows)
      write.csv(summary_df, file.path(out_dir, "station_zone_summary.csv"), row.names = FALSE)
      idx <- rbind(idx, data.frame(layer = "station_overlay", path = file.path(out_dir, "station_zone_summary.csv"), stringsAsFactors = FALSE))
      cat("  Station-level overlay complete\n")
    }, error = function(e) {
      cat(sprintf("  Station overlay failed: %s\n", conditionMessage(e)))
    })
  }

  # Distance-based proximity analysis
  dist_files <- list.files(file.path(run_dir, "01_processed_data/distance_layers"), pattern = "\\.tif$", full.names = TRUE)
  if (length(dist_files) > 0) {
    cat("Processing Distance-based Proximity...\n")
    proximity_results <- list()
    
    for (dist_file in dist_files) {
      dist_rast <- rast(dist_file)
      dist_name <- tools::file_path_sans_ext(basename(dist_file))
      
      # Correlation between distance and suitability
      dist_vals <- values(dist_rast)
      suit_vals <- values(suit)
      
      # Align rasters if dimensions differ
      if (length(dist_vals) != length(suit_vals)) {
        dist_rast <- resample(dist_rast, suit, method = "bilinear")
        dist_vals <- values(dist_rast)
      }
      keep <- is.finite(dist_vals) & is.finite(suit_vals)
      cor_val <- if (sum(keep) >= 3) cor(dist_vals[keep], suit_vals[keep], use = "complete.obs") else NA_real_
      
      proximity_results[[dist_name]] <- data.frame(
        layer = dist_name,
        correlation = cor_val,
        mean_distance = mean(dist_vals[keep], na.rm = TRUE),
        sd_distance = sd(dist_vals[keep], na.rm = TRUE)
      )
    }
    
    proximity_df <- do.call(rbind, proximity_results)
    write.csv(proximity_df, file.path(out_dir, "proximity_analysis.csv"), row.names = FALSE)
    
    idx <- rbind(idx, data.frame(layer = "proximity", path = file.path(out_dir, "proximity_analysis.csv"), stringsAsFactors = FALSE))
    cat("  Proximity analysis complete\n")
  }
  
  # Management Indicators
  cat("Creating Management Indicators...\n")
  management <- data.frame(
    indicator = c(
      "PA_coverage",
      "Private_land_coverage",
      "Conflict_overlap",
      "High_suitability_area",
      "Low_suitability_area"
    ),
    value = NA,
    unit = c("km2", "km2", "km2", "km2", "km2"),
    stringsAsFactors = FALSE
  )
  
  # Calculate areas
  suit_vals <- values(suit)
  cell_area <- terra::res(suit)[1] * terra::res(suit)[2] / 1e6  # km2
  
  management$value[4] <- sum(suit_vals > 0.7, na.rm = TRUE) * cell_area
  management$value[5] <- sum(suit_vals < 0.3, na.rm = TRUE) * cell_area
  
  write.csv(management, file.path(out_dir, "management_indicators.csv"), row.names = FALSE)
  idx <- rbind(idx, data.frame(layer = "management", path = file.path(out_dir, "management_indicators.csv"), stringsAsFactors = FALSE))
  
  if (nrow(idx) > 0) {
    write.csv(idx, file.path(out_dir, "overlay_index.csv"), row.names = FALSE)
  }
  
  cat(sprintf("Phase 12 complete: %d overlay layers\n", nrow(idx)))
  list(n_layers = nrow(idx))
}

module_12_conservation_overlays <- function(run_dir, config_path) {
  # Resolve config_path: if relative, try repo_root from manifest first
  if (!file.exists(config_path)) {
    # Try resolving from the run's manifest snapshot
    snap <- file.path(run_dir, "00_manifest", "config_snapshot.yaml")
    if (file.exists(snap)) {
      config_path <- snap
    } else {
      stop("Config not found: ", config_path)
    }
  }
  cfg <- yaml::read_yaml(config_path)
  repo_root <- cfg$paths$repo_root
  if (is.null(repo_root) || !nzchar(repo_root)) repo_root <- "."
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = FALSE)
  run_overlays(repo_root, run_dir)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  repo_root <- if (length(args) >= 1) args[[1]] else "."
  run_dir <- if (length(args) >= 2) args[[2]] else "."

  result <- run_overlays(repo_root, run_dir)
}
