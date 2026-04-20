#!/usr/bin/env Rscript
# =============================================================================
# 01_data_ingest_harmonize.R — Phase 1: Data Ingest & Harmonization
# CRITICAL FIX: Uses align_raster() for perfect grid alignment
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(yaml)
})

# Guard: skip re-sourcing when already loaded by run_pipeline.R
if (!exists("align_raster", mode = "function")) {
  src_dir <- if (exists("repo_root")) file.path(repo_root, "03_analysis") else "03_analysis"
  source(file.path(src_dir, "00_spatial_alignment.R"))
}
if (!exists("initialize_logs", mode = "function")) {
  src_dir <- if (exists("repo_root")) file.path(repo_root, "03_analysis") else "03_analysis"
  source(file.path(src_dir, "00_logging.R"))
}

as_repo_path <- function(repo_root, p) {
  if (is.null(p) || !length(p) || !nzchar(p)) return(NA_character_)
  if (grepl("^[A-Za-z]:[/\\\\]", p)) return(p)
  file.path(repo_root, p)
}

# =============================================================================
# MAIN FUNCTION
# =============================================================================

harmonize_data <- function(repo_root, config_path, run_dir, run_id) {
  # Initialize logging
  initialize_logs(run_id, file.path(repo_root, "06_logs"))
  log_phase_start("PHASE_1", "Data Ingest & Harmonization")
  
  # Load config
  config <- read_yaml(config_path)
  
  # Create output directories
  intermediate_dir <- file.path(run_dir, "02_data_intermediate")
  dir.create(intermediate_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ============================================================================
  # LOAD CONFIG VALUES
  # ============================================================================
  target_crs <- if (!is.null(config$spatial$target_crs_epsg)) config$spatial$target_crs_epsg else NA
  target_res <- if (!is.null(config$spatial$target_resolution)) config$spatial$target_resolution else NA
  resample_method <- if (!is.null(config$spatial$resampling_method)) config$spatial$resampling_method else "bilinear"
  
  # ============================================================================
  # CREATE UNIFIED TEMPLATE (CRITICAL FIX)
  # ============================================================================
  log_info("PHASE_1", "Creating unified template grid", script = "01_data_ingest_harmonize.R")
  
  # CRITICAL: Get unified template - ALL scripts will use this
  template <- get_unified_template(run_dir, config, force_recreate = FALSE)
  
  # Save template path for other scripts
  template_path <- file.path(intermediate_dir, "template_grid.tif")
  if (!is.finite(target_crs)) {
    target_crs <- suppressWarnings(terra::crs(template, proj = TRUE))
  }
  log_info("PHASE_1", sprintf("Template saved: %s", template_path), script = "01_data_ingest_harmonize.R")
  
  # ============================================================================
  # HARMONIZE CLIMATE RASTERS (CRITICAL FIX: Use align_raster)
  # ============================================================================
  log_info("PHASE_1", "Harmonizing climate rasters", script = "01_data_ingest_harmonize.R")
  
  bio_glob <- if (!is.null(config$predictors$baseline_climate_glob) &&
                  nzchar(config$predictors$baseline_climate_glob)) {
    config$predictors$baseline_climate_glob
  } else {
    file.path(config$paths$raw_data_root,
              "02_rasters/present/Historical_bioclims/Historical_1986-2015_bio*.tif")
  }
  climate_files <- Sys.glob(file.path(repo_root, bio_glob))
  if (length(climate_files) == 0) climate_files <- Sys.glob(bio_glob)
  
  harmonized_climate <- list()
  for (f in climate_files) {
    r <- suppressWarnings(rast(f))
    r <- suppressWarnings(align_raster(r, template, method = "bilinear", verbose = FALSE))
    harmonized_climate[[basename(f)]] <- r
  }

  # Save harmonized climate stack
  if (length(harmonized_climate) > 0) {
    climate_stack <- rast(harmonized_climate)
    climate_stack_path <- file.path(intermediate_dir, "climate_baseline_harmonized.tif")
    suppressWarnings(writeRaster(climate_stack, climate_stack_path, overwrite = TRUE))
    log_info("PHASE_1", sprintf("Climate stack saved: %d layers", nlyr(climate_stack)), script = "01_data_ingest_harmonize.R")
    n_climate_written <- length(harmonized_climate)
    rm(climate_stack, harmonized_climate); gc(verbose = FALSE)
  } else {
    n_climate_written <- 0L
  }
  
  # ============================================================================
  # HARMONIZE ANTHROPOGENIC RASTERS (CRITICAL FIX: Use align_raster)
  # ============================================================================
  log_info("PHASE_1", "Harmonizing anthropogenic rasters", script = "01_data_ingest_harmonize.R")
  
  # ============================================================================
  # MOSAIC DYNAMICWORLD PROBABILITY TILES (if not already done)
  # ============================================================================
  # Use VRT (virtual mosaic) to avoid materialising a huge TIF for large 10m tiles.
  # VRT is a lightweight XML descriptor; terra reads tiles on-demand via it.
  dw_prob_mosaic_path <- file.path(intermediate_dir, "dynamicworld_prob_mosaic.vrt")
  dw_prob_glob <- config$predictors$landcover_probability_dynamicworld_glob
  if (!is.null(dw_prob_glob) && nzchar(dw_prob_glob)) {
    dw_prob_tiles <- Sys.glob(file.path(repo_root, dw_prob_glob))
    if (length(dw_prob_tiles) >= 2) {
      tryCatch({
        terra::vrt(dw_prob_tiles, filename = dw_prob_mosaic_path, overwrite = TRUE)
        log_info("PHASE_1", sprintf("DynamicWorld prob tiles VRT created (%d tiles)", length(dw_prob_tiles)),
                 script = "01_data_ingest_harmonize.R")
      }, error = function(e) {
        log_warning("PHASE_1", sprintf("Failed to create DW prob VRT: %s — falling back to first tile", e$message),
                    script = "01_data_ingest_harmonize.R")
        dw_prob_mosaic_path <<- dw_prob_tiles[1]
      })
    } else if (length(dw_prob_tiles) == 1) {
      dw_prob_mosaic_path <- dw_prob_tiles[1]
      log_info("PHASE_1", "Single DynamicWorld prob tile found, used directly",
               script = "01_data_ingest_harmonize.R")
    } else {
      log_warning("PHASE_1", "No DynamicWorld prob tiles found matching glob",
                  script = "01_data_ingest_harmonize.R")
    }
  }

  anthropogenic_paths <- list(
    human_footprint = config$predictors$present_human_footprint,
    human_footprint_alt = config$predictors$alternate_human_footprint,
    local_lulc = config$predictors$local_lulc_raster,
    ndvi = config$predictors$ndvi,
    evi = config$predictors$evi,
    dynamicworld_label = config$predictors$present_landcover_dynamicworld,
    dynamicworld_prob = if (file.exists(dw_prob_mosaic_path)) dw_prob_mosaic_path else NULL
  )
  
  # Process and write each anthropogenic predictor immediately to avoid
  # accumulating open file connections (R limit = 128; VRT + NDVI/EVI stacks exhaust it).
  harmonized_anthro_names <- character()
  for (name in names(anthropogenic_paths)) {
    path <- as_repo_path(repo_root, anthropogenic_paths[[name]])
    if (!file.exists(path)) next
    out_path <- file.path(intermediate_dir, sprintf("%s_harmonized.tif", name))
    ok <- tryCatch({
      suppressWarnings({
        r <- rast(path)

        # For multi-layer temporal stacks (NDVI/EVI annual means), collapse to single layer
        # Use app() wrapper to avoid terra 1.8.x app(na.rm=TRUE) segfault
        if (nlyr(r) > 1 && grepl("ndvi|evi", name, ignore.case = TRUE)) {
          r <- app(r, function(v) mean(v, na.rm = TRUE))
        }

        # Align to template; large VRT/tile inputs can fail warp — skip non-fatal
        method <- if (grepl("landcover|lulc|dynamicworld_label", name, ignore.case = TRUE)) "near" else "bilinear"
        r <- align_raster(r, template, method = method, verbose = FALSE)

        # Fill NAs in landcover via expanding focal window (handles tile boundary gaps)
        if (grepl("landcover|lulc|dynamicworld_label", name, ignore.case = TRUE) && any(is.na(values(r)))) {
          for (w in c(3, 7, 15, 31)) {
            if (!any(is.na(values(r)))) break
            r <- focal(r, w = w, fun = "modal", na.policy = "only", na.rm = TRUE)
          }
        }

        writeRaster(r, out_path, overwrite = TRUE)
        rm(r); gc(verbose = FALSE)
        TRUE
      })
    }, error = function(e) {
      # Use cat() as fallback if log connections are also exhausted
      tryCatch(
        log_warning("PHASE_1", sprintf("Skipping predictor '%s': %s", name, e$message),
                    script = "01_data_ingest_harmonize.R"),
        error = function(e2) cat(sprintf("  WARN: Skipping %s: %s\n", name, e$message))
      )
      FALSE
    })
    if (isTRUE(ok)) {
      harmonized_anthro_names <- c(harmonized_anthro_names, name)
      log_info("PHASE_1", sprintf("Harmonized: %s", name), script = "01_data_ingest_harmonize.R")
    }
  }
  gc(verbose = FALSE)

  # ============================================================================
  # DERIVE TERRAIN PREDICTORS FROM DEM
  # ============================================================================
  log_info("PHASE_1", "Deriving terrain predictors from DEM", script = "01_data_ingest_harmonize.R")

  harmonized_terrain <- list()
  dem_rel <- config$predictors$dem
  dem_path <- if (!is.null(dem_rel)) file.path(repo_root, dem_rel) else NA_character_
  if (!is.na(dem_path) && file.exists(dem_path)) {
    tryCatch({
      dem <- suppressWarnings(rast(dem_path))
      dem <- suppressWarnings(align_raster(dem, template, method = "bilinear", verbose = FALSE))
      names(dem) <- "elevation"
      harmonized_terrain$elevation <- dem
      harmonized_terrain$slope <- suppressWarnings(terrain(dem, v = "slope", unit = "degrees", neighbors = 8))
      harmonized_terrain$aspect <- suppressWarnings(terrain(dem, v = "aspect", unit = "degrees", neighbors = 8))
      harmonized_terrain$tri    <- suppressWarnings(terrain(dem, v = "TRI", neighbors = 8))
      rm(dem); gc(verbose = FALSE)

      dir.create(intermediate_dir, recursive = TRUE, showWarnings = FALSE)
      for (name in names(harmonized_terrain)) {
        out_path <- file.path(intermediate_dir, sprintf("%s_harmonized.tif", name))
        tryCatch({
          suppressWarnings(writeRaster(harmonized_terrain[[name]], out_path, overwrite = TRUE))
          log_info("PHASE_1", sprintf("Derived terrain layer: %s", name), script = "01_data_ingest_harmonize.R")
        }, error = function(e) {
          cat(sprintf("  WARN: terrain %s write failed: %s\n", name, e$message))
        })
      }
    }, error = function(e) {
      cat(sprintf("  WARN: Terrain block failed: %s\n", e$message))
    })
  } else {
    log_warning("PHASE_1", sprintf("DEM not found, terrain predictors skipped: %s", dem_path), script = "01_data_ingest_harmonize.R")
  }
  
  # ============================================================================
  # HARMONIZE VECTOR LAYERS
  # ============================================================================
  log_info("PHASE_1", "Harmonizing vector layers", script = "01_data_ingest_harmonize.R")
  
  vectors_root <- file.path(repo_root, config$vectors$root)
  shp_files <- list.files(vectors_root, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  
  harmonized_vectors <- list()
  for (shp in shp_files) {
    tryCatch({
      v <- st_read(shp, quiet = TRUE)
      v <- tryCatch(st_zm(v, drop = TRUE, what = "ZM"), error = function(e) v)
      
      # Transform to target CRS
      target_crs_obj <- if (is.numeric(target_crs)) st_crs(as.integer(target_crs)) else st_crs(target_crs)
      if (is.na(target_crs_obj)) stop("Unable to resolve target CRS for vector harmonization")
      # Assign WGS84 if CRS is missing — Bhutan national shapefiles commonly lack CRS metadata
      if (is.na(st_crs(v))) {
        st_crs(v) <- 4326
        log_warning("PHASE_1", sprintf("No CRS in %s — assumed EPSG:4326 (WGS84)", basename(shp)),
                    script = "01_data_ingest_harmonize.R")
      }
      if (st_crs(v) != target_crs_obj) {
        v <- st_transform(v, target_crs_obj)
      }
      
      # Make valid geometries
      v <- st_make_valid(v)
      
      harmonized_vectors[[basename(tools::file_path_sans_ext(shp))]] <- v
      
    }, error = function(e) {
      log_warning("PHASE_1", sprintf("Failed to harmonize vector: %s (%s)", shp, e$message), script = "01_data_ingest_harmonize.R")
    })
  }
  
  # Save harmonized vectors
  for (name in names(harmonized_vectors)) {
    out_path <- file.path(intermediate_dir, sprintf("%s_harmonized.gpkg", name))
    v <- harmonized_vectors[[name]]
    # Drop 'fid' column if present — GDAL auto-creates it in GPKG, duplicates cause failure
    if ("fid" %in% names(v)) v <- v[, setdiff(names(v), "fid")]
    # Remove existing file before writing to avoid layer conflicts
    if (file.exists(out_path)) unlink(out_path)
    tryCatch(
      st_write(v, out_path, quiet = TRUE),
      error = function(e) {
        log_warning("PHASE_1", sprintf("Failed to write vector %s: %s", name, e$message),
                    script = "01_data_ingest_harmonize.R")
      }
    )
  }
  
  # ============================================================================
  # REPORT ALIGNMENT STATUS
  # ============================================================================
  log_info("PHASE_1", "Reporting alignment status", script = "01_data_ingest_harmonize.R")
  
  # Check alignment using written climate files (reload up to 3)
  tryCatch({
    clim_written <- list.files(intermediate_dir, pattern = "^Historical.*\\.tif$", full.names = TRUE)
    if (length(clim_written) == 0)
      clim_written <- list.files(intermediate_dir, pattern = "climate_baseline.*\\.tif$", full.names = TRUE)
    if (length(clim_written) > 0) {
      check_r <- suppressWarnings(rast(clim_written[[1]]))
      alignment_report <- check_alignment(list(check_r))
      if (alignment_report$aligned) {
        log_info("PHASE_1", "All rasters perfectly aligned", script = "01_data_ingest_harmonize.R")
      } else {
        log_warning("PHASE_1", paste("Alignment issues:", paste(alignment_report$issues, collapse = ", ")),
                    script = "01_data_ingest_harmonize.R")
      }
      rm(check_r)
    }
  }, error = function(e) NULL)
  
  # ============================================================================
  # CREATE DATA REGISTRY ENTRY
  # ============================================================================
  registry_entry <- data.frame(
    dataset_id = sprintf("harmonized_%s", format(Sys.time(), "%Y%m%d")),
    path = intermediate_dir,
    type = "harmonized",
    format = "mixed",
    crs = if (is.numeric(target_crs)) paste0("EPSG:", target_crs) else as.character(target_crs),
    resolution = target_res,
    rows_or_bands = n_climate_written + length(harmonized_anthro_names) + length(harmonized_terrain),
    checksum = tools::md5sum(template_path),
    notes = "Harmonized to unified template grid",
    stringsAsFactors = FALSE
  )
  
  log_phase_end("PHASE_1", "COMPLETE")
  
  return(list(
    template_path = template_path,
    intermediate_dir = intermediate_dir,
    n_climate = n_climate_written,
    n_anthropogenic = length(harmonized_anthro_names),
    n_terrain = length(harmonized_terrain),
    n_vectors = length(harmonized_vectors),
    registry_entry = registry_entry
  ))
}

# =============================================================================
# COMMAND LINE EXECUTION
# =============================================================================

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  config_path <- if (length(args) >= 1) args[[1]] else "00_governance/config.yaml"
  run_dir <- if (length(args) >= 2) args[[2]] else "."
  run_id <- if (length(args) >= 3) args[[3]] else paste0("RUN_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  
  # Extract repo_root from config
  config_lines <- readLines(config_path)
  repo_root_line <- grep("^\\s*repo_root:", config_lines, value = TRUE)
  repo_root <- sub("^.*repo_root:\\s*['\"]?([^'\"\n]+)['\"]?.*$", "\\1", repo_root_line)
  
  result <- harmonize_data(repo_root, config_path, run_dir, run_id)
  cat(sprintf("Phase 1 complete: %d climate, %d anthropogenic, %d terrain, %d vectors harmonized\n",
              result$n_climate, result$n_anthropogenic, result$n_terrain, result$n_vectors))
}
