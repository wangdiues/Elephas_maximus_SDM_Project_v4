#!/usr/bin/env Rscript
# =============================================================================
# 00_spatial_alignment.R — CRITICAL FIX for raster shifting issues
# =============================================================================
# Purpose: Ensure ALL rasters use EXACTLY same grid (origin, extent, resolution)
# Version: 2.1-ensemble
# 
# ROOT CAUSE OF SHIFT:
# - project() changes origin to match target
# - resample() changes resolution but keeps origin
# - Different scripts create templates differently
# 
# FIX: Use align_rasters() for ALL raster operations
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
})

# =============================================================================
# CRITICAL: Unified Template Management
# =============================================================================

#' Get or create unified template for entire run
#' @param run_dir Run directory
#' @param config Config list
#' @param force_recreate Force template recreation
#' @return SpatRaster template
get_unified_template <- function(run_dir, config = NULL, force_recreate = FALSE) {
  template_path <- file.path(run_dir, "02_data_intermediate", "template_grid.tif")
  
  # Return existing template if valid
  if (!force_recreate && file.exists(template_path)) {
    template <- rast(template_path)
    if (!all(is.na(values(template)))) {
      return(template)
    }
  }
  
  # Create new template from reference raster
  if (is.null(config)) stop("Config required for template creation when template_grid.tif does not exist")
  # Use baseline_climate_glob from config (canonical path), fall back to raw_data_root subdirectory
  bio_glob <- if (!is.null(config$predictors$baseline_climate_glob) &&
                  nzchar(config$predictors$baseline_climate_glob)) {
    config$predictors$baseline_climate_glob
  } else {
    file.path(config$paths$raw_data_root,
              "02_rasters/present/Historical_bioclims/Historical_1986-2015_bio*.tif")
  }
  climate_files <- Sys.glob(bio_glob)
  if (length(climate_files) == 0 && exists("repo_root") && !grepl("^[A-Za-z]:", bio_glob)) {
    climate_files <- Sys.glob(file.path(repo_root, bio_glob))
  }
  
  if (length(climate_files) == 0) {
    stop("No reference rasters found for template creation")
  }
  
  ref <- rast(climate_files[1])

  # CRITICAL: Force template CRS to EPSG:32645 (project standard).
  # Input rasters may carry a non-standard Transverse Mercator CRS
  # (e.g. lon_0=90, k=1, x_0=250000) that differs from UTM Zone 45N.
  target_crs <- "EPSG:32645"
  if (!is.null(config$spatial$target_crs_epsg)) {
    target_crs <- paste0("EPSG:", config$spatial$target_crs_epsg)
  }

  # Project reference raster to target CRS if needed
  ref_crs_epsg <- crs(ref, describe = TRUE)$code
  if (is.na(ref_crs_epsg) || ref_crs_epsg != gsub("EPSG:", "", target_crs)) {
    ref <- project(ref, target_crs, res = res(ref)[1])
  }

  # Create template with properties from (reprojected) reference
  template <- rast(
    nrows = nrow(ref),
    ncols = ncol(ref),
    xmin = xmin(ref),
    xmax = xmax(ref),
    ymin = ymin(ref),
    ymax = ymax(ref),
    crs = crs(ref),
    resolution = res(ref)
  )
  
  # Fill with dummy values to mark as valid
  values(template) <- 0
  
  # Save template
  dir.create(dirname(template_path), recursive = TRUE, showWarnings = FALSE)
  writeRaster(template, template_path, overwrite = TRUE)
  
  cat(sprintf("✓ Created unified template: %d x %d, res=%s, CRS=%s\n",
              ncol(template), nrow(template),
              paste(res(template), collapse = "x"),
              crs(template)))
  
  return(template)
}

# =============================================================================
# CRITICAL: Perfect Raster Alignment
# =============================================================================

#' Align raster to template with PERFECT grid matching
#' @param r Raster to align
#' @param template Template raster
#' @param method Resampling method ("bilinear", "near", "cubic")
#' @param verbose Print alignment info
#' @return Aligned raster
align_raster <- function(r, template, method = "bilinear", verbose = FALSE) {
  if (is.null(r)) return(NULL)
  
  # Check if already aligned
  if (isTRUE(compareGeom(r, template, crs = TRUE, stopOnError = FALSE))) {
    if (verbose) cat("  Already aligned\n")
    return(r)
  }
  
  # Get template properties
  tpl_crs <- crs(template)
  tpl_ext <- ext(template)
  tpl_res <- res(template)
  tpl_dim <- dim(template)[1:2]
  
  # Get raster properties
  r_crs <- crs(r)
  r_ext <- ext(r)
  r_res <- res(r)
  r_dim <- dim(r)[1:2]
  
  if (verbose) {
    cat(sprintf("  Aligning: %dx%d → %dx%d\n", r_dim[2], r_dim[1], tpl_dim[2], tpl_dim[1]))
    cat(sprintf("  CRS: %s → %s\n", if(is.na(r_crs)) "NA" else "OK", if(is.na(tpl_crs)) "NA" else "OK"))
    cat(sprintf("  Res: %s → %s\n", paste(r_res, collapse = "x"), paste(tpl_res, collapse = "x")))
  }
  
  # Step 1: Project if CRS differs
  if (!isTRUE(r_crs == tpl_crs)) {
    r <- project(r, template, method = method)
  }
  
  # Step 2: Resample to match resolution and origin EXACTLY
  # Use 'resample' not 'project' to preserve exact grid alignment
  r <- resample(r, template, method = method)
  
  # Step 3: Crop to exact template extent
  r <- crop(r, template)
  
  # Verify alignment
  if (!isTRUE(compareGeom(r, template, crs = TRUE, stopOnError = FALSE))) {
    warning("Alignment failed - raster still doesn't match template")
  }
  
  return(r)
}

# =============================================================================
# CRITICAL: Vector to Raster with Perfect Alignment
# =============================================================================

#' Rasterize vector to template grid with perfect alignment
#' @param v Vector (SpatVector or sf)
#' @param template Template raster
#' @param field Field to rasterize
#' @param background Background value
#' @return Rasterized vector
rasterize_vector <- function(v, template, field = NULL, background = NA) {
  # Convert sf to SpatVector if needed
  if (inherits(v, "sf")) {
    v <- vect(v)
  }
  
  # Project vector to template CRS
  if (!isTRUE(crs(v) == crs(template))) {
    v <- project(v, crs(template))
  }
  
  # Rasterize
  if (is.null(field)) {
    r <- rasterize(v, template, background = background)
  } else {
    r <- rasterize(v, template, field = field, background = background)
  }
  
  return(r)
}

# =============================================================================
# CRITICAL: Distance Calculation with Perfect Alignment
# =============================================================================

#' Calculate distance with perfect template alignment
#' @param template Template raster
#' @param v Vector (SpatVector or sf)
#' @return Distance raster
calculate_distance_aligned <- function(template, v) {
  # Convert sf to SpatVector if needed
  if (inherits(v, "sf")) {
    v <- vect(v)
  }
  
  # Project vector to template CRS
  if (!isTRUE(crs(v) == crs(template))) {
    v <- project(v, crs(template))
  }
  
  # Calculate distance (terra::distance automatically aligns to template)
  d <- distance(template, v)
  
  return(d)
}

# =============================================================================
# CRITICAL: Extract Values with Perfect Alignment
# =============================================================================

#' Extract raster values to points with proper alignment
#' @param rasters List of rasters or raster stack
#' @param points Points (data.frame with lon/lat, or sf)
#' @param template Template raster for alignment
#' @param method Extraction method ("simple", "bilinear")
#' @return Data frame with extracted values
extract_aligned <- function(rasters, points, template = NULL, method = "simple") {
  # Convert points to SpatVector if needed
  if (inherits(points, "sf")) {
    pts <- vect(points)
  } else if (is.data.frame(points)) {
    if (all(c("longitude", "latitude") %in% names(points))) {
      pts <- vect(points, geom = c("longitude", "latitude"), crs = "EPSG:4326")
    } else {
      stop("Points data.frame must have 'longitude' and 'latitude' columns")
    }
  } else {
    pts <- points
  }
  
  # Project points to raster CRS
  if (inherits(rasters, "SpatRaster")) {
    r_crs <- crs(rasters)
  } else {
    r_crs <- crs(rasters[[1]])
  }
  
  if (!isTRUE(crs(pts) == r_crs)) {
    pts <- project(pts, r_crs)
  }
  
  # Extract values
  if (inherits(rasters, "SpatRaster")) {
    vals <- extract(rasters, pts, method = method)
  } else {
    # Multiple rasters - extract from each
    vals <- data.frame(id = 1:nrow(pts))
    for (i in seq_along(rasters)) {
      v <- extract(rasters[[i]], pts, method = method)
      vals[[i + 1]] <- v[, 2]
    }
  }
  
  return(vals)
}

# =============================================================================
# CRITICAL: Stack Rasters with Perfect Alignment
# =============================================================================

#' Stack multiple rasters with perfect alignment
#' @param raster_list List of raster paths or objects
#' @param template Template raster
#' @param names Optional names for layers
#' @return Stacked raster
stack_aligned <- function(raster_list, template = NULL, names = NULL) {
  rasters <- list()
  
  for (i in seq_along(raster_list)) {
    r <- raster_list[[i]]
    
    # Load if path
    if (is.character(r)) {
      r <- rast(r)
    }
    
    # Align to template if provided
    if (!is.null(template)) {
      r <- align_raster(r, template)
    }
    
    rasters[[i]] <- r
  }
  
  # Stack
  stk <- rast(rasters)
  
  # Set names if provided
  if (!is.null(names)) {
    names(stk) <- names
  }
  
  return(stk)
}

# =============================================================================
# VALIDATION: Check Alignment
# =============================================================================

#' Check if rasters are perfectly aligned
#' @param rasters List of rasters or raster stack
#' @return List with aligned (bool) and details
check_alignment <- function(rasters) {
  if (inherits(rasters, "SpatRaster")) {
    rasters <- as.list(rasters)
  }
  
  if (length(rasters) < 2) {
    return(list(aligned = TRUE, reason = "Single raster"))
  }
  
  template <- rasters[[1]]
  issues <- c()
  
  for (i in 2:length(rasters)) {
    r <- rasters[[i]]
    
    if (!isTRUE(compareGeom(r, template, crs = TRUE, stopOnError = FALSE))) {
      issues <- c(issues, sprintf("Raster %d: geometry mismatch", i))
    }
    
    if (!isTRUE(crs(r) == crs(template))) {
      issues <- c(issues, sprintf("Raster %d: CRS mismatch", i))
    }
    
    if (!isTRUE(all(res(r) == res(template)))) {
      issues <- c(issues, sprintf("Raster %d: resolution mismatch", i))
    }
    
    if (!isTRUE(all(ext(r) == ext(template)))) {
      issues <- c(issues, sprintf("Raster %d: extent mismatch", i))
    }
  }
  
  return(list(
    aligned = length(issues) == 0,
    issues = issues
  ))
}

# =============================================================================
# DIAGNOSTIC: Report Alignment Status
# =============================================================================

#' Report alignment status for debugging
#' @param rasters Named list of rasters
#' @param template Template raster
report_alignment <- function(rasters, template = NULL) {
  cat("\n=== RASTER ALIGNMENT REPORT ===\n\n")
  
  if (is.null(template)) {
    template <- rasters[[1]]
  }
  
  cat("Template:\n")
  cat(sprintf("  Dimensions: %d x %d\n", ncol(template), nrow(template)))
  cat(sprintf("  Resolution: %s\n", paste(res(template), collapse = " x ")))
  cat(sprintf("  Extent: %s\n", paste(as.numeric(ext(template)), collapse = ", ")))
  cat(sprintf("  CRS: %s\n\n", crs(template)))
  
  for (name in names(rasters)) {
    r <- rasters[[name]]
    
    if (is.character(r)) {
      if (!file.exists(r)) {
        cat(sprintf("%s: FILE NOT FOUND\n", name))
        next
      }
      r <- rast(r)
    }
    
    aligned <- isTRUE(compareGeom(r, template, crs = TRUE, stopOnError = FALSE))
    status <- if (aligned) "✓" else "✗"
    
    cat(sprintf("%s %s:\n", status, name))
    cat(sprintf("    Dimensions: %d x %d %s\n", ncol(r), nrow(r), 
                if (all(dim(r)[1:2] == dim(template)[1:2])) "" else "(MISMATCH)"))
    cat(sprintf("    Resolution: %s %s\n", paste(res(r), collapse = " x "),
                if (all(res(r) == res(template))) "" else "(MISMATCH)"))
    cat(sprintf("    CRS: %s %s\n\n", if(is.na(crs(r))) "NA" else "OK",
                if (isTRUE(crs(r) == crs(template))) "" else "(MISMATCH)"))
  }
}

# =============================================================================
# END OF SPATIAL ALIGNMENT HELPERS
# =============================================================================
