#!/usr/bin/env Rscript
# STD-HARDENING: 2026-02-24
# role: phase_or_helper
# note: normalized script metadata for deterministic maintenance
# =============================================================================
# 02_accessible_area_M.R — Phase 2: Accessible Area (M) Definition
# =============================================================================
# Purpose: Create biologically defensible accessible area mask
# Analytics Contract: Phase 2
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(yaml)
})

if (!exists("auc_score", mode = "function")) {
  src_dir <- if (exists("repo_root")) file.path(repo_root, "03_analysis") else "03_analysis"
  source(file.path(src_dir, "00_sdm_helpers.R"))
}
if (!exists("initialize_logs", mode = "function")) {
  src_dir <- if (exists("repo_root")) file.path(repo_root, "03_analysis") else "03_analysis"
  source(file.path(src_dir, "00_logging.R"))
}

# =============================================================================
# MAIN FUNCTION
# =============================================================================

create_accessible_area <- function(repo_root, config_path, run_dir, run_id) {
  # Initialize logging
  initialize_logs(run_id, file.path(repo_root, "06_logs"))
  log_phase_start("PHASE_2", "Accessible Area (M) Definition")
  
  # Load config
  config <- read_yaml(config_path)
  
  # Create output directories
  intermediate_dir <- file.path(run_dir, "02_data_intermediate")
  dir.create(intermediate_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ============================================================================
  # LOAD CONFIG VALUES
  # ============================================================================
  aoi_path <- file.path(repo_root, config$vectors$aoi_bhutan)
  target_crs <- if (!is.null(config$spatial$target_crs_epsg)) config$spatial$target_crs_epsg else 32645L
  
  # ============================================================================
  # LOAD AOI
  # ============================================================================
  log_info("PHASE_2", sprintf("Loading AOI: %s", aoi_path), script = "02_accessible_area_M.R")
  
  if (!file.exists(aoi_path)) {
    log_error("PHASE_2", sprintf("AOI not found: %s", aoi_path), script = "02_accessible_area_M.R")
    stop("AOI file not found")
  }
  
  aoi <- st_read(aoi_path, quiet = TRUE)
  log_info("PHASE_2", sprintf("AOI loaded: %d polygons", nrow(aoi)), script = "02_accessible_area_M.R")
  
  # ============================================================================
  # TRANSFORM TO TARGET CRS
  # ============================================================================
  if (!is.finite(target_crs)) {
    # Fallback: use AOI's own CRS rather than inheriting from template
    # (template may carry a non-standard CRS from input rasters)
    target_crs <- st_crs(aoi)$wkt
  }
  target_crs_obj <- if (is.numeric(target_crs)) st_crs(as.integer(target_crs)) else st_crs(target_crs)
  if (st_crs(aoi) != target_crs_obj) {
    aoi <- st_transform(aoi, target_crs_obj)
    log_info("PHASE_2", sprintf("Transformed AOI to target CRS"), script = "02_accessible_area_M.R")
  }
  
  # ============================================================================
  # USE AOI DIRECTLY AS M (no buffer)
  # ============================================================================
  aoi_valid <- st_make_valid(aoi)
  aoi_final <- st_union(st_geometry(aoi_valid))
  log_info("PHASE_2", "Using AOI boundary directly as M (no buffer)", script = "02_accessible_area_M.R")
  
  # ============================================================================
  # DISSOLVE TO SINGLE POLYGON
  # ============================================================================
  aoi_dissolved <- st_union(aoi_final)
  geom <- if (inherits(aoi_dissolved, "sfc")) aoi_dissolved else st_as_sfc(aoi_dissolved)
  aoi_sf <- st_sf(id = seq_along(geom), geometry = geom)
  
  # Calculate area
  area_km2 <- as.numeric(st_area(aoi_sf)) / 1e6
  log_info("PHASE_2", sprintf("M area: %.2f km²", area_km2), script = "02_accessible_area_M.R")
  
  # ============================================================================
  # SAVE VECTOR
  # ============================================================================
  m_vector_path <- file.path(intermediate_dir, "m_area_vector.gpkg")
  write_ok <- tryCatch({
    st_write(aoi_sf, m_vector_path, delete_dsn = TRUE, quiet = TRUE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(write_ok) || !file.exists(m_vector_path)) {
    m_vector_path <- file.path(intermediate_dir, "m_area_vector.geojson")
    sf::st_write(aoi_sf, m_vector_path, delete_dsn = TRUE, quiet = TRUE, driver = "GeoJSON")
    log_warning("PHASE_2", "GPKG write failed; using GeoJSON fallback", script = "02_accessible_area_M.R")
  }
  log_info("PHASE_2", sprintf("M vector saved: %s", m_vector_path), script = "02_accessible_area_M.R")
  
  # ============================================================================
  # RASTERIZE TO TEMPLATE GRID
  # ============================================================================
  template_path <- file.path(intermediate_dir, "template_grid.tif")
  
  if (!file.exists(template_path)) {
    log_warning("PHASE_2", "Template not found, creating from AOI extent", script = "02_accessible_area_M.R")
    template <- rast(ext(aoi_sf), resolution = 1000, crs = if (is.numeric(target_crs)) paste0("EPSG:", target_crs) else target_crs)
  } else {
    template <- rast(template_path)
  }
  
  # Rasterize
  m_raster <- rasterize(aoi_sf, template, field = 1, background = 0)
  names(m_raster) <- "M_mask"
  
  # Save raster
  m_raster_path <- file.path(intermediate_dir, "m_mask.tif")
  writeRaster(m_raster, m_raster_path, overwrite = TRUE)
  log_info("PHASE_2", sprintf("M mask saved: %s", m_raster_path), script = "02_accessible_area_M.R")
  
  # ============================================================================
  # CREATE QA PLOT
  # ============================================================================
  qa_plot_path <- file.path(intermediate_dir, "m_area_qa_plot.png")
  
  tryCatch({
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = aoi, fill = "lightblue", alpha = 0.5) +
      ggplot2::labs(
        title = sprintf("Accessible Area (M) — %.0f km²", area_km2),
        subtitle = "AOI boundary (Bhutan, no buffer)"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::coord_sf()
    ggplot2::ggsave(qa_plot_path, p, width = 8, height = 6, dpi = 150)
    log_info("PHASE_2", sprintf("QA plot saved: %s", qa_plot_path), script = "02_accessible_area_M.R")
  }, error = function(e) {
    log_warning("PHASE_2", sprintf("QA plot failed: %s", e$message), script = "02_accessible_area_M.R")
  })
  
  # ============================================================================
  # CREATE REPORT
  # ============================================================================
  report_lines <- c(
    "ACCESSIBLE AREA (M) REPORT",
    paste0("Run ID: ", run_id),
    paste0("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    "INPUT",
    paste0("AOI path: ", aoi_path),
    paste0("AOI polygons: ", nrow(aoi)),
    "",
    "PARAMETERS",
    paste0("Buffer distance: none (AOI boundary used directly)"),
    paste0("Target CRS: ", if (is.numeric(target_crs)) paste0("EPSG:", target_crs) else target_crs),
    "",
    "OUTPUT",
    paste0("M area: ", round(area_km2, 2), " km²"),
    paste0("M vector: ", m_vector_path),
    paste0("M raster: ", m_raster_path),
    paste0("QA plot: ", qa_plot_path)
  )
  
  report_path <- file.path(intermediate_dir, "m_area_report.txt")
  writeLines(report_lines, report_path)
  
  log_phase_end("PHASE_2", "COMPLETE")
  
  return(list(
    m_vector = m_vector_path,
    m_raster = m_raster_path,
    m_area_km2 = area_km2,
    qa_plot = qa_plot_path,
    report = report_path
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
  
  result <- create_accessible_area(repo_root, config_path, run_dir, run_id)
  cat(sprintf("Phase 2 complete: M area = %.2f km²\n", result$m_area_km2))
}
