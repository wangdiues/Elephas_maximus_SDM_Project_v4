#!/usr/bin/env Rscript
# fix_vector_crs.R — Write correct .prj files for all shapefiles
# Governance: targets.md S7 — CRS mismatch is FATAL
#
# All raster data uses DRUKREF 03 / Bhutan National Grid (EPSG:5264):
#   lon_0=90, k=1, x_0=250000 (easting ~100K-460K range)
# Shapefiles in this project share that CRS, NOT UTM Zone 45N (EPSG:32645).
# The pipeline reprojects everything to EPSG:32645 as the analysis CRS.
#
# Strategy: coordinate range check:
#   easting 100K-500K  → DRUKREF 03 (EPSG:5264)
#   easting 500K+      → UTM Zone 45N (EPSG:32645)
#   max(abs(x)) <= 360 → WGS84 (EPSG:4326)

suppressPackageStartupMessages({ library(sf) })

# ESRI WKT1 strings — these are the canonical .prj format that GDAL recognizes
.ESRI_WKT <- list()
.ESRI_WKT[["5264"]] <- 'PROJCS["DRUKREF_03_Bhutan_National_Grid",GEOGCS["GCS_DRUKREF_03",DATUM["Bhutan_National_Geodetic_Datum",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",250000.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",90.0],PARAMETER["Scale_Factor",1.0],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0]]'
.ESRI_WKT[["32645"]] <- 'PROJCS["WGS_1984_UTM_Zone_45N",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",500000.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",87.0],PARAMETER["Scale_Factor",0.9996],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0]]'
.ESRI_WKT[["4326"]] <- 'GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]'

fix_vector_crs <- function(shapefiles_root = "01_data_raw/03_vector/shapefiles") {
  shp_files <- list.files(shapefiles_root, pattern = "\\.shp$", recursive = TRUE,
                          full.names = TRUE, ignore.case = TRUE)

  if (length(shp_files) == 0) {
    cat("No shapefiles found under:", shapefiles_root, "\n")
    return(invisible(character(0)))
  }

  corrections <- character(0)

  for (shp in shp_files) {
    prj_path <- sub("\\.shp$", ".prj", shp, ignore.case = TRUE)

    result <- tryCatch({
      # Back up existing .prj before removing (restore on failure)
      prj_backup <- NULL
      if (file.exists(prj_path)) {
        prj_backup <- readLines(prj_path, warn = FALSE)
        file.remove(prj_path)
      }

      sf_obj <- sf::st_read(shp, quiet = TRUE)
      coords <- sf::st_coordinates(sf::st_geometry(sf_obj))

      if (nrow(coords) == 0) {
        if (!is.null(prj_backup)) writeLines(prj_backup, prj_path)
        cat(sprintf("  SKIP (no coords): %s\n", basename(shp)))
        next
      }

      # Detect CRS by coordinate range
      x_max <- max(abs(coords[, 1]), na.rm = TRUE)
      if (x_max <= 360) {
        epsg <- "4326"
      } else if (x_max < 500000) {
        # Easting < 500K → DRUKREF 03 (false easting 250000, lon_0=90)
        epsg <- "5264"
      } else {
        # Easting >= 500K → UTM Zone 45N (false easting 500000, lon_0=87)
        epsg <- "32645"
      }

      # Write ESRI WKT1 .prj directly — no temp shapefile needed
      wkt <- .ESRI_WKT[[epsg]]
      writeLines(wkt, prj_path)

      if (file.exists(prj_path) && file.info(prj_path)$size > 0) {
        cat(sprintf("  FIXED: %s -> EPSG:%s\n", basename(shp), epsg))
        corrections <- c(corrections, sprintf("%s -> EPSG:%s", shp, epsg))
      } else {
        # Restore backup .prj if write failed
        if (!is.null(prj_backup)) {
          writeLines(prj_backup, prj_path)
          cat(sprintf("  RESTORED: %s (write failed)\n", basename(shp)))
        } else {
          cat(sprintf("  WARN: could not write .prj for %s\n", basename(shp)))
        }
      }
      "ok"
    }, error = function(e) {
      # Restore backup .prj on error
      if (!is.null(prj_backup) && !file.exists(prj_path)) {
        writeLines(prj_backup, prj_path)
      }
      cat(sprintf("  ERROR: %s -- %s\n", basename(shp), conditionMessage(e)))
      "error"
    })
  }

  cat(sprintf("\nCRS fix complete: %d shapefiles corrected\n", length(corrections)))
  invisible(corrections)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  root <- if (length(args) >= 1) args[[1]] else "01_data_raw/03_vector/shapefiles"
  fix_vector_crs(root)
}
