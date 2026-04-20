#!/usr/bin/env Rscript
# STD-HARDENING: 2026-02-24
# role: phase_or_helper
# note: normalized script metadata for deterministic maintenance
# =============================================================================
# setup.R — Install Dependencies & Verify Setup
# =============================================================================
# Run this FIRST before running the pipeline
# =============================================================================

cat("=== Elephas maximus SDM Project v2.1 - Setup ===\n\n")

# Check R version
r_version <- as.numeric(paste(R.version$major, R.version$minor, sep = "."))
if (r_version < 4.3) {
  stop(sprintf("R version 4.3+ required. You have %s. Please upgrade.", R.version$major))
}
cat(sprintf("✓ R version: %s.%s\n\n", R.version$major, R.version$minor))

# Required packages
required_packages <- c(
  "terra",        # Raster processing
  "sf",           # Vector processing
  "yaml",         # Config parsing
  "ggplot2",      # Figures
  "viridis",      # Colorblind-safe palettes
  "ranger",       # Random Forest
  "maxnet",       # MaxEnt
  "gbm"           # Boosted Regression Trees
)

# Check which are installed
installed <- sapply(required_packages, requireNamespace, quietly = TRUE)
missing <- required_packages[!installed]

if (length(missing) > 0) {
  cat("Installing missing packages...\n")
  cat(sprintf("Missing: %s\n\n", paste(missing, collapse = ", ")))
  
  # Set CRAN mirror
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  
  # Install each missing package
  for (pkg in missing) {
    cat(sprintf("Installing %s... ", pkg))
    tryCatch({
      install.packages(pkg, dependencies = TRUE, quiet = TRUE)
      cat("✓\n")
    }, error = function(e) {
      cat(sprintf("✗ Failed: %s\n", e$message))
    })
  }
  
  # Verify installation
  cat("\nVerifying installation...\n")
  installed <- sapply(required_packages, requireNamespace, quietly = TRUE)
  still_missing <- required_packages[!installed]
  
  if (length(still_missing) > 0) {
    cat(sprintf("\n⚠ Still missing: %s\n", paste(still_missing, collapse = ", ")))
    cat("You can install these manually and re-run setup.\n\n")
  } else {
    cat("\n✓ All packages installed successfully!\n\n")
  }
} else {
  cat("✓ All required packages already installed\n\n")
}

# Verify data files
cat("Verifying data files...\n\n")

data_checks <- list(
  list(
    name = "Occurrence data (primary)",
    path = "01_data_raw/01_occurrences/elephant_PA_data.csv",
    required = TRUE
  ),
  list(
    name = "AOI shapefile",
    path = "01_data_raw/03_vector/shapefiles/Bhutan/bhutan.shp",
    required = TRUE
  ),
  list(
    name = "Climate data directory",
    path = "01_data_raw/02_rasters/present/Historical_bioclims/data/Historical/1986-2015",
    required = TRUE,
    is_dir = TRUE
  ),
  list(
    name = "DEM",
    path = "01_data_raw/02_rasters/dem/DEM_Bhutan_12.5NG.tif",
    required = TRUE
  ),
  list(
    name = "Human footprint",
    path = "01_data_raw/02_rasters/present/HII_Bhutan_2020.tif",
    required = FALSE
  ),
  list(
    name = "Land cover",
    path = "01_data_raw/02_rasters/present/esa_landcover.tif",
    required = FALSE
  )
)

all_ok <- TRUE
for (check in data_checks) {
  exists <- if (isTRUE(check$is_dir)) dir.exists(check$path) else file.exists(check$path)
  status <- if (exists) "✓" else "✗"
  
  if (!exists && check$required) {
    all_ok <- FALSE
    cat(sprintf("%s %s (REQUIRED - MISSING)\n", status, check$name))
  } else if (exists) {
    cat(sprintf("%s %s\n", status, check$name))
  } else {
    cat(sprintf("  %s (optional)\n", check$name))
  }
}

cat("\n")

# Create required directories
cat("Creating directories...\n")
dirs_to_create <- c(
  "04_outputs/runs",
  "06_logs",
  "00_registry"
)

for (dir in dirs_to_create) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat(sprintf("✓ Created: %s\n", dir))
  } else {
    cat(sprintf("  Exists: %s\n", dir))
  }
}

cat("\n")

# Final status
cat("=== SETUP SUMMARY ===\n\n")

if (length(missing) == 0 && all_ok) {
  cat("✓ Setup complete! Ready to run pipeline.\n\n")
  cat("To run the full pipeline:\n")
  cat("  Rscript 03_analysis/run_pipeline.R 00_governance/config.yaml\n\n")
} else {
  if (length(missing) > 0) {
    cat(sprintf("⚠ Missing packages: %s\n", paste(missing, collapse = ", ")))
    cat(sprintf("  Run: install.packages(c('%s'))\n\n", paste(missing, collapse = "', '")))
  }
  if (!all_ok) {
    cat("⚠ Some required data files are missing.\n")
    cat("  Please add the required files before running the pipeline.\n\n")
  }
}

cat("Setup script finished.\n")
