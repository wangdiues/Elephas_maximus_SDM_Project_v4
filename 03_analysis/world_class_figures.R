#!/usr/bin/env Rscript
# =============================================================================
# WORLD-CLASS PUBLICATION FIGURES — Elephas maximus SDM, Bhutan
# Enhanced multi-panel figures for CMIP6 future projections
# 9 GCMs × 4 SSPs × 3 Periods × 4 Algorithms
# =============================================================================
# Author: SDM Pipeline v3.0
# Standards: Nature/Science figure quality, 300+ DPI, publication-ready
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(viridis)
  library(cowplot)
  library(scales)
})

# =============================================================================
# CONFIGURATION
# =============================================================================
script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_file) == 0) {
  frame_files <- vapply(sys.frames(), function(env) if (is.null(env$ofile)) "" else as.character(env$ofile), character(1))
  frame_files <- frame_files[nzchar(frame_files)]
  if (length(frame_files) > 0) script_file <- frame_files[[length(frame_files)]]
}
script_dir <- if (length(script_file) > 0) dirname(normalizePath(sub("^--file=", "", script_file[1]), winslash = "/", mustWork = FALSE)) else normalizePath("03_analysis", winslash = "/", mustWork = FALSE)
source(file.path(script_dir, "00_repo_paths.R"))

repo_root <- find_repo_root()
args <- commandArgs(trailingOnly = TRUE)
RUN_DIR <- resolve_run_dir(if (length(args) >= 1) args[[1]] else NULL, repo_root = repo_root)
OUT_DIR <- file.path(RUN_DIR, "08_figures_tables")
FUT_DIR <- file.path(RUN_DIR, "04_future_projections")
UNC_DIR <- file.path(RUN_DIR, "06_uncertainty")
CHANGE_DIR <- file.path(RUN_DIR, "05_change_metrics")
PRESENT_DIR <- file.path(RUN_DIR, "03_present_suitability")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("=" , rep("=", 70), "\n", sep="")
cat("WORLD-CLASS FIGURE GENERATION — Elephas maximus SDM, Bhutan\n")
cat("=" , rep("=", 70), "\n", sep="")
cat(sprintf("Run directory: %s\n", RUN_DIR))
cat(sprintf("Output directory: %s\n", OUT_DIR))
cat("\n")

# =============================================================================
# COLOUR PALETTES & THEMES
# =============================================================================

# Publication-quality suitability palette (ColorBrewer YlOrRd enhanced)
SUIT_COLS <- c("#ffffff", "#fff7bc", "#fee391", "#fec44f", "#fe9929", "#ec7014", "#cc4c02", "#993404")
SUIT_VALS <- c(0, 0.05, 0.15, 0.25, 0.40, 0.60, 0.80, 1.00)

# Diverging palette for change maps (blue-white-red, ColorBrewer RdBu enhanced)
CHANGE_COLS <- c("#053061", "#2166ac", "#67a9cf", "#f7f7f7", "#ef8a62", "#ca0020", "#91000f")

# Uncertainty palette (viridis inferno)
UNC_COLS <- viridis(100, option = "inferno", direction = -1)

# SSP scenario colors (IPCC AR6 style)
SSP_COLS <- c(
  "SSP1-2.6" = "#008000",      # Green
  "SSP2-4.5" = "#ffd700",      # Gold/Yellow
  "SSP3-7.0" = "#ff6347",      # Tomato/Orange-red
  "SSP5-8.5" = "#8b0000"       # Dark red
)

# Algorithm colors (ColorBrewer Set1)
ALGO_COLS <- c(
  brt    = "#e41a1c",   # Red
  glm    = "#377eb8",   # Blue
  maxent = "#4daf4a",   # Green
  rf     = "#984ea3"    # Purple
)

# GCM colors (distinct, colorblind-friendly)
GCM_COLS <- c(
  "ACCESS-CM2"     = "#e41a1c",
  "CNRM-CM6-1"     = "#377eb8",
  "CNRM-ESM2-1"    = "#4daf4a",
  "INM-CM4-8"      = "#984ea3",
  "INM-CM5-0"      = "#ff7f00",
  "MIROC6"         = "#ffff33",
  "MIROC-ES2L"     = "#a65628",
  "MPI-ESM1-2-LR"  = "#f781bf"
)

# Background and grid colors
MAP_BG <- "#e8f4f8"
GRID_COL <- "white"

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Create publication-quality suitability color scale
suit_scale <- function(name = "Suitability", limits = c(0, 1)) {
  scale_fill_gradientn(
    colours = SUIT_COLS,
    values = SUIT_VALS,
    limits = limits,
    na.value = "grey85",
    name = name,
    guide = guide_colorbar(
      barwidth = 1.2, barheight = 8,
      ticks.linewidth = 0.5, frame.linewidth = 0.5,
      title.theme = element_text(size = 11, face = "bold", colour = "#2c3e50"),
      label.theme = element_text(size = 9, colour = "#555555"),
      title.position = "top"
    ),
    oob = squish
  )
}

#' Create diverging color scale for change maps
change_scale <- function(name = "Δ Suitability", lim = 0.5) {
  scale_fill_gradientn(
    colours = CHANGE_COLS,
    limits = c(-lim, lim),
    oob = squish,
    na.value = "grey85",
    name = name,
    guide = guide_colorbar(
      barwidth = 1.2, barheight = 8,
      ticks.linewidth = 0.5, frame.linewidth = 0.5,
      title.theme = element_text(size = 11, face = "bold", colour = "#2c3e50"),
      label.theme = element_text(size = 9, colour = "#555555"),
      title.position = "top"
    )
  )
}

#' Create uncertainty color scale
unc_scale <- function(name = "Uncertainty (SD)", max_val = NULL) {
  scale_fill_gradientn(
    colours = UNC_COLS,
    limits = c(0, if(is.null(max_val)) 0.5 else max_val),
    oob = squish,
    na.value = "grey85",
    name = name,
    guide = guide_colorbar(
      barwidth = 1.2, barheight = 8,
      ticks.linewidth = 0.5, frame.linewidth = 0.5,
      title.theme = element_text(size = 11, face = "bold", colour = "#2c3e50"),
      label.theme = element_text(size = 9, colour = "#555555"),
      title.position = "top"
    )
  )
}

#' Base map theme for all figures
map_theme <- function() {
  theme_bw(base_size = 10, base_family = "sans") +
    theme(
      panel.background = element_rect(fill = MAP_BG, colour = NA),
      panel.grid.major = element_line(colour = GRID_COL, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5, colour = "#2c3e50", margin = margin(b = 4)),
      plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "#555555", margin = margin(b = 8)),
      plot.caption = element_text(size = 8, colour = "#777777", hjust = 1, margin = margin(t = 6)),
      axis.title = element_blank(),
      axis.text = element_text(size = 8, colour = "#555555"),
      axis.ticks = element_line(colour = "#aaaaaa", linewidth = 0.3),
      legend.title = element_text(size = 10, face = "bold", colour = "#2c3e50"),
      legend.text = element_text(size = 9, colour = "#555555"),
      legend.position = "right",
      legend.box.margin = margin(0, 0, 0, 10),
      plot.margin = margin(8, 8, 8, 8)
    )
}

#' Add cartographic elements (scale bar, north arrow, CRS)
add_cartography <- function(p, location = "bottomleft") {
  if (requireNamespace("ggspatial", quietly = TRUE)) {
    p <- p +
      ggspatial::annotation_scale(
        location = location, width_hint = 0.35,
        bar_style = "simple", height_in_mm = 3,
        text_size = 7, text_colour = "#2c3e50"
      ) +
      ggspatial::annotation_north_arrow(
        location = "topright",
        style = ggspatial::north_arrow_minimal(
          line_width = 0.8,
          height = unit(1.2, "cm"),
          text_size = 8
        )
      ) +
      labs(caption = "CRS: EPSG:32645 (WGS 84 / UTM Zone 45N)")
  } else {
    p <- p + labs(caption = "CRS: EPSG:32645 | Scale ~1:500,000")
  }
  p + theme(plot.caption = element_text(size = 7.5, colour = "#666666", hjust = 1))
}

#' Load AOI boundary for map outlines
load_aoi <- function() {
  p <- repo_path("01_data_raw", "03_vector", "shapefiles", "Bhutan", "bhutan.shp", repo_root = repo_root)
  aoi <- tryCatch(st_read(p, quiet = TRUE), error = function(e) NULL)
  if (is.null(aoi)) {
    aoi <- tryCatch(
      st_read(file.path(RUN_DIR, "02_data_intermediate/m_area_vector.gpkg"), quiet = TRUE),
      error = function(e) NULL
    )
  }
  if (!is.null(aoi) && is.na(st_crs(aoi))) st_crs(aoi) <- 32645
  aoi
}

#' Convert raster to data frame for ggplot
raster_to_df <- function(r, aoi_sf = NULL, clip = TRUE) {
  if (clip && !is.null(aoi_sf)) {
    av <- vect(st_transform(aoi_sf, crs(r)))
    r <- tryCatch(mask(crop(r, av), av), error = function(e) r)
  }
  df <- as.data.frame(r[[1]], xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "value"
  df
}

#' Create AOI layer for ggplot
aoi_layer <- function(aoi_wgs, fill = NA, colour = "black", linewidth = 0.4) {
  if (!is.null(aoi_wgs)) {
    geom_sf(data = aoi_wgs, fill = fill, colour = colour, linewidth = linewidth, inherit.aes = FALSE)
  } else {
    NULL
  }
}

#' Parse scenario information from filename
parse_scenario <- function(filename) {
  bn <- tools::file_path_sans_ext(basename(filename))
  parts <- strsplit(bn, "_")[[1]]
  ssp_idx <- grep("^ssp[0-9]+$", parts, ignore.case = TRUE)
  if (length(ssp_idx) == 0) return(NULL)

  gcm <- paste(parts[3:(ssp_idx - 1)], collapse = "_")
  ssp <- parts[ssp_idx]
  period <- paste(parts[(ssp_idx + 1):(length(parts) - 1)], collapse = "_")
  algo <- parts[length(parts)]

  data.frame(
    gcm = gcm, ssp = ssp, period = period, algo = algo,
    path = filename, stringsAsFactors = FALSE
  )
}

#' Standardize GCM names for display
standardize_gcm_name <- function(gcm) {
  # Handle vector input with sapply
  if (length(gcm) > 1) {
    return(sapply(gcm, standardize_gcm_name, USE.NAMES = FALSE))
  }
  
  if (length(gcm) == 0) return("")
  if (is.na(gcm)) return("")
  
  gcm_upper <- toupper(gcm)
  gcm_upper <- gsub("_", "-", gcm_upper)
  
  # Common mappings
  mappings <- c(
    "ACCESS-CM2" = "ACCESS-CM2",
    "CNRM-CM6-1" = "CNRM-CM6-1",
    "CNRM-ESM2-1" = "CNRM-ESM2-1",
    "INM-CM4-8" = "INM-CM4-8",
    "INM-CM5-0" = "INM-CM5-0",
    "MIROC6" = "MIROC6",
    "MIROC-ES2L" = "MIROC-ES2L",
    "MPI-ESM1-2-LR" = "MPI-ESM1-2-LR"
  )
  
  if (gcm_upper %in% names(mappings)) {
    return(mappings[gcm_upper][[1]])
  } else {
    return(gcm_upper)
  }
}

#' Format SSP labels
format_ssp_label <- function(ssp) {
  ssp_lower <- tolower(ssp)
  mappings <- c(
    "ssp126" = "SSP1-2.6",
    "ssp245" = "SSP2-4.5",
    "ssp370" = "SSP3-7.0",
    "ssp585" = "SSP5-8.5"
  )
  if (ssp_lower %in% names(mappings)) mappings[ssp_lower] else toupper(ssp)
}

#' Format period labels
format_period_label <- function(period) {
  period_lower <- tolower(period)
  mappings <- c(
    "2021_2050" = "2021-2050",
    "2051_2080" = "2051-2080",
    "2071_2100" = "2071-2100",
    "2021-2050" = "2021-2050",
    "2051-2080" = "2051-2080",
    "2071-2100" = "2071-2100"
  )
  if (period_lower %in% names(mappings)) mappings[period_lower] else period
}

# =============================================================================
# LOAD DATA
# =============================================================================
cat("Loading spatial data...\n")
aoi <- load_aoi()
aoi_wgs <- if (!is.null(aoi)) st_transform(aoi, 4326) else NULL
cat(sprintf("  AOI loaded: %s\n", if(!is.null(aoi)) "YES" else "NO"))

# Discover available scenarios
cat("Discovering available scenarios...\n")
future_files <- list.files(FUT_DIR, pattern = "suitability_future.*\\.tif$", full.names = TRUE)
scenarios <- do.call(rbind, lapply(future_files, parse_scenario))
scenarios <- scenarios[!sapply(scenarios, is.null), ]

if (nrow(scenarios) == 0) {
  stop("No future scenario files found in ", FUT_DIR)
}

# Get unique values
gcms <- unique(scenarios$gcm)
ssps <- unique(scenarios$ssp)
periods <- unique(scenarios$period)
algos <- unique(scenarios$algo)

cat(sprintf("  GCMs: %d (%s)\n", length(gcms), paste(head(gcms, 3), collapse = ", ")))
if (length(gcms) > 3) cat(sprintf("        ... and %d more\n", length(gcms) - 3))
cat(sprintf("  SSPs: %d (%s)\n", length(ssps), paste(ssps, collapse = ", ")))
cat(sprintf("  Periods: %d (%s)\n", length(periods), paste(periods, collapse = ", ")))
cat(sprintf("  Algorithms: %d (%s)\n", length(algos), paste(algos, collapse = ", ")))
cat("\n")

# Load present suitability
present_file <- file.path(PRESENT_DIR, "suitability_present_ensemble.tif")
present_rast <- NULL
if (file.exists(present_file)) {
  present_rast <- rast(present_file)
  cat("Present suitability: LOADED\n")
} else {
  cat("Present suitability: NOT FOUND\n")
}

# =============================================================================
# FIGURE 1: MULTI-PANEL SSP × PERIOD ENSEMBLE (ALL GCMS)
# =============================================================================
cat("\n", rep("-", 70), "\n", sep = "")
cat("FIGURE 1: SSP × Period Ensemble Matrix (All GCMs)\n")
cat(rep("-", 70), "\n", sep = "")

# Create ensemble mean across all GCMs for each SSP × period
fig1_panels <- list()
for (si in seq_along(ssps)) {
  for (pi in seq_along(periods)) {
    # Get all files for this SSP × period (all GCMs, ensemble algos only)
    subset_files <- scenarios[
      scenarios$ssp == ssps[si] &
      scenarios$period == periods[pi] &
      scenarios$algo %in% c("glm", "rf", "brt"),
    ]

    if (nrow(subset_files) == 0) {
      fig1_panels[[length(fig1_panels) + 1]] <- patchwork::plot_spacer()
      next
    }

    # Load and ensemble
    rasters <- lapply(subset_files$path, function(f) {
      tryCatch(rast(f), error = function(e) NULL)
    })
    rasters <- rasters[!sapply(rasters, is.null)]

    if (length(rasters) == 0) {
      fig1_panels[[length(fig1_panels) + 1]] <- patchwork::plot_spacer()
      next
    }

    # Mean across all GCMs and algorithms
    ens <- app(rast(rasters), mean, na.rm = TRUE)
    df <- raster_to_df(ens, aoi_wgs)

    ssp_lbl <- format_ssp_label(ssps[si])
    per_lbl <- format_period_label(periods[pi])

    p <- ggplot() +
      geom_raster(data = df, aes(x = x, y = y, fill = value)) +
      aoi_layer(aoi_wgs) +
      suit_scale() +
      labs(
        title = if (pi == 1) ssp_lbl else "",
        subtitle = if (si == 1) per_lbl else ""
      ) +
      map_theme() +
      coord_sf(crs = 4326) +
      theme(
        plot.title = element_text(colour = SSP_COLS[ssp_lbl], size = 11),
        plot.subtitle = element_text(face = "bold", colour = "#2c3e50")
      )

    fig1_panels[[length(fig1_panels) + 1]] <- p
  }
}

# Arrange: 3 rows (periods) × 4 columns (SSPs)
fig1 <- wrap_plots(fig1_panels, ncol = 4, nrow = 3, guides = "collect")

fig1 <- fig1 + plot_annotation(
  title = "Future Habitat Suitability for Asian Elephants (Elephas maximus) in Bhutan",
  subtitle = "Ensemble mean across 9 GCMs and 3 algorithms (GLM, RF, BRT)",
  caption = "CMIP6 projections | Rows: Time period | Columns: Emission scenario | CRS: EPSG:32645",
  theme = theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, colour = "#555555", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, hjust = 1, colour = "#777777")
  )
)

ggsave(
  file.path(OUT_DIR, "fig01_ssp_period_ensemble.png"),
  fig1, width = 20, height = 16, dpi = 300, limitsize = FALSE
)
cat("  + Saved: fig01_ssp_period_ensemble.png (20×16 in, 300 DPI)\n")

# =============================================================================
# FIGURE 2: GCM COMPARISON (END-OF-CENTURY, ALL SSPS)
# =============================================================================
cat("\n", rep("-", 70), "\n", sep = "")
cat("FIGURE 2: GCM Comparison Matrix (2071-2100)\n")
cat(rep("-", 70), "\n", sep = "")

# Select end-of-century period
per_end <- "2071_2100"
fig2_panels <- list()

for (si in seq_along(ssps)) {
  for (gi in seq_along(gcms)) {
    # Get ensemble files for this GCM × SSP × period
    subset_files <- scenarios[
      scenarios$gcm == gcms[gi] &
      scenarios$ssp == ssps[si] &
      scenarios$period == per_end &
      scenarios$algo %in% c("glm", "rf", "brt"),
    ]

    if (nrow(subset_files) == 0) {
      fig2_panels[[length(fig2_panels) + 1]] <- patchwork::plot_spacer()
      next
    }

    rasters <- lapply(subset_files$path, function(f) {
      tryCatch(rast(f), error = function(e) NULL)
    })
    rasters <- rasters[!sapply(rasters, is.null)]

    if (length(rasters) == 0) {
      fig2_panels[[length(fig2_panels) + 1]] <- patchwork::plot_spacer()
      next
    }

    ens <- app(rast(rasters), mean, na.rm = TRUE)
    df <- raster_to_df(ens, aoi_wgs)

    ssp_lbl <- format_ssp_label(ssps[si])
    gcm_lbl <- standardize_gcm_name(gcms[gi])

    p <- ggplot() +
      geom_raster(data = df, aes(x = x, y = y, fill = value)) +
      aoi_layer(aoi_wgs) +
      suit_scale() +
      labs(
        title = if (si == 1) gcm_lbl else "",
        subtitle = if (gi == 1) paste0(ssp_lbl, "\n", "(2071-2100)") else ""
      ) +
      map_theme() +
      coord_sf(crs = 4326) +
      theme(
        plot.title = element_text(colour = GCM_COLS[gcm_lbl], size = 10),
        plot.subtitle = element_text(colour = SSP_COLS[ssp_lbl], face = "bold", size = 9)
      )

    fig2_panels[[length(fig2_panels) + 1]] <- p
  }
}

# Arrange: 4 rows (SSPs) × n GCMs columns
fig2 <- wrap_plots(fig2_panels, ncol = length(gcms), nrow = 4, guides = "collect")

fig2 <- fig2 + plot_annotation(
  title = "GCM Uncertainty in End-of-Century Projections (2071-2100)",
  subtitle = "Comparing 8 CMIP6 Global Climate Models across 4 emission scenarios",
  caption = sprintf("Ensemble mean of GLM, RF, BRT | GCMs: %s | CRS: EPSG:32645",
                    paste(standardize_gcm_name(gcms), collapse = ", ")),
  theme = theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, colour = "#555555", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, hjust = 1, colour = "#777777")
  )
)

ggsave(
  file.path(OUT_DIR, "fig02_gcm_comparison_2071_2100.png"),
  fig2, width = 22, height = 20, dpi = 300, limitsize = FALSE
)
cat("  + Saved: fig02_gcm_comparison_2071_2100.png (22×20 in, 300 DPI)\n")

# =============================================================================
# FIGURE 3: HABITAT CHANGE MAPS (DELTA SUITABILITY)
# =============================================================================
cat("\n", rep("-", 70), "\n", sep = "")
cat("FIGURE 3: Habitat Change Maps (Δ vs Present)\n")
cat(rep("-", 70), "\n", sep = "")

if (!is.null(present_rast)) {
  fig3_panels <- list()
  change_max <- 0

  for (si in seq_along(ssps)) {
    for (pi in seq_along(periods)) {
      # Get all files for this SSP × period
      subset_files <- scenarios[
        scenarios$ssp == ssps[si] &
        scenarios$period == periods[pi] &
        scenarios$algo %in% c("glm", "rf", "brt"),
      ]

      if (nrow(subset_files) == 0) {
        fig3_panels[[length(fig3_panels) + 1]] <- patchwork::plot_spacer()
        next
      }

      rasters <- lapply(subset_files$path, function(f) {
        tryCatch(rast(f), error = function(e) NULL)
      })
      rasters <- rasters[!sapply(rasters, is.null)]

      if (length(rasters) == 0) {
        fig3_panels[[length(fig3_panels) + 1]] <- patchwork::plot_spacer()
        next
      }

      # Ensemble mean
      fut_ens <- app(rast(rasters), mean, na.rm = TRUE)

      # Resample to present grid if needed
      if (nlyr(fut_ens) != nlyr(present_rast) || !compareGeom(fut_ens, present_rast, stopOnError = FALSE)) {
        fut_ens <- tryCatch(resample(fut_ens, present_rast, method = "bilinear"), error = function(e) fut_ens)
      }

      # Calculate delta
      delta <- fut_ens - present_rast
      df <- raster_to_df(delta, aoi_wgs)

      # Track max for consistent scale
      q <- quantile(df$value, c(0.02, 0.98), na.rm = TRUE)
      change_max <- max(change_max, abs(q[1]), abs(q[2]))

      ssp_lbl <- format_ssp_label(ssps[si])
      per_lbl <- format_period_label(periods[pi])

      p <- ggplot() +
        geom_raster(data = df, aes(x = x, y = y, fill = value)) +
        aoi_layer(aoi_wgs) +
        labs(
          title = if (pi == 1) ssp_lbl else "",
          subtitle = if (si == 1) per_lbl else ""
        ) +
        map_theme() +
        coord_sf(crs = 4326) +
        theme(
          plot.title = element_text(colour = SSP_COLS[ssp_lbl], size = 11),
          plot.subtitle = element_text(face = "bold")
        )

      fig3_panels[[length(fig3_panels) + 1]] <- p
    }
  }

  # Arrange and add common scale
  fig3 <- wrap_plots(fig3_panels, ncol = 4, nrow = 3, guides = "collect")

  fig3 <- fig3 + plot_annotation(
    title = "Projected Change in Habitat Suitability vs Present-Day Baseline",
    subtitle = "Blue = suitability loss | Red = suitability gain | White = no change",
    caption = "Δ = Future ensemble mean - Present ensemble | CRS: EPSG:32645",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 6)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, colour = "#555555", margin = margin(b = 10)),
      plot.caption = element_text(size = 9, hjust = 1, colour = "#777777")
    )
  )

  ggsave(
    file.path(OUT_DIR, "fig03_habitat_change_matrix.png"),
    fig3, width = 20, height = 16, dpi = 300, limitsize = FALSE
  )
  cat("  + Saved: fig03_habitat_change_matrix.png (20×16 in, 300 DPI)\n")
} else {
  cat("  SKIPPED: Present suitability not available\n")
}

# =============================================================================
# FIGURE 4: TIME-SERIES OF SUITABLE HABITAT AREA
# =============================================================================
cat("\n", rep("-", 70), "\n", sep = "")
cat("FIGURE 4: Time-Series of Suitable Habitat Area\n")
cat(rep("-", 70), "\n", sep = "")

# Calculate suitable area for all scenarios
threshold <- 0.5
area_data <- data.frame()

for (i in seq_len(nrow(scenarios))) {
  r <- tryCatch(rast(scenarios$path[i]), error = function(e) NULL)
  if (is.null(r)) next

  # Extract values within AOI
  if (!is.null(aoi)) {
    av <- vect(st_transform(aoi, crs(r)))
    vals <- tryCatch(values(mask(crop(r, av), av), na.rm = TRUE), error = function(e) values(r, na.rm = TRUE))
  } else {
    vals <- values(r, na.rm = TRUE)
  }

  if (length(vals) == 0) next

  area_data <- rbind(area_data, data.frame(
    gcm = scenarios$gcm[i],
    ssp = scenarios$ssp[i],
    period = scenarios$period[i],
    algo = scenarios$algo[i],
    pct_suitable = mean(vals >= threshold, na.rm = TRUE) * 100,
    mean_suitability = mean(vals, na.rm = TRUE),
    stringsAsFactors = FALSE
  ))
}

# Add present baseline
if (!is.null(present_rast)) {
  if (!is.null(aoi)) {
    av <- vect(st_transform(aoi, crs(present_rast)))
    pres_vals <- tryCatch(values(mask(crop(present_rast, av), av), na.rm = TRUE), error = function(e) values(present_rast, na.rm = TRUE))
  } else {
    pres_vals <- values(present_rast, na.rm = TRUE)
  }
  pres_pct <- mean(pres_vals >= threshold, na.rm = TRUE) * 100
} else {
  pres_pct <- NA
}

if (nrow(area_data) > 0) {
  # Format labels
  area_data$ssp_label <- factor(area_data$ssp,
    levels = c("ssp126", "ssp245", "ssp370", "ssp585"),
    labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
  )
  area_data$period_label <- factor(area_data$period,
    levels = c("2021_2050", "2051_2080", "2071_2100"),
    labels = c("2021-2050", "2051-2080", "2071-2100")
  )
  area_data$algo_label <- factor(area_data$algo,
    levels = c("glm", "rf", "brt"),
    labels = c("GLM", "Random Forest", "BRT")
  )
  area_data$gcm_label <- sapply(area_data$gcm, standardize_gcm_name)

  # Panel A: Line plot by algorithm
  fig4a <- ggplot(area_data, aes(x = period_label, y = pct_suitable,
                                  colour = ssp_label, group = interaction(ssp_label, gcm, algo))) +
    geom_line(aes(linetype = gcm_label), linewidth = 0.9, alpha = 0.6) +
    geom_point(aes(shape = gcm_label), size = 3, alpha = 0.8) +
    { if (!is.na(pres_pct)) geom_hline(yintercept = pres_pct, linetype = "dashed",
                                        colour = "grey30", linewidth = 1) } +
    { if (!is.na(pres_pct)) annotate("text", x = 0.5, y = pres_pct + 1,
                                      label = sprintf("Present: %.1f%%", pres_pct),
                                      colour = "grey30", size = 4, fontface = "bold", hjust = 0) } +
    scale_colour_manual(values = SSP_COLS, name = "Scenario") +
    scale_linetype_manual(values = c("solid", "dashed", "dotted", "twodash",
                                      "longdash", "dotdash", "dashed", "solid"),
                          name = "GCM") +
    scale_shape_manual(values = c(16, 17, 15, 18, 8, 4, 3, 1), name = "GCM") +
    facet_wrap(~algo_label, ncol = 3) +
    labs(x = "Time Period", y = "Suitable Habitat (% of Bhutan)",
         title = "Trajectory of Suitable Habitat by Algorithm") +
    theme_bw(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "#2c3e50"),
      strip.text = element_text(colour = "white", face = "bold", size = 10),
      axis.text.x = element_text(angle = 25, hjust = 1, size = 9),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.position = "right",
      legend.text = element_text(size = 8)
    )

  # Panel B: Violin + boxplot distribution
  fig4b <- ggplot(area_data, aes(x = ssp_label, y = pct_suitable, fill = ssp_label)) +
    geom_violin(alpha = 0.4, scale = "width", trim = FALSE) +
    geom_boxplot(width = 0.2, outlier.shape = 21, outlier.size = 1.5,
                 linewidth = 0.5, alpha = 0.7) +
    geom_jitter(width = 0.08, size = 1.5, alpha = 0.5, colour = "grey30") +
    { if (!is.na(pres_pct)) geom_hline(yintercept = pres_pct, linetype = "dashed",
                                        colour = "grey30", linewidth = 1) } +
    scale_fill_manual(values = SSP_COLS, guide = "none") +
    facet_wrap(~period_label, ncol = 3) +
    labs(x = "Emission Scenario", y = "Suitable Habitat (% of Bhutan)",
         title = "Distribution Across GCMs and Algorithms") +
    theme_bw(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "#2c3e50"),
      strip.text = element_text(colour = "white", face = "bold", size = 10),
      axis.text.x = element_text(angle = 20, hjust = 1, size = 9),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
    )

  # Combine panels
  fig4 <- fig4a / fig4b + plot_layout(heights = c(1.1, 1)) +
    plot_annotation(
      title = expression(paste("Projected Changes in ", italic("Elephas maximus"), " Suitable Habitat — Bhutan")),
      subtitle = sprintf("Suitability threshold: ≥ %.1f | Dashed line = present-day baseline", threshold),
      caption = sprintf("9 GCMs × 4 SSPs × 3 periods × 3 algorithms | Total scenarios: %d", nrow(area_data)),
      theme = theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 6)),
        plot.subtitle = element_text(size = 12, hjust = 0.5, colour = "#555555", margin = margin(b = 10)),
        plot.caption = element_text(size = 9, hjust = 1, colour = "#777777")
      )
    )

  ggsave(
    file.path(OUT_DIR, "fig04_habitat_area_timeseries.png"),
    fig4, width = 18, height = 20, dpi = 300, limitsize = FALSE
  )
  cat("  + Saved: fig04_habitat_area_timeseries.png (18×20 in, 300 DPI)\n")

  # Save data
  write.csv(area_data, file.path(OUT_DIR, "suitable_area_all_scenarios.csv"), row.names = FALSE)
  cat("  + Saved: suitable_area_all_scenarios.csv\n")
} else {
  cat("  SKIPPED: No scenario data available\n")
}

# =============================================================================
# FIGURE 5: ALGORITHM COMPARISON (SSP3-7.0, 2071-2100)
# =============================================================================
cat("\n", rep("-", 70), "\n", sep = "")
cat("FIGURE 5: Algorithm Comparison\n")
cat(rep("-", 70), "\n", sep = "")

ssp_algo <- "ssp370"
per_algo <- "2071_2100"
fig5_panels <- list()

for (gi in seq_along(gcms)) {
  for (ai in seq_along(algos)) {
    f <- file.path(FUT_DIR, sprintf("suitability_future_%s_%s_%s_%s.tif",
                                     gcms[gi], ssp_algo, per_algo, algos[ai]))
    if (!file.exists(f)) {
      fig5_panels[[length(fig5_panels) + 1]] <- patchwork::plot_spacer()
      next
    }

    r <- tryCatch(rast(f), error = function(e) NULL)
    if (is.null(r)) {
      fig5_panels[[length(fig5_panels) + 1]] <- patchwork::plot_spacer()
      next
    }

    df <- raster_to_df(r, aoi_wgs)
    gcm_lbl <- standardize_gcm_name(gcms[gi])
    algo_lbl <- toupper(algos[ai])

    p <- ggplot() +
      geom_raster(data = df, aes(x = x, y = y, fill = value)) +
      aoi_layer(aoi_wgs) +
      suit_scale() +
      labs(
        title = if (ai == 1) gcm_lbl else "",
        subtitle = if (gi == 1) algo_lbl else ""
      ) +
      map_theme() +
      coord_sf(crs = 4326) +
      theme(
        plot.title = element_text(colour = GCM_COLS[gcm_lbl], size = 10),
        plot.subtitle = element_text(colour = ALGO_COLS[algos[ai]], face = "bold", size = 10)
      )

    fig5_panels[[length(fig5_panels) + 1]] <- p
  }
}

fig5 <- wrap_plots(fig5_panels, ncol = 4, nrow = length(gcms), guides = "collect")

fig5 <- fig5 + plot_annotation(
  title = "Algorithm Comparison — SSP3-7.0 | 2071-2100",
  subtitle = "Comparing 4 SDM algorithms across 9 GCMs",
  caption = "Rows: GCM | Columns: Algorithm | CRS: EPSG:32645",
  theme = theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, colour = "#555555", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, hjust = 1, colour = "#777777")
  )
)

ggsave(
  file.path(OUT_DIR, "fig05_algorithm_comparison_ssp370.png"),
  fig5, width = 20, height = 16, dpi = 300, limitsize = FALSE
)
cat("  + Saved: fig05_algorithm_comparison_ssp370.png (20×16 in, 300 DPI)\n")

# =============================================================================
# FIGURE 6: MODEL VALIDATION (ROC + METRICS)
# =============================================================================
cat("\n", rep("-", 70), "\n", sep = "")
cat("FIGURE 6: Model Validation\n")
cat(rep("-", 70), "\n", sep = "")

roc_file <- file.path(RUN_DIR, "02_models/roc_curve_data.csv")
eval_file <- file.path(RUN_DIR, "02_models/evaluation_all.csv")

if (file.exists(roc_file) && file.exists(eval_file)) {
  roc_df <- read.csv(roc_file)
  eval_df <- read.csv(eval_file)
  eval_df <- eval_df[eval_df$algorithm != "ensemble", ]

  # Calculate FPR
  roc_df$fpr <- 1 - roc_df$specificity

  # Get AUC values for annotation
  auc_lut <- setNames(round(eval_df$auc_mean, 3), eval_df$algorithm)

  # Panel A: ROC curves
  fig6a <- ggplot(roc_df, aes(x = fpr, y = sensitivity, colour = algorithm)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.8) +
    geom_ribbon(aes(ymin = 0, ymax = sensitivity, fill = algorithm), alpha = 0.08) +
    geom_line(linewidth = 1.3) +
    scale_colour_manual(values = ALGO_COLS, guide = "none") +
    scale_fill_manual(values = ALGO_COLS, guide = "none") +
    annotate("text",
      x = rep(0.55, 4), y = seq(0.50, 0.15, length.out = 4),
      label = sprintf("%s\nAUC = %.3f", toupper(names(auc_lut)), auc_lut),
      hjust = 0, size = 4.5, fontface = "bold",
      colour = ALGO_COLS[names(auc_lut)]
    ) +
    labs(
      title = "ROC Curves (5-Fold Spatial Cross-Validation)",
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity)"
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank()
    )

  # Panel B: Metrics heatmap
  metric_cols <- c("auc_mean", "tss_mean", "boyce", "brier", "calibration_slope", "moran_i")
  metric_labels <- c("AUC", "TSS", "Boyce Index", "Brier Score", "Calibration Slope", "Moran's I")

  metric_long <- do.call(rbind, lapply(seq_along(metric_cols), function(i) {
    m <- metric_cols[i]
    if (!m %in% names(eval_df)) return(NULL)
    data.frame(
      Algorithm = toupper(eval_df$algorithm),
      Metric = metric_labels[i],
      Value = eval_df[[m]],
      stringsAsFactors = FALSE
    )
  }))
  metric_long <- metric_long[!is.na(metric_long$Value), ]

  fig6b <- ggplot(metric_long, aes(x = Algorithm, y = Metric, fill = Value)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.3f", Value)), size = 4, fontface = "bold") +
    scale_fill_gradientn(colours = c("#ca0020", "#f4a582", "#f7f7f7", "#92c5de", "#053061"),
                          na.value = "grey85", name = "Value") +
    labs(title = "Performance Metrics Comparison", x = NULL, y = NULL) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = 10, face = "bold", colour = ALGO_COLS),
      axis.text.y = element_text(size = 10),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )

  # Combine
  fig6 <- fig6a / fig6b + plot_layout(heights = c(2, 1.3)) +
    plot_annotation(
      title = expression(paste(italic("Elephas maximus"), " SDM Performance — Bhutan")),
      subtitle = "Out-of-fold predictions from 5-fold spatial cross-validation (15 km blocks)",
      caption = "AUC ≥ 0.65, TSS ≥ 0.30, Boyce ≥ 0.10: Minimum quality thresholds | Moran's I < 0.3: Acceptable spatial autocorrelation",
      theme = theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 6)),
        plot.subtitle = element_text(size = 12, hjust = 0.5, colour = "#555555", margin = margin(b = 10)),
        plot.caption = element_text(size = 9, hjust = 1, colour = "#777777")
      )
    )

  ggsave(
    file.path(OUT_DIR, "fig06_model_validation.png"),
    fig6, width = 16, height = 18, dpi = 300, limitsize = FALSE
  )
  cat("  + Saved: fig06_model_validation.png (16×18 in, 300 DPI)\n")
} else {
  cat("  SKIPPED: Model evaluation files not found\n")
}

# =============================================================================
# FIGURE 7: UNCERTAINTY MAPS (IF AVAILABLE)
# =============================================================================
cat("\n", rep("-", 70), "\n", sep = "")
cat("FIGURE 7: Uncertainty Quantification\n")
cat(rep("-", 70), "\n", sep = "")

# Check for uncertainty files
unc_files_gcm <- list.files(UNC_DIR, pattern = "gcm_sd.*\\.tif$", full.names = TRUE)
unc_files_algo <- list.files(UNC_DIR, pattern = "algorithm.*\\.tif$", full.names = TRUE)
unc_files_combined <- list.files(UNC_DIR, pattern = "combined.*\\.tif$", full.names = TRUE)

if (length(unc_files_gcm) > 0 || length(unc_files_combined) > 0) {
  fig7_panels <- list()

  # Get max uncertainty value for consistent scale
  all_unc_files <- c(unc_files_gcm, unc_files_algo, unc_files_combined)
  max_unc <- 0
  for (f in all_unc_files) {
    if (file.exists(f)) {
      r <- tryCatch(rast(f), error = function(e) NULL)
      if (!is.null(r)) {
        vals <- values(r, na.rm = TRUE)
        max_unc <- max(max_unc, quantile(vals, 0.98, na.rm = TRUE))
      }
    }
  }

  # Plot GCM uncertainty
  if (length(unc_files_gcm) > 0) {
    for (i in seq_along(unc_files_gcm)) {
      r <- tryCatch(rast(unc_files_gcm[i]), error = function(e) NULL)
      if (is.null(r)) next

      df <- raster_to_df(r, aoi_wgs)
      bn <- tools::file_path_sans_ext(basename(unc_files_gcm[i]))

      p <- ggplot() +
        geom_raster(data = df, aes(x = x, y = y, fill = value)) +
        aoi_layer(aoi_wgs) +
        unc_scale(max_val = round(max_unc, 2)) +
        labs(title = gsub("gcm_sd_", "", bn), subtitle = "GCM SD") +
        map_theme() +
        coord_sf(crs = 4326)

      fig7_panels[[length(fig7_panels) + 1]] <- p
    }
  }

  # Plot combined uncertainty
  if (length(unc_files_combined) > 0) {
    for (i in seq_along(unc_files_combined)) {
      r <- tryCatch(rast(unc_files_combined[i]), error = function(e) NULL)
      if (is.null(r)) next

      df <- raster_to_df(r, aoi_wgs)
      bn <- tools::file_path_sans_ext(basename(unc_files_combined[i]))

      p <- ggplot() +
        geom_raster(data = df, aes(x = x, y = y, fill = value)) +
        aoi_layer(aoi_wgs) +
        unc_scale(max_val = round(max_unc, 2)) +
        labs(title = gsub("combined_uncertainty_", "", bn), subtitle = "Combined SD") +
        map_theme() +
        coord_sf(crs = 4326)

      fig7_panels[[length(fig7_panels) + 1]] <- p
    }
  }

  if (length(fig7_panels) > 0) {
    fig7 <- wrap_plots(fig7_panels, ncol = 3, guides = "collect")

    fig7 <- fig7 + plot_annotation(
      title = "Projection Uncertainty Quantification",
      subtitle = "Standard deviation across GCMs and algorithms",
      caption = "Higher values indicate greater uncertainty in projections | CRS: EPSG:32645",
      theme = theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 6)),
        plot.subtitle = element_text(size = 12, hjust = 0.5, colour = "#555555", margin = margin(b = 10)),
        plot.caption = element_text(size = 9, hjust = 1, colour = "#777777")
      )
    )

    ggsave(
      file.path(OUT_DIR, "fig07_uncertainty_maps.png"),
      fig7, width = 18, height = 14, dpi = 300, limitsize = FALSE
    )
    cat("  + Saved: fig07_uncertainty_maps.png (18×14 in, 300 DPI)\n")
  }
} else {
  cat("  SKIPPED: Uncertainty files not found (run Phase 11)\n")
}

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n", rep("=", 70), "\n", sep = "")
cat("FIGURE GENERATION COMPLETE\n")
cat(rep("=", 70), "\n", sep = "")
cat(sprintf("Output directory: %s\n", OUT_DIR))
cat("\nGenerated figures:\n")

generated_figs <- list.files(OUT_DIR, pattern = "^fig[0-9].*\\.png$", full.names = TRUE)
for (f in generated_figs) {
  info <- file.info(f)
  size_mb <- round(info$size / 1024 / 1024, 2)
  cat(sprintf("  ✓ %s (%.2f MB)\n", basename(f), size_mb))
}

cat("\n")
cat("All figures saved at 300 DPI, publication-ready quality.\n")
cat("For manuscript submission, consider converting to TIFF or PDF format.\n")
cat("\n")
