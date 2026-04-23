#!/usr/bin/env Rscript
# =============================================================================
# 10_figures_enhanced.R — Publication-quality figure enhancements
# Standalone script: operates on existing run artifacts without full re-run
#
# Generates / fixes:
#   E01  Figure 07 change map       — fixed consistent ±0.5 colour scale
#   E02  MaxEnt metrics             — computed from raster extraction (not OOF)
#   E03  Figure 08 trajectories     — all 4 SSPs + ensemble mean ± SD ribbon
#   E04  Gain/Loss/Persistence maps — 3-class categorical (SSP245 vs SSP585)
#   E05  PA overlay analysis        — suitability inside/outside protected areas
#   E06  Response curves            — partial dependence, 4 algorithms per predictor
#   E07  Variable importance        — 4 algorithms, colour-coded by predictor type
#
# Usage (PowerShell):
#   Rscript 03_analysis/10_figures_enhanced.R <run_dir>
#   Rscript 03_analysis/10_figures_enhanced.R 04_outputs
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(ggplot2)
})

# ── helpers ──────────────────────────────────────────────────────────────────

map_theme <- function() {
  theme_bw() +
  theme(
    panel.background  = element_rect(fill = "#d6e8f5"),
    panel.grid.major  = element_line(colour = "white", linewidth = 0.3),
    plot.title        = element_text(size = 13, face = "bold"),
    plot.subtitle     = element_text(size = 9,  colour = "grey40"),
    axis.title        = element_text(size = 9),
    legend.title      = element_text(size = 9,  face = "bold"),
    legend.text       = element_text(size = 8),
    plot.caption      = element_text(size = 7,  colour = "grey50")
  )
}

suit_scale <- function(name = "Suitability") {
  scale_fill_gradientn(
    colours  = c("#f7f7f7","#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),
    values   = c(0, 0.05, 0.2, 0.4, 0.65, 1),
    limits   = c(0, 1),
    na.value = "grey88",
    name     = name
  )
}

add_scalebar <- function(p) {
  if (requireNamespace("ggspatial", quietly = TRUE)) {
    p <- p +
      ggspatial::annotation_scale(location = "bl", width_hint = 0.25) +
      ggspatial::annotation_north_arrow(
        location = "tl", style = ggspatial::north_arrow_fancy_orienteering())
  }
  p + labs(caption = "CRS: EPSG:32645 (WGS 84 / UTM Zone 45N)")
}

rast_to_df <- function(r, aoi_v = NULL) {
  if (!is.null(aoi_v)) {
    aoi_v2 <- if (!isTRUE(crs(aoi_v) == crs(r))) project(aoi_v, crs(r)) else aoi_v
    r <- tryCatch(mask(crop(r, aoi_v2), aoi_v2), error = function(e) r)
  }
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  if (ncol(df) >= 3) names(df)[3] <- "value"
  df
}

pct_suitable <- function(r, thr = 0.5) {
  v <- values(r)
  valid <- !is.na(v)
  if (!any(valid)) return(NA_real_)
  round(100 * sum(v[valid] >= thr) / sum(valid), 1)
}

# Crop display extent to bounding box of non-NA data + small padding
data_bbox <- function(r, pad_frac = 0.04) {
  r_trim <- tryCatch(terra::trim(r), error = function(e) r)
  e  <- as.vector(terra::ext(r_trim))
  dx <- (e[2] - e[1]) * pad_frac
  dy <- (e[4] - e[3]) * pad_frac
  list(xlim = c(e[1] - dx, e[2] + dx), ylim = c(e[3] - dy, e[4] + dy))
}

load_aoi <- function(run_dir) {
  repo_root <- normalizePath(file.path(run_dir, "..", "..", ".."), mustWork = FALSE)
  paths <- c(
    file.path(repo_root, "01_data_raw", "03_vector", "shapefiles", "Bhutan", "bhutan.shp"),
    file.path(run_dir,   "02_data_intermediate", "m_area_vector.gpkg")
  )
  for (p in paths) {
    if (file.exists(p)) {
      aoi <- tryCatch(st_read(p, quiet = TRUE), error = function(e) NULL)
      if (!is.null(aoi)) return(st_make_valid(aoi))
    }
  }
  NULL
}

load_dzongkhag <- function(run_dir) {
  repo_root <- normalizePath(file.path(run_dir, "..", "..", ".."), mustWork = FALSE)
  p <- file.path(repo_root, "01_data_raw", "03_vector", "shapefiles",
                 "Dzongkhag Boundary", "Dzongkhag Boundary.shp")
  if (!file.exists(p)) return(NULL)
  d <- tryCatch(st_read(p, quiet = TRUE), error = function(e) NULL)
  if (is.null(d)) return(NULL)
  d <- st_make_valid(d)
  # Shapefile has wrong CRS metadata (labelled EPSG:32645 but coords are EPSG:3857)
  sf::st_crs(d) <- 3857L
  d
}

# ── main ─────────────────────────────────────────────────────────────────────

create_enhanced_figures <- function(run_dir) {
  run_dir  <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)
  fig_dir  <- file.path(run_dir, "08_figures_tables")
  mod_dir  <- file.path(run_dir, "02_models")
  proc_dir <- file.path(run_dir, "01_processed_data")
  fut_dir  <- file.path(run_dir, "04_future_projections")
  chg_dir  <- file.path(run_dir, "05_change_metrics")
  pres_dir <- file.path(run_dir, "03_present_suitability")
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

  cat("=== Enhanced Figures ===\n")
  n <- 0

  aoi_sf     <- load_aoi(run_dir)
  aoi_sf_utm <- if (!is.null(aoi_sf)) st_transform(aoi_sf, 32645) else NULL
  aoi_v      <- if (!is.null(aoi_sf_utm)) vect(aoi_sf_utm) else NULL
  aoi_layer  <- if (!is.null(aoi_sf_utm))
    geom_sf(data = aoi_sf_utm, fill = NA, color = "black", linewidth = 0.55) else NULL

  dzong_sf  <- load_dzongkhag(run_dir)
  dzong_layer_utm <- if (!is.null(dzong_sf))
    tryCatch({
      dz <- st_transform(dzong_sf, 32645)
      geom_sf(data = dz, inherit.aes = FALSE,
              fill = NA, color = "grey40", linewidth = 0.22, linetype = "dashed")
    }, error = function(e) NULL) else NULL

  has_repel <- requireNamespace("ggrepel", quietly = TRUE)
  dzong_label_layer_utm <- if (!is.null(dzong_sf))
    tryCatch({
      dz  <- st_transform(dzong_sf, 32645)
      ctr <- sf::st_centroid(dz)
      nm  <- if ("dzongkhag" %in% names(ctr)) "dzongkhag" else names(ctr)[1]
      coords <- sf::st_coordinates(ctr)
      df_lbl <- data.frame(x = coords[,1], y = coords[,2],
                            label = ctr[[nm]], stringsAsFactors = FALSE)
      if (has_repel)
        ggrepel::geom_label_repel(
          data = df_lbl, aes(x = x, y = y, label = label), inherit.aes = FALSE,
          size = 2.8, colour = "grey10", fill = alpha("white", 0.72),
          label.size = 0.18, label.padding = unit(0.10, "lines"),
          min.segment.length = 0.25, segment.colour = "grey50",
          segment.size = 0.3, max.overlaps = 50, seed = 42, fontface = "bold")
      else
        geom_sf_text(data = ctr, aes(label = .data[[nm]]), inherit.aes = FALSE,
                     size = 2.8, color = "grey10", fontface = "bold",
                     check_overlap = FALSE)
    }, error = function(e) NULL) else NULL

  # Load HWC incident data (clean file: longitude/latitude in WGS84)
  hwc_sf <- tryCatch({
    repo_root <- normalizePath(file.path(run_dir, "..", "..", ".."), mustWork = FALSE)
    hwc_path  <- file.path(repo_root, "01_data_raw", "04_conflicts",
                           "HWC_Species_Elephant_clean.csv")
    if (!file.exists(hwc_path)) stop("HWC file not found")
    hw <- read.csv(hwc_path, stringsAsFactors = FALSE)
    # longitude/latitude are in WGS84 (EPSG:4326)
    hw <- hw[!is.na(hw$longitude) & !is.na(hw$latitude) & hw$longitude != 0 & hw$latitude != 0, ]
    hw$conflict_type <- ifelse(
      grepl("Casualty", hw$damage_type, ignore.case = TRUE), "Human Casualty",
      ifelse(grepl("Crop",     hw$damage_type, ignore.case = TRUE), "Crop Damage",
      ifelse(grepl("Property", hw$damage_type, ignore.case = TRUE), "Property Damage",
             "Other")))
    # Reproject from WGS84 to EPSG:32645 for plotting
    hw_sf <- st_as_sf(hw, coords = c("longitude", "latitude"), crs = 4326L)
    hw_sf <- st_transform(hw_sf, 32645L)
    # Clip to Bhutan boundary — remove incidents outside national extent
    if (!is.null(aoi_sf_utm)) {
      hw_sf <- tryCatch(
        hw_sf[st_within(hw_sf, st_union(aoi_sf_utm), sparse = FALSE)[, 1], ],
        error = function(e) hw_sf
      )
    }
    hw_sf
  }, error = function(e) { message("  HWC data not loaded: ", e$message); NULL })

  # Local save_fig (ggsave wrapper) for E10 block
  save_fig <- function(p, path, width = 10, height = 8, dpi = 300) {
    tryCatch(
      suppressWarnings(ggplot2::ggsave(path, p, width = width, height = height,
                                       dpi = dpi, bg = "white")),
      error   = function(e) message("  save_fig failed: ", e$message),
      warning = function(w) message("  save_fig warning: ", w$message)
    )
  }

  # Read threshold from evaluation_all.csv
  threshold <- 0.5
  eval_csv  <- file.path(mod_dir, "evaluation_all.csv")
  if (file.exists(eval_csv)) {
    ev <- tryCatch(read.csv(eval_csv), error = function(e) NULL)
    if (!is.null(ev) && "threshold" %in% names(ev)) {
      tv <- mean(ev$threshold, na.rm = TRUE)
      if (is.finite(tv)) threshold <- tv
    }
  }

  # Read modeling dataset
  dat <- tryCatch(read.csv(file.path(proc_dir, "modeling_dataset.csv")), error = function(e) NULL)

  # Shared SSP / period lookups (used by E01, E11, E10)
  ssp_order_all   <- c("ssp126","ssp245","ssp370","ssp585")
  ssp_labels_all  <- c(ssp126 = "SSP1-2.6", ssp245 = "SSP2-4.5",
                        ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5")
  periods_all     <- c("2021_2050","2051_2080","2071_2100")
  period_disp_all <- c("2021_2050" = "2021\u20132050",
                        "2051_2080" = "2051\u20132080",
                        "2071_2100" = "2071\u20132100")

  # Shared helper: 2×2 patchwork figure
  make_2x2_figure <- function(panels, title_str, subtitle_str, caption_str = "CRS: EPSG:32645 (WGS 84 / UTM Zone 45N)") {
    if (!requireNamespace("patchwork", quietly = TRUE) || length(panels) == 0) return(NULL)
    ordered <- panels[intersect(ssp_order_all, names(panels))]
    patchwork::wrap_plots(ordered, ncol = 2) +
      patchwork::plot_annotation(title = title_str, subtitle = subtitle_str, caption = caption_str)
  }

  # ── E01: Habitat Change Maps — all 4 SSPs × all 3 periods (2×2 per period) ─
  tryCatch({
    if (dir.exists(chg_dir)) {
      delta_files <- list.files(chg_dir, pattern = "^delta_suitability_.*\\.tif$", full.names = TRUE)
      if (length(delta_files) > 0) {
        cat("E01: Habitat change maps (all SSPs \u00d7 all periods)...\n")

        change_scale <- function()
          scale_fill_gradientn(
            colours  = c("#053061","#2166ac","#67a9cf","#f7f7f7","#ef8a62","#ca0020","#91000f"),
            limits   = c(-0.5, 0.5), breaks = c(-0.5,-0.25,0,0.25,0.5),
            oob = scales::squish, na.value = "grey88", name = "\u0394 Suitability")

        for (period in periods_all) {
          yrs       <- strsplit(period, "_")[[1]]
          period_lb <- period_disp_all[period]
          panels    <- list()

          for (ssp in ssp_order_all) {
            f_match <- delta_files[grepl(ssp, delta_files, ignore.case = TRUE) &
                                     grepl(yrs[1], delta_files) & grepl(yrs[2], delta_files)]
            if (length(f_match) == 0) next
            r <- tryCatch(rast(f_match[1]), error = function(e) NULL)
            if (is.null(r)) next
            df   <- rast_to_df(r, aoi_v)
            bbox <- data_bbox(r)
            pct_gain <- round(100 * sum(df$value >  0.1, na.rm = TRUE) / nrow(df), 1)
            pct_loss <- round(100 * sum(df$value < -0.1, na.rm = TRUE) / nrow(df), 1)
            p <- ggplot() +
              geom_raster(data = df, aes(x = x, y = y, fill = value)) +
              aoi_layer + change_scale() +
              annotate("text", x = -Inf, y = -Inf,
                       label = sprintf("Gain >0.1: %s%%\nLoss <\u22120.1: %s%%", pct_gain, pct_loss),
                       hjust = -0.08, vjust = -0.4, size = 2.8, fontface = "bold") +
              labs(title = sprintf("%s | \u0394 suitability (%s \u2212 Present)",
                                   ssp_labels_all[ssp], period_lb),
                   x = NULL, y = NULL) +
              map_theme() +
              coord_sf(xlim = bbox$xlim, ylim = bbox$ylim, expand = FALSE, crs = st_crs(32645))
            panels[[ssp]] <- add_scalebar(p)
            rm(r, df); gc(verbose = FALSE)
          }

          fig <- make_2x2_figure(panels,
            title_str    = sprintf("Change in Habitat Suitability (%s vs Present) \u2014 Elephas maximus | Bhutan", period_lb),
            subtitle_str = sprintf("Diverging scale [\u22120.5, +0.5] | Threshold = %.3f | Ensemble mean (GLM + RF + BRT + MaxEnt)", threshold))
          if (!is.null(fig)) {
            fname <- sprintf("figure_habitat_change_%s.png", period)
            ggsave(file.path(fig_dir, fname), fig, width = 18, height = 14, dpi = 300, limitsize = FALSE)
            cat(sprintf("  + %s\n", fname)); n <- n + 1
          }
        }
      }
    }
  }, error = function(e) cat(sprintf("  E01 failed: %s\n", conditionMessage(e))))

  # ── E11: Future Habitat Suitability Maps — all 4 SSPs × all 3 periods ──────
  tryCatch({
    ens_files_e11 <- list.files(fut_dir, pattern = "^future_gcm_ensemble_.*\\.tif$", full.names = TRUE)
    if (length(ens_files_e11) > 0) {
      cat("E11: Future habitat suitability maps (all SSPs \u00d7 all periods)...\n")

      for (period in periods_all) {
        yrs       <- strsplit(period, "_")[[1]]
        period_lb <- period_disp_all[period]
        panels    <- list()

        for (ssp in ssp_order_all) {
          f_match <- ens_files_e11[grepl(ssp, ens_files_e11, ignore.case = TRUE) &
                                     grepl(yrs[1], ens_files_e11) & grepl(yrs[2], ens_files_e11)]
          if (length(f_match) == 0) next
          r <- tryCatch(rast(f_match[1]), error = function(e) NULL)
          if (is.null(r)) next
          df   <- rast_to_df(r, aoi_v)
          bbox <- data_bbox(r)
          pct_suit <- round(100 * sum(df$value >= threshold, na.rm = TRUE) / nrow(df), 1)
          p <- ggplot() +
            geom_raster(data = df, aes(x = x, y = y, fill = value)) +
            aoi_layer + suit_scale() +
            annotate("text", x = -Inf, y = -Inf,
                     label = sprintf("Suitable (\u2265%.2f): %s%%", threshold, pct_suit),
                     hjust = -0.06, vjust = -0.4, size = 2.8, fontface = "bold", color = "grey20") +
            labs(title = sprintf("%s | %s", ssp_labels_all[ssp], period_lb),
                 x = NULL, y = NULL) +
            map_theme() +
            coord_sf(xlim = bbox$xlim, ylim = bbox$ylim, expand = FALSE, crs = st_crs(32645))
          panels[[ssp]] <- add_scalebar(p)
          rm(r, df); gc(verbose = FALSE)
        }

        fig <- make_2x2_figure(panels,
          title_str    = sprintf("Future Habitat Suitability \u2014 %s | Elephas maximus | Bhutan", period_lb),
          subtitle_str = sprintf("Threshold = %.3f | Ensemble mean (GLM + RF + BRT + MaxEnt) | 8 CMIP6 GCMs", threshold))
        if (!is.null(fig)) {
          fname <- sprintf("figure_future_suit_%s.png", period)
          ggsave(file.path(fig_dir, fname), fig, width = 18, height = 14, dpi = 300, limitsize = FALSE)
          cat(sprintf("  + %s\n", fname)); n <- n + 1
        }
      }
    }
  }, error = function(e) cat(sprintf("  E11 failed: %s\n", conditionMessage(e))))

  # ── E02: MaxEnt metrics from raster extraction ────────────────────────────
  tryCatch({
    maxent_tif <- file.path(pres_dir, "suitability_present_maxent.tif")
    oof_csv    <- file.path(proc_dir, "oof_predictions.csv")
    if (file.exists(maxent_tif) && file.exists(oof_csv)) {
      cat("E02: Computing MaxEnt metrics from raster...\n")
      oof <- read.csv(oof_csv)
      mx_rast <- rast(maxent_tif)

      pres_coords <- oof[oof$response == 1, c("longitude", "latitude")]
      bg_coords   <- oof[, c("longitude", "latitude")]

      # Project to raster CRS
      pres_sf_mx <- st_transform(st_as_sf(pres_coords, coords = c("longitude","latitude"), crs = 4326),
                                 crs(mx_rast))
      bg_sf_mx   <- st_transform(st_as_sf(bg_coords,   coords = c("longitude","latitude"), crs = 4326),
                                 crs(mx_rast))

      extract_vals <- function(r, coords) {
        ex <- terra::extract(r, coords)
        # terra::extract with matrix coords returns matrix/df — grab last column (value col)
        if (is.data.frame(ex)) as.numeric(ex[[ncol(ex)]]) else as.numeric(ex[, ncol(ex)])
      }
      pres_pred <- extract_vals(mx_rast, st_coordinates(pres_sf_mx))
      bg_pred   <- extract_vals(mx_rast, st_coordinates(bg_sf_mx))
      pres_pred <- pres_pred[!is.na(pres_pred)]
      bg_pred   <- bg_pred[!is.na(bg_pred)]

      if (length(pres_pred) > 5 && length(bg_pred) > 5) {
        # Load metric functions
        src_dir <- file.path(normalizePath(file.path(run_dir, "..", "..", ".."), mustWork = FALSE), "03_analysis")
        if (!exists("boyce_continuous") && file.exists(file.path(src_dir, "00_sdm_helpers.R"))) {
          source(file.path(src_dir, "00_sdm_helpers.R"))
        }

        if (exists("boyce_continuous")) {
          boyce_mx  <- boyce_continuous(pres_pred, bg_pred)$boyce_index
          cat(sprintf("  MaxEnt Boyce (raster) = %.3f\n", boyce_mx))
          # Write to metrics_summary alongside note
          mx_note <- data.frame(
            metric = "boyce_maxent_raster",
            value  = boyce_mx,
            note   = "Computed from full raster predictions at P/A locations (not OOF holdout)",
            stringsAsFactors = FALSE
          )
          write.csv(mx_note, file.path(mod_dir, "maxent_raster_metrics.csv"), row.names = FALSE)

          # Update metrics_summary if maxent row exists
          ms <- tryCatch(read.csv(file.path(mod_dir, "metrics_summary.csv")), error = function(e) NULL)
          if (!is.null(ms) && "maxent" %in% ms$algorithm) {
            ms$boyce[ms$algorithm == "maxent"] <- boyce_mx
            write.csv(ms, file.path(mod_dir, "metrics_summary.csv"), row.names = FALSE)
            cat("  Updated metrics_summary.csv with MaxEnt Boyce\n")
          }
        }
      }
    }
  }, error = function(e) cat(sprintf("  E02 failed: %s\n", conditionMessage(e))))

  # ── E03: All-4-SSP suitability area trajectories ──────────────────────────
  tryCatch({
    ens_files <- list.files(fut_dir, pattern = "^future_gcm_ensemble_.*\\.tif$", full.names = TRUE)
    if (length(ens_files) == 0)
      ens_files <- list.files(fut_dir, pattern = ".*ensemble.*\\.tif$", full.names = TRUE)

    if (length(ens_files) >= 4) {
      cat("E03: All-SSP suitability trajectories...\n")

      # Parse filename → ssp + period
      parse_ens <- function(f) {
        x     <- tools::file_path_sans_ext(basename(f))
        x     <- sub("^(future_gcm_ensemble_|suitability_future_ensemble_)", "", x)
        parts <- strsplit(x, "_")[[1]]
        ssp_i <- grep("^ssp[0-9]+$", parts, ignore.case = TRUE)
        if (length(ssp_i) == 0) return(NULL)
        ssp    <- toupper(parts[ssp_i[1]])
        period_parts <- parts[(ssp_i[1]+1):min(ssp_i[1]+2, length(parts))]
        period <- paste(period_parts, collapse = "-")
        data.frame(path = f, ssp = ssp, period = period, stringsAsFactors = FALSE)
      }
      idx <- do.call(rbind, lapply(ens_files, parse_ens))
      idx <- idx[!sapply(idx$ssp, is.null), , drop = FALSE]

      if (!is.null(idx) && nrow(idx) > 0) {
        # Compute % suitable area per ensemble map
        traj_rows <- list()
        for (i in seq_len(nrow(idx))) {
          r <- tryCatch(rast(idx$path[i]), error = function(e) NULL)
          if (is.null(r)) next
          pct <- pct_suitable(r, threshold)
          traj_rows[[i]] <- data.frame(
            ssp = idx$ssp[i], period = idx$period[i], pct_suitable = pct,
            stringsAsFactors = FALSE)
          rm(r); gc(verbose = FALSE)
        }
        traj_df <- do.call(rbind, traj_rows)

        # Present-day baseline
        pres_ens <- file.path(pres_dir, "suitability_present_ensemble.tif")
        pres_pct <- if (file.exists(pres_ens)) pct_suitable(rast(pres_ens), threshold) else NA

        # Factor ordering
        period_order <- sort(unique(traj_df$period))
        traj_df$period <- factor(traj_df$period, levels = period_order)

        ssp_colors <- c("SSP126" = "#1a9641", "SSP245" = "#a6d96a",
                        "SSP370" = "#fdae61", "SSP585" = "#d7191c")
        ssp_labs   <- c("SSP126" = "SSP1-2.6 (Low)", "SSP245" = "SSP2-4.5 (Intermediate)",
                        "SSP370" = "SSP3-7.0 (High)",  "SSP585" = "SSP5-8.5 (Very High)")

        p <- ggplot(traj_df, aes(x = period, y = pct_suitable,
                                  color = ssp, group = ssp)) +
          geom_line(linewidth = 1.2) +
          geom_point(size = 3) +
          geom_text(aes(label = sprintf("%.1f%%", pct_suitable)),
                    vjust = -0.8, size = 2.8, show.legend = FALSE) +
          scale_color_manual(values = ssp_colors, labels = ssp_labs, name = "Scenario") +
          labs(title = "Projected Suitable Habitat — All 4 SSPs",
               subtitle = sprintf("Threshold = %.3f | Present baseline = %.1f%%", threshold, pres_pct),
               x = "Time Period", y = "% of Bhutan Classified as Suitable") +
          theme_bw() +
          theme(legend.position = "bottom",
                axis.text.x     = element_text(angle = 20, hjust = 1),
                plot.title      = element_text(face = "bold", size = 13))

        if (!is.na(pres_pct)) {
          p <- p + geom_hline(yintercept = pres_pct, linetype = "dashed",
                               color = "grey40", linewidth = 0.8) +
                   annotate("text", x = Inf, y = pres_pct,
                            label = sprintf(" Present (%.1f%%)", pres_pct),
                            hjust = 1.05, vjust = -0.5, size = 3, color = "grey40")
        }

        ggsave(file.path(fig_dir, "figure_E03_trajectories_all_ssp.png"), p,
               width = 10, height = 7, dpi = 300)
        write.csv(traj_df, file.path(fig_dir, "table_trajectories_all_ssp.csv"), row.names = FALSE)
        cat("  + figure_E03_trajectories_all_ssp.png\n"); n <- n + 1

        # Faceted version (one panel per SSP)
        p2 <- ggplot(traj_df, aes(x = period, y = pct_suitable, group = 1)) +
          geom_line(linewidth = 1.0, color = "#2166ac") +
          geom_point(size = 2.5, color = "#2166ac") +
          geom_text(aes(label = sprintf("%.1f%%", pct_suitable)),
                    vjust = -0.8, size = 2.5) +
          facet_wrap(~ ssp, scales = "free_y", ncol = 2,
                     labeller = as_labeller(ssp_labs)) +
          labs(title = "Projected Suitable Habitat by Emissions Scenario",
               x = "Time Period", y = "% of Bhutan") +
          theme_bw() +
          theme(strip.background = element_rect(fill = "#f0f0f0"),
                strip.text       = element_text(face = "bold"),
                axis.text.x      = element_text(angle = 20, hjust = 1),
                plot.title       = element_text(face = "bold", size = 13))
        if (!is.na(pres_pct))
          p2 <- p2 + geom_hline(yintercept = pres_pct, linetype = "dashed",
                                  color = "grey40", linewidth = 0.7)

        ggsave(file.path(fig_dir, "figure_E03b_trajectories_faceted.png"), p2,
               width = 12, height = 8, dpi = 300)
        cat("  + figure_E03b_trajectories_faceted.png\n"); n <- n + 1
      }
    }
  }, error = function(e) cat(sprintf("  E03 failed: %s\n", conditionMessage(e))))

  # ── E04: Gain/Loss/Persistence — all 4 SSPs × all 3 periods (2×2 per period)
  tryCatch({
    pres_ens      <- file.path(pres_dir, "suitability_present_ensemble.tif")
    ens_files_glp <- list.files(fut_dir, pattern = "^future_gcm_ensemble_.*\\.tif$", full.names = TRUE)

    if (file.exists(pres_ens) && length(ens_files_glp) > 0) {
      cat("E04: Gain/Loss/Persistence maps (all SSPs \u00d7 all periods)...\n")
      present  <- rast(pres_ens)
      p0       <- present >= threshold
      bbox_glp <- data_bbox(present)   # fixed extent across all panels

      glp_colors <- c("Unsuitable" = "grey88", "Gain" = "#4393c3",
                      "Loss" = "#d6604d", "Persistence" = "#4dac26")

      aoi_layer_glp <- if (!is.null(aoi_sf_utm))
        geom_sf(data = aoi_sf_utm, fill = NA, color = "black", linewidth = 0.55,
                inherit.aes = FALSE) else NULL

      for (period in periods_all) {
        yrs       <- strsplit(period, "_")[[1]]
        period_lb <- period_disp_all[period]
        panels    <- list()

        for (ssp in ssp_order_all) {
          f_match <- ens_files_glp[grepl(ssp, ens_files_glp, ignore.case = TRUE) &
                                     grepl(yrs[1], ens_files_glp) & grepl(yrs[2], ens_files_glp)]
          if (length(f_match) == 0) next
          fut <- tryCatch(rast(f_match[1]), error = function(e) NULL)
          if (is.null(fut)) next

          fut <- tryCatch(resample(fut, present, method = "bilinear"), error = function(e) fut)
          p1  <- fut >= threshold

          # 0 = unsuitable both, 1 = gain, 2 = loss, 3 = persistence
          glp        <- (p0 * 2) + (p1 * 1); names(glp) <- "class"
          cell_km2   <- prod(res(glp)) / 1e6
          counts     <- table(values(glp))
          area_gain  <- { v <- counts["1"]; if (is.na(v)) 0 else as.numeric(v) * cell_km2 }
          area_loss  <- { v <- counts["2"]; if (is.na(v)) 0 else as.numeric(v) * cell_km2 }

          df_glp <- rast_to_df(glp, aoi_v)
          df_glp$class_label <- factor(df_glp$value,
            levels = c(0,1,2,3), labels = c("Unsuitable","Gain","Loss","Persistence"))
          df_glp <- df_glp[!is.na(df_glp$class_label), ]

          p <- ggplot(df_glp, aes(x = x, y = y, fill = class_label)) +
            geom_raster() +
            aoi_layer_glp +
            scale_fill_manual(values = glp_colors, name = NULL, drop = FALSE) +
            annotate("text", x = -Inf, y = -Inf,
                     label = sprintf("Gain: %.0f km\u00b2\nLoss: %.0f km\u00b2", area_gain, area_loss),
                     hjust = -0.08, vjust = -0.4, size = 2.8, fontface = "bold") +
            labs(title = sprintf("Habitat Change \u2014 %s %s", ssp_labels_all[ssp], period_lb),
                 x = NULL, y = NULL) +
            map_theme() +
            coord_sf(xlim = bbox_glp$xlim, ylim = bbox_glp$ylim, expand = FALSE, crs = st_crs(32645))
          panels[[ssp]] <- add_scalebar(p)
          rm(fut, glp, df_glp); gc(verbose = FALSE)
        }

        fig <- make_2x2_figure(panels,
          title_str    = sprintf("Habitat Gain, Loss, and Persistence (%s vs Present) \u2014 Elephas maximus | Bhutan", period_lb),
          subtitle_str = sprintf("Threshold = %.3f | Gain = new suitable habitat | Loss = habitat no longer suitable | Persistence = continuously suitable", threshold))
        if (!is.null(fig)) {
          fname <- sprintf("figure_gain_loss_persistence_%s.png", period)
          ggsave(file.path(fig_dir, fname), fig, width = 18, height = 14, dpi = 300, limitsize = FALSE)
          cat(sprintf("  + %s\n", fname)); n <- n + 1
          # Backward-compatible alias for the end-century period
          if (period == "2071_2100")
            file.copy(file.path(fig_dir, fname),
                      file.path(fig_dir, "figure_E04_gain_loss_persistence.png"),
                      overwrite = TRUE)
        }
      }
    }
  }, error = function(e) cat(sprintf("  E04 failed: %s\n", conditionMessage(e))))

  # ── E05: Protected area overlay ───────────────────────────────────────────
  tryCatch({
    repo_root <- normalizePath(file.path(run_dir, "..", "..", ".."), mustWork = FALSE)
    pa_shp <- file.path(repo_root, "01_data_raw", "03_vector", "shapefiles",
                        "PA_Bhutan", "PA_Bnd_Final_20230316.shp")
    pres_ens <- file.path(pres_dir, "suitability_present_ensemble.tif")

    if (file.exists(pa_shp) && file.exists(pres_ens)) {
      cat("E05: Protected area overlay...\n")
      suit      <- rast(pres_ens)
      bbox_suit <- data_bbox(suit)
      pa_sf     <- st_read(pa_shp, quiet = TRUE)
      pa_sf <- st_make_valid(pa_sf)
      pa_v  <- vect(st_transform(pa_sf, crs(suit)))

      # Rasterize PA boundaries
      pa_mask <- rasterize(pa_v, suit, field = 1, background = 0)
      suit_inside  <- mask(suit, pa_mask, maskvalues = 0)
      suit_outside <- mask(suit, pa_mask, maskvalues = 1)

      vals_in  <- values(suit_inside,  na.rm = TRUE)
      vals_out <- values(suit_outside, na.rm = TRUE)
      pct_in   <- round(100 * sum(vals_in  >= threshold) / sum(!is.na(values(suit))), 1)
      pct_out  <- round(100 * sum(vals_out >= threshold) / sum(!is.na(values(suit))), 1)
      cat(sprintf("  Suitable inside PAs:  %.1f%%\n", pct_in))
      cat(sprintf("  Suitable outside PAs: %.1f%%\n", pct_out))

      # Map: present suitability + PA boundaries
      df_suit <- rast_to_df(suit, aoi_v)
      pa_sf_plot <- st_transform(pa_sf, crs = st_crs(32645))
      pa_only_e05 <- pa_sf_plot[!grepl("^Biological Corridor", pa_sf_plot$park), ]
      bc_only_e05 <- pa_sf_plot[ grepl("^Biological Corridor", pa_sf_plot$park), ]

      # Centroid label helpers
      .pa_lbl <- function(sf_obj, col, sz, col_txt, ff) {
        ctr    <- tryCatch(sf::st_centroid(sf_obj), error = function(e) NULL)
        if (is.null(ctr) || nrow(ctr) == 0) return(NULL)
        coords <- sf::st_coordinates(ctr)
        df_l   <- data.frame(x = coords[,1], y = coords[,2],
                              label = ctr[[col]], stringsAsFactors = FALSE)
        if (has_repel)
          ggrepel::geom_label_repel(
            data = df_l, aes(x = x, y = y, label = label), inherit.aes = FALSE,
            size = sz, colour = col_txt, fill = alpha("white", 0.78), fontface = ff,
            label.size = 0.18, label.padding = unit(0.10, "lines"),
            min.segment.length = 0.25, segment.colour = col_txt,
            segment.size = 0.3, max.overlaps = 50, seed = 42)
        else
          geom_text(data = df_l, aes(x = x, y = y, label = label),
                    inherit.aes = FALSE, size = sz, colour = col_txt, fontface = ff)
      }

      p_map <- ggplot() +
        geom_raster(data = df_suit, aes(x = x, y = y, fill = value)) +
        aoi_layer +
        { if (nrow(bc_only_e05) > 0)
            geom_sf(data = bc_only_e05, fill = NA, color = "#4dac26",
                    linewidth = 0.5, linetype = "dashed", inherit.aes = FALSE) } +
        geom_sf(data = pa_only_e05, fill = NA, color = "#1b7837",
                linewidth = 0.6, inherit.aes = FALSE) +
        .pa_lbl(pa_only_e05, "PA_name", 2.5, "#1b7837", "bold") +
        .pa_lbl(bc_only_e05, "PA_name", 2.2, "#4dac26", "italic") +
        suit_scale() +
        annotate("text", x = Inf, y = Inf,
                 label = sprintf("Inside PAs:  %.1f%% suitable\nOutside PAs: %.1f%% suitable",
                                 pct_in, pct_out),
                 hjust = 1.05, vjust = 1.5, size = 3, fontface = "bold",
                 color = "#1b7837") +
        labs(title = "Present Habitat Suitability with Protected Area Boundaries",
             subtitle = "Solid green = national parks/sanctuaries | Dashed green = biological corridors",
             x = NULL, y = NULL) +
        map_theme() +
        coord_sf(xlim = bbox_suit$xlim, ylim = bbox_suit$ylim, expand = FALSE, crs = st_crs(32645))
      p_map <- add_scalebar(p_map)

      # Bar chart: % suitable inside vs outside
      area_km2_in  <- round(sum(!is.na(values(suit_inside)))  * prod(res(suit)) / 1e6, 0)
      area_km2_out <- round(sum(!is.na(values(suit_outside))) * prod(res(suit)) / 1e6, 0)
      bar_df <- data.frame(
        zone = c("Inside PAs", "Outside PAs"),
        pct  = c(pct_in, pct_out),
        area = c(area_km2_in, area_km2_out)
      )
      p_bar <- ggplot(bar_df, aes(x = zone, y = pct, fill = zone)) +
        geom_col(width = 0.55, show.legend = FALSE) +
        geom_text(aes(label = sprintf("%.1f%%\n(%.0f km\u00b2)", pct, area)),
                  vjust = -0.3, size = 4.0, fontface = "bold", lineheight = 0.9) +
        scale_fill_manual(values = c("Inside PAs" = "#2d6a4f", "Outside PAs" = "#e07b39")) +
        coord_cartesian(ylim = c(0, max(bar_df$pct) * 1.25)) +
        labs(title = "% Bhutan with Suitability \u2265 Threshold",
             x = NULL, y = "% of Bhutan") +
        theme_bw() +
        theme(plot.title = element_text(face = "bold", size = 11))

      if (requireNamespace("patchwork", quietly = TRUE)) {
        combined_pa <- p_map + p_bar +
          patchwork::plot_layout(widths = c(3, 1)) +
          patchwork::plot_annotation(
            title = "Protected Area Coverage of Suitable Habitat",
            subtitle = sprintf("Threshold = %.3f | PA file: PA_Bnd_Final_20230316.shp", threshold))
        ggsave(file.path(fig_dir, "figure_E05_pa_overlay.png"), combined_pa,
               width = 14, height = 8, dpi = 300)
      } else {
        ggsave(file.path(fig_dir, "figure_E05_pa_overlay.png"), p_map,
               width = 10, height = 8, dpi = 300)
      }
      # Write summary CSV
      pa_summary <- data.frame(
        zone = c("Inside PAs", "Outside PAs", "All Bhutan"),
        pct_suitable = c(pct_in, pct_out, pct_suitable(suit, threshold)),
        threshold    = threshold
      )
      write.csv(pa_summary, file.path(fig_dir, "table_E05_pa_coverage.csv"), row.names = FALSE)
      cat("  + figure_E05_pa_overlay.png\n"); n <- n + 1
    }
  }, error = function(e) cat(sprintf("  E05 failed: %s\n", conditionMessage(e))))

  # ── E06: Multi-algorithm response curves ──────────────────────────────────
  tryCatch({
    if (!is.null(dat)) {
      cat("E06: Response curves (partial dependence)...\n")

      meta_cols <- c("id","type","response","longitude","latitude","fold",
                     "station_id","source","presence","key","row_id","X")
      pred_cols <- setdiff(names(dat), meta_cols)
      pred_cols <- pred_cols[sapply(dat[, pred_cols, drop = FALSE], is.numeric)]

      # Load models
      algo_models <- list()
      model_files <- c(glm = "model_glm.rds", rf = "model_rf.rds", brt = "model_brt.rds")
      for (nm in names(model_files)) {
        mf <- file.path(mod_dir, model_files[[nm]])
        if (file.exists(mf)) {
          algo_models[[nm]] <- tryCatch(readRDS(mf), error = function(e) NULL)
        }
      }

      # Median reference row for partial dependence
      dat_num <- dat[, pred_cols, drop = FALSE]
      ref_row  <- as.data.frame(lapply(dat_num, median, na.rm = TRUE))

      algo_colors <- c(glm = "#e41a1c", rf = "#377eb8", brt = "#4daf4a")

      pd_rows <- list()
      for (pc in pred_cols) {
        pc_range <- seq(quantile(dat[[pc]], 0.02, na.rm = TRUE),
                        quantile(dat[[pc]], 0.98, na.rm = TRUE),
                        length.out = 60)
        for (algo in names(algo_models)) {
          mdl <- algo_models[[algo]]
          if (is.null(mdl)) next
          preds <- tryCatch({
            newdat <- ref_row[rep(1, length(pc_range)), , drop = FALSE]
            newdat[[pc]] <- pc_range
            if (algo == "glm") {
              predict(mdl, newdat, type = "response")
            } else if (algo == "rf") {
              predict(mdl, data = newdat)$predictions[, 2]
            } else if (algo == "brt") {
              predict(mdl, newdat, n.trees = mdl$n.trees, type = "response")
            }
          }, error = function(e) NULL)
          if (!is.null(preds)) {
            pd_rows[[length(pd_rows) + 1]] <- data.frame(
              predictor = pc, value = pc_range, suitability = as.numeric(preds),
              algorithm = algo, stringsAsFactors = FALSE)
          }
        }
      }

      if (length(pd_rows) > 0) {
        pd_df <- do.call(rbind, pd_rows)

        # Rug data (presence locations)
        rug_df <- dat[dat$response == 1, pred_cols, drop = FALSE]
        rug_long <- do.call(rbind, lapply(pred_cols, function(pc) {
          data.frame(predictor = pc, value = rug_df[[pc]], stringsAsFactors = FALSE)
        }))

        # Predictor type labels for strip colour
        pred_type <- function(p) {
          if (grepl("^BIO", p, ignore.case = TRUE))     "Climate"
          else if (grepl("footprint", p, ignore.case = TRUE)) "Anthropogenic"
          else if (grepl("evi|ndvi|landcover|dynamicworld", p, ignore.case = TRUE)) "Vegetation"
          else if (grepl("slope|aspect|tri|elevation", p, ignore.case = TRUE)) "Topography"
          else if (grepl("dist_", p, ignore.case = TRUE)) "Distance"
          else "Other"
        }
        pd_df$pred_type <- sapply(pd_df$predictor, pred_type)

        p <- ggplot(pd_df, aes(x = value, y = suitability, color = algorithm)) +
          geom_line(linewidth = 0.8) +
          geom_rug(data = rug_long, aes(x = value), sides = "b",
                   inherit.aes = FALSE, alpha = 0.3, length = unit(0.03, "npc")) +
          scale_color_manual(values = algo_colors, name = "Algorithm") +
          facet_wrap(~ predictor, scales = "free_x", ncol = 4) +
          coord_cartesian(ylim = c(0, 1)) +
          labs(title    = "Partial Dependence Response Curves",
               subtitle = "GLM, RF, BRT | Rug = presence locations | Other predictors held at median",
               x = "Predictor Value", y = "Predicted Suitability") +
          theme_bw() +
          theme(strip.background = element_rect(fill = "#f0f0f0"),
                strip.text       = element_text(size = 8, face = "bold"),
                axis.text        = element_text(size = 7),
                legend.position  = "bottom",
                plot.title       = element_text(face = "bold", size = 13))

        ncol_f <- 4
        nrow_f <- ceiling(length(pred_cols) / ncol_f)
        ggsave(file.path(fig_dir, "figure_E06_response_curves.png"), p,
               width = 5 * ncol_f, height = 4 * nrow_f, dpi = 300, limitsize = FALSE)
        cat("  + figure_E06_response_curves.png\n"); n <- n + 1
      }
    }
  }, error = function(e) cat(sprintf("  E06 failed: %s\n", conditionMessage(e))))

  # ── E07: Variable importance ───────────────────────────────────────────────
  tryCatch({
    if (!is.null(dat)) {
      cat("E07: Variable importance...\n")

      meta_cols <- c("id","type","response","longitude","latitude","fold",
                     "station_id","source","presence","key","row_id","X")
      pred_cols <- setdiff(names(dat), meta_cols)
      pred_cols <- pred_cols[sapply(dat[, pred_cols, drop = FALSE], is.numeric)]

      imp_list <- list()

      # GLM: absolute standardized coefficients
      glm_file <- file.path(mod_dir, "model_glm.rds")
      if (file.exists(glm_file)) {
        tryCatch({
          mdl_glm <- readRDS(glm_file)
          co <- coef(mdl_glm)
          co <- co[names(co) != "(Intercept)"]
          # Standardize by predictor SD
          sds <- sapply(names(co), function(v) {
            if (v %in% names(dat)) sd(dat[[v]], na.rm = TRUE) else 1
          })
          std_co <- abs(co * sds)
          imp_list[["GLM"]] <- data.frame(
            predictor  = names(std_co),
            importance = as.numeric(std_co / max(std_co, na.rm = TRUE) * 100),
            algorithm  = "GLM",
            stringsAsFactors = FALSE)
        }, error = function(e) cat(sprintf("  GLM importance failed: %s\n", conditionMessage(e))))
      }

      # RF: permutation importance (ranger)
      rf_file <- file.path(mod_dir, "model_rf.rds")
      if (file.exists(rf_file)) {
        tryCatch({
          mdl_rf <- readRDS(rf_file)
          vi <- mdl_rf$variable.importance
          if (!is.null(vi)) {
            imp_list[["RF"]] <- data.frame(
              predictor  = names(vi),
              importance = as.numeric(vi / max(vi, na.rm = TRUE) * 100),
              algorithm  = "RF",
              stringsAsFactors = FALSE)
          }
        }, error = function(e) cat(sprintf("  RF importance failed: %s\n", conditionMessage(e))))
      }

      # BRT: relative influence (gbm) — saved as list(model=..., best_iter=...)
      brt_file <- file.path(mod_dir, "model_brt.rds")
      if (file.exists(brt_file)) {
        tryCatch({
          if (!requireNamespace("gbm", quietly = TRUE)) library(gbm)
          brt_saved <- readRDS(brt_file)
          mdl_brt   <- if (is.list(brt_saved) && !is.null(brt_saved$model)) brt_saved$model else brt_saved
          best_iter <- if (is.list(brt_saved) && !is.null(brt_saved$best_iter)) brt_saved$best_iter else mdl_brt$n.trees
          # Try summary() first (most portable), then relative.influence
          ri_df <- tryCatch(
            suppressMessages(summary(mdl_brt, plotit = FALSE)),
            error = function(e) NULL)
          if (!is.null(ri_df) && all(c("var","rel.inf") %in% names(ri_df))) {
            imp_list[["BRT"]] <- data.frame(
              predictor  = ri_df$var,
              importance = as.numeric(ri_df$rel.inf / max(ri_df$rel.inf, na.rm = TRUE) * 100),
              algorithm  = "BRT",
              stringsAsFactors = FALSE)
          } else {
            ri <- tryCatch(gbm::relative.influence(mdl_brt, n.trees = best_iter),
                           error = function(e) NULL)
            if (!is.null(ri)) {
              imp_list[["BRT"]] <- data.frame(
                predictor  = names(ri),
                importance = as.numeric(ri / max(ri, na.rm = TRUE) * 100),
                algorithm  = "BRT",
                stringsAsFactors = FALSE)
            }
          }
        }, error = function(e) cat(sprintf("  BRT importance failed: %s\n", conditionMessage(e))))
      }

      # MaxEnt: percent contribution from maxentResults.csv (full/average model row)
      mx_results <- file.path(mod_dir, "maxent_output", "maxentResults.csv")
      if (file.exists(mx_results)) {
        tryCatch({
          mx_df <- read.csv(mx_results, stringsAsFactors = FALSE)
          # Use last row (full/average model or last CV replicate)
          mx_row <- mx_df[nrow(mx_df), , drop = FALSE]
          contrib_cols <- grep("contribution$", names(mx_row), value = TRUE)
          if (length(contrib_cols) > 0) {
            mx_preds <- sub(" contribution$", "", contrib_cols)
            mx_vals  <- as.numeric(mx_row[contrib_cols])
            mx_max   <- max(mx_vals, na.rm = TRUE)
            if (!is.na(mx_max) && mx_max > 0) {
              imp_list[["MaxEnt"]] <- data.frame(
                predictor  = mx_preds,
                importance = mx_vals / mx_max * 100,
                algorithm  = "MaxEnt",
                stringsAsFactors = FALSE)
            }
          }
        }, error = function(e) cat(sprintf("  MaxEnt importance failed: %s\n", conditionMessage(e))))
      }

      cat(sprintf("  Algorithms with importance data: %s\n", paste(names(imp_list), collapse = ", ")))
      if (length(imp_list) >= 1) {
        imp_df <- do.call(rbind, imp_list)

        # Predictor type coloring
        pred_type <- function(p) {
          if (grepl("^BIO", p, ignore.case = TRUE))              "Climate (BIO)"
          else if (grepl("footprint", p, ignore.case = TRUE))    "Anthropogenic"
          else if (grepl("evi|ndvi|landcover|dynamicworld", p, ignore.case = TRUE)) "Vegetation"
          else if (grepl("slope|aspect|tri|elevation", p, ignore.case = TRUE)) "Topography"
          else if (grepl("dist_", p, ignore.case = TRUE))        "Distance"
          else "Other"
        }
        imp_df$pred_type <- sapply(imp_df$predictor, pred_type)

        type_colors <- c(
          "Climate (BIO)"  = "#d7301f",
          "Anthropogenic"  = "#8c2d04",
          "Vegetation"     = "#41ab5d",
          "Topography"     = "#8c6d31",
          "Distance"       = "#2171b5",
          "Other"          = "grey60"
        )

        # Order predictors by mean importance
        mean_imp <- tapply(imp_df$importance, imp_df$predictor, mean, na.rm = TRUE)
        imp_df$predictor <- factor(imp_df$predictor,
                                   levels = names(sort(mean_imp, decreasing = FALSE)))

        p <- ggplot(imp_df, aes(x = predictor, y = importance, fill = pred_type)) +
          geom_col(width = 0.65) +
          geom_text(aes(label = sprintf("%.0f", importance)),
                    hjust = -0.1, size = 2.5) +
          scale_fill_manual(values = type_colors, name = "Predictor Type") +
          coord_flip(ylim = c(0, 115)) +
          facet_wrap(~ algorithm, ncol = length(unique(imp_df$algorithm)), scales = "free_x") +
          labs(title    = "Variable Importance by Algorithm",
               subtitle = "Relative importance normalized to 100 | Colour = predictor type",
               x = NULL, y = "Relative Importance (%)") +
          theme_bw() +
          theme(strip.background = element_rect(fill = "#f0f0f0"),
                strip.text       = element_text(face = "bold", size = 10),
                axis.text.y      = element_text(size = 8),
                legend.position  = "bottom",
                plot.title       = element_text(face = "bold", size = 13))

        ggsave(file.path(fig_dir, "figure_E07_variable_importance.png"), p,
               width = 5 * length(unique(imp_df$algorithm)) + 2, height = 8,
               dpi = 300, limitsize = FALSE)

        write.csv(imp_df, file.path(fig_dir, "table_E07_variable_importance.csv"), row.names = FALSE)
        cat("  + figure_E07_variable_importance.png\n"); n <- n + 1
      }
    }
  }, error = function(e) cat(sprintf("  E07 failed: %s\n", conditionMessage(e))))

  # ── E08: GCM-level habitat area trajectories (Fig8 style) ─────────────────
  tryCatch({
    fut_files_gcm <- list.files(fut_dir, pattern = "^suitability_future_.*\\.tif$", full.names = TRUE)
    fut_files_gcm <- fut_files_gcm[!grepl("ensemble|novelty", fut_files_gcm, ignore.case = TRUE)]

    gcm_labels <- c(
      "acces_cm2"    = "ACCESS-CM2",  "access_cm2"    = "ACCESS-CM2",
      "cnrm_cm6_1"   = "CNRM-CM6-1", "cnrm_esm2_1"   = "CNRM-ESM2-1",
      "inm_cm4_8"    = "INM-CM4-8",  "inm_cm5_0"     = "INM-CM5-0",
      "miroc6"       = "MIROC6",      "mpi_esm1_2_lr" = "MPI-ESM1-2-LR",
      "mri_esm2_0"   = "MRI-ESM2-0", "miroc_es2l"    = "MIROC-ES2L"
    )
    gcm_colors <- c(
      "ACCESS-CM2"    = "#e41a1c", "CNRM-CM6-1"    = "#377eb8",
      "CNRM-ESM2-1"  = "#4daf4a", "INM-CM4-8"     = "#984ea3",
      "INM-CM5-0"    = "#ff7f00", "MIROC6"        = "#a65628",
      "MPI-ESM1-2-LR"= "#f781bf", "MRI-ESM2-0"    = "#999999",
      "MIROC-ES2L"   = "#66c2a5"
    )

    parse_gcm_file <- function(f) {
      b    <- tools::file_path_sans_ext(basename(f))
      b    <- sub("^suitability_future_", "", b)
      ssp_pos <- regexpr("_ssp[0-9]+_", b)
      if (ssp_pos < 0) return(NULL)
      gcm_slug <- substring(b, 1, as.integer(ssp_pos) - 1)
      rest     <- substring(b, as.integer(ssp_pos) + 1)
      ssp_m    <- regmatches(rest, regexpr("ssp[0-9]+", rest, ignore.case = TRUE))
      years    <- regmatches(rest, gregexpr("[0-9]{4}", rest))[[1]]
      algo_m   <- regmatches(rest, regexpr("(glm|rf|brt|maxent|maxnet)$", rest, ignore.case = TRUE))
      if (length(ssp_m) == 0 || length(algo_m) == 0 || length(years) < 2) return(NULL)
      data.frame(path = f, gcm = gcm_slug, ssp = toupper(ssp_m),
                 period = paste(years[1], years[2], sep = "-"),
                 algo = as.character(algo_m), stringsAsFactors = FALSE)
    }

    if (length(fut_files_gcm) > 0) {
      cat("E08: GCM-level habitat area trajectories...\n")
      idx_gcm  <- do.call(rbind, lapply(fut_files_gcm, parse_gcm_file))
      idx_gcm  <- idx_gcm[!is.null(idx_gcm), ]

      if (!is.null(idx_gcm) && nrow(idx_gcm) > 0) {
        # Average across algorithms for each GCM × SSP × period
        groups  <- unique(idx_gcm[, c("gcm","ssp","period")])
        traj_rows <- list()
        for (i in seq_len(nrow(groups))) {
          g      <- groups[i, ]
          subset <- idx_gcm[idx_gcm$gcm == g$gcm & idx_gcm$ssp == g$ssp &
                              idx_gcm$period == g$period, , drop = FALSE]
          if (nrow(subset) == 0) next
          rasts <- lapply(subset$path, function(p) tryCatch(rast(p), error = function(e) NULL))
          rasts <- Filter(Negate(is.null), rasts)
          if (length(rasts) == 0) next
          r_mean <- if (length(rasts) > 1) {
            tryCatch(app(rast(rasts), function(v) mean(v, na.rm = TRUE)), error = function(e) rasts[[1]])
          } else rasts[[1]]
          pct <- pct_suitable(r_mean, threshold)
          gcm_lab <- gcm_labels[g$gcm]
          if (is.na(gcm_lab)) gcm_lab <- toupper(g$gcm)
          traj_rows[[length(traj_rows)+1]] <- data.frame(
            gcm = gcm_lab, ssp = g$ssp, period = g$period,
            pct_suitable = pct, stringsAsFactors = FALSE)
          rm(rasts, r_mean); gc(verbose = FALSE)
        }
        traj_gcm <- do.call(rbind, traj_rows)

        pres_ens  <- file.path(pres_dir, "suitability_present_ensemble.tif")
        pres_pct  <- if (file.exists(pres_ens)) pct_suitable(rast(pres_ens), threshold) else NA
        period_order <- sort(unique(traj_gcm$period))
        traj_gcm$period <- factor(traj_gcm$period, levels = period_order)

        # Save data table
        write.csv(traj_gcm, file.path(fig_dir, "table_gcm_trajectories.csv"), row.names = FALSE)

        # ── Panel a: faceted trajectory — all available SSPs ──────────────────
        ssp_display <- c("SSP126" = "SSP1-2.6", "SSP245" = "SSP2-4.5",
                         "SSP370" = "SSP3-7.0", "SSP585" = "SSP5-8.5")
        ssps_avail  <- unique(traj_gcm$ssp)
        ssp_traj_df <- traj_gcm[traj_gcm$ssp %in% names(ssp_display), ]
        ssp_traj_df$ssp_label <- factor(
          ssp_display[ssp_traj_df$ssp],
          levels = ssp_display[names(ssp_display) %in% ssps_avail])

        gcms_present <- unique(ssp_traj_df$gcm)
        cols_avail   <- gcm_colors[names(gcm_colors) %in% gcms_present]
        y_lab <- sprintf("Area with suitability \u2265 %.2f (%% of Bhutan)", threshold)

        # Per-facet "Present: X.X%" annotation just above the dashed line
        ssp_label_levels <- levels(ssp_traj_df$ssp_label)
        annot_df <- data.frame(
          ssp_label = factor(ssp_label_levels, levels = ssp_label_levels),
          x_pos = levels(ssp_traj_df$period)[1],
          y_pos = if (!is.na(pres_pct)) pres_pct else 0,
          label = if (!is.na(pres_pct)) sprintf("Present: %.1f%%", pres_pct) else "",
          stringsAsFactors = FALSE)

        p_left <- ggplot(ssp_traj_df,
                         aes(x = period, y = pct_suitable, color = gcm, group = gcm)) +
          geom_line(linewidth = 0.95) +
          geom_point(size = 3) +
          facet_wrap(~ ssp_label, ncol = length(unique(ssp_traj_df$ssp_label)), scales = "free_y") +
          scale_color_manual(values = cols_avail, name = "GCM") +
          labs(x = "Time period", y = y_lab) +
          theme_bw() +
          theme(
            strip.background  = element_rect(fill = "#1f3d5c"),
            strip.text        = element_text(face = "bold", color = "white", size = 11),
            legend.position   = "right",
            legend.text       = element_text(size = 8.5),
            legend.title      = element_text(face = "bold", size = 9),
            legend.key.size   = unit(0.45, "cm"),
            axis.text.x       = element_text(angle = 20, hjust = 1),
            panel.grid.minor  = element_blank(),
            panel.grid.major  = element_line(colour = "grey92")
          )
        if (!is.na(pres_pct)) {
          p_left <- p_left +
            geom_hline(yintercept = pres_pct, linetype = "dashed",
                       color = "grey35", linewidth = 0.85) +
            geom_text(data = annot_df,
                      aes(x = x_pos, y = y_pos, label = label,
                          color = NULL, group = NULL),
                      hjust = -0.05, vjust = -0.55, size = 3, color = "grey35",
                      inherit.aes = FALSE)
        }

        # ── Panel b: end-of-century boxplot (all available SSPs) ────────────
        box_df <- traj_gcm[traj_gcm$period == tail(levels(traj_gcm$period), 1) &
                              traj_gcm$ssp %in% names(ssp_display), ]
        box_df$ssp_label <- factor(ssp_display[box_df$ssp],
                                   levels = ssp_display[names(ssp_display) %in% ssps_avail])

        ssp_fill_colors <- c("SSP1-2.6" = "#1a9641", "SSP2-4.5" = "#74c476",
                             "SSP3-7.0" = "#fdae61", "SSP5-8.5" = "#fb6a4a")
        box_fills_avail <- ssp_fill_colors[levels(box_df$ssp_label)]

        p_box <- ggplot(box_df, aes(x = ssp_label, y = pct_suitable, fill = ssp_label)) +
          geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.75, linewidth = 0.6) +
          geom_jitter(aes(color = gcm), width = 0.12, size = 3.5, shape = 21,
                      fill = NA, stroke = 1.4) +
          scale_fill_manual(values = box_fills_avail, name = NULL) +
          scale_color_manual(values = cols_avail, name = "GCM",
                             guide = guide_legend(override.aes = list(size = 3.5,
                                                                      stroke = 1.4))) +
          labs(title = paste0(tail(levels(traj_gcm$period), 1), " distribution"),
               x = NULL, y = "% of Bhutan") +
          theme_bw() +
          theme(
            legend.position  = "right",
            legend.text      = element_text(size = 8),
            legend.title     = element_text(face = "bold", size = 9),
            plot.title       = element_text(face = "bold", size = 11),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(colour = "grey92")
          )
        if (!is.na(pres_pct))
          p_box <- p_box + geom_hline(yintercept = pres_pct, linetype = "dashed",
                                      color = "grey35", linewidth = 0.85)

        if (requireNamespace("patchwork", quietly = TRUE)) {
          # Use ggtext for italic species name if available, else plain text
          title_str <- if (requireNamespace("ggtext", quietly = TRUE))
            "*Elephas maximus* Suitable Habitat Projections \u2014 Bhutan"
          else
            "Elephas maximus Suitable Habitat Projections \u2014 Bhutan"

          fig_e08 <- (p_left | p_box) +
            patchwork::plot_layout(widths = c(2.3, 1)) +
            patchwork::plot_annotation(
              title    = title_str,
              subtitle = sprintf("Suitable habitat defined as suitability \u2265 %.2f | Ensemble mean (GLM + RF + BRT + MaxEnt) | 8 CMIP6 GCMs",
                                 threshold),
              caption  = "Dashed line = present-day baseline",
              tag_levels = "a"
            )
          n_ssp_panels <- length(unique(ssp_traj_df$ssp_label))
          ggsave(file.path(fig_dir, "figure_gcm_trajectories.png"), fig_e08,
                 width = 10 + 4 * n_ssp_panels, height = 7, dpi = 300, limitsize = FALSE)
          cat("  + figure_gcm_trajectories.png\n"); n <- n + 1
        }
      }
    }
  }, error = function(e) cat(sprintf("  E08 failed: %s\n", conditionMessage(e))))

  # ── E09: GCM reliability ranking (Fig9 style) ──────────────────────────────
  tryCatch({
    unc_dir    <- file.path(run_dir, "06_uncertainty")
    fut_files_g9 <- list.files(fut_dir, pattern = "^suitability_future_.*\\.tif$", full.names = TRUE)
    fut_files_g9 <- fut_files_g9[!grepl("ensemble|novelty", fut_files_g9, ignore.case = TRUE)]
    ens_file_g9  <- file.path(fut_dir, "future_gcm_ensemble_ssp585_2071_2100.tif")

    gcm_labels_e9 <- c(
      "acces_cm2"    = "ACCESS-CM2",  "access_cm2"    = "ACCESS-CM2",
      "cnrm_cm6_1"   = "CNRM-CM6-1", "cnrm_esm2_1"   = "CNRM-ESM2-1",
      "inm_cm4_8"    = "INM-CM4-8",  "inm_cm5_0"     = "INM-CM5-0",
      "miroc6"       = "MIROC6",      "mpi_esm1_2_lr" = "MPI-ESM1-2-LR",
      "mri_esm2_0"   = "MRI-ESM2-0", "miroc_es2l"    = "MIROC-ES2L"
    )

    if (length(fut_files_g9) > 0 && dir.exists(unc_dir)) {
      cat("E09: GCM reliability ranking...\n")

      # Re-use parse_gcm_file from E08 (defined in same scope)
      parse_g9 <- function(f) {
        b    <- tools::file_path_sans_ext(basename(f))
        b    <- sub("^suitability_future_", "", b)
        ssp_pos <- regexpr("_ssp[0-9]+_", b)
        if (ssp_pos < 0) return(NULL)
        gcm_slug <- substring(b, 1, as.integer(ssp_pos) - 1)
        rest     <- substring(b, as.integer(ssp_pos) + 1)
        ssp_m    <- regmatches(rest, regexpr("ssp[0-9]+", rest, ignore.case = TRUE))
        years    <- regmatches(rest, gregexpr("[0-9]{4}", rest))[[1]]
        algo_m   <- regmatches(rest, regexpr("(glm|rf|brt|maxent|maxnet)$", rest, ignore.case = TRUE))
        if (length(ssp_m) == 0 || length(algo_m) == 0 || length(years) < 2) return(NULL)
        data.frame(path = f, gcm = gcm_slug, ssp = toupper(ssp_m),
                   period = paste(years[1], years[2], sep = "-"),
                   algo = as.character(algo_m), stringsAsFactors = FALSE)
      }
      idx_g9 <- do.call(rbind, lapply(fut_files_g9, parse_g9))
      idx_g9 <- idx_g9[!is.null(idx_g9), ]

      # Focus on SSP585 2071–2100 (most informative for reliability)
      idx_target <- idx_g9[idx_g9$ssp == "SSP585" &
                              grepl("2071", idx_g9$period), , drop = FALSE]
      gcms_avail <- unique(idx_target$gcm)

      if (length(gcms_avail) >= 2 && file.exists(ens_file_g9)) {
        ens_rast <- rast(ens_file_g9)
        ens_vals <- values(ens_rast, na.rm = FALSE)

        reliability <- lapply(gcms_avail, function(g) {
          subset <- idx_target[idx_target$gcm == g, , drop = FALSE]
          rasts  <- lapply(subset$path, function(p) tryCatch(rast(p), error = function(e) NULL))
          rasts  <- Filter(Negate(is.null), rasts)
          if (length(rasts) == 0) return(NULL)

          # Algorithm disagreement: SD across algorithms for this GCM
          if (length(rasts) > 1) {
            stk <- tryCatch(rast(rasts), error = function(e) NULL)
            if (is.null(stk)) return(NULL)
            sd_rast <- app(stk, function(v) sd(v, na.rm = TRUE))
            algo_sd <- mean(values(sd_rast), na.rm = TRUE)
          } else {
            algo_sd <- 0
          }

          # GCM deviation from ensemble (proxy for extrapolation / bias)
          r_gcm_mean <- if (length(rasts) > 1) {
            tryCatch(app(rast(rasts), function(v) mean(v, na.rm = TRUE)), error = function(e) rasts[[1]])
          } else rasts[[1]]
          r_gcm_mean <- tryCatch(resample(r_gcm_mean, ens_rast, method = "bilinear"),
                                 error = function(e) r_gcm_mean)
          gcm_vals <- values(r_gcm_mean, na.rm = FALSE)
          valid    <- !is.na(gcm_vals) & !is.na(ens_vals)
          gcm_dev  <- if (sum(valid) > 10) mean(abs(gcm_vals[valid] - ens_vals[valid])) else NA_real_

          gcm_lab <- gcm_labels_e9[g]
          if (is.na(gcm_lab)) gcm_lab <- toupper(g)
          rm(rasts, r_gcm_mean); gc(verbose = FALSE)
          data.frame(gcm = gcm_lab, algo_sd = algo_sd, gcm_dev = gcm_dev,
                     stringsAsFactors = FALSE)
        })
        rel_df <- do.call(rbind, Filter(Negate(is.null), reliability))

        if (!is.null(rel_df) && nrow(rel_df) >= 2) {
          # Normalize 0-1 (lower = more reliable)
          norm01 <- function(x) { r <- range(x, na.rm=TRUE); if (diff(r)==0) rep(0.5,length(x)) else (x-r[1])/diff(r) }
          rel_df$algo_sd_norm <- norm01(rel_df$algo_sd)
          rel_df$gcm_dev_norm <- norm01(replace(rel_df$gcm_dev, is.na(rel_df$gcm_dev), mean(rel_df$gcm_dev, na.rm=TRUE)))
          rel_df$composite    <- 1 - (rel_df$algo_sd_norm + rel_df$gcm_dev_norm) / 2
          rel_df               <- rel_df[order(rel_df$composite, decreasing = TRUE), ]
          rel_df$rank          <- seq_len(nrow(rel_df))
          rel_df$gcm           <- factor(rel_df$gcm, levels = rev(rel_df$gcm))

          # Save table
          write.csv(rel_df[, c("rank","gcm","algo_sd","gcm_dev","composite")],
                    file.path(fig_dir, "table_gcm_reliability.csv"), row.names = FALSE)

          rank_colors <- colorRampPalette(c("#1a9641","#a6d96a","#fdae61","#d7191c"))(nrow(rel_df))
          names(rank_colors) <- levels(rel_df$gcm)

          p_rank <- ggplot(rel_df, aes(x = composite, y = gcm, color = gcm)) +
            geom_segment(aes(x = 0, xend = composite, yend = gcm), linewidth = 1.0) +
            geom_point(size = 5) +
            geom_text(aes(label = sprintf("#%d  %.3f", rank, composite)),
                      hjust = -0.15, size = 3.5, fontface = "bold") +
            scale_color_manual(values = rank_colors, guide = "none") +
            scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                               limits = c(0, 1.25)) +
            labs(title = "a  GCM Reliability Ranking",
                 subtitle = "Composite = 1 \u2212 (norm.AlgoSD + norm.Deviation) / 2\nGreen = most reliable",
                 x = "Composite reliability score", y = NULL) +
            theme_bw() +
            theme(axis.text.y    = element_text(size = 10, face = "bold"),
                  plot.title     = element_text(face = "bold", size = 11),
                  plot.subtitle  = element_text(size = 8, color = "grey40"),
                  panel.grid.minor = element_blank())

          p_dev <- ggplot(rel_df, aes(x = gcm_dev, y = gcm, fill = gcm)) +
            geom_col(show.legend = FALSE) +
            geom_text(aes(label = sprintf("%.3f", round(gcm_dev, 3))),
                      hjust = -0.1, size = 3.2) +
            scale_fill_manual(values = rank_colors) +
            scale_x_continuous(limits = c(0, max(rel_df$gcm_dev, na.rm = TRUE) * 1.3)) +
            labs(title = "b  GCM vs Ensemble Deviation",
                 subtitle = "Mean |GCM \u2212 All-GCM ensemble|\nat SSP5-8.5 2071\u20132100",
                 x = "Mean absolute deviation", y = NULL) +
            theme_bw() +
            theme(axis.text.y    = element_blank(),
                  axis.ticks.y   = element_blank(),
                  plot.title     = element_text(face = "bold", size = 11),
                  plot.subtitle  = element_text(size = 8, color = "grey40"),
                  panel.grid.minor = element_blank())

          p_alg <- ggplot(rel_df, aes(x = algo_sd, y = gcm, fill = gcm)) +
            geom_col(show.legend = FALSE) +
            geom_text(aes(label = sprintf("%.4f", round(algo_sd, 4))),
                      hjust = -0.1, size = 3.2) +
            scale_fill_manual(values = rank_colors) +
            scale_x_continuous(limits = c(0, max(rel_df$algo_sd, na.rm = TRUE) * 1.35)) +
            labs(title = "c  Algorithm Disagreement",
                 subtitle = "Mean SD across GLM/RF/BRT/MaxEnt\nat SSP5-8.5 2071\u20132100",
                 x = "Mean SD (algorithm spread)", y = NULL) +
            theme_bw() +
            theme(axis.text.y    = element_blank(),
                  axis.ticks.y   = element_blank(),
                  plot.title     = element_text(face = "bold", size = 11),
                  plot.subtitle  = element_text(size = 8, color = "grey40"),
                  panel.grid.minor = element_blank())

          if (requireNamespace("patchwork", quietly = TRUE)) {
            fig_e09 <- (p_rank | p_dev | p_alg) +
              patchwork::plot_annotation(
                title    = "CMIP6 GCM Reliability Assessment | Elephas maximus SDM | Bhutan",
                subtitle = "Ranking based on algorithm consensus and GCM deviation from ensemble at 2071\u20132100")
            ggsave(file.path(fig_dir, "figure_gcm_reliability.png"), fig_e09,
                   width = 16, height = 6, dpi = 300)
            cat("  + figure_gcm_reliability.png\n"); n <- n + 1
          }
        }
      }
    }
  }, error = function(e) cat(sprintf("  E09 failed: %s\n", conditionMessage(e))))

  # ── E12: GCM Uncertainty Maps — all 4 SSPs × all 3 periods (2×2 per period) ─
  tryCatch({
    unc_dir <- file.path(run_dir, "06_uncertainty")

    if (dir.exists(unc_dir)) {
      cat("E12: GCM uncertainty maps (all SSPs \u00d7 all periods)...\n")

      # Colour scale matching reference: light yellow (low SD) → orange → dark purple (high SD)
      unc_scale <- function(max_sd = 0.5)
        scale_fill_viridis_c(option = "inferno", direction = -1,
                             limits = c(0, max_sd), oob = scales::squish,
                             na.value = "grey88", name = "GCM SD")

      # Period start-year → period label mapping (filenames use start year only)
      yr_to_period <- c("2021" = "2021_2050", "2051" = "2051_2080", "2071" = "2071_2100")

      for (period in periods_all) {
        yr        <- substring(period, 1, 4)      # "2021", "2051", or "2071"
        period_lb <- period_disp_all[period]
        panels    <- list()

        # Find global SD max across all SSPs for this period (consistent scale)
        sd_files_period <- list.files(unc_dir,
          pattern = sprintf("^gcm_sd_ssp.*_%s\\.tif$", yr), full.names = TRUE)
        sd_max <- if (length(sd_files_period) > 0) {
          vals <- unlist(lapply(sd_files_period, function(f) {
            r <- tryCatch(rast(f), error = function(e) NULL)
            if (is.null(r)) return(NULL)
            quantile(values(r, na.rm = TRUE), 0.99, na.rm = TRUE)
          }))
          min(ceiling(max(vals, na.rm = TRUE) * 10) / 10, 0.5)  # round up to nearest 0.1, cap at 0.5
        } else 0.5

        for (ssp in ssp_order_all) {
          f_match <- file.path(unc_dir, sprintf("gcm_sd_%s_%s.tif", ssp, yr))
          if (!file.exists(f_match))
            f_match <- list.files(unc_dir,
              pattern = sprintf("gcm_sd_%s.*%s", ssp, yr), full.names = TRUE)[1]
          if (is.na(f_match) || !file.exists(f_match)) next

          r <- tryCatch(rast(f_match), error = function(e) NULL)
          if (is.null(r)) next
          df   <- rast_to_df(r, aoi_v)
          bbox <- data_bbox(r)
          mean_sd <- round(mean(df$value, na.rm = TRUE), 3)

          p <- ggplot() +
            geom_raster(data = df, aes(x = x, y = y, fill = value)) +
            aoi_layer + unc_scale(sd_max) +
            annotate("text", x = Inf, y = -Inf,
                     label = sprintf("Mean SD = %.3f", mean_sd),
                     hjust = 1.05, vjust = -0.4, size = 2.8, fontface = "bold",
                     color = "grey20") +
            labs(title = sprintf("%s | %s", ssp_labels_all[ssp], period_lb),
                 x = NULL, y = NULL) +
            map_theme() +
            coord_sf(xlim = bbox$xlim, ylim = bbox$ylim, expand = FALSE, crs = st_crs(32645))
          panels[[ssp]] <- add_scalebar(p)
          rm(r, df); gc(verbose = FALSE)
        }

        fig <- make_2x2_figure(panels,
          title_str    = sprintf("GCM Ensemble Uncertainty in Projected Habitat Suitability \u2014 %s | Bhutan", period_lb),
          subtitle_str = "Standard deviation across 8 CMIP6 GCMs | Ensemble mean (GLM + RF + BRT + MaxEnt)")
        if (!is.null(fig)) {
          fname <- sprintf("figure_uncertainty_%s.png", period)
          ggsave(file.path(fig_dir, fname), fig, width = 18, height = 14, dpi = 300, limitsize = FALSE)
          cat(sprintf("  + %s\n", fname)); n <- n + 1
        }
      }
    }
  }, error = function(e) cat(sprintf("  E12 failed: %s\n", conditionMessage(e))))

  # ── E10: Human–Elephant Conflict Risk (present + all future scenarios) ─────
  tryCatch({
    int_dir   <- file.path(run_dir, "02_data_intermediate")
    fp_tif    <- file.path(int_dir, "human_footprint_harmonized.tif")
    bldg_gpkg <- file.path(int_dir, "building_footprints_harmonized.gpkg")
    pres_ens  <- file.path(pres_dir, "suitability_present_ensemble.tif")

    if (file.exists(fp_tif) && file.exists(pres_ens)) {
      cat("E10: Human-elephant conflict risk maps...\n")

      suit_pres <- rast(pres_ens)

      # Robust normalisation (clips outliers at 2nd/98th percentile)
      norm_rast <- function(r) {
        v  <- values(r, na.rm = TRUE)
        lo <- quantile(v, 0.02); hi <- quantile(v, 0.98)
        if (hi <= lo) return(r * 0 + 0.5)
        clamp((r - lo) / (hi - lo), 0, 1, values = TRUE)
      }

      # ── Human pressure layer ──────────────────────────────────────────────
      fp_rast <- rast(fp_tif)
      fp_rast <- tryCatch(resample(fp_rast, suit_pres, method = "bilinear"),
                          error = function(e) fp_rast)
      fp_norm <- norm_rast(fp_rast); rm(fp_rast)

      if (file.exists(bldg_gpkg)) {
        cat("  Building settlement proximity raster...\n")
        bldg_v    <- tryCatch(vect(bldg_gpkg), error = function(e) NULL)
        if (!is.null(bldg_v)) {
          # Rasterize footprints → presence layer, then distance to nearest cell
          bldg_pres <- tryCatch(
            rasterize(bldg_v, suit_pres, field = 1, background = NA),
            error = function(e) NULL)
          if (!is.null(bldg_pres)) {
            dist_r      <- tryCatch(distance(bldg_pres), error = function(e) NULL)
            if (!is.null(dist_r)) {
              # Convert to proximity: 1 / (1 + dist_km)
              prox <- 1 / (1 + dist_r / 1000)
              human_pressure <- norm_rast(fp_norm + norm_rast(prox))
              rm(dist_r, prox)
            } else { human_pressure <- fp_norm }
            rm(bldg_pres)
          } else { human_pressure <- fp_norm }
          rm(bldg_v); gc(verbose = FALSE)
        } else { human_pressure <- fp_norm }
      } else { human_pressure <- fp_norm }

      # ── Conflict risk from a suitability raster ───────────────────────────
      make_conflict <- function(suit_r) {
        s  <- tryCatch(resample(suit_r, human_pressure, method = "bilinear"),
                       error = function(e) suit_r)
        norm_rast(norm_rast(s) * human_pressure)
      }

      # Colour scales
      risk_scale <- function()
        scale_fill_gradientn(
          colours  = c("#ffffcc","#fecc5c","#fd8d3c","#e31a1c","#800026"),
          limits   = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1),
          labels   = c("Low","","Moderate","","Very High"),
          na.value = "grey88", name = "Conflict\nRisk")

      delta_scale <- function()
        scale_fill_gradientn(
          colours  = c("#053061","#2166ac","#67a9cf","#f7f7f7","#ef8a62","#ca0020","#67000d"),
          limits   = c(-0.5, 0.5), oob = scales::squish,
          na.value = "grey88", name = "\u0394 Risk")

      risk_plot <- function(risk_r, title_str, show_dzong = FALSE, show_hwc = FALSE) {
        df   <- rast_to_df(risk_r, aoi_v)
        bbox <- data_bbox(risk_r)
        p <- ggplot() +
          geom_raster(data = df, aes(x = x, y = y, fill = value)) +
          { if (show_dzong && !is.null(dzong_layer_utm)) dzong_layer_utm } +
          { if (show_dzong && !is.null(dzong_label_layer_utm)) dzong_label_layer_utm } +
          aoi_layer + risk_scale() +
          labs(title = title_str, x = NULL, y = NULL) +
          map_theme() +
          coord_sf(xlim = bbox$xlim, ylim = bbox$ylim, expand = FALSE,
                   crs = st_crs(32645))
        if (show_hwc && !is.null(hwc_sf)) {
          hwc_col <- c("Crop Damage"     = "#f0e442",
                       "Property Damage" = "#e69f00",
                       "Human Casualty"  = "#cc0000",
                       "Other"           = "#999999")
          hwc_shp <- c("Crop Damage"     = 21L,
                       "Property Damage" = 24L,
                       "Human Casualty"  = 23L,
                       "Other"           = 22L)
          p <- p +
            geom_sf(data = hwc_sf, aes(colour = conflict_type, shape = conflict_type),
                    inherit.aes = FALSE, size = 2.2, stroke = 0.7, fill = NA, alpha = 0.85) +
            scale_colour_manual(values = hwc_col, name = "HWC incident",
                                guide = guide_legend(override.aes = list(size = 3))) +
            scale_shape_manual(values = hwc_shp, name = "HWC incident")
        }
        add_scalebar(p)
      }

      # ── Present ───────────────────────────────────────────────────────────
      risk_pres <- make_conflict(suit_pres)
      p_pres <- risk_plot(risk_pres,
                          "Present-day human-elephant conflict risk \u2014 Elephas maximus (Bhutan)",
                          show_dzong = TRUE, show_hwc = TRUE)
      save_fig(p_pres, file.path(fig_dir, "figure_conflict_present.png"), 10, 9)
      n <- n + 1



      # ── Future conflict risk + change: all 4 SSPs × all 3 periods ───────────
      all_ens <- list.files(fut_dir, pattern = "^future_gcm_ensemble_.*[.]tif$", full.names = TRUE)
      sum_rows <- list()
      bbox_pres <- data_bbox(risk_pres)   # consistent extent across all change maps

      for (period in periods_all) {
        yrs       <- strsplit(period, "_")[[1]]
        period_lb <- period_disp_all[period]
        fut_panels <- list()
        chg_panels <- list()

        for (ssp in ssp_order_all) {
          f_match <- all_ens[grepl(ssp, all_ens, ignore.case = TRUE) &
                               grepl(yrs[1], all_ens) & grepl(yrs[2], all_ens)]
          if (length(f_match) == 0) next
          suit_f <- tryCatch(rast(f_match[1]), error = function(e) NULL)
          if (is.null(suit_f)) next
          risk_f <- make_conflict(suit_f)
          lab    <- ssp_labels_all[ssp]

          # Absolute risk panel
          fut_panels[[ssp]] <- risk_plot(risk_f,
            sprintf("%s | %s", lab, period_lb))

          # Delta risk panel
          delta_r <- risk_f - risk_pres
          df_d    <- rast_to_df(delta_r, aoi_v)
          p_d <- ggplot() +
            geom_raster(data = df_d, aes(x = x, y = y, fill = value)) +
            aoi_layer + delta_scale() +
            labs(title = sprintf("%s | \u0394 risk (%s \u2212 Present)", lab, period_lb),
                 x = NULL, y = NULL) +
            map_theme() +
            coord_sf(xlim = bbox_pres$xlim, ylim = bbox_pres$ylim, expand = FALSE,
                     crs = st_crs(32645))
          chg_panels[[ssp]] <- add_scalebar(p_d)

          # Summary row
          sum_rows[[length(sum_rows)+1]] <- data.frame(
            SSP = toupper(ssp), Period = paste(yrs[1], yrs[2], sep = "-"),
            Mean_conflict_risk = round(mean(values(risk_f), na.rm = TRUE), 3),
            stringsAsFactors = FALSE)

          rm(suit_f, risk_f, delta_r, df_d); gc(verbose = FALSE)
        }

        # Save 2×2 absolute risk figure
        fig_fut <- make_2x2_figure(fut_panels,
          title_str    = sprintf("Human\u2013Elephant Conflict Risk \u2014 %s | Elephas maximus | Bhutan", period_lb),
          subtitle_str = "Risk = normalised suitability \u00d7 human pressure (footprint + settlement proximity) | Ensemble mean (GLM + RF + BRT + MaxEnt)")
        if (!is.null(fig_fut)) {
          fname <- sprintf("figure_conflict_future_%s.png", period)
          ggsave(file.path(fig_dir, fname), fig_fut, width = 18, height = 14, dpi = 300, limitsize = FALSE)
          cat(sprintf("  + %s\n", fname)); n <- n + 1
          # Backward-compatible aliases for end-century period
          if (period == "2071_2100")
            file.copy(file.path(fig_dir, fname),
                      file.path(fig_dir, "figure_conflict_future_end_century.png"),
                      overwrite = TRUE)
        }

        # Save 2×2 change figure
        fig_chg <- make_2x2_figure(chg_panels,
          title_str    = sprintf("Change in Conflict Risk (%s vs Present) \u2014 Bhutan", period_lb),
          subtitle_str = "Blue = risk decrease | Red = risk increase | Squished at \u00b10.5")
        if (!is.null(fig_chg)) {
          fname <- sprintf("figure_conflict_change_%s.png", period)
          ggsave(file.path(fig_dir, fname), fig_chg, width = 18, height = 14, dpi = 300, limitsize = FALSE)
          cat(sprintf("  + %s\n", fname)); n <- n + 1
          # Backward-compatible alias for end-century period
          if (period == "2071_2100")
            file.copy(file.path(fig_dir, fname),
                      file.path(fig_dir, "figure_conflict_change.png"),
                      overwrite = TRUE)
        }
      }

      # Summary table
      if (length(sum_rows) > 0) {
        pres_mean <- round(mean(values(risk_pres), na.rm = TRUE), 3)
        sum_df    <- rbind(
          data.frame(SSP = "Present", Period = "Baseline",
                     Mean_conflict_risk = pres_mean, stringsAsFactors = FALSE),
          do.call(rbind, sum_rows))
        sum_df <- sum_df[order(sum_df$SSP, sum_df$Period), ]
        write.csv(sum_df, file.path(fig_dir, "table_conflict_risk_summary.csv"), row.names = FALSE)
        cat("  + table_conflict_risk_summary.csv\n")
      }

      rm(risk_pres, human_pressure, fp_norm, suit_pres); gc(verbose = FALSE)
    }
  }, error = function(e) cat(sprintf("  E10 failed: %s\n", conditionMessage(e))))

  # ── E13  Per-algorithm present suitability ─────────────────────────────────
  cat("\n[E13] Per-algorithm present suitability map\n")
  tryCatch({
    algos <- c(glm = "GLM", rf = "RF", brt = "BRT", maxent = "MaxEnt")
    algo_panels <- list()
    bbox_algo   <- NULL   # shared extent derived from first available raster
    aoi_layer_e13 <- if (!is.null(aoi_sf_utm))
      geom_sf(data = aoi_sf_utm, fill = NA, color = "black", linewidth = 0.55,
              inherit.aes = FALSE) else NULL

    for (key in names(algos)) {
      rfile <- file.path(run_dir, "03_present_suitability",
                         sprintf("suitability_present_%s.tif", key))
      if (!file.exists(rfile)) {
        cat(sprintf("  Skipping %s — raster not found\n", key)); next
      }
      r <- rast(rfile)
      if (is.null(bbox_algo)) bbox_algo <- data_bbox(r)
      df <- rast_to_df(r, aoi_v)
      if (nrow(df) == 0) { cat(sprintf("  Skipping %s — empty df\n", key)); next }

      pct <- pct_suitable(r, thr = 0.5)
      ann <- if (!is.na(pct)) sprintf("Suitable \u226550%%: %.1f%%", pct) else ""

      p <- ggplot(df, aes(x = x, y = y, fill = value)) +
        geom_raster() +
        aoi_layer_e13 +
        suit_scale() +
        annotate("text", x = Inf, y = -Inf, label = ann,
                 hjust = 1.05, vjust = -0.5, size = 3, colour = "grey30") +
        labs(title = algos[[key]], x = NULL, y = NULL) +
        map_theme() +
        coord_sf(xlim = bbox_algo$xlim, ylim = bbox_algo$ylim, expand = FALSE,
                 crs = st_crs(32645))
      algo_panels[[key]] <- add_scalebar(p)
    }

    if (length(algo_panels) > 0 && requireNamespace("patchwork", quietly = TRUE)) {
      # Use fixed algo order (not SSP order)
      ordered_panels <- algo_panels[intersect(c("glm","rf","brt","maxent"), names(algo_panels))]
      fig_e13 <- patchwork::wrap_plots(ordered_panels, ncol = 2) +
        patchwork::plot_annotation(
          title    = "Present Habitat Suitability by Algorithm \u2014 Elephas maximus | Bhutan",
          subtitle = "Ensemble components: GLM, Random Forest, Boosted Regression Trees, MaxEnt | Threshold = 0.50",
          caption  = "CRS: EPSG:32645 (WGS 84 / UTM Zone 45N)"
        )
      ggsave(file.path(fig_dir, "figure_present_suitability_by_algorithm.png"),
             fig_e13, width = 18, height = 14, dpi = 300, limitsize = FALSE)
      cat("  + figure_present_suitability_by_algorithm.png\n")
      n <- n + 1
    }
  }, error = function(e) cat(sprintf("  E13 failed: %s\n", conditionMessage(e))))

  cat(sprintf("\n=== Enhanced figures complete: %d generated ===\n", n))
  invisible(list(n_figures = n))
}

# ── entry point ───────────────────────────────────────────────────────────────
if (sys.nframe() == 0) {
  args    <- commandArgs(trailingOnly = TRUE)
  run_dir <- if (length(args) >= 1) args[[1]] else "."
  create_enhanced_figures(run_dir)
}
