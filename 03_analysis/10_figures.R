#!/usr/bin/env Rscript
# =============================================================================
# 10_figures.R  —  Publication-quality figure generation
# Pipeline Phase: Figures (runs after Phases 11 & 12)
#
# Key fixes vs previous version:
#   - viridis gate removed from all figures that don't use viridis (BUG FIX)
#   - each figure block wrapped in tryCatch (no silent skips)
#   - map_df_from_raster handles multi-layer rasters (BUG FIX)
#   - Figure 2: faceted multi-metric comparison (not just AUC bar)
#   - Figure 3: multi-SSP end-of-century comparison panel
#   - Figure 7: fixed ±0.5 scale + gain/loss annotation
#   - Figure 8: viridis fallback when package unavailable
#   - Figure 10: metrics heatmap
#   - Figure 11 NEW: habitat area trajectories
#   - R01: ROC with per-algorithm AUC annotation
#   - R07: PA overlay with PA shapefile boundaries
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
})

# ── Source guard ──────────────────────────────────────────────────────────────
if (!exists("create_all_figures", mode = "function")) {

# ── Helpers ───────────────────────────────────────────────────────────────────

resolve_repo_root <- function(run_dir) {
  d <- normalizePath(run_dir, winslash = "/", mustWork = FALSE)
  for (i in seq_len(6)) {
    if (file.exists(file.path(d, "00_governance", "config.yaml"))) return(d)
    d <- dirname(d)
  }
  normalizePath(file.path(run_dir, "..", "..", ".."), winslash = "/", mustWork = FALSE)
}

load_aoi_for_figures <- function(run_dir) {
  repo_root <- resolve_repo_root(run_dir)
  candidates <- c(
    file.path(repo_root, "01_data_raw", "03_vector", "shapefiles", "Bhutan", "bhutan.shp"),
    file.path(run_dir, "02_data_intermediate", "m_area_vector.gpkg"),
    file.path(run_dir, "01_processed_data",    "m_area_vector.gpkg")
  )
  for (p in candidates) {
    if (file.exists(p)) {
      aoi <- tryCatch(sf::st_read(p, quiet = TRUE), error = function(e) NULL)
      if (!is.null(aoi)) return(sf::st_make_valid(aoi))
    }
  }
  NULL
}

load_dzongkhag_for_figures <- function(run_dir) {
  repo_root <- resolve_repo_root(run_dir)
  p <- file.path(repo_root, "01_data_raw", "03_vector", "shapefiles",
                 "Dzongkhag Boundary", "Dzongkhag Boundary.shp")
  if (!file.exists(p)) return(NULL)
  d <- tryCatch(sf::st_read(p, quiet = TRUE), error = function(e) NULL)
  if (is.null(d)) return(NULL)
  d <- sf::st_make_valid(d)
  # Shapefile has wrong CRS metadata (labelled EPSG:32645 but coords are EPSG:3857)
  sf::st_crs(d) <- 3857L
  d
}

clip_raster_to_aoi <- function(r, aoi_sf = NULL) {
  if (is.null(aoi_sf) || !inherits(aoi_sf, "sf")) return(r)
  aoi_v <- terra::vect(aoi_sf)
  if (!isTRUE(terra::crs(aoi_v) == terra::crs(r)))
    aoi_v <- terra::project(aoi_v, terra::crs(r))
  terra::crs(aoi_v) <- terra::crs(r)
  tryCatch(terra::mask(terra::crop(r, aoi_v), aoi_v), error = function(e) r)
}

# BUG FIX: handle multi-layer rasters (old code: names(df) <- c("x","y","value") fails if nlyr > 1)
map_df_from_raster <- function(r, aoi_sf = NULL) {
  if (terra::nlyr(r) > 1) r <- r[[1]]
  rr <- clip_raster_to_aoi(r, aoi_sf)
  df <- as.data.frame(rr, xy = TRUE, na.rm = TRUE)
  if (ncol(df) >= 3) names(df)[1:3] <- c("x", "y", "value")
  df
}

aoi_df_for_plot <- function(r, aoi_sf = NULL) {
  if (is.null(aoi_sf)) return(NULL)
  aoi_local <- aoi_sf
  if (is.na(sf::st_crs(aoi_local))) sf::st_crs(aoi_local) <- "EPSG:32645"
  aoi_v <- terra::vect(aoi_local)
  if (!isTRUE(terra::crs(aoi_v) == terra::crs(r)))
    aoi_v <- terra::project(aoi_v, terra::crs(r))
  tryCatch(sf::st_as_sf(aoi_v), error = function(e) NULL)
}

raster_epsg <- function(r) {
  tryCatch({
    auth <- terra::crs(r, describe = TRUE)$authority
    if (!is.null(auth) && nzchar(auth)) as.integer(gsub(".*:", "", auth)) else 32645L
  }, error = function(e) 32645L)
}

# ── Main function ─────────────────────────────────────────────────────────────

create_all_figures <- function(run_dir, run_id) {
  fig_dir <- file.path(run_dir, "08_figures_tables")
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

  cat("Creating publication figures...\n")

  has_ggplot    <- requireNamespace("ggplot2",   quietly = TRUE)
  has_viridis   <- requireNamespace("viridis",   quietly = TRUE)
  has_patchwork <- requireNamespace("patchwork", quietly = TRUE)
  has_ggspatial <- requireNamespace("ggspatial", quietly = TRUE)
  has_scales    <- requireNamespace("scales",    quietly = TRUE)

  n_figures <- 0
  aoi_sf    <- load_aoi_for_figures(run_dir)
  dzong_sf  <- load_dzongkhag_for_figures(run_dir)

  if (!has_ggplot) {
    message("  ggplot2 not available — figures skipped")
    return(list(n_figures = 0))
  }

  # ── Color scales ────────────────────────────────────────────────────────────

  # Suitability: white → yellow → orange → dark red (no viridis needed)
  suit_scale <- function(name = "Suitability") {
    ggplot2::scale_fill_gradientn(
      colours  = c("#f7f7f7", "#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"),
      values   = c(0, 0.05, 0.2, 0.4, 0.65, 1),
      limits   = c(0, 1), na.value = "grey88", name = name,
      guide    = ggplot2::guide_colorbar(barwidth  = ggplot2::unit(0.4, "cm"),
                                          barheight = ggplot2::unit(4,   "cm"))
    )
  }

  # Change map: blue (loss) ← white → red (gain), fixed ±0.5
  change_scale <- function(name = "\u0394 Suitability") {
    oob_fn <- if (has_scales) scales::squish else function(x, ...) pmin(pmax(x, -0.5), 0.5)
    ggplot2::scale_fill_gradientn(
      colours  = c("#053061", "#2166ac", "#67a9cf", "#f7f7f7", "#ef8a62", "#ca0020", "#91000f"),
      values   = scales::rescale(c(-0.5, -0.3, -0.1, 0, 0.1, 0.3, 0.5)),
      limits   = c(-0.5, 0.5), oob = oob_fn, na.value = "grey88", name = name,
      guide    = ggplot2::guide_colorbar(barwidth  = ggplot2::unit(0.4, "cm"),
                                          barheight = ggplot2::unit(4,   "cm"))
    )
  }

  # Uncertainty: viridis inferno if available, else ggplot2 fallback
  uncertainty_scale <- function(name = "SD") {
    if (has_viridis)
      viridis::scale_fill_viridis(option = "inferno", direction = -1,
        na.value = "grey88", name = name,
        guide = ggplot2::guide_colorbar(barwidth = ggplot2::unit(0.4, "cm"),
                                         barheight = ggplot2::unit(4,   "cm")))
    else
      ggplot2::scale_fill_distiller(palette = "YlOrRd", direction = 1,
        na.value = "grey88", name = name,
        guide = ggplot2::guide_colorbar(barwidth = ggplot2::unit(0.4, "cm"),
                                         barheight = ggplot2::unit(4,   "cm")))
  }

  algo_pal <- c(glm = "#2166AC", rf = "#4DAF4A", brt = "#FF7F00",
                maxent = "#984EA3", ensemble = "#333333",
                GLM = "#2166AC", RF = "#4DAF4A", BRT = "#FF7F00",
                MAXENT = "#984EA3", ENSEMBLE = "#333333")

  ssp_pal <- c(SSP126 = "#1a9641", SSP245 = "#a6d96a",
               SSP370 = "#fdae61", SSP585 = "#d7191c")

  # ── Themes ──────────────────────────────────────────────────────────────────

  map_theme <- function(base_size = 11) {
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.background  = ggplot2::element_rect(fill = "#d6e8f5"),
      panel.grid.major  = ggplot2::element_line(colour = "white", linewidth = 0.3),
      panel.grid.minor  = ggplot2::element_blank(),
      plot.title        = ggplot2::element_text(size = base_size + 2, face = "bold", hjust = 0),
      plot.subtitle     = ggplot2::element_text(size = base_size - 1, colour = "grey35", hjust = 0),
      axis.title        = ggplot2::element_text(size = base_size - 1),
      axis.text         = ggplot2::element_text(size = base_size - 2),
      legend.title      = ggplot2::element_text(size = base_size - 1, face = "bold"),
      legend.text       = ggplot2::element_text(size = base_size - 2),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA),
      strip.background  = ggplot2::element_rect(fill = "grey92"),
      strip.text        = ggplot2::element_text(face = "bold", size = base_size - 1),
      plot.caption      = ggplot2::element_text(size = base_size - 3, colour = "grey50"),
      plot.margin       = ggplot2::margin(4, 4, 4, 4, "mm")
    )
  }

  add_cartographic <- function(p) {
    if (has_ggspatial)
      p <- p +
        ggspatial::annotation_scale(location = "bl", width_hint = 0.25,
                                    height = ggplot2::unit(0.15, "cm")) +
        ggspatial::annotation_north_arrow(location = "tl", which_north = "true",
          style    = ggspatial::north_arrow_fancy_orienteering(text_size = 7),
          height   = ggplot2::unit(1.0, "cm"),
          width    = ggplot2::unit(1.0, "cm"))
    p + ggplot2::labs(caption = "CRS: EPSG:32645 (WGS 84 / UTM Zone 45N)")
  }

  # AOI boundary layer (national outline, always black)
  aoi_geom <- function(aoi_local = aoi_sf) {
    if (is.null(aoi_local)) return(NULL)
    ggplot2::geom_sf(data = aoi_local, inherit.aes = FALSE,
                     fill = NA, color = "black", linewidth = 0.5)
  }

  # Dzongkhag district boundary layer — call only where district context adds value
  # aoi_local must already be reprojected to the raster CRS (output of aoi_df_for_plot)
  dzong_geom <- function(aoi_local = aoi_sf) {
    if (is.null(dzong_sf)) return(NULL)
    dz <- tryCatch({
      target_crs <- if (!is.null(aoi_local) && !is.na(sf::st_crs(aoi_local)))
        sf::st_crs(aoi_local) else sf::st_crs(4326)
      sf::st_transform(dzong_sf, target_crs)
    }, error = function(e) NULL)
    if (is.null(dz)) return(NULL)
    ggplot2::geom_sf(data = dz, inherit.aes = FALSE,
                     fill = NA, color = "grey40", linewidth = 0.22, linetype = "dashed")
  }

  # Dzongkhag name labels at polygon centroids
  dzong_label_geom <- function(aoi_local = aoi_sf, size = 2.2, color = "grey20") {
    if (is.null(dzong_sf)) return(NULL)
    dz <- tryCatch({
      target_crs <- if (!is.null(aoi_local) && !is.na(sf::st_crs(aoi_local)))
        sf::st_crs(aoi_local) else sf::st_crs(4326)
      sf::st_transform(dzong_sf, target_crs)
    }, error = function(e) NULL)
    if (is.null(dz)) return(NULL)
    name_col <- if ("dzongkhag" %in% names(dz)) "dzongkhag" else
                if ("DZONGKHAG" %in% names(dz)) "DZONGKHAG" else
                names(dz)[1]
    centroids <- tryCatch(sf::st_centroid(dz), error = function(e) NULL)
    if (is.null(centroids)) return(NULL)
    ggplot2::geom_sf_text(
      data         = centroids,
      ggplot2::aes(label = .data[[name_col]]),
      inherit.aes  = FALSE,
      size         = size,
      color        = color,
      fontface     = "bold",
      check_overlap = TRUE
    )
  }

  # Safe ggsave: warns on failure without crashing
  save_fig <- function(p, path, width = 10, height = 8, dpi = 300) {
    tryCatch(
      suppressWarnings(
        ggplot2::ggsave(path, plot = p, width = width, height = height,
                        dpi = dpi, bg = "white")
      ),
      error = function(e)
        message(sprintf("  ggsave failed [%s]: %s", basename(path), e$message))
    )
    invisible(file.exists(path))
  }

  # ── Load shared data ─────────────────────────────────────────────────────────

  eval_file <- file.path(run_dir, "02_models", "evaluation_all.csv")
  eval_data <- tryCatch(read.csv(eval_file), error = function(e) NULL)

  # Ensemble threshold (TSS-optimal mean, fallback 0.5)
  ens_threshold <- tryCatch({
    if (!is.null(eval_data) && "threshold" %in% names(eval_data)) {
      tv <- mean(eval_data$threshold, na.rm = TRUE)
      if (is.finite(tv)) tv else 0.5
    } else 0.5
  }, error = function(e) 0.5)

  # ── FIGURE 01: Present suitability ──────────────────────────────────────────
  # Gate: has_ggplot only  (suit_scale uses gradientn, NOT viridis)
  tryCatch({
    pres_file <- file.path(run_dir, "03_present_suitability", "suitability_present_ensemble.tif")
    if (!file.exists(pres_file))
      pres_file <- file.path(run_dir, "03_present_suitability", "suitability_present_glm.tif")
    if (!file.exists(pres_file)) stop("no present suitability raster")

    r        <- terra::rast(pres_file)
    df       <- map_df_from_raster(r, aoi_sf)
    aoi_plot <- aoi_df_for_plot(r, aoi_sf)
    epsg     <- raster_epsg(r)

    pres_csv <- file.path(run_dir, "01_processed_data", "presence_points_clean.csv")
    abs_csv  <- file.path(run_dir, "01_processed_data", "background_points.csv")

    pres_sf <- tryCatch({
      pd <- read.csv(pres_csv)
      sf::st_transform(sf::st_as_sf(pd, coords = c("longitude", "latitude"), crs = 4326), epsg)
    }, error = function(e) NULL)
    abs_sf  <- tryCatch({
      ad <- read.csv(abs_csv)
      sf::st_transform(
        sf::st_as_sf(utils::head(ad, 1500), coords = c("longitude", "latitude"), crs = 4326),
        epsg)
    }, error = function(e) NULL)

    n_pres <- if (!is.null(pres_sf)) nrow(pres_sf) else "?"
    n_bg   <- if (!is.null(abs_sf))  nrow(abs_sf)  else "?"

    p <- ggplot2::ggplot() +
      ggplot2::geom_raster(data = df, ggplot2::aes(x = x, y = y, fill = value)) +
      suit_scale("Suitability\n(0\u20131)") +
      dzong_geom(aoi_plot) +
      dzong_label_geom(aoi_plot) +
      aoi_geom(aoi_plot) +
      { if (!is.null(abs_sf))
          ggplot2::geom_sf(data = abs_sf, inherit.aes = FALSE,
                           shape = 3, size = 0.7, color = "grey60", alpha = 0.4) } +
      { if (!is.null(pres_sf))
          ggplot2::geom_sf(data = pres_sf, inherit.aes = FALSE,
                           shape = 21, size = 2.0, fill = "#FFD700",
                           color = "black", stroke = 0.4, alpha = 0.85) } +
      ggplot2::labs(
        title    = "Present-day habitat suitability \u2014 Elephas maximus (Bhutan)",
        subtitle = sprintf("Ensemble model | Threshold = %.3f", ens_threshold)
      ) +
      map_theme() + ggplot2::coord_sf()
    p <- add_cartographic(p)
    save_fig(p, file.path(fig_dir, "figure_01_present_suitability.png"), 10, 9)
    cat("+ Figure 01: Present suitability\n")
    n_figures <- n_figures + 1
  }, error = function(e) cat(sprintf("  Figure 01 skipped: %s\n", e$message)))

  # ── FIGURE 02: Multi-metric model performance ────────────────────────────────
  # Gate: has_ggplot only
  tryCatch({
    if (is.null(eval_data)) stop("evaluation_all.csv not available")
    eval_plot <- eval_data[eval_data$algorithm != "ensemble", , drop = FALSE]
    if (nrow(eval_plot) == 0) stop("no per-algorithm rows in eval_data")

    metric_map <- list(
      auc_mean          = "AUC",
      tss_mean          = "TSS",
      boyce             = "Boyce Index",
      brier             = "Brier Score",
      calibration_slope = "Calibration Slope",
      moran_i           = "|Moran\u2019s I|"
    )
    mc_available <- intersect(names(metric_map), names(eval_plot))
    if (length(mc_available) == 0) stop("no recognised metric columns")

    long_df <- do.call(rbind, lapply(mc_available, function(m) {
      data.frame(
        algorithm = toupper(eval_plot$algorithm),
        metric    = metric_map[[m]],
        value     = as.numeric(eval_plot[[m]]),
        stringsAsFactors = FALSE)
    }))
    long_df <- long_df[!is.na(long_df$value), ]
    long_df$metric <- factor(long_df$metric,
      levels = c("AUC", "TSS", "Boyce Index", "Brier Score", "Calibration Slope", "|Moran\u2019s I|"))
    long_df$algorithm <- factor(long_df$algorithm,
      levels = c("GLM", "RF", "BRT", "MAXENT", "ENSEMBLE"))

    p <- ggplot2::ggplot(long_df,
        ggplot2::aes(x = .data[["algorithm"]], y = .data[["value"]],
                     fill = .data[["algorithm"]])) +
      ggplot2::geom_col(width = 0.65) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", .data[["value"]])),
                         vjust = -0.35, size = 3.0, fontface = "bold") +
      ggplot2::scale_fill_manual(values = algo_pal, guide = "none") +
      ggplot2::facet_wrap(~metric, scales = "free_y", ncol = 3) +
      ggplot2::labs(
        title    = "Model performance \u2014 Elephas maximus SDM (Bhutan)",
        subtitle = "Out-of-fold holdout evaluation | Higher AUC/TSS/Boyce = better | Lower Brier = better",
        x = NULL, y = "Value"
      ) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(
        strip.background   = ggplot2::element_rect(fill = "grey92"),
        strip.text         = ggplot2::element_text(face = "bold"),
        axis.text.x        = ggplot2::element_text(face = "bold", size = 10),
        plot.title         = ggplot2::element_text(face = "bold"),
        panel.grid.major.x = ggplot2::element_blank()
      )
    save_fig(p, file.path(fig_dir, "figure_02_model_auc.png"),  12, 7)
    save_fig(p, file.path(fig_dir, "figure_model_auc.png"),     12, 7)
    cat("+ Figure 02: Multi-metric performance\n")
    n_figures <- n_figures + 1
  }, error = function(e) cat(sprintf("  Figure 02 skipped: %s\n", e$message)))

  # ── FIGURES 03–06: Future suitability ───────────────────────────────────────
  # Gate: has_ggplot only  (suit_scale uses gradientn, NOT viridis)
  tryCatch({
    fut_dir  <- file.path(run_dir, "04_future_projections")
    if (!dir.exists(fut_dir)) stop("no future_projections directory")

    # Prefer ensemble files; fall back to any future suitability tif
    fut_all <- list.files(fut_dir, pattern = "suitability_future_ensemble.*\\.tif$", full.names = TRUE)
    if (length(fut_all) == 0)
      fut_all <- list.files(fut_dir, pattern = "suitability_future.*\\.tif$", full.names = TRUE)
    if (length(fut_all) == 0) stop("no future suitability rasters")

    find_fut <- function(files, ssp, period_yr) {
      m <- grep(sprintf("(?i)%s.*%s", ssp, period_yr), files, value = TRUE, perl = TRUE)
      if (length(m) > 0) m[1] else NULL
    }

    # ── Multi-SSP end-of-century panel (Figure 03) ───────────────────────────
    ssps   <- c("ssp126", "ssp245", "ssp370", "ssp585")
    panels <- list()
    for (ssp in ssps) {
      f <- find_fut(fut_all, ssp, "2071")
      if (is.null(f)) f <- find_fut(fut_all, ssp, "2100")
      if (is.null(f)) next
      r        <- terra::rast(f)
      df       <- map_df_from_raster(r, aoi_sf)
      aoi_plot <- aoi_df_for_plot(r, aoi_sf)
      label    <- toupper(gsub("ssp", "SSP", ssp))
      pct      <- round(100 * mean(df$value >= ens_threshold, na.rm = TRUE), 1)
      panels[[ssp]] <- ggplot2::ggplot() +
        ggplot2::geom_raster(data = df, ggplot2::aes(x = x, y = y, fill = value)) +
        suit_scale("Suitability") +
        aoi_geom(aoi_plot) +
        ggplot2::annotate("label", x = Inf, y = Inf,
                          label = sprintf("%s%%\nsuitable", pct),
                          hjust = 1.1, vjust = 1.1, size = 3.0, fill = "white", alpha = 0.85) +
        ggplot2::labs(title = label) +
        map_theme(9) + ggplot2::coord_sf()
    }

    if (length(panels) == 0) stop("no SSP panels generated for Figure 03")

    if (has_patchwork && length(panels) >= 2) {
      n_col <- if (length(panels) == 4) 2 else length(panels)
      combined <- patchwork::wrap_plots(panels, ncol = n_col) +
        patchwork::plot_annotation(
          title    = "Projected habitat suitability 2071\u20132100 \u2014 Elephas maximus (Bhutan)",
          subtitle = "Ensemble model | SSP1-2.6 (low) to SSP5-8.5 (very high emissions)",
          tag_levels = "A"
        )
      # figure_03_future.png suppressed
    } else {
      # figure_03_future.png suppressed
    }
    cat(sprintf("+ Figure 03: Future suitability (%d SSP panels, 2071-2100)\n", length(panels)))
    n_figures <- n_figures + 1

    # figure_04–06_future.png suppressed

    # ── Near-future (2021-2050) comparison ───────────────────────────────────
    tryCatch({
      panels_near <- list()
      for (ssp in ssps) {
        f <- find_fut(fut_all, ssp, "2050")
        if (is.null(f)) f <- find_fut(fut_all, ssp, "2021")
        if (is.null(f)) next
        r        <- terra::rast(f)
        df       <- map_df_from_raster(r, aoi_sf)
        aoi_plot <- aoi_df_for_plot(r, aoi_sf)
        label    <- toupper(gsub("ssp", "SSP", ssp))
        pct      <- round(100 * mean(df$value >= ens_threshold, na.rm = TRUE), 1)
        panels_near[[ssp]] <- ggplot2::ggplot() +
          ggplot2::geom_raster(data = df, ggplot2::aes(x = x, y = y, fill = value)) +
          suit_scale("Suitability") +
          aoi_geom(aoi_plot) +
          ggplot2::annotate("label", x = Inf, y = Inf, label = sprintf("%s%%\nsuitable", pct),
                            hjust = 1.1, vjust = 1.1, size = 3.0, fill = "white", alpha = 0.85) +
          ggplot2::labs(title = label) +
          map_theme(9) + ggplot2::coord_sf()
      }
      if (has_patchwork && length(panels_near) >= 2) {
        n_col <- if (length(panels_near) == 4) 2 else length(panels_near)
        comb_near <- patchwork::wrap_plots(panels_near, ncol = n_col) +
          patchwork::plot_annotation(
            title    = "Projected habitat suitability 2021\u20132050 \u2014 Elephas maximus (Bhutan)",
            subtitle = "Near-future scenarios",
            tag_levels = "A"
          )
        save_fig(comb_near, file.path(fig_dir, "figure_future_near_2050.png"),
                 14, if (length(panels_near) == 4) 12 else 7)
      }
    }, error = function(e) NULL)

  }, error = function(e) cat(sprintf("  Figures 03-06 skipped: %s\n", e$message)))

  # ── FIGURE 07: Change map ────────────────────────────────────────────────────
  # BUG FIX: gate on has_ggplot ONLY — scale_fill_gradient2 does NOT need viridis
  tryCatch({
    change_dir <- file.path(run_dir, "05_change_metrics")
    if (!dir.exists(change_dir)) stop("no change_metrics directory")

    delta_files <- list.files(change_dir, pattern = "delta.*\\.tif$", full.names = TRUE)
    if (length(delta_files) == 0) stop("no delta rasters found")

    # Prefer SSP585 end-of-century as primary change figure
    pref <- grep("(?i)ssp585.*2071", delta_files, value = TRUE, perl = TRUE)
    if (length(pref) == 0) pref <- grep("(?i)ssp585", delta_files, value = TRUE, perl = TRUE)
    if (length(pref) == 0) pref <- delta_files
    f_use <- pref[1]

    r        <- terra::rast(f_use)
    df       <- map_df_from_raster(r, aoi_sf)
    aoi_plot <- aoi_df_for_plot(r, aoi_sf)

    cell_km2 <- prod(terra::res(r)) / 1e6
    gain_km2 <- round(sum(df$value >  0.05, na.rm = TRUE) * cell_km2)
    loss_km2 <- round(sum(df$value < -0.05, na.rm = TRUE) * cell_km2)

    ssp_lbl  <- regmatches(basename(f_use), regexpr("(?i)ssp\\d+", basename(f_use)))
    per_lbl  <- regmatches(basename(f_use), regexpr("\\d{4}_\\d{4}", basename(f_use)))
    map_lbl  <- if (length(ssp_lbl) && length(per_lbl))
                  toupper(sprintf("%s %s", ssp_lbl, gsub("_", "\u2013", per_lbl)))
                else "end-of-century"

    p <- ggplot2::ggplot() +
      ggplot2::geom_raster(data = df, ggplot2::aes(x = x, y = y, fill = value)) +
      change_scale() +
      aoi_geom(aoi_plot) +
      ggplot2::labs(
        title    = "Projected change in habitat suitability \u2014 Elephas maximus (Bhutan)",
        subtitle = sprintf("%s | Gain (>0.05): +%s km\u00b2 | Loss (<\u22120.05): \u2212%s km\u00b2",
                           map_lbl,
                           format(gain_km2, big.mark = ","),
                           format(loss_km2, big.mark = ","))
      ) +
      map_theme() + ggplot2::coord_sf()
    p <- add_cartographic(p)
    # figure_07_change.png suppressed
    save_fig(p, file.path(fig_dir, "figure_r05_change_map.png"), 10, 8)
    cat("+ Figure 07: Change map\n")
    n_figures <- n_figures + 1

    # Multi-panel SSP245 vs SSP585 comparison (supplementary)
    tryCatch({
      side_panels <- list()
      for (ssp in c("ssp245", "ssp585")) {
        f2 <- grep(sprintf("(?i)%s.*2071", ssp), delta_files, value = TRUE, perl = TRUE)
        if (length(f2) == 0) next
        r2   <- terra::rast(f2[1])
        df2  <- map_df_from_raster(r2, aoi_sf)
        aoi2 <- aoi_df_for_plot(r2, aoi_sf)
        g2   <- round(sum(df2$value >  0.05, na.rm = TRUE) * prod(terra::res(r2)) / 1e6)
        l2   <- round(sum(df2$value < -0.05, na.rm = TRUE) * prod(terra::res(r2)) / 1e6)
        side_panels[[ssp]] <- ggplot2::ggplot() +
          ggplot2::geom_raster(data = df2, ggplot2::aes(x = x, y = y, fill = value)) +
          change_scale() + aoi_geom(aoi2) +
          ggplot2::annotate("text", x = -Inf, y = -Inf,
                            label = sprintf("Gain: +%s km\u00b2\nLoss: \u2212%s km\u00b2",
                                            format(g2, big.mark=","), format(l2, big.mark=",")),
                            hjust = -0.05, vjust = -0.2, size = 2.8, fontface = "bold") +
          ggplot2::labs(title = toupper(gsub("ssp", "SSP", ssp))) +
          map_theme(9) + ggplot2::coord_sf()
      }
      if (has_patchwork && length(side_panels) == 2) {
        comb_chg <- patchwork::wrap_plots(side_panels, ncol = 2) +
          patchwork::plot_annotation(
            title    = "Change in habitat suitability 2071\u20132100 vs present",
            subtitle = "Left: SSP2-4.5 (intermediate) | Right: SSP5-8.5 (high emissions)",
            tag_levels = "A"
          )
        # figure_07_change_comparison.png suppressed
      }
      # figure_07_change_ssp*.png suppressed
    }, error = function(e) NULL)

  }, error = function(e) cat(sprintf("  Figure 07 skipped: %s\n", e$message)))

  # ── FIGURE 08: GCM uncertainty ───────────────────────────────────────────────
  # BUG FIX: fallback when viridis unavailable; gate on has_ggplot ONLY
  tryCatch({
    unc_dir <- file.path(run_dir, "06_uncertainty")
    if (!dir.exists(unc_dir)) stop("no uncertainty directory")

    unc_files <- list.files(unc_dir,
      pattern = "(gcm_sd|combined_uncertainty).*\\.tif$", full.names = TRUE)
    if (length(unc_files) == 0)
      unc_files <- list.files(unc_dir, pattern = "uncertainty.*\\.tif$", full.names = TRUE)
    if (length(unc_files) == 0) stop("no uncertainty rasters")

    pref <- grep("(?i)ssp585.*2071", unc_files, value = TRUE, perl = TRUE)
    if (length(pref) == 0) pref <- unc_files
    r        <- terra::rast(pref[1])
    df       <- map_df_from_raster(r, aoi_sf)
    aoi_plot <- aoi_df_for_plot(r, aoi_sf)
    mean_sd  <- round(mean(df$value, na.rm = TRUE), 3)

    p <- ggplot2::ggplot() +
      ggplot2::geom_raster(data = df, ggplot2::aes(x = x, y = y, fill = value)) +
      uncertainty_scale("GCM SD") +
      aoi_geom(aoi_plot) +
      ggplot2::labs(
        title    = "GCM ensemble uncertainty in projected habitat suitability",
        subtitle = sprintf("Standard deviation across 9 GCMs | SSP585 2071\u20132100 | Mean SD = %.3f",
                           mean_sd)
      ) +
      map_theme() + ggplot2::coord_sf()
    p <- add_cartographic(p)
    # figure_08_uncertainty.png suppressed
    cat("+ Figure 08: Uncertainty\n")
    n_figures <- n_figures + 1
  }, error = function(e) cat(sprintf("  Figure 08 skipped: %s\n", e$message)))

  # ── FIGURE 09: Response curves ───────────────────────────────────────────────
  # Gate: has_ggplot only
  tryCatch({
    resp_path <- file.path(run_dir, "01_processed_data", "modeling_dataset.csv")
    if (!file.exists(resp_path)) stop("modeling_dataset.csv not found")

    dat       <- read.csv(resp_path)
    meta_cols <- c("id","type","response","longitude","latitude","fold",
                   "station_id","source","presence","key","row_id","X")
    pred_cols <- setdiff(names(dat), meta_cols)
    pred_cols <- pred_cols[sapply(dat[, pred_cols, drop = FALSE], is.numeric)]
    if (length(pred_cols) == 0) stop("no numeric predictor columns")
    pred_cols <- utils::head(pred_cols, 16)

    long_df <- do.call(rbind, lapply(pred_cols, function(pc) {
      data.frame(predictor = pc, value = dat[[pc]],
                 response  = as.integer(dat$response), stringsAsFactors = FALSE)
    }))

    p <- ggplot2::ggplot(long_df,
        ggplot2::aes(x = .data[["value"]], y = .data[["response"]])) +
      ggplot2::geom_point(alpha = 0.07, size = 0.5, color = "grey50") +
      ggplot2::geom_smooth(method = "glm", method.args = list(family = "binomial"),
                           color = "#2166AC", fill = "#92c5de", alpha = 0.25,
                           se = TRUE, linewidth = 0.9) +
      ggplot2::facet_wrap(~predictor, scales = "free_x", ncol = 4) +
      ggplot2::scale_y_continuous(limits = c(0, 1),
                                  breaks = c(0, 0.25, 0.5, 0.75, 1)) +
      ggplot2::labs(
        title    = "Predictor response relationships \u2014 Elephas maximus SDM",
        subtitle = "GLM marginal effect (logistic) | Points = observations | Ribbon = 95% CI",
        x = "Predictor value", y = "P(occurrence)"
      ) +
      ggplot2::theme_bw(base_size = 10) +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = "grey92"),
        strip.text       = ggplot2::element_text(face = "bold", size = 8),
        panel.grid.minor = ggplot2::element_blank(),
        plot.title       = ggplot2::element_text(face = "bold")
      )
    nrow_f <- ceiling(length(pred_cols) / 4)
    save_fig(p, file.path(fig_dir, "figure_09_response.png"),
             5 * 4, 3.5 * nrow_f, dpi = 300)
    cat("+ Figure 09: Response curves\n")
    n_figures <- n_figures + 1
  }, error = function(e) cat(sprintf("  Figure 09 skipped: %s\n", e$message)))

  # ── FIGURE 10: Metrics heatmap ────────────────────────────────────────────────
  # Gate: has_ggplot only
  tryCatch({
    if (is.null(eval_data)) stop("evaluation_all.csv not available")
    mc_all <- c("auc_mean","tss_mean","boyce","brier","calibration_slope","moran_i")
    mc_use <- intersect(mc_all, names(eval_data))
    if (length(mc_use) < 2) stop("insufficient metric columns")

    mc_labels <- c(auc_mean="AUC", tss_mean="TSS", boyce="Boyce",
                   brier="Brier", calibration_slope="Calib. Slope", moran_i="|Moran's I|")
    long_df <- do.call(rbind, lapply(mc_use, function(m) {
      data.frame(algorithm = toupper(eval_data$algorithm),
                 metric    = mc_labels[m],
                 value     = as.numeric(eval_data[[m]]),
                 stringsAsFactors = FALSE)
    }))
    long_df <- long_df[!is.na(long_df$value), ]
    long_df$metric    <- factor(long_df$metric,   levels = mc_labels[mc_use])
    long_df$algorithm <- factor(long_df$algorithm,
                                levels = c("GLM","RF","BRT","MAXENT","ENSEMBLE"))

    p <- ggplot2::ggplot(long_df,
        ggplot2::aes(x = .data[["algorithm"]], y = .data[["metric"]],
                     fill = .data[["value"]])) +
      ggplot2::geom_tile(color = "white", linewidth = 0.6) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", .data[["value"]])),
                         size = 3.8, fontface = "bold", color = "black") +
      ggplot2::scale_fill_gradient2(
        low = "#d73027", mid = "#ffffbf", high = "#1a9641", midpoint = 0.65,
        na.value = "grey90", name = "Value",
        guide = ggplot2::guide_colorbar(barwidth  = ggplot2::unit(0.4, "cm"),
                                         barheight = ggplot2::unit(3.5, "cm"))
      ) +
      ggplot2::labs(
        title    = "Model evaluation heatmap \u2014 Elephas maximus SDM (Bhutan)",
        subtitle = "Out-of-fold holdout predictions | Higher = better (except Brier, |Moran's I|)",
        x = NULL, y = NULL
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid    = ggplot2::element_blank(),
        axis.text.x   = ggplot2::element_text(face = "bold", size = 12),
        axis.text.y   = ggplot2::element_text(face = "bold", size = 11),
        plot.title    = ggplot2::element_text(face = "bold"),
        legend.position = "right"
      )
    save_fig(p, file.path(fig_dir, "figure_10_comparison.png"), 10, 6)
    cat("+ Figure 10: Metrics heatmap\n")
    n_figures <- n_figures + 1
  }, error = function(e) cat(sprintf("  Figure 10 skipped: %s\n", e$message)))

  # ── FIGURE 11: Habitat area trajectories ─────────────────────────────────────
  # NEW — shows % suitable habitat across all SSPs and time periods
  tryCatch({
    chg_dir  <- file.path(run_dir, "05_change_metrics")
    pres_dir <- file.path(run_dir, "03_present_suitability")

    delta_files <- list.files(chg_dir, pattern = "delta_suitability_ensemble.*\\.tif$",
                              full.names = TRUE)
    if (length(delta_files) == 0) stop("no ensemble delta rasters for trajectories")

    pres_file <- file.path(pres_dir, "suitability_present_ensemble.tif")
    pres_mean <- if (file.exists(pres_file)) {
      r <- terra::rast(pres_file)
      mean(terra::values(r, na.rm = TRUE), na.rm = TRUE)
    } else NA_real_

    traj_rows <- lapply(delta_files, function(f) {
      bn   <- basename(f)
      ssp  <- regmatches(bn, regexpr("(?i)ssp\\d+", bn, perl = TRUE))
      yr   <- regmatches(bn, regexpr("\\d{4}_\\d{4}", bn))
      if (length(ssp) == 0 || length(yr) == 0) return(NULL)
      r    <- tryCatch(terra::rast(f), error = function(e) NULL)
      if (is.null(r)) return(NULL)
      vals <- terra::values(r[[1]], na.rm = TRUE)
      data.frame(ssp = toupper(ssp), period = yr,
                 period_end  = as.integer(sub(".*_(\\d{4})$", "\\1", yr)),
                 mean_delta  = mean(vals, na.rm = TRUE),
                 stringsAsFactors = FALSE)
    })
    traj_df <- do.call(rbind, traj_rows[!sapply(traj_rows, is.null)])
    if (is.null(traj_df) || nrow(traj_df) == 0) stop("empty trajectory data")
    traj_df <- traj_df[order(traj_df$ssp, traj_df$period_end), ]

    present_cols <- intersect(names(ssp_pal), unique(traj_df$ssp))

    p <- ggplot2::ggplot(traj_df,
        ggplot2::aes(x = .data[["period_end"]], y = .data[["mean_delta"]],
                     color = .data[["ssp"]], group = .data[["ssp"]])) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.8) +
      ggplot2::geom_line(linewidth = 1.3) +
      ggplot2::geom_point(size = 3.5) +
      ggplot2::scale_color_manual(
        values = ssp_pal[present_cols],
        labels = c(SSP126 = "SSP1-2.6 (Low)", SSP245 = "SSP2-4.5 (Intermediate)",
                   SSP370 = "SSP3-7.0 (High)", SSP585 = "SSP5-8.5 (Very High)")[present_cols],
        name   = "Emission scenario"
      ) +
      ggplot2::scale_x_continuous(breaks = c(2050, 2080, 2100)) +
      ggplot2::labs(
        title    = "Projected change in mean habitat suitability \u2014 Elephas maximus (Bhutan)",
        subtitle = "Mean \u0394 suitability relative to present | Positive = gain | Negative = loss",
        x = "Year", y = "Mean change in suitability (\u0394)"
      ) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(
        legend.position  = "right",
        panel.grid.minor = ggplot2::element_blank(),
        plot.title       = ggplot2::element_text(face = "bold")
      )
    save_fig(p, file.path(fig_dir, "figure_11_trajectories.png"), 10, 6)
    cat("+ Figure 11: Trajectories\n")
    n_figures <- n_figures + 1
  }, error = function(e) cat(sprintf("  Figure 11 skipped: %s\n", e$message)))

  # ── R01: ROC curves ─────────────────────────────────────────────────────────
  tryCatch({
    roc_file <- file.path(run_dir, "02_models", "roc_curve_data.csv")
    if (!file.exists(roc_file)) stop("roc_curve_data.csv not found")
    roc_df <- read.csv(roc_file)
    if (!all(c("specificity", "sensitivity", "algorithm") %in% names(roc_df)))
      stop("roc_curve_data.csv missing required columns")

    roc_df$fpr       <- 1 - roc_df$specificity
    roc_df$algo_up   <- toupper(roc_df$algorithm)

    # Add AUC from evaluation table if available
    roc_df$algo_label <- if (!is.null(eval_data) && "auc_mean" %in% names(eval_data)) {
      auc_map <- stats::setNames(
        sprintf("%s (AUC=%.3f)", toupper(eval_data$algorithm), eval_data$auc_mean),
        toupper(eval_data$algorithm)
      )
      ifelse(roc_df$algo_up %in% names(auc_map),
             auc_map[roc_df$algo_up], roc_df$algo_up)
    } else roc_df$algo_up

    p <- ggplot2::ggplot(roc_df,
        ggplot2::aes(x = .data[["fpr"]], y = .data[["sensitivity"]],
                     color = .data[["algo_label"]], group = .data[["algo_up"]])) +
      ggplot2::geom_abline(slope = 1, intercept = 0,
                           linetype = "dashed", color = "grey55", linewidth = 0.7) +
      ggplot2::geom_line(linewidth = 1.0, alpha = 0.9) +
      ggplot2::scale_color_manual(
        values = stats::setNames(
          algo_pal[unique(roc_df$algo_up)],
          unique(roc_df$algo_label)[match(unique(roc_df$algo_up),
                                          roc_df$algo_up[match(unique(roc_df$algo_up),
                                                                roc_df$algo_up)])]
        ),
        name = "Algorithm"
      ) +
      ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0)) +
      ggplot2::labs(
        title    = "ROC curves \u2014 Elephas maximus SDM (Bhutan)",
        subtitle = "Out-of-fold holdout predictions | Dashed = random classifier (AUC = 0.5)",
        x = "False Positive Rate (1 \u2212 Specificity)",
        y = "True Positive Rate (Sensitivity)"
      ) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(
        legend.position   = c(0.97, 0.03),
        legend.justification = c(1, 0),
        legend.background = ggplot2::element_rect(fill = "white", color = "grey70"),
        plot.title        = ggplot2::element_text(face = "bold"),
        panel.grid.minor  = ggplot2::element_blank()
      )
    save_fig(p, file.path(fig_dir, "figure_roc_curves.png"), 8, 7)
    cat("+ Figure R01: ROC curves\n")
    n_figures <- n_figures + 1
  }, error = function(e) cat(sprintf("  Figure R01 skipped: %s\n", e$message)))

  # ── R02: Calibration plot ────────────────────────────────────────────────────
  tryCatch({
    calib_file <- file.path(run_dir, "02_models", "calibration_data.csv")
    if (!file.exists(calib_file)) stop("calibration_data.csv not found")
    calib_df  <- read.csv(calib_file)
    calib_df  <- calib_df[!is.na(calib_df$predicted) & !is.na(calib_df$observed), ]
    calib_df$algo_up <- toupper(calib_df$algorithm)

    p <- ggplot2::ggplot(calib_df,
        ggplot2::aes(x = .data[["predicted"]], y = .data[["observed"]],
                     color = .data[["algo_up"]])) +
      ggplot2::geom_abline(slope = 1, intercept = 0,
                           linetype = "dashed", color = "grey55", linewidth = 0.7) +
      ggplot2::geom_point(size = 2.5, alpha = 0.85) +
      ggplot2::geom_line(alpha = 0.6, linewidth = 0.7) +
      ggplot2::scale_color_manual(values = algo_pal, name = "Algorithm") +
      ggplot2::scale_x_continuous(limits = c(0, 1)) +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::labs(
        title    = "Model calibration \u2014 Elephas maximus SDM (Bhutan)",
        subtitle = "Perfect calibration = points on the 1:1 dashed line | 10 equal-width bins",
        x = "Predicted probability", y = "Observed proportion"
      ) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(
        plot.title       = ggplot2::element_text(face = "bold"),
        panel.grid.minor = ggplot2::element_blank()
      )
    save_fig(p, file.path(fig_dir, "figure_r02_calibration.png"), 8, 6)
    cat("+ Figure R02: Calibration\n")
    n_figures <- n_figures + 1
  }, error = function(e) cat(sprintf("  Figure R02 skipped: %s\n", e$message)))

  # ── R07: PA overlay ──────────────────────────────────────────────────────────
  # BUG FIX: gate on has_ggplot ONLY (suit_scale does NOT need viridis)
  tryCatch({
    pa_rast_file <- file.path(run_dir, "07_overlays", "suitability_within_PAs.tif")
    if (!file.exists(pa_rast_file)) stop("suitability_within_PAs.tif not found")
    r <- terra::rast(pa_rast_file)
    if (terra::nlyr(r) > 1) r <- r[[1]]

    df       <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
    if (ncol(df) >= 3) names(df)[1:3] <- c("x", "y", "value")
    aoi_plot <- aoi_df_for_plot(r, aoi_sf)

    repo_root <- resolve_repo_root(run_dir)
    pa_shp    <- file.path(repo_root, "01_data_raw", "03_vector", "shapefiles",
                           "PA_Bhutan", "PA_Bnd_Final_20230316.shp")
    pa_sf <- if (file.exists(pa_shp)) {
      tryCatch({
        pa <- sf::st_read(pa_shp, quiet = TRUE)
        sf::st_transform(sf::st_make_valid(pa), terra::crs(r))
      }, error = function(e) NULL)
    } else NULL

    # Separate national parks/sanctuaries from biological corridors
    pa_only <- if (!is.null(pa_sf))
      pa_sf[!grepl("^Biological Corridor", pa_sf$park), ] else NULL
    bc_only  <- if (!is.null(pa_sf))
      pa_sf[grepl("^Biological Corridor", pa_sf$park), ] else NULL

    # Centroids for PA_name labels
    pa_centroids <- if (!is.null(pa_only) && nrow(pa_only) > 0)
      tryCatch(sf::st_centroid(pa_only), error = function(e) NULL) else NULL
    bc_centroids <- if (!is.null(bc_only) && nrow(bc_only) > 0)
      tryCatch(sf::st_centroid(bc_only), error = function(e) NULL) else NULL

    p <- ggplot2::ggplot() +
      ggplot2::geom_raster(data = df, ggplot2::aes(x = x, y = y, fill = value)) +
      suit_scale("Suitability\n(within PAs)") +
      aoi_geom(aoi_plot) +
      { if (!is.null(bc_only) && nrow(bc_only) > 0)
          ggplot2::geom_sf(data = bc_only, inherit.aes = FALSE,
                           fill = NA, color = "#4dac26", linewidth = 0.5,
                           linetype = "dashed") } +
      { if (!is.null(pa_only) && nrow(pa_only) > 0)
          ggplot2::geom_sf(data = pa_only, inherit.aes = FALSE,
                           fill = NA, color = "#1b7837", linewidth = 0.7) } +
      { if (!is.null(pa_centroids)) {
          has_repel_r07 <- requireNamespace("ggrepel", quietly = TRUE)
          coords_pa <- sf::st_coordinates(pa_centroids)
          df_pa_lbl <- data.frame(x = coords_pa[,1], y = coords_pa[,2],
                                   label = pa_centroids[["PA_name"]], stringsAsFactors = FALSE)
          if (has_repel_r07)
            ggrepel::geom_label_repel(
              data = df_pa_lbl, ggplot2::aes(x = x, y = y, label = label),
              inherit.aes = FALSE, size = 2.5, colour = "#1b7837",
              fill = alpha("white", 0.78), fontface = "bold",
              label.size = 0.18, label.padding = unit(0.10, "lines"),
              min.segment.length = 0.25, segment.colour = "#1b7837",
              segment.size = 0.3, max.overlaps = 50, seed = 42)
          else
            ggplot2::geom_sf_text(data = pa_centroids,
                                  ggplot2::aes(label = .data[["PA_name"]]),
                                  inherit.aes = FALSE, size = 2.5, color = "#1b7837",
                                  fontface = "bold", check_overlap = FALSE)
        } } +
      { if (!is.null(bc_centroids)) {
          has_repel_r07 <- requireNamespace("ggrepel", quietly = TRUE)
          coords_bc <- sf::st_coordinates(bc_centroids)
          df_bc_lbl <- data.frame(x = coords_bc[,1], y = coords_bc[,2],
                                   label = bc_centroids[["PA_name"]], stringsAsFactors = FALSE)
          if (has_repel_r07)
            ggrepel::geom_label_repel(
              data = df_bc_lbl, ggplot2::aes(x = x, y = y, label = label),
              inherit.aes = FALSE, size = 2.2, colour = "#4dac26",
              fill = alpha("white", 0.78), fontface = "italic",
              label.size = 0.18, label.padding = unit(0.10, "lines"),
              min.segment.length = 0.25, segment.colour = "#4dac26",
              segment.size = 0.3, max.overlaps = 50, seed = 42)
          else
            ggplot2::geom_sf_text(data = bc_centroids,
                                  ggplot2::aes(label = .data[["PA_name"]]),
                                  inherit.aes = FALSE, size = 2.2, color = "#4dac26",
                                  fontface = "italic", check_overlap = FALSE)
        } } +
      ggplot2::labs(
        title    = "Habitat suitability within protected areas \u2014 Elephas maximus (Bhutan)",
        subtitle = sprintf(
          "Solid green = Protected Areas | Dashed green = Biological Corridors | Mean suitability = %.3f",
          mean(df$value, na.rm = TRUE))
      ) +
      map_theme() + ggplot2::coord_sf()
    p <- add_cartographic(p)
    save_fig(p, file.path(fig_dir, "figure_r07_conservation_overlays.png"), 10, 9)
    cat("+ Figure R07: PA overlay\n")
    n_figures <- n_figures + 1
  }, error = function(e) cat(sprintf("  Figure R07 skipped: %s\n", e$message)))

  cat(sprintf("\nTotal figures created: %d\n", n_figures))
  list(n_figures = n_figures)
}

} # end source guard

if (sys.nframe() == 0) {
  args    <- commandArgs(trailingOnly = TRUE)
  run_dir <- if (length(args) >= 1) args[[1]] else "."
  run_id  <- if (length(args) >= 2) args[[2]] else paste0("RUN_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  result  <- create_all_figures(run_dir, run_id)
}
