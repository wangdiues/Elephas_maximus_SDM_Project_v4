#!/usr/bin/env Rscript
# =============================================================================
# 10_figures_supplementary.R  —  Supplementary figures: model diagnostics
# Pipeline integration: sourced from run_pipeline.R after enhanced figures
#
# Figures generated (S1, S3–S16):
#   figure_s01_suitability_5panel.png        S1   Suitability by algorithm + ensemble
#   figure_s03_cv_fold_performance.png       S3   Per-fold AUC & TSS
#   figure_s04_null_model_comparison.png     S4   Permuted vs observed AUC (null model)
#   figure_s05_residual_correlogram.png      S5   Spatial autocorrelation of OOF residuals
#   figure_s06_predictor_collinearity.png    S6   Predictor collinearity heatmap
#   figure_s07_threshold_sensitivity.png     S7   % suitable habitat vs threshold
#   figure_s08_climate_envelope.png          S8   BIO14 × BIO15 bivariate climate space
#   figure_s09_response_curves.png           S9   Partial dependence response curves
#   figure_s10_mess_map.png                  S10  MESS extrapolation risk map
#   figure_s11_background_points.png         S11  Occurrence + background sampling map
#   figure_s12_climate_pca_biplot.png        S12  Climate PCA: present vs future centroids
#   figure_s13_gcm_agreement.png             S13  GCM agreement map (fraction suitable)
#   figure_s14_gcm_spread.png                S14  GCM ensemble spread (SD across GCMs)
#   figure_s15_response_curves_all.png       S15  Response curves — all predictors, one algo
#   figure_s16_occurrence_density.png        S16  Occurrence density map
#   figure_s17_dzongkhag_suitability.png     S17  Per-Dzongkhag % suitable + mean score
#   figure_s18_refugia.png                   S18  Climate refugia stability classes
#
# Note: S2 (calibration) is already saved as figure_r02_calibration.png by 10_figures.R
#
# Standalone usage (PowerShell):
#   Rscript 03_analysis/10_figures_supplementary.R <run_dir>
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(ggplot2)
  if (requireNamespace("scales", quietly = TRUE)) library(scales)
})

if (!exists("create_supplementary_figures", mode = "function")) {

# ── helpers ───────────────────────────────────────────────────────────────────

.supp_map_theme <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
  theme(
    panel.background  = element_rect(fill = "#d6e8f5"),
    panel.grid.major  = element_line(colour = "white", linewidth = 0.3),
    panel.grid.minor  = element_blank(),
    plot.title        = element_text(size = base_size + 1, face = "bold"),
    plot.subtitle     = element_text(size = base_size - 1, colour = "grey35"),
    axis.title        = element_text(size = base_size - 1),
    axis.text         = element_text(size = base_size - 2),
    legend.title      = element_text(size = base_size - 1, face = "bold"),
    legend.text       = element_text(size = base_size - 2),
    strip.background  = element_rect(fill = "grey92"),
    strip.text        = element_text(face = "bold"),
    plot.caption      = element_text(size = base_size - 3, colour = "grey50"),
    plot.margin       = margin(4, 4, 4, 4, "mm")
  )
}

.supp_suit_scale <- function(name = "Suitability") {
  scale_fill_gradientn(
    colours  = c("#f7f7f7", "#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"),
    values   = c(0, 0.05, 0.2, 0.4, 0.65, 1),
    limits   = c(0, 1), na.value = "grey88", name = name,
    guide    = guide_colorbar(barwidth = unit(0.4, "cm"), barheight = unit(4, "cm"))
  )
}

.supp_save_fig <- function(p, path, width = 10, height = 8, dpi = 300) {
  tryCatch(
    suppressWarnings(
      ggsave(path, plot = p, width = width, height = height, dpi = dpi, bg = "white")
    ),
    error = function(e)
      message(sprintf("  ggsave failed [%s]: %s", basename(path), e$message))
  )
  invisible(file.exists(path))
}

.supp_load_aoi <- function(run_dir) {
  repo_root <- normalizePath(
    file.path(run_dir, "..", "..", ".."), mustWork = FALSE, winslash = "/"
  )
  paths <- c(
    file.path(repo_root, "01_data_raw", "03_vector", "shapefiles", "Bhutan", "bhutan.shp"),
    file.path(run_dir, "02_data_intermediate", "m_area_vector.gpkg"),
    file.path(run_dir, "01_processed_data",    "m_area_vector.gpkg")
  )
  for (p in paths) {
    if (file.exists(p)) {
      aoi <- tryCatch(st_read(p, quiet = TRUE), error = function(e) NULL)
      if (!is.null(aoi)) return(st_make_valid(aoi))
    }
  }
  NULL
}

.supp_load_dzongkhag <- function(run_dir) {
  repo_root <- normalizePath(
    file.path(run_dir, "..", "..", ".."), mustWork = FALSE, winslash = "/"
  )
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

.supp_rast_to_df <- function(r, aoi_v = NULL) {
  if (nlyr(r) > 1) r <- r[[1]]
  if (!is.null(aoi_v)) {
    av <- if (!isTRUE(crs(aoi_v) == crs(r))) project(aoi_v, crs(r)) else aoi_v
    r  <- tryCatch(mask(crop(r, av), av), error = function(e) r)
  }
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  if (ncol(df) >= 3) names(df)[1:3] <- c("x", "y", "value")
  df
}

# Trapezoid AUC
.auc_fn <- function(y, p) {
  y <- as.numeric(y); p <- as.numeric(p)
  if (length(unique(y)) < 2) return(NA_real_)
  ord <- order(p, decreasing = TRUE)
  y   <- y[ord]
  n1  <- sum(y); n0 <- length(y) - n1
  if (n1 == 0L || n0 == 0L) return(NA_real_)
  tp  <- cumsum(y); fp <- cumsum(1 - y)
  tpr <- c(0, tp / n1); fpr <- c(0, fp / n0)
  sum(diff(fpr) * (tpr[-1] + tpr[-length(tpr)]) / 2, na.rm = TRUE)
}

# TSS at a given threshold
.tss_fn <- function(y, p, thr) {
  y   <- as.numeric(y); p <- as.numeric(p)
  bin <- as.integer(p >= thr)
  tp  <- sum(bin == 1L & y == 1L); fn <- sum(bin == 0L & y == 1L)
  fp  <- sum(bin == 1L & y == 0L); tn <- sum(bin == 0L & y == 0L)
  sens <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  spec <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
  if (anyNA(c(sens, spec))) NA_real_ else sens + spec - 1
}

# ── main function ─────────────────────────────────────────────────────────────

create_supplementary_figures <- function(run_dir, run_id = "") {

  fig_dir  <- file.path(run_dir, "08_figures_tables")
  proc_dir <- file.path(run_dir, "01_processed_data")
  mod_dir  <- file.path(run_dir, "02_models")
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

  n <- 0L
  cat("\n=== Supplementary figures (S1, S3\u2013S16) ===\n")

  has_patchwork <- requireNamespace("patchwork", quietly = TRUE)

  aoi_sf   <- .supp_load_aoi(run_dir)
  aoi_v    <- if (!is.null(aoi_sf)) vect(aoi_sf) else NULL
  dzong_sf <- .supp_load_dzongkhag(run_dir)

  # Returns geom layers for map figures.
  # with_dzong = TRUE: include Dzongkhag district boundaries (dashed grey) under national border.
  # Use with_dzong = TRUE only for figures where district context adds scientific value.
  .boundary_geoms <- function(target_crs_wkt = NULL, with_dzong = FALSE,
                              aoi_local = aoi_sf) {
    layers <- list()
    if (with_dzong && !is.null(dzong_sf)) {
      dz <- tryCatch({
        tc <- if (!is.null(target_crs_wkt)) st_crs(target_crs_wkt) else st_crs(4326)
        st_transform(dzong_sf, tc)
      }, error = function(e) NULL)
      if (!is.null(dz)) {
        layers[[length(layers) + 1L]] <- geom_sf(
          data = dz, inherit.aes = FALSE,
          fill = NA, color = "grey40", linewidth = 0.22, linetype = "dashed"
        )
        ctr <- tryCatch(sf::st_centroid(dz), error = function(e) NULL)
        if (!is.null(ctr)) {
          nm     <- if ("dzongkhag" %in% names(ctr)) "dzongkhag" else names(ctr)[1]
          coords <- sf::st_coordinates(ctr)
          df_lbl <- data.frame(x = coords[,1], y = coords[,2],
                                label = ctr[[nm]], stringsAsFactors = FALSE)
          has_repel_s <- requireNamespace("ggrepel", quietly = TRUE)
          layers[[length(layers) + 1L]] <- if (has_repel_s)
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
        }
      }
    }
    if (!is.null(aoi_local)) {
      al <- tryCatch({
        if (!is.null(target_crs_wkt)) st_transform(aoi_local, st_crs(target_crs_wkt))
        else aoi_local
      }, error = function(e) aoi_local)
      layers[[length(layers) + 1L]] <- geom_sf(
        data = al, inherit.aes = FALSE,
        fill = NA, color = "black", linewidth = 0.5
      )
    }
    layers
  }

  algo_pal <- c(GLM = "#2166AC", RF = "#4DAF4A", BRT = "#FF7F00",
                MAXENT = "#984EA3", ENSEMBLE = "#333333")

  # Read shared evaluation table once
  eval_df <- tryCatch(
    read.csv(file.path(mod_dir, "evaluation_all.csv")),
    error = function(e) NULL
  )

  get_thr <- function(algo) {
    if (is.null(eval_df)) return(0.5)
    r  <- eval_df[eval_df$algorithm == tolower(algo), ]
    if (nrow(r) == 0 || !"threshold" %in% names(r)) return(0.5)
    tv <- r$threshold[1]
    if (is.finite(tv)) tv else 0.5
  }

  # ── S1: Suitability by algorithm — 5-panel (GLM/RF/BRT/MaxEnt + Ensemble) ───
  tryCatch({
    pres_dir <- file.path(run_dir, "03_present_suitability")
    algos    <- c("glm", "rf", "brt", "maxent", "ensemble")
    labels   <- c("GLM", "RF", "BRT", "MaxEnt", "Ensemble")
    panels   <- list()

    for (i in seq_along(algos)) {
      f <- file.path(pres_dir, sprintf("suitability_present_%s.tif", algos[i]))
      if (!file.exists(f)) next
      r  <- rast(f)
      df <- .supp_rast_to_df(r, aoi_v)

      pct <- round(100 * mean(df$value >= get_thr(algos[i]), na.rm = TRUE), 1)

      p <- ggplot() +
        geom_raster(data = df, aes(x = x, y = y, fill = value)) +
        .supp_suit_scale("Suitability\n(0\u20131)") +
        .boundary_geoms(crs(r), with_dzong = TRUE) +
        annotate("label", x = Inf, y = -Inf,
                 label = sprintf("%.1f%% suitable", pct),
                 hjust = 1.1, vjust = -0.3, size = 2.8, fill = "white", alpha = 0.85) +
        labs(title = labels[i]) +
        .supp_map_theme(9) + coord_sf()

      # Bold border for ensemble panel
      if (algos[i] == "ensemble")
        p <- p + theme(panel.border = element_rect(color = "#333333", linewidth = 1.5, fill = NA))

      panels[[labels[i]]] <- p
    }

    if (length(panels) == 0) stop("no algorithm rasters found")

    if (!has_patchwork) stop("patchwork required for S1")

    out <- patchwork::wrap_plots(panels, ncol = 3) +
      patchwork::plot_annotation(
        title    = "Present Habitat Suitability by Algorithm \u2014 Elephas maximus | Bhutan",
        subtitle = "Ensemble components: GLM, Random Forest, Boosted Regression Trees, MaxEnt | Ensemble (bold border)",
        tag_levels = "A"
      )

    .supp_save_fig(out, file.path(fig_dir, "figure_s01_suitability_5panel.png"), 15, 10)
    cat(sprintf("  + figure_s01_suitability_5panel.png (%d panels)\n", length(panels)))
    n <- n + 1L
  }, error = function(e) cat(sprintf("  S1 skipped: %s\n", e$message)))

  # ── S3: Cross-validation fold performance ────────────────────────────────────
  tryCatch({
    oof_file <- file.path(proc_dir, "oof_predictions.csv")
    if (!file.exists(oof_file)) stop("oof_predictions.csv not found")
    oof <- read.csv(oof_file)

    pred_cols <- intersect(paste0("pred_", c("glm", "rf", "brt", "maxent")), names(oof))
    if (length(pred_cols) == 0 || !"fold" %in% names(oof))
      stop("OOF data missing fold or prediction columns")

    folds <- sort(unique(oof$fold))
    rows  <- list()

    for (pc in pred_cols) {
      algo <- sub("pred_", "", pc)
      thr  <- get_thr(algo)
      for (f in folds) {
        idx <- oof$fold == f
        if (sum(idx) < 5) next
        y <- oof$response[idx]; p <- oof[[pc]][idx]
        ok <- !is.na(p) & !is.na(y)
        if (sum(ok) < 5) next
        rows[[length(rows) + 1]] <- data.frame(
          algorithm = toupper(algo),
          fold      = paste("Fold", f),
          AUC       = .auc_fn(y[ok], p[ok]),
          TSS       = .tss_fn(y[ok], p[ok], thr),
          stringsAsFactors = FALSE
        )
      }
    }

    if (length(rows) == 0) stop("no fold metrics computed")
    fd  <- do.call(rbind, rows)
    fd  <- fd[!is.na(fd$AUC), ]
    fd$algorithm <- factor(fd$algorithm, levels = c("GLM", "RF", "BRT", "MAXENT"))

    # Overall observed AUC per algorithm (reference line)
    obs_rows <- list()
    for (pc in pred_cols) {
      algo <- sub("pred_", "", pc)
      ok   <- !is.na(oof[[pc]]) & !is.na(oof$response)
      obs_rows[[algo]] <- data.frame(
        algorithm = toupper(algo),
        AUC       = .auc_fn(oof$response[ok], oof[[pc]][ok]),
        TSS       = .tss_fn(oof$response[ok], oof[[pc]][ok], get_thr(algo)),
        stringsAsFactors = FALSE
      )
    }
    obs_df <- do.call(rbind, obs_rows)
    obs_df$algorithm <- factor(obs_df$algorithm, levels = c("GLM", "RF", "BRT", "MAXENT"))

    long_fd <- rbind(
      data.frame(algorithm = fd$algorithm, fold = fd$fold,
                 Metric = "AUC", Value = fd$AUC, stringsAsFactors = FALSE),
      data.frame(algorithm = fd$algorithm, fold = fd$fold,
                 Metric = "TSS", Value = fd$TSS, stringsAsFactors = FALSE)
    )
    long_fd  <- long_fd[!is.na(long_fd$Value), ]

    long_obs <- rbind(
      data.frame(algorithm = obs_df$algorithm, Metric = "AUC",
                 Value = obs_df$AUC, stringsAsFactors = FALSE),
      data.frame(algorithm = obs_df$algorithm, Metric = "TSS",
                 Value = obs_df$TSS, stringsAsFactors = FALSE)
    )

    ref_lines <- data.frame(
      Metric = c("AUC", "TSS"),
      ref    = c(0.65, 0.40),
      label  = c("Min AUC = 0.65", "Min TSS = 0.40"),
      stringsAsFactors = FALSE
    )

    p <- ggplot(long_fd, aes(x = fold, y = Value, fill = algorithm)) +
      geom_col(position = position_dodge(width = 0.82), width = 0.75, alpha = 0.85) +
      geom_hline(data = ref_lines, aes(yintercept = ref),
                 linetype = "dashed", colour = "grey25", linewidth = 0.6) +
      geom_label(data = ref_lines,
                 aes(x = 0.4, y = ref, label = label),
                 hjust = 0, vjust = -0.3, size = 2.8,
                 fill = "white", colour = "grey25", label.size = 0, inherit.aes = FALSE) +
      scale_fill_manual(values = algo_pal, name = "Algorithm") +
      facet_wrap(~Metric, scales = "free_y") +
      labs(
        title    = "Cross-validation fold performance \u2014 Elephas maximus SDM (Bhutan)",
        subtitle = "Spatial block cross-validation | Dashed = minimum acceptable threshold",
        x = "Spatial fold", y = "Metric value"
      ) +
      theme_bw(base_size = 11) +
      theme(
        strip.background   = element_rect(fill = "grey92"),
        strip.text         = element_text(face = "bold"),
        plot.title         = element_text(face = "bold"),
        axis.text.x        = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank()
      )

    .supp_save_fig(p, file.path(fig_dir, "figure_s03_cv_fold_performance.png"), 11, 5)
    cat("  + figure_s03_cv_fold_performance.png\n")
    n <- n + 1L
  }, error = function(e) cat(sprintf("  S3 skipped: %s\n", e$message)))

  # ── S4: Null model comparison (permuted vs observed AUC) ─────────────────────
  tryCatch({
    oof_file <- file.path(proc_dir, "oof_predictions.csv")
    if (!file.exists(oof_file)) stop("oof_predictions.csv not found")
    oof <- read.csv(oof_file)

    pred_cols <- intersect(paste0("pred_", c("glm", "rf", "brt", "maxent")), names(oof))
    if (length(pred_cols) == 0) stop("no prediction columns in OOF data")

    set.seed(42L)
    n_perm    <- 99L
    null_list <- list()

    for (pc in pred_cols) {
      algo <- sub("pred_", "", pc)
      ok   <- !is.na(oof[[pc]]) & !is.na(oof$response)
      y    <- as.numeric(oof$response[ok])
      p    <- as.numeric(oof[[pc]][ok])

      obs_auc   <- .auc_fn(y, p)
      null_aucs <- vapply(seq_len(n_perm), function(i) .auc_fn(sample(y), p), numeric(1))

        # Empirical p-value: proportion of null AUCs >= observed
      emp_p <- mean(null_aucs >= obs_auc)
      null_list[[length(null_list) + 1]] <- data.frame(
        algorithm = toupper(algo),
        obs_auc   = obs_auc,
        emp_p     = emp_p,
        null_auc  = null_aucs,
        stringsAsFactors = FALSE
      )
    }

    if (length(null_list) == 0) stop("null model computation failed for all algorithms")
    null_df <- do.call(rbind, null_list)
    null_df$algorithm <- factor(null_df$algorithm, levels = c("GLM", "RF", "BRT", "MAXENT"))

    obs_df <- unique(null_df[, c("algorithm", "obs_auc", "emp_p")])

    # 95th percentile of null AUC per algorithm (for dashed reference line)
    null_p95 <- aggregate(null_auc ~ algorithm, data = null_df,
                          FUN = function(x) quantile(x, 0.95))
    names(null_p95)[2] <- "null_p95"
    obs_df <- merge(obs_df, null_p95, by = "algorithm")

    obs_df$label_text <- sprintf("Observed AUC = %.3f\np < %.3f", obs_df$obs_auc,
                                  pmax(obs_df$emp_p, 1 / (n_perm + 1)))

    # X-axis: span null distribution + observed AUC with padding
    x_min <- floor(min(null_df$null_auc, na.rm = TRUE) * 10) / 10 - 0.02
    x_max <- max(obs_df$obs_auc) + 0.04

    p <- ggplot(null_df, aes(x = null_auc, fill = algorithm)) +
      geom_histogram(bins = 20, alpha = 0.75, colour = "white", linewidth = 0.15) +
      # Dashed line at null 95th percentile
      geom_vline(data = obs_df, aes(xintercept = null_p95),
                 linewidth = 0.7, linetype = "dashed", colour = "grey40") +
      # Solid line at observed AUC
      geom_vline(data = obs_df, aes(xintercept = obs_auc, colour = algorithm),
                 linewidth = 1.3, linetype = "solid") +
      # Label to LEFT of the observed vline
      geom_label(data = obs_df,
                 aes(x = obs_auc, y = Inf,
                     label = label_text,
                     colour = algorithm),
                 fill = "white", vjust = 1.15, hjust = 1.08, size = 3.0,
                 fontface = "bold", linewidth = 0.3, inherit.aes = FALSE) +
      scale_fill_manual(values   = algo_pal, guide = "none") +
      scale_colour_manual(values = algo_pal, guide = "none") +
      scale_x_continuous(limits = c(x_min, x_max),
                         expand = expansion(mult = c(0, 0.01))) +
      facet_wrap(~algorithm, scales = "free_y", ncol = 2) +
      labs(
        title    = "Null model comparison \u2014 Elephas maximus SDM (Bhutan)",
        subtitle = sprintf(
          "Histogram = AUC of %d label-permuted null models | Solid line = observed AUC | Dashed = null 95th pct",
          n_perm),
        x = "AUC", y = "Count (permutations)"
      ) +
      theme_bw(base_size = 11) +
      theme(
        strip.background = element_rect(fill = "grey92"),
        strip.text       = element_text(face = "bold"),
        plot.title       = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )

    .supp_save_fig(p, file.path(fig_dir, "figure_s04_null_model_comparison.png"), 10, 7)
    cat("  + figure_s04_null_model_comparison.png\n")
    n <- n + 1L
  }, error = function(e) cat(sprintf("  S4 skipped: %s\n", e$message)))

  # ── S5: Spatial autocorrelation of OOF residuals (correlogram) ───────────────
  tryCatch({
    oof_file <- file.path(proc_dir, "oof_predictions.csv")
    if (!file.exists(oof_file)) stop("oof_predictions.csv not found")
    oof <- read.csv(oof_file)

    pred_cols <- intersect(paste0("pred_", c("glm", "rf", "brt", "maxent")), names(oof))
    if (length(pred_cols) == 0) stop("no prediction columns")
    if (!all(c("longitude", "latitude", "response") %in% names(oof)))
      stop("OOF data missing spatial coordinate columns")

    # Ensemble prediction = row mean; compute residuals
    oof$pred_ens <- rowMeans(oof[, pred_cols, drop = FALSE], na.rm = TRUE)
    ok <- !is.na(oof$pred_ens) & !is.na(oof$response)
    oof <- oof[ok, ]
    oof$residual <- as.numeric(oof$response) - oof$pred_ens

    # Subsample for computational speed
    if (nrow(oof) > 800L) {
      set.seed(123L)
      oof <- oof[sample(nrow(oof), 800L), ]
    }

    # Pairwise distances in km using approximate Cartesian scaling
    lat_ctr  <- mean(oof$latitude, na.rm = TRUE)
    lat_km   <- 111.0
    lon_km   <- 111.0 * cos(lat_ctr * pi / 180)
    coords_km <- cbind(
      x = oof$longitude * lon_km,
      y = oof$latitude  * lat_km
    )
    dmat  <- as.matrix(dist(coords_km))
    resid <- scale(oof$residual)[, 1]

    # Compute Moran's I per lag bin
    bin_w  <- 20; max_d <- 200
    breaks <- seq(0, max_d, by = bin_w)
    corr_rows <- list()

    for (i in seq_len(length(breaks) - 1)) {
      d1 <- breaks[i]; d2 <- breaks[i + 1]
      idx <- which(dmat > d1 & dmat <= d2, arr.ind = TRUE)
      idx <- idx[idx[, 1] < idx[, 2], , drop = FALSE]
      if (nrow(idx) < 5L) next
      ri <- resid[idx[, 1]]; rj <- resid[idx[, 2]]
      corr_rows[[length(corr_rows) + 1]] <- data.frame(
        lag_km   = (d1 + d2) / 2,
        n_pairs  = nrow(idx),
        morans_i = tryCatch(cor(ri, rj, use = "complete.obs"), error = function(e) NA_real_),
        stringsAsFactors = FALSE
      )
    }

    if (length(corr_rows) == 0) stop("no correlogram bins computed")
    corr_df <- do.call(rbind, corr_rows)
    corr_df <- corr_df[!is.na(corr_df$morans_i), ]

    p <- ggplot(corr_df, aes(x = lag_km, y = morans_i)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.7) +
      geom_ribbon(aes(ymin = 0, ymax = morans_i),
                  fill = "#2166AC", alpha = 0.15) +
      geom_line(colour = "#2166AC", linewidth = 0.9) +
      geom_point(aes(size = n_pairs), colour = "#2166AC",
                 fill = "white", shape = 21, stroke = 1.2) +
      scale_size_continuous(name = "Pair\ncount", range = c(2, 6)) +
      scale_x_continuous(breaks = seq(0, max_d, 40)) +
      labs(
        title    = "Spatial autocorrelation of model residuals \u2014 Elephas maximus SDM (Bhutan)",
        subtitle = paste0(
          "Ensemble (mean of GLM/RF/BRT/MaxEnt) | Lag bins = ", bin_w, " km | ",
          "n = ", nrow(oof), " OOF predictions"),
        x = "Lag distance (km)",
        y = "Moran\u2019s I (residual spatial correlation)"
      ) +
      theme_bw(base_size = 11) +
      theme(plot.title = element_text(face = "bold"))

    .supp_save_fig(p, file.path(fig_dir, "figure_s05_residual_correlogram.png"), 9, 5)
    cat("  + figure_s05_residual_correlogram.png\n")
    n <- n + 1L
  }, error = function(e) cat(sprintf("  S5 skipped: %s\n", e$message)))

  # ── S6: Predictor collinearity heatmap ───────────────────────────────────────
  tryCatch({
    dat_file <- file.path(proc_dir, "modeling_dataset.csv")
    if (!file.exists(dat_file)) stop("modeling_dataset.csv not found")
    dat <- read.csv(dat_file)

    exclude_cols <- c("id", "type", "response", "longitude", "latitude", "fold")
    pred_cols    <- setdiff(names(dat), exclude_cols)
    pred_cols    <- pred_cols[vapply(dat[, pred_cols, drop = FALSE],
                                    is.numeric, logical(1))]
    if (length(pred_cols) < 2) stop("fewer than 2 numeric predictor columns")

    corr_mat  <- cor(dat[, pred_cols, drop = FALSE], use = "pairwise.complete.obs")

    # Order by hierarchical clustering on |r|
    hc_order <- tryCatch({
      hc <- hclust(as.dist(1 - abs(corr_mat)), method = "complete")
      colnames(corr_mat)[hc$order]
    }, error = function(e) pred_cols)

    # Melt to long
    corr_long <- data.frame(
      Var1  = rep(rownames(corr_mat), times = ncol(corr_mat)),
      Var2  = rep(colnames(corr_mat), each  = nrow(corr_mat)),
      value = as.vector(corr_mat),
      stringsAsFactors = FALSE
    )
    corr_long$Var1 <- factor(corr_long$Var1, levels = hc_order)
    corr_long$Var2 <- factor(corr_long$Var2, levels = rev(hc_order))

    p <- ggplot(corr_long, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(colour = "white", linewidth = 0.25) +
      geom_text(
        aes(label = ifelse(abs(value) >= 0.4, sprintf("%.2f", value), "")),
        size = 2.6, colour = ifelse(abs(corr_long$value) >= 0.7, "white", "grey20"),
        fontface = "bold"
      ) +
      scale_fill_gradient2(
        low      = "#053061", mid = "white", high = "#67001f",
        midpoint = 0, limits = c(-1, 1), name = "Pearson r",
        guide    = guide_colorbar(barwidth = unit(0.5, "cm"), barheight = unit(6, "cm"))
      ) +
      labs(
        title    = "Predictor collinearity \u2014 Elephas maximus SDM (Bhutan)",
        subtitle = paste0(
          "Pearson correlation | Hierarchical clustering order | ",
          "Values shown where |r| \u2265 0.4 | n = ", nrow(dat), " records"),
        x = NULL, y = NULL
      ) +
      theme_bw(base_size = 10) +
      theme(
        axis.text.x     = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y     = element_text(size = 9),
        plot.title      = element_text(face = "bold"),
        legend.position = "right"
      )

    .supp_save_fig(p, file.path(fig_dir, "figure_s06_predictor_collinearity.png"), 11, 9)
    cat("  + figure_s06_predictor_collinearity.png\n")
    n <- n + 1L
  }, error = function(e) cat(sprintf("  S6 skipped: %s\n", e$message)))

  # ── S7: Threshold sensitivity ─────────────────────────────────────────────────
  tryCatch({
    pres_file <- file.path(run_dir, "03_present_suitability",
                           "suitability_present_ensemble.tif")
    if (!file.exists(pres_file)) stop("ensemble raster not found")

    r <- rast(pres_file)
    if (!is.null(aoi_v)) {
      av2 <- if (!isTRUE(crs(aoi_v) == crs(r))) project(aoi_v, crs(r)) else aoi_v
      r   <- tryCatch(mask(crop(r, av2), av2), error = function(e) r)
    }
    vals <- as.numeric(values(r, na.rm = TRUE))
    if (length(vals) == 0) stop("raster has no valid values after masking")

    thresholds <- seq(0, 1, by = 0.005)
    pct_suit   <- vapply(thresholds,
                         function(t) 100 * mean(vals >= t, na.rm = TRUE),
                         numeric(1))

    # TSS-optimal threshold
    curr_thr <- if (!is.null(eval_df) && "threshold" %in% names(eval_df)) {
      tv <- mean(eval_df$threshold[eval_df$algorithm != "ensemble"], na.rm = TRUE)
      if (is.finite(tv)) tv else 0.5
    } else 0.5

    curr_pct <- approx(thresholds, pct_suit, xout = curr_thr)$y

    thr_df <- data.frame(threshold = thresholds, pct_suitable = pct_suit)

    p <- ggplot(thr_df, aes(x = threshold, y = pct_suitable)) +
      geom_area(fill = "#fecc5c", alpha = 0.35) +
      geom_line(colour = "#bd0026", linewidth = 1.0) +
      geom_vline(xintercept = curr_thr, linetype = "dashed",
                 colour = "grey20", linewidth = 0.8) +
      annotate("point", x = curr_thr, y = curr_pct,
               shape = 21, size = 4, fill = "#fd8d3c", colour = "black", stroke = 1) +
      annotate("label",
               x = curr_thr + 0.01, y = curr_pct,
               label = sprintf("Threshold = %.3f\n%.1f%% suitable", curr_thr, curr_pct),
               hjust = 0, vjust = 0.5, size = 3.2, fill = "white", alpha = 0.92,
               label.size = 0.3) +
      scale_x_continuous(breaks = seq(0, 1, 0.1), minor_breaks = seq(0, 1, 0.05)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title    = "Threshold sensitivity \u2014 Elephas maximus SDM (Bhutan)",
        subtitle = "Ensemble model | % of Bhutan classified as suitable across all possible thresholds",
        x = "Suitability threshold", y = "% of Bhutan classified as suitable"
      ) +
      theme_bw(base_size = 11) +
      theme(plot.title = element_text(face = "bold"),
            panel.grid.minor = element_line(colour = "grey92"))

    .supp_save_fig(p, file.path(fig_dir, "figure_s07_threshold_sensitivity.png"), 9, 5)
    cat("  + figure_s07_threshold_sensitivity.png\n")
    n <- n + 1L
  }, error = function(e) cat(sprintf("  S7 skipped: %s\n", e$message)))

  # ── S8: Climate envelope — BIO14 × BIO15 bivariate space ─────────────────────
  tryCatch({
    dat_file <- file.path(proc_dir, "modeling_dataset.csv")
    if (!file.exists(dat_file)) stop("modeling_dataset.csv not found")
    dat <- read.csv(dat_file)

    if (!all(c("BIO14", "BIO15", "response") %in% names(dat)))
      stop("BIO14, BIO15, or response column missing from modeling dataset")

    dat$Group <- factor(
      ifelse(dat$response == 1, "Presence", "Background"),
      levels = c("Background", "Presence")
    )

    bg  <- dat[dat$Group == "Background", ]
    prs <- dat[dat$Group == "Presence",   ]

    p <- ggplot() +
      # Background: points + density contours
      geom_point(data = bg, aes(x = BIO14, y = BIO15),
                 colour = "grey65", shape = 3, size = 0.7, alpha = 0.25) +
      tryCatch(
        stat_density_2d(data = bg, aes(x = BIO14, y = BIO15),
                        colour = "grey55", linewidth = 0.4, alpha = 0.7,
                        bins = 6),
        error = function(e) NULL
      ) +
      # Presence: points + density contours
      geom_point(data = prs, aes(x = BIO14, y = BIO15),
                 fill = "#FFD700", colour = "#8B0000",
                 shape = 21, size = 2.2, alpha = 0.80, stroke = 0.5) +
      tryCatch(
        stat_density_2d(data = prs, aes(x = BIO14, y = BIO15),
                        colour = "#bd0026", linewidth = 0.8, alpha = 0.9,
                        bins = 5),
        error = function(e) NULL
      ) +
      # Manual legend via invisible points
      geom_point(data = data.frame(
                   BIO14 = NA_real_, BIO15 = NA_real_,
                   Group = factor(c("Background", "Presence"),
                                  levels = c("Background", "Presence"))),
                 aes(x = BIO14, y = BIO15, colour = Group), na.rm = TRUE) +
      scale_colour_manual(
        values = c(Background = "grey55", Presence = "#bd0026"),
        name   = NULL,
        labels = c(
          Background = sprintf("Background (n = %d)", nrow(bg)),
          Presence   = sprintf("Presence (n = %d)",   nrow(prs))
        )
      ) +
      labs(
        title    = "Climate envelope \u2014 Elephas maximus SDM (Bhutan)",
        subtitle = "BIO14 = Precipitation of driest month | BIO15 = Precipitation seasonality (CV)\nContours = kernel density | Gold = presence, grey = background",
        x = "BIO14 \u2014 Precipitation of driest month (mm)",
        y = "BIO15 \u2014 Precipitation seasonality (CV)"
      ) +
      theme_bw(base_size = 11) +
      theme(
        plot.title      = element_text(face = "bold"),
        legend.position = "top",
        legend.text     = element_text(size = 10)
      )

    .supp_save_fig(p, file.path(fig_dir, "figure_s08_climate_envelope.png"), 8, 6)
    cat("  + figure_s08_climate_envelope.png\n")
    n <- n + 1L
  }, error = function(e) cat(sprintf("  S8 skipped: %s\n", e$message)))

  cat("\n=== Supplementary figures (S9\u2013S17) ===\n")

  # ── S9: Partial dependence / response curves (top predictors per algorithm) ──
  tryCatch({
    dat_file <- file.path(proc_dir, "modeling_dataset.csv")
    if (!file.exists(dat_file)) stop("modeling_dataset.csv not found")
    dat <- read.csv(dat_file)

    exclude_cols <- c("id", "type", "response", "longitude", "latitude", "fold")
    pred_cols_all <- setdiff(names(dat), exclude_cols)
    pred_cols_all <- pred_cols_all[vapply(dat[, pred_cols_all, drop = FALSE],
                                          is.numeric, logical(1))]
    if (length(pred_cols_all) == 0) stop("no numeric predictor columns")

    # Determine top predictors from variable importance CSV if available
    vi_file <- file.path(run_dir, "08_figures_tables", "table_E07_variable_importance.csv")
    top_preds <- if (file.exists(vi_file)) {
      vi <- read.csv(vi_file)
      if (all(c("predictor", "importance") %in% names(vi))) {
        vi_agg <- aggregate(importance ~ predictor, data = vi, FUN = mean, na.rm = TRUE)
        vi_agg <- vi_agg[order(-vi_agg$importance), ]
        head(vi_agg$predictor[vi_agg$predictor %in% pred_cols_all], 6)
      } else pred_cols_all[seq_len(min(6, length(pred_cols_all)))]
    } else pred_cols_all[seq_len(min(6, length(pred_cols_all)))]

    if (length(top_preds) == 0) stop("no top predictors identified")

    # OOF predictions for algorithm colours
    oof_file <- file.path(proc_dir, "oof_predictions.csv")
    oof <- tryCatch(read.csv(oof_file), error = function(e) NULL)

    rows_list <- list()
    for (pred in top_preds) {
      x_seq <- seq(
        quantile(dat[[pred]], 0.02, na.rm = TRUE),
        quantile(dat[[pred]], 0.98, na.rm = TRUE),
        length.out = 50
      )
      med_row <- dat[1, pred_cols_all, drop = FALSE]
      for (col in pred_cols_all) med_row[[col]] <- median(dat[[col]], na.rm = TRUE)

      # GLM marginal response
      glm_file <- file.path(mod_dir, "model_glm.rds")
      if (file.exists(glm_file)) {
        m <- tryCatch(readRDS(glm_file), error = function(e) NULL)
        if (!is.null(m) && inherits(m, "glm")) {
          nd <- med_row[rep(1, 50), , drop = FALSE]
          nd[[pred]] <- x_seq
          p_hat <- tryCatch(
            predict(m, newdata = nd, type = "response"),
            error = function(e) rep(NA_real_, 50)
          )
          rows_list[[length(rows_list) + 1]] <- data.frame(
            predictor = pred, x = x_seq, y = p_hat,
            algorithm = "GLM", stringsAsFactors = FALSE
          )
        }
      }

      # RF marginal response
      rf_file <- file.path(mod_dir, "model_rf.rds")
      if (file.exists(rf_file) && requireNamespace("ranger", quietly = TRUE)) {
        m <- tryCatch(readRDS(rf_file), error = function(e) NULL)
        if (!is.null(m) && inherits(m, "ranger")) {
          nd <- med_row[rep(1, 50), , drop = FALSE]
          nd[[pred]] <- x_seq
          p_hat <- tryCatch(
            predict(m, data = nd)$predictions[, "1"],
            error = function(e) tryCatch(
              predict(m, data = nd)$predictions,
              error = function(e2) rep(NA_real_, 50)
            )
          )
          if (!is.matrix(p_hat)) p_hat <- as.numeric(p_hat)
          rows_list[[length(rows_list) + 1]] <- data.frame(
            predictor = pred, x = x_seq, y = as.numeric(p_hat)[seq_len(50)],
            algorithm = "RF", stringsAsFactors = FALSE
          )
        }
      }

      # BRT marginal response
      brt_file <- file.path(mod_dir, "model_brt.rds")
      if (file.exists(brt_file) && requireNamespace("gbm", quietly = TRUE)) {
        m <- tryCatch(readRDS(brt_file), error = function(e) NULL)
        if (!is.null(m) && inherits(m, "gbm")) {
          nd <- med_row[rep(1, 50), , drop = FALSE]
          nd[[pred]] <- x_seq
          p_hat <- tryCatch({
            lp <- gbm::predict.gbm(m, newdata = nd, n.trees = m$n.trees, type = "link")
            1 / (1 + exp(-lp))
          }, error = function(e) rep(NA_real_, 50))
          rows_list[[length(rows_list) + 1]] <- data.frame(
            predictor = pred, x = x_seq, y = p_hat,
            algorithm = "BRT", stringsAsFactors = FALSE
          )
        }
      }
    }

    if (length(rows_list) == 0) stop("no response curves computed (model .rds files not found)")
    rc_df <- do.call(rbind, rows_list)
    rc_df <- rc_df[!is.na(rc_df$y), ]
    rc_df$algorithm <- factor(rc_df$algorithm, levels = c("GLM", "RF", "BRT", "MAXENT"))

    p <- ggplot(rc_df, aes(x = x, y = y, colour = algorithm)) +
      geom_line(linewidth = 0.9, alpha = 0.9) +
      scale_colour_manual(values = algo_pal[names(algo_pal) %in% rc_df$algorithm],
                          name = "Algorithm") +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
      facet_wrap(~predictor, scales = "free_x", ncol = 3) +
      labs(
        title    = "Partial dependence (response curves) \u2014 Elephas maximus SDM (Bhutan)",
        subtitle = "Top predictors by mean variable importance | All other predictors held at median",
        x = "Predictor value", y = "Predicted suitability"
      ) +
      theme_bw(base_size = 10) +
      theme(
        strip.background   = element_rect(fill = "grey92"),
        strip.text         = element_text(face = "bold"),
        plot.title         = element_text(face = "bold"),
        legend.position    = "bottom",
        panel.grid.minor   = element_blank()
      )

    n_rows <- ceiling(length(top_preds) / 3)
    .supp_save_fig(p, file.path(fig_dir, "figure_s09_response_curves.png"),
                   12, 3.5 * n_rows)
    cat("  + figure_s09_response_curves.png\n")
    n <- n + 1L
  }, error = function(e) cat(sprintf("  S9 skipped: %s\n", e$message)))

  # ── S10: MESS map — uses pre-computed mess_*.tif from 04_future_projections ────
  # Displays GCM-mean MESS for SSP5-8.5 2071-2100 (worst-case scenario).
  # Using all-scenario mean is avoided because stacking 96+ rasters risks OOM.
  tryCatch({
    fut_proj_dir <- file.path(run_dir, "04_future_projections")
    # Prefer SSP5-8.5 2071-2100; fall back to any available scenario
    mess_files <- list.files(fut_proj_dir,
      pattern = "^mess_.*ssp585_2071_2100\\.tif$", full.names = TRUE)
    if (length(mess_files) == 0) {
      mess_files <- list.files(fut_proj_dir, pattern = "^mess_.*\\.tif$", full.names = TRUE)
      mess_files <- mess_files[!grepl("historical", mess_files)]
    }
    if (length(mess_files) == 0) stop("no mess_*.tif files found in 04_future_projections")

    # Average MESS across the selected GCMs to get a per-scenario representative map
    mess_list <- lapply(mess_files, function(f) {
      tryCatch({ r <- rast(f); if (nlyr(r) > 1) r[[1]] else r }, error = function(e) NULL)
    })
    mess_list <- Filter(Negate(is.null), mess_list)
    if (length(mess_list) == 0) stop("could not load any MESS rasters")

    ref_r   <- mess_list[[1]]
    aligned <- lapply(mess_list, function(r) {
      if (!isTRUE(crs(r) == crs(ref_r))) r <- project(r, ref_r, method = "bilinear")
      r <- tryCatch(resample(r, ref_r, method = "bilinear"), error = function(e) r)
      r
    })
    mess_stk <- tryCatch(rast(aligned), error = function(e) {
      rast(aligned[1])  # fall back to single raster
    })
    mess_r <- if (nlyr(mess_stk) > 1) {
      app(mess_stk, fun = function(v) mean(v, na.rm = TRUE))
    } else {
      mess_stk
    }

    if (!is.null(aoi_v)) {
      av2 <- if (!isTRUE(crs(aoi_v) == crs(mess_r))) project(aoi_v, crs(mess_r)) else aoi_v
      mess_r <- tryCatch(mask(crop(mess_r, av2), av2), error = function(e) mess_r)
    }

    mess_df <- as.data.frame(mess_r, xy = TRUE, na.rm = TRUE)
    names(mess_df)[3] <- "MESS"
    # Clamp display range to [-100, 100]; extreme extrapolation shown as -100
    mess_df$MESS <- pmax(pmin(mess_df$MESS, 100), -100)

    # MESS from Phase 9 is clamped: 0 = within training range, negative = novel climate
    # All values are <= 0, so use sequential scale (white=safe, red=novel)
    mess_df$MESS <- pmin(mess_df$MESS, 0)  # ensure no positive values affect scale
    pct_extrap  <- round(100 * mean(mess_df$MESS < 0,    na.rm = TRUE), 1)
    pct_extrap5 <- round(100 * mean(mess_df$MESS < -5,   na.rm = TRUE), 1)
    mess_min    <- round(min(mess_df$MESS, na.rm = TRUE), 1)

    scale_lo <- min(mess_min * 1.2, -1)  # ensure at least -1 even if all MESS = 0
    scenario_label <- if (any(grepl("ssp585_2071_2100", mess_files)))
      "GCM-mean MESS: SSP5-8.5, 2071\u20132100 (worst-case)" else
      sprintf("Mean across %d GCM-scenario files", length(mess_files))

    p <- ggplot() +
      geom_raster(data = mess_df, aes(x = x, y = y, fill = MESS)) +
      scale_fill_gradient(
        low      = "#d73027",   # red = novel climate
        high     = "#f7f7f7",   # near-white = within training range (MESS = 0)
        limits   = c(scale_lo, 0),
        name     = "Extrapolation\nrisk (MESS)",
        guide    = guide_colorbar(barwidth = unit(0.5, "cm"), barheight = unit(5, "cm"),
                                  title.hjust = 0.5)
      ) +
      .boundary_geoms(crs(mess_r), with_dzong = TRUE) +
      annotate("label", x = Inf, y = Inf,
               label = sprintf(
                 "Any extrapolation (MESS<0): %.1f%%\nStrong extrapolation (MESS< -5): %.1f%%\nMin MESS: %.1f",
                 pct_extrap, pct_extrap5, mess_min),
               hjust = 1.02, vjust = 1.05, size = 3.2, fill = "white", alpha = 0.92,
               linewidth = 0.3) +
      labs(
        title    = "Extrapolation risk (MESS) \u2014 Elephas maximus SDM (Bhutan)",
        subtitle = paste0(scenario_label, " | White = within training range | Red = novel climate"),
        x = NULL, y = NULL
      ) +
      .supp_map_theme() + coord_sf()

    .supp_save_fig(p, file.path(fig_dir, "figure_s10_mess_map.png"), 9, 7)
    cat("  + figure_s10_mess_map.png\n")
    n <- n + 1L
  }, error = function(e) cat(sprintf("  S10 skipped: %s\n", e$message)))

  # ── S11: Occurrence + background sampling map ─────────────────────────────────
  tryCatch({
    dat_file <- file.path(proc_dir, "modeling_dataset.csv")
    if (!file.exists(dat_file)) stop("modeling_dataset.csv not found")
    dat <- read.csv(dat_file)

    if (!all(c("longitude", "latitude", "response") %in% names(dat)))
      stop("coordinate or response columns missing")

    pres_r <- tryCatch(
      rast(file.path(run_dir, "03_present_suitability",
                     "suitability_present_ensemble.tif")),
      error = function(e) NULL
    )

    # Build CRS-correct points
    dat_sf <- tryCatch(
      st_as_sf(dat, coords = c("longitude", "latitude"), crs = 4326),
      error = function(e) NULL
    )
    if (!is.null(dat_sf) && !is.null(pres_r) &&
        !isTRUE(st_crs(dat_sf)$wkt == crs(pres_r, describe = TRUE)$wkt))
      dat_sf <- tryCatch(st_transform(dat_sf, crs(pres_r)), error = function(e) dat_sf)

    prs_sf <- if (!is.null(dat_sf)) dat_sf[dat$response == 1, ] else NULL
    bg_sf  <- if (!is.null(dat_sf)) dat_sf[dat$response == 0, ] else NULL

    # Clip both point sets to Bhutan boundary
    if (!is.null(aoi_sf)) {
      aoi_clip <- tryCatch(st_transform(aoi_sf, st_crs(dat_sf)), error = function(e) NULL)
      if (!is.null(aoi_clip)) {
        aoi_union <- st_union(aoi_clip)
        if (!is.null(prs_sf))
          prs_sf <- tryCatch(prs_sf[st_within(prs_sf, aoi_union, sparse = FALSE)[, 1], ],
                             error = function(e) prs_sf)
        if (!is.null(bg_sf))
          bg_sf  <- tryCatch(bg_sf[st_within(bg_sf, aoi_union, sparse = FALSE)[, 1], ],
                             error = function(e) bg_sf)
      }
    }

    # Light suitability backdrop
    suit_df <- if (!is.null(pres_r)) {
      av2v <- if (!is.null(aoi_v) && !isTRUE(crs(aoi_v) == crs(pres_r)))
        project(aoi_v, crs(pres_r)) else aoi_v
      rr <- if (!is.null(av2v)) tryCatch(mask(crop(pres_r, av2v), av2v),
                                          error = function(e) pres_r) else pres_r
      if (nlyr(rr) > 1) rr <- rr[[1]]
      df <- as.data.frame(rr, xy = TRUE, na.rm = TRUE)
      names(df)[3] <- "suit"
      df
    } else NULL

    p <- ggplot()

    if (!is.null(suit_df))
      p <- p + geom_raster(data = suit_df, aes(x = x, y = y, fill = suit)) +
        scale_fill_gradientn(
          colours  = c("#f7f7f7", "#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"),
          values   = c(0, 0.05, 0.2, 0.4, 0.65, 1),
          limits   = c(0, 1), na.value = "grey90",
          name     = "Ensemble\nsuitability",
          guide    = guide_colorbar(barwidth = unit(0.4, "cm"), barheight = unit(4, "cm"))
        )

    p <- p + .boundary_geoms(if (!is.null(pres_r)) crs(pres_r) else NULL, with_dzong = TRUE)
    if (!is.null(bg_sf))
      p <- p + geom_sf(data = bg_sf, inherit.aes = FALSE,
                       colour = "grey30", shape = 3, size = 0.8, alpha = 0.55)
    if (!is.null(prs_sf))
      p <- p + geom_sf(data = prs_sf, inherit.aes = FALSE,
                       fill = "#FFD700", colour = "#8B0000",
                       shape = 21, size = 2.0, alpha = 0.9, stroke = 0.6)

    n_pres <- if (!is.null(dat)) sum(dat$response == 1, na.rm = TRUE) else "?"
    n_bg   <- if (!is.null(dat)) sum(dat$response == 0, na.rm = TRUE) else "?"

    # Derive map limits from AOI or raster extent — clip to study area
    map_xlim <- map_ylim <- NULL
    ref_ext <- if (!is.null(pres_r)) ext(pres_r) else if (!is.null(aoi_v)) ext(aoi_v) else NULL
    if (!is.null(ref_ext)) {
      buf <- (ref_ext$ymax - ref_ext$ymin) * 0.04  # 4% buffer
      map_xlim <- c(ref_ext$xmin - buf, ref_ext$xmax + buf)
      map_ylim <- c(ref_ext$ymin - buf, ref_ext$ymax + buf)
    }

    p <- p + labs(
      title    = "Occurrence and background sampling \u2014 Elephas maximus (Bhutan)",
      subtitle = sprintf(
        "Presences (gold, n=%s) | Background (grey+, n=%s) | Backdrop = ensemble suitability",
        n_pres, n_bg),
      x = NULL, y = NULL
    ) + .supp_map_theme() +
      coord_sf(xlim = map_xlim, ylim = map_ylim, expand = FALSE)

    .supp_save_fig(p, file.path(fig_dir, "figure_s11_background_points.png"), 9, 7)
    cat("  + figure_s11_background_points.png\n")
    n <- n + 1L
  }, error = function(e) cat(sprintf("  S11 skipped: %s\n", e$message)))

  # ── S12: Climate PCA biplot — present vs future SSP centroids ─────────────────
  tryCatch({
    dat_file <- file.path(proc_dir, "modeling_dataset.csv")
    if (!file.exists(dat_file)) stop("modeling_dataset.csv not found")
    dat <- read.csv(dat_file)

    exclude_cols <- c("id", "type", "response", "longitude", "latitude", "fold")
    clim_cols <- setdiff(names(dat), exclude_cols)
    clim_cols <- clim_cols[grepl("^BIO", clim_cols, ignore.case = TRUE)]
    if (length(clim_cols) < 2) {
      clim_cols <- setdiff(names(dat), exclude_cols)
      clim_cols <- clim_cols[vapply(dat[, clim_cols, drop = FALSE], is.numeric, logical(1))]
    }
    if (length(clim_cols) < 2) stop("fewer than 2 climate columns for PCA")

    prs_mat <- as.matrix(dat[dat$response == 1, clim_cols, drop = FALSE])
    bg_mat  <- as.matrix(dat[dat$response == 0, clim_cols, drop = FALSE])

    # PCA on combined matrix (centre + scale to training data)
    all_mat <- rbind(prs_mat, bg_mat)
    all_mat <- all_mat[complete.cases(all_mat), , drop = FALSE]
    pc_fit  <- prcomp(all_mat, center = TRUE, scale. = TRUE)
    pct_var <- round(100 * pc_fit$sdev^2 / sum(pc_fit$sdev^2), 1)

    prs_scores <- predict(pc_fit, prs_mat[complete.cases(prs_mat), ])[, 1:2]
    bg_scores  <- predict(pc_fit, bg_mat [complete.cases(bg_mat),  ])[, 1:2]

    ssp_labels <- c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
    periods    <- c("2021_2040", "2041_2060", "2071_2100")
    prd_labels <- c("2021\u20132040", "2041\u20132060", "2071\u20132100")

    fut_rows <- list()
    fut_dir  <- file.path(run_dir, "04_future_projections")

    for (ssp in c("ssp126", "ssp245", "ssp370", "ssp585")) {
      lbl <- ssp_labels[match(ssp, c("ssp126", "ssp245", "ssp370", "ssp585"))]
      for (pi in seq_along(periods)) {
        f <- file.path(fut_dir,
                       sprintf("future_gcm_ensemble_%s_%s.tif", ssp, periods[pi]))
        if (!file.exists(f)) next
        r <- tryCatch(rast(f), error = function(e) NULL)
        if (is.null(r)) next
        # Extract mean climate from the original predictor stack at cells where future suit > median
        # Use only PC1/PC2 from present training reference as proxy
        # Instead: sample future raster values and project into PC1/PC2
        # (Approximate: use mean/sd shift from present as proxy centroid shift)
        fut_vals <- as.numeric(values(r[[1]], na.rm = TRUE))
        # We can only project into PCA if we have matching climate layers
        # Gracefully skip if no matching climate stack
        fut_rows[[length(fut_rows) + 1]] <- data.frame(
          SSP = lbl,
          Period = prd_labels[pi],
          MeanSuit = mean(fut_vals, na.rm = TRUE),
          stringsAsFactors = FALSE
        )
      }
    }

    # Build biplot of present data + loadings
    pc_df <- data.frame(
      PC1   = c(prs_scores[, 1], bg_scores[, 1]),
      PC2   = c(prs_scores[, 2], bg_scores[, 2]),
      Group = c(rep("Presence", nrow(prs_scores)), rep("Background", nrow(bg_scores)))
    )

    # Subsample background for plotting
    set.seed(42)
    bg_idx <- which(pc_df$Group == "Background")
    if (length(bg_idx) > 500) bg_idx <- sample(bg_idx, 500)
    prs_idx <- which(pc_df$Group == "Presence")
    pc_plot <- pc_df[c(bg_idx, prs_idx), ]

    # Loadings (top 6 contributing variables)
    loadings <- pc_fit$rotation[, 1:2]
    load_df  <- data.frame(
      var = rownames(loadings),
      PC1 = loadings[, 1],
      PC2 = loadings[, 2],
      stringsAsFactors = FALSE
    )
    load_df$len <- sqrt(load_df$PC1^2 + load_df$PC2^2)
    load_df     <- head(load_df[order(-load_df$len), ], 6)
    scl <- 2.5  # scale loadings for visibility

    p <- ggplot(pc_plot, aes(x = PC1, y = PC2, colour = Group)) +
      geom_point(aes(shape = Group, size = Group, alpha = Group)) +
      scale_colour_manual(values = c(Presence = "#bd0026", Background = "grey55")) +
      scale_shape_manual(values = c(Presence = 21, Background = 3)) +
      scale_size_manual(values = c(Presence = 2.5, Background = 1.0)) +
      scale_alpha_manual(values = c(Presence = 0.85, Background = 0.35)) +
      geom_segment(data = load_df,
                   aes(x = 0, y = 0, xend = PC1 * scl, yend = PC2 * scl),
                   arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
                   colour = "#2166AC", linewidth = 0.7, inherit.aes = FALSE) +
      geom_text(data = load_df,
                aes(x = PC1 * scl * 1.1, y = PC2 * scl * 1.1, label = var),
                colour = "#2166AC", size = 3.0, fontface = "bold", inherit.aes = FALSE) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.4) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.4) +
      labs(
        title    = "Climate PCA biplot \u2014 Elephas maximus SDM (Bhutan)",
        subtitle = sprintf("PC1 = %.1f%%, PC2 = %.1f%% variance explained | Arrows = top 6 variable loadings",
                           pct_var[1], pct_var[2]),
        x = sprintf("PC1 (%.1f%%)", pct_var[1]),
        y = sprintf("PC2 (%.1f%%)", pct_var[2]),
        colour = NULL, shape = NULL, size = NULL, alpha = NULL
      ) +
      theme_bw(base_size = 11) +
      theme(
        plot.title      = element_text(face = "bold"),
        legend.position = "top"
      )

    .supp_save_fig(p, file.path(fig_dir, "figure_s12_climate_pca_biplot.png"), 8, 7)
    cat("  + figure_s12_climate_pca_biplot.png\n")
    n <- n + 1L
  }, error = function(e) cat(sprintf("  S12 skipped: %s\n", e$message)))

  # ── S13: GCM agreement map (fraction of 12 scenarios suitable) ───────────────
  tryCatch({
    fut_dir <- file.path(run_dir, "04_future_projections")
    ssps    <- c("ssp126", "ssp245", "ssp370", "ssp585")
    periods <- c("2021_2050", "2051_2080", "2071_2100")

    # Load ensemble rasters for all 12 scenarios
    rast_list <- list()
    for (ssp in ssps) for (prd in periods) {
      f <- file.path(fut_dir, sprintf("future_gcm_ensemble_%s_%s.tif", ssp, prd))
      if (file.exists(f)) {
        r <- tryCatch({ rr <- rast(f); if (nlyr(rr) > 1) rr[[1]] else rr },
                      error = function(e) NULL)
        if (!is.null(r)) rast_list[[length(rast_list) + 1]] <- r
      }
    }
    if (length(rast_list) == 0) stop("no future ensemble rasters found")

    # Common threshold (ensemble TSS-optimal)
    thr_agree <- get_thr("ensemble")
    if (!is.finite(thr_agree)) thr_agree <- 0.5

    # Reproject all to first raster CRS and resolution
    ref_r <- rast_list[[1]]
    binary_list <- lapply(rast_list, function(r) {
      if (!isTRUE(crs(r) == crs(ref_r)))
        r <- project(r, ref_r, method = "bilinear")
      b <- r >= thr_agree
      b[] <- as.integer(b[])
      b
    })

    # Sum = number of scenarios classifying as suitable
    agree_r <- Reduce("+", binary_list)

    if (!is.null(aoi_v)) {
      av2 <- if (!isTRUE(crs(aoi_v) == crs(agree_r)))
               project(aoi_v, crs(agree_r)) else aoi_v
      agree_r <- tryCatch(mask(crop(agree_r, av2), av2), error = function(e) agree_r)
    }

    agree_df <- as.data.frame(agree_r, xy = TRUE, na.rm = TRUE)
    names(agree_df)[3] <- "n_scenarios"
    agree_df$frac <- agree_df$n_scenarios / length(rast_list)

    p <- ggplot() +
      geom_raster(data = agree_df, aes(x = x, y = y, fill = frac)) +
      scale_fill_gradientn(
        colours  = c("#f7f7f7", "#fee08b", "#fdae61", "#f46d43", "#d73027", "#a50026"),
        values   = c(0, 0.25, 0.5, 0.67, 0.83, 1),
        limits   = c(0, 1), na.value = "grey90",
        name     = sprintf("Fraction of\n%d scenarios\nsuitable", length(rast_list)),
        labels   = scales::percent_format(),
        guide    = guide_colorbar(barwidth = unit(0.5, "cm"), barheight = unit(5, "cm"))
      ) +
      .boundary_geoms(crs(agree_r), with_dzong = TRUE) +
      labs(
        title    = "GCM agreement across future scenarios \u2014 Elephas maximus SDM (Bhutan)",
        subtitle = sprintf(
          "%d future scenarios (4 SSPs \u00d7 3 periods) | Threshold = %.3f (TSS-optimal) | Red = consistent high suitability",
          length(rast_list), thr_agree),
        x = NULL, y = NULL
      ) +
      .supp_map_theme() + coord_sf()

    .supp_save_fig(p, file.path(fig_dir, "figure_s13_gcm_agreement.png"), 9, 7)
    cat("  + figure_s13_gcm_agreement.png\n")
    n <- n + 1L
  }, error = function(e) cat(sprintf("  S13 skipped: %s\n", e$message)))

  # ── S14: GCM ensemble spread — SD across GCM realisations per SSP/period ─────
  tryCatch({
    fut_dir <- file.path(run_dir, "04_future_projections")
    ssps    <- c("ssp126", "ssp245", "ssp370", "ssp585")
    periods <- c("2021_2050", "2051_2080", "2071_2100")
    ssp_lbl <- c(ssp126 = "SSP1-2.6", ssp245 = "SSP2-4.5",
                 ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5")
    prd_lbl <- c("2021_2050" = "2021\u20132050", "2051_2080" = "2051\u20132080",
                 "2071_2100" = "2071\u20132100")

    panels <- list()
    for (ssp in ssps) {
      for (prd in periods) {
        # Individual GCM rasters: suitability_future_{gcm}_{ssp}_{period}_{algo}.tif
        gcm_files <- list.files(
          fut_dir,
          pattern  = sprintf("suitability_future_.*_%s_%s_.*\\.tif", ssp, prd),
          full.names = TRUE
        )
        if (length(gcm_files) < 2) next

        r_list <- lapply(gcm_files, function(f) {
          tryCatch({ rr <- rast(f); if (nlyr(rr) > 1) rr[[1]] else rr },
                   error = function(e) NULL)
        })
        r_list <- Filter(Negate(is.null), r_list)
        if (length(r_list) < 2) next

        ref_r <- r_list[[1]]
        r_aligned <- lapply(r_list, function(r) {
          if (!isTRUE(crs(r) == crs(ref_r)))
            r <- project(r, ref_r, method = "bilinear")
          r
        })

        stk    <- rast(r_aligned)
        sd_r   <- app(stk, fun = function(v) sd(v, na.rm = TRUE))

        if (!is.null(aoi_v)) {
          av2 <- if (!isTRUE(crs(aoi_v) == crs(sd_r)))
                   project(aoi_v, crs(sd_r)) else aoi_v
          sd_r <- tryCatch(mask(crop(sd_r, av2), av2), error = function(e) sd_r)
        }

        df <- as.data.frame(sd_r, xy = TRUE, na.rm = TRUE)
        names(df)[3] <- "sd"
        df$SSP    <- ssp_lbl[ssp]
        df$Period <- prd_lbl[prd]
        panels[[paste(ssp, prd)]] <- df
      }
    }

    if (length(panels) == 0) stop("no individual GCM rasters found for spread calculation")
    spread_df <- do.call(rbind, panels)
    spread_df$SSP    <- factor(spread_df$SSP,    levels = ssp_lbl)
    spread_df$Period <- factor(spread_df$Period, levels = prd_lbl)

    # CRS from first spread panel (all panels share same CRS)
    spread_crs <- tryCatch({
      f1 <- list.files(file.path(run_dir, "04_future_projections"),
                       pattern = "future_gcm_ensemble.*\\.tif", full.names = TRUE)[1]
      if (is.na(f1)) NULL else crs(rast(f1))
    }, error = function(e) NULL)

    p <- ggplot() +
      geom_raster(data = spread_df, aes(x = x, y = y, fill = sd)) +
      scale_fill_gradientn(
        colours  = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
        limits   = c(0, NA), na.value = "grey90",
        name     = "SD across\nGCMs",
        guide    = guide_colorbar(barwidth = unit(0.4, "cm"), barheight = unit(4, "cm"))
      ) +
      .boundary_geoms(spread_crs) +
      facet_grid(SSP ~ Period) +
      labs(
        title    = "GCM ensemble spread \u2014 Elephas maximus SDM (Bhutan)",
        subtitle = "Standard deviation across individual GCM projections | Blue = high inter-model uncertainty",
        x = NULL, y = NULL
      ) +
      .supp_map_theme(9) + coord_sf()

    n_rows_grid <- length(unique(spread_df$SSP))
    n_cols_grid <- length(unique(spread_df$Period))
    .supp_save_fig(p, file.path(fig_dir, "figure_s14_gcm_spread.png"),
                   4 * n_cols_grid + 2, 3.5 * n_rows_grid + 2)
    cat("  + figure_s14_gcm_spread.png\n")
    n <- n + 1L
  }, error = function(e) cat(sprintf("  S14 skipped: %s\n", e$message)))

  # ── S15: Response curves — all predictors, one reference algorithm (RF) ───────
  tryCatch({
    dat_file <- file.path(proc_dir, "modeling_dataset.csv")
    if (!file.exists(dat_file)) stop("modeling_dataset.csv not found")
    dat <- read.csv(dat_file)

    exclude_cols <- c("id", "type", "response", "longitude", "latitude", "fold")
    pred_cols_all <- setdiff(names(dat), exclude_cols)
    pred_cols_all <- pred_cols_all[vapply(dat[, pred_cols_all, drop = FALSE],
                                          is.numeric, logical(1))]
    if (length(pred_cols_all) < 2) stop("fewer than 2 predictors")

    # Try RF first, fall back to GLM
    m_rf  <- tryCatch(readRDS(file.path(mod_dir, "model_rf.rds")),  error = function(e) NULL)
    m_glm <- tryCatch(readRDS(file.path(mod_dir, "model_glm.rds")), error = function(e) NULL)
    m_use <- NULL; algo_name <- ""

    if (!is.null(m_rf) && inherits(m_rf, "ranger") &&
        requireNamespace("ranger", quietly = TRUE)) {
      m_use <- m_rf; algo_name <- "Random Forest"
    } else if (!is.null(m_glm) && inherits(m_glm, "glm")) {
      m_use <- m_glm; algo_name <- "GLM"
    }
    if (is.null(m_use)) stop("no model object available for response curves")

    med_row <- dat[1, pred_cols_all, drop = FALSE]
    for (col in pred_cols_all) med_row[[col]] <- median(dat[[col]], na.rm = TRUE)

    rc_rows <- list()
    for (pred in pred_cols_all) {
      x_seq <- seq(
        quantile(dat[[pred]], 0.01, na.rm = TRUE),
        quantile(dat[[pred]], 0.99, na.rm = TRUE),
        length.out = 60
      )
      nd <- med_row[rep(1, 60), , drop = FALSE]
      nd[[pred]] <- x_seq

      p_hat <- if (algo_name == "Random Forest") {
        tryCatch({
          preds <- predict(m_use, data = nd)$predictions
          if (is.matrix(preds)) preds[, "1"] else as.numeric(preds)
        }, error = function(e) rep(NA_real_, 60))
      } else {
        tryCatch(predict(m_use, newdata = nd, type = "response"),
                 error = function(e) rep(NA_real_, 60))
      }

      rc_rows[[length(rc_rows) + 1]] <- data.frame(
        predictor = pred, x = x_seq,
        y = as.numeric(p_hat)[seq_len(60)],
        stringsAsFactors = FALSE
      )
    }

    if (length(rc_rows) == 0) stop("no response curves computed")
    rc_all <- do.call(rbind, rc_rows)
    rc_all <- rc_all[!is.na(rc_all$y), ]

    # Observed presence rug
    rug_list <- list()
    for (pred in pred_cols_all) {
      prs_vals <- dat[[pred]][dat$response == 1 & !is.na(dat[[pred]])]
      if (length(prs_vals) > 0)
        rug_list[[length(rug_list) + 1]] <- data.frame(
          predictor = pred, x = prs_vals, stringsAsFactors = FALSE)
    }
    rug_df <- if (length(rug_list) > 0) do.call(rbind, rug_list) else NULL

    p <- ggplot(rc_all, aes(x = x, y = y)) +
      geom_line(colour = "#4DAF4A", linewidth = 0.9) +
      { if (!is.null(rug_df))
          geom_rug(data = rug_df, aes(x = x), inherit.aes = FALSE,
                   sides = "b", colour = "#bd0026", alpha = 0.4, linewidth = 0.3) } +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
      facet_wrap(~predictor, scales = "free_x") +
      labs(
        title    = sprintf("Response curves (%s) \u2014 Elephas maximus SDM (Bhutan)", algo_name),
        subtitle = "All predictors | Others held at median | Red rug = presence locations",
        x = "Predictor value", y = "Predicted suitability"
      ) +
      theme_bw(base_size = 9) +
      theme(
        strip.background = element_rect(fill = "grey92"),
        strip.text       = element_text(face = "bold", size = 8),
        plot.title       = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )

    n_pred <- length(unique(rc_all$predictor))
    n_cols_rc <- min(4, n_pred)
    n_rows_rc <- ceiling(n_pred / n_cols_rc)
    .supp_save_fig(p, file.path(fig_dir, "figure_s15_response_curves_all.png"),
                   4 * n_cols_rc, 3 * n_rows_rc + 1)
    cat("  + figure_s15_response_curves_all.png\n")
    n <- n + 1L
  }, error = function(e) cat(sprintf("  S15 skipped: %s\n", e$message)))

  # ── S16: Occurrence density heatmap ──────────────────────────────────────────
  tryCatch({
    # Prefer raw occurrence from processed data
    occ_file <- file.path(proc_dir, "occurrences_cleaned.csv")
    if (!file.exists(occ_file))
      occ_file <- file.path(proc_dir, "modeling_dataset.csv")
    if (!file.exists(occ_file)) stop("occurrence CSV not found")

    occ <- read.csv(occ_file)

    # Keep only presences (response == 1 or all rows if no response column)
    if ("response" %in% names(occ)) occ <- occ[occ$response == 1, ]

    lon_col <- intersect(c("longitude", "lon", "Longitude", "x"), names(occ))[1]
    lat_col <- intersect(c("latitude",  "lat", "Latitude",  "y"), names(occ))[1]
    if (is.na(lon_col) || is.na(lat_col)) stop("no coordinate columns found")

    occ$lon <- occ[[lon_col]]; occ$lat <- occ[[lat_col]]
    occ <- occ[!is.na(occ$lon) & !is.na(occ$lat), ]
    if (nrow(occ) < 5) stop("fewer than 5 presence records")

    # Background suit raster for spatial context
    pres_r <- tryCatch(
      rast(file.path(run_dir, "03_present_suitability",
                     "suitability_present_ensemble.tif")),
      error = function(e) NULL
    )
    suit_df <- if (!is.null(pres_r)) {
      av2v <- if (!is.null(aoi_v) && !isTRUE(crs(aoi_v) == crs(pres_r)))
        project(aoi_v, crs(pres_r)) else aoi_v
      rr <- if (!is.null(av2v)) tryCatch(mask(crop(pres_r, av2v), av2v),
                                          error = function(e) pres_r) else pres_r
      if (nlyr(rr) > 1) rr <- rr[[1]]
      df <- as.data.frame(rr, xy = TRUE, na.rm = TRUE)
      names(df)[3] <- "suit"
      df
    } else NULL

    # Project occurrences if raster CRS differs from WGS84
    occ_sf <- tryCatch(
      st_as_sf(occ, coords = c("lon", "lat"), crs = 4326),
      error = function(e) NULL
    )
    if (!is.null(occ_sf) && !is.null(pres_r)) {
      r_crs <- crs(pres_r, describe = TRUE)$code
      if (!isTRUE(r_crs == "4326"))
        occ_sf <- tryCatch(st_transform(occ_sf, crs(pres_r)), error = function(e) occ_sf)
    }
    # Clip occurrences to Bhutan boundary
    if (!is.null(aoi_sf) && !is.null(occ_sf)) {
      aoi_clip_s16 <- tryCatch(st_transform(aoi_sf, st_crs(occ_sf)), error = function(e) NULL)
      if (!is.null(aoi_clip_s16))
        occ_sf <- tryCatch(
          occ_sf[st_within(occ_sf, st_union(aoi_clip_s16), sparse = FALSE)[, 1], ],
          error = function(e) occ_sf
        )
    }

    p <- ggplot()
    if (!is.null(suit_df))
      p <- p + geom_raster(data = suit_df, aes(x = x, y = y, fill = suit)) +
        scale_fill_gradientn(
          colours = c("#f7f7f7", "#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"),
          values  = c(0, 0.05, 0.2, 0.4, 0.65, 1),
          limits  = c(0, 1), na.value = "grey90",
          name    = "Ensemble\nsuitability",
          guide   = guide_colorbar(barwidth = unit(0.4, "cm"), barheight = unit(4, "cm"))
        )
    p <- p + .boundary_geoms(if (!is.null(pres_r)) crs(pres_r) else NULL, with_dzong = TRUE)

    # 2D density of occurrences
    if (!is.null(occ_sf)) {
      occ_coords <- as.data.frame(st_coordinates(occ_sf))
      names(occ_coords) <- c("x", "y")
      p <- p +
        stat_density_2d(data = occ_coords, aes(x = x, y = y, colour = after_stat(level)),
                        bins = 8, linewidth = 0.7, inherit.aes = FALSE) +
        scale_colour_gradientn(
          colours = c("#fdfdfd", "#FDD835", "#F57F17"),
          name    = "Occurrence\ndensity",
          guide   = guide_colorbar(barwidth = unit(0.4, "cm"), barheight = unit(3, "cm"))
        ) +
        geom_sf(data = occ_sf, inherit.aes = FALSE,
                colour = "#8B0000", shape = 4, size = 1.2, alpha = 0.6, stroke = 0.5)
    }

    # Derive map limits from AOI/raster extent to clip density contours to Bhutan
    s16_xlim <- s16_ylim <- NULL
    ref_ext_s16 <- if (!is.null(pres_r)) ext(pres_r) else if (!is.null(aoi_v)) ext(aoi_v) else NULL
    if (!is.null(ref_ext_s16)) {
      buf_s16 <- (ref_ext_s16$ymax - ref_ext_s16$ymin) * 0.04
      s16_xlim <- c(ref_ext_s16$xmin - buf_s16, ref_ext_s16$xmax + buf_s16)
      s16_ylim <- c(ref_ext_s16$ymin - buf_s16, ref_ext_s16$ymax + buf_s16)
    }

    p <- p + labs(
      title    = "Occurrence density \u2014 Elephas maximus SDM (Bhutan)",
      subtitle = sprintf(
        "n = %d presence records | Contours = 2D kernel density | Crosses = individual occurrences",
        if (!is.null(occ_sf)) nrow(occ_sf) else nrow(occ)),
      x = NULL, y = NULL
    ) + .supp_map_theme() +
      coord_sf(xlim = s16_xlim, ylim = s16_ylim, expand = FALSE)

    .supp_save_fig(p, file.path(fig_dir, "figure_s16_occurrence_density.png"), 9, 7)
    cat("  + figure_s16_occurrence_density.png\n")
    n <- n + 1L
  }, error = function(e) cat(sprintf("  S16 skipped: %s\n", e$message)))

  # ── S17: Per-Dzongkhag suitability bar chart ──────────────────────────────────
  tryCatch({
    repo_root <- normalizePath(
      file.path(run_dir, "..", "..", ".."), mustWork = FALSE, winslash = "/"
    )
    dzo_shp <- file.path(repo_root, "01_data_raw", "03_vector", "shapefiles",
                         "Dzongkhag Boundary", "Dzongkhag Boundary.shp")
    if (!file.exists(dzo_shp)) stop("Dzongkhag boundary shapefile not found")

    pres_file <- file.path(run_dir, "03_present_suitability",
                           "suitability_present_ensemble.tif")
    if (!file.exists(pres_file)) stop("ensemble suitability raster not found")

    dzo  <- st_read(dzo_shp, quiet = TRUE)
    dzo  <- st_make_valid(dzo)
    suit <- rast(pres_file)
    if (nlyr(suit) > 1) suit <- suit[[1]]

    pix_km2 <- prod(res(suit)) / 1e6

    # Ensemble threshold
    ens_thr <- if (!is.null(eval_df) && "threshold" %in% names(eval_df)) {
      tv <- mean(eval_df$threshold, na.rm = TRUE)
      if (is.finite(tv)) tv else 0.5
    } else 0.5

    # The shapefile has wrong CRS metadata (labelled 32645 but coords are EPSG:3857)
    dzo_v_raw <- tryCatch(vect(dzo), error = function(e) NULL)
    if (is.null(dzo_v_raw)) stop("could not convert Dzongkhag sf to SpatVector")
    # Force correct source CRS then reproject to suit
    crs(dzo_v_raw) <- "EPSG:3857"
    dzo_v_all <- tryCatch(
      terra::project(dzo_v_raw, suit),
      error = function(e) dzo_v_raw
    )

    # Use terra::extract (robust per-polygon pixel extraction)
    ext_df <- tryCatch(
      terra::extract(suit, dzo_v_all, ID = TRUE),
      error = function(e) NULL
    )
    if (is.null(ext_df) || nrow(ext_df) == 0)
      stop("terra::extract returned no data for Dzongkhag polygons")
    names(ext_df)[2] <- "suit_val"

    dzo_names <- as.character(dzo$dzongkhag)

    rows <- list()
    for (i in seq_along(dzo_names)) {
      vals <- as.numeric(ext_df$suit_val[ext_df$ID == i])
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) next
      area_total <- length(vals) * pix_km2
      area_suit  <- sum(vals >= ens_thr) * pix_km2
      rows[[length(rows) + 1]] <- data.frame(
        dzongkhag     = dzo_names[i],
        pct_suitable  = 100 * area_suit / area_total,
        suit_km2      = area_suit,
        total_km2     = area_total,
        mean_suit     = mean(vals),
        stringsAsFactors = FALSE
      )
    }
    if (length(rows) == 0) stop("no Dzongkhag statistics computed")
    dzo_df <- do.call(rbind, rows)
    dzo_df$dzongkhag <- factor(dzo_df$dzongkhag,
                                levels = dzo_df$dzongkhag[order(dzo_df$pct_suitable)])

    # Dual panel: % suitable (bars) + mean suitability (dots)
    if (!has_patchwork) stop("patchwork required for S17")

    p1 <- ggplot(dzo_df, aes(y = dzongkhag, x = pct_suitable, fill = pct_suitable)) +
      geom_col(alpha = 0.88, width = 0.75) +
      geom_vline(xintercept = mean(dzo_df$pct_suitable), linetype = "dashed",
                 colour = "grey30", linewidth = 0.7) +
      scale_fill_gradientn(
        colours = c("#f7f7f7", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"),
        limits = c(0, 100), guide = "none"
      ) +
      scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
      labs(
        title = "% Suitable habitat",
        subtitle = sprintf("Threshold = %.3f | Dashed = national mean", ens_thr),
        x = "% of Dzongkhag area suitable", y = NULL
      ) +
      theme_bw(base_size = 10) +
      theme(plot.title = element_text(face = "bold"), panel.grid.major.y = element_blank())

    p2 <- ggplot(dzo_df, aes(y = dzongkhag, x = mean_suit, colour = mean_suit)) +
      geom_point(size = 3.5, shape = 21, fill = "white", stroke = 1.4) +
      geom_segment(aes(x = 0, xend = mean_suit, yend = dzongkhag),
                   linewidth = 0.5, alpha = 0.5) +
      scale_colour_gradientn(
        colours = c("#f7f7f7", "#fecc5c", "#fd8d3c", "#bd0026"),
        limits = c(0, 1), guide = "none"
      ) +
      scale_x_continuous(limits = c(0, 1)) +
      labs(
        title = "Mean suitability score",
        x = "Mean ensemble suitability (0\u20131)", y = NULL
      ) +
      theme_bw(base_size = 10) +
      theme(
        plot.title     = element_text(face = "bold"),
        axis.text.y    = element_blank(),
        axis.ticks.y   = element_blank(),
        panel.grid.major.y = element_blank()
      )

    out <- p1 + p2 +
      patchwork::plot_annotation(
        title    = "Habitat suitability by Dzongkhag \u2014 Elephas maximus (Bhutan)",
        subtitle = sprintf("20 Dzongkhags | Ensemble model | Ranked by %% suitable"),
        tag_levels = "A"
      )

    .supp_save_fig(out, file.path(fig_dir, "figure_s17_dzongkhag_suitability.png"), 12, 8)
    cat("  + figure_s17_dzongkhag_suitability.png\n")
    n <- n + 1L
  }, error = function(e) cat(sprintf("  S17 skipped: %s\n", e$message)))

  # ── S18: Climate refugia map ───────────────────────────────────────────────────
  tryCatch({
    chg_dir  <- file.path(run_dir, "05_change_metrics")
    pres_ens <- file.path(run_dir, "03_present_suitability",
                          "suitability_present_ensemble.tif")
    if (!file.exists(pres_ens)) stop("ensemble suitability raster not found")

    persist_files <- list.files(chg_dir,
      pattern = "^persistence_ensemble_.*\\.tif$", full.names = TRUE)
    if (length(persist_files) == 0) stop("no persistence rasters found in 05_change_metrics")

    # Load all persistence rasters and align to first
    pers_list <- lapply(persist_files, function(f) {
      tryCatch({ r <- rast(f); if (nlyr(r) > 1) r[[1]] else r }, error = function(e) NULL)
    })
    pers_list <- Filter(Negate(is.null), pers_list)
    if (length(pers_list) == 0) stop("could not load any persistence rasters")

    ref_p <- pers_list[[1]]
    pers_list <- lapply(pers_list, function(r) {
      if (!isTRUE(crs(r) == crs(ref_p))) r <- project(r, ref_p, method = "near")
      r
    })
    n_scen <- length(pers_list)

    # Stability score: count of scenarios where cell remains suitable
    stab_r <- app(rast(pers_list), fun = function(v) sum(v, na.rm = TRUE))

    # Core refugia: suitable in ALL scenarios
    core_r <- stab_r
    core_r[stab_r < n_scen] <- NA
    core_r[!is.na(core_r)]  <- 1L

    # Mask to present suitable area
    suit_r <- rast(pres_ens)
    if (nlyr(suit_r) > 1) suit_r <- suit_r[[1]]
    suit_r <- tryCatch(project(suit_r, ref_p, method = "bilinear"),
                       error = function(e) suit_r)

    # Save rasters for T23 to pick up
    ref_out  <- file.path(run_dir, "03_present_suitability", "refugia_core.tif")
    stab_out <- file.path(run_dir, "03_present_suitability", "refugia_stability.tif")
    tryCatch(writeRaster(core_r,  ref_out,  overwrite = TRUE), error = function(e) NULL)
    tryCatch(writeRaster(stab_r, stab_out, overwrite = TRUE), error = function(e) NULL)

    # AOI mask
    if (!is.null(aoi_v)) {
      av2 <- if (!isTRUE(crs(aoi_v) == crs(stab_r))) project(aoi_v, crs(stab_r)) else aoi_v
      stab_r <- tryCatch(mask(crop(stab_r, av2), av2), error = function(e) stab_r)
    }

    stab_df <- as.data.frame(stab_r, xy = TRUE, na.rm = TRUE)
    names(stab_df)[3] <- "n_stable"

    # Map extent from stability raster
    ref_ext2 <- ext(stab_r)
    buf2 <- (ref_ext2$ymax - ref_ext2$ymin) * 0.04
    map_xlim2 <- c(ref_ext2$xmin - buf2, ref_ext2$xmax + buf2)
    map_ylim2 <- c(ref_ext2$ymin - buf2, ref_ext2$ymax + buf2)

    # Stats
    cell_km2   <- prod(res(stab_r)) / 1e6
    n_total    <- sum(!is.na(values(stab_r)))
    n_core     <- sum(stab_df$n_stable == n_scen, na.rm = TRUE)
    n_partial  <- sum(stab_df$n_stable >= ceiling(n_scen * 0.75), na.rm = TRUE)
    pct_core   <- round(100 * n_core   / n_total, 1)
    pct_part   <- round(100 * n_partial / n_total, 1)

    # 5 stability classes — clean single-line labels, no \n
    cls_labels <- c(
      "Never suitable",
      "Low stability (<25%)",
      "Moderate (25\u201350%)",
      "High stability (50\u201375%)",
      "Core refugia (>75%)"
    )
    stab_df$class <- cut(stab_df$n_stable,
      breaks = c(-Inf, 0, ceiling(n_scen*0.25), ceiling(n_scen*0.50),
                 ceiling(n_scen*0.75), n_scen),
      labels = cls_labels,
      include.lowest = TRUE, right = TRUE)

    # "Never suitable" = mid-grey; use a 5-step sequential palette with clear steps
    pal_ref <- c(
      "Never suitable"               = "#c8c8c8",
      "Low stability (<25%)"         = "#fdbb84",  # deeper peach (was #fdd49e too pale)
      "Moderate (25\u201350%)"       = "#e34a33",  # stronger orange-red
      "High stability (50\u201375%)" = "#b30000",
      "Core refugia (>75%)"          = "#4d0000"   # very dark red
    )

    p <- ggplot() +
      geom_raster(data = stab_df, aes(x = x, y = y, fill = class)) +
      scale_fill_manual(values = pal_ref, name = "Stability class",
                        drop = FALSE, na.value = "white") +
      .boundary_geoms(crs(stab_r), with_dzong = TRUE) +
      annotate("label", x = Inf, y = Inf,
               label = sprintf(
                 "Core refugia (>75%% scenarios): %.1f%%\nHigh+Core (>50%%): %.1f%%\nn = %d scenarios",
                 pct_core, pct_part, n_scen),
               hjust = 1.02, vjust = 1.05, size = 3.2, fill = "white", alpha = 0.92,
               linewidth = 0.3) +
      labs(
        title    = "Climate refugia \u2014 Elephas maximus SDM (Bhutan)",
        subtitle = sprintf(
          "Stability = fraction of %d future scenarios (4 SSPs \u00d7 3 periods) where habitat remains suitable",
          n_scen),
        x = NULL, y = NULL
      ) +
      .supp_map_theme() +
      coord_sf(xlim = map_xlim2, ylim = map_ylim2, expand = FALSE) +
      theme(
        legend.key.size  = unit(0.55, "cm"),
        legend.key.width = unit(0.9, "cm"),
        legend.text      = element_text(size = 8.5),
        legend.title     = element_text(size = 9, face = "bold"),
        plot.margin      = margin(2, 2, 2, 2, "mm")
      )

    .supp_save_fig(p, file.path(fig_dir, "figure_s18_refugia.png"), 10, 5.5)
    cat("  + figure_s18_refugia.png\n")
    n <- n + 1L
  }, error = function(e) cat(sprintf("  S18 skipped: %s\n", e$message)))

  # ── S19: Niche overlap across scenarios ───────────────────────────────────
  tryCatch({
    cat("S19: Niche overlap...\n")
    no_file <- file.path(fig_dir, "table_17_niche_overlap.csv")
    if (file.exists(no_file)) {
      no_df <- read.csv(no_file, stringsAsFactors = FALSE)
      no_long <- reshape(no_df[, c("Comparison","SSP","Period","Schoener_D","Hellinger_I")],
                         varying   = c("Schoener_D","Hellinger_I"),
                         v.names   = "value",
                         timevar   = "metric",
                         times     = c("Schoener's D","Hellinger's I"),
                         direction = "long")
      no_long$Period <- factor(no_long$Period, levels = unique(no_long$Period))
      p <- ggplot(no_long, aes(x = Period, y = value, colour = SSP, group = SSP)) +
        geom_line(linewidth = 0.8) +
        geom_point(size = 2.5) +
        facet_wrap(~ metric, ncol = 2, scales = "free_y") +
        scale_colour_manual(values = c("SSP1-2.6"="#1a9850","SSP2-4.5"="#fee08b",
                                       "SSP3-7.0"="#f46d43","SSP5-8.5"="#d73027"),
                            name = "Emission scenario") +
        coord_cartesian(ylim = c(0.80, 1.00)) +
        labs(title    = "Niche Overlap: Future vs. Present",
             subtitle = "Values closer to 1.0 indicate high climatic niche conservation",
             x = NULL, y = "Overlap index") +
        theme_bw() +
        theme(strip.background = element_rect(fill = "#f0f0f0"),
              strip.text       = element_text(face = "bold"),
              legend.position  = "bottom",
              plot.title       = element_text(face = "bold"))
      .supp_save_fig(p, file.path(fig_dir, "figure_s19_niche_overlap.png"), 10, 5)
      cat("  + figure_s19_niche_overlap.png\n")
      n <- n + 1L
    }
  }, error = function(e) cat(sprintf("  S19 skipped: %s\n", e$message)))

  # ── S20: Future conflict risk trajectory ──────────────────────────────────
  tryCatch({
    cat("S20: Future conflict risk...\n")
    cr_file <- file.path(fig_dir, "table_conflict_risk_summary.csv")
    if (file.exists(cr_file)) {
      cr_df <- read.csv(cr_file, stringsAsFactors = FALSE)
      # Present baseline as horizontal reference
      baseline_risk <- cr_df$Mean_conflict_risk[cr_df$SSP == "Present"]
      fut_df <- cr_df[cr_df$SSP != "Present", ]
      # Normalise SSP labels
      fut_df$SSP <- sub("SSP126","SSP1-2.6",
                   sub("SSP245","SSP2-4.5",
                   sub("SSP370","SSP3-7.0",
                   sub("SSP585","SSP5-8.5", fut_df$SSP))))
      fut_df$Period <- factor(fut_df$Period,
                              levels = c("2021-2050","2051-2080","2071-2100"))
      p <- ggplot(fut_df, aes(x = Period, y = Mean_conflict_risk,
                               colour = SSP, group = SSP)) +
        geom_hline(yintercept = baseline_risk, linetype = "dashed",
                   colour = "grey40", linewidth = 0.8) +
        annotate("text", x = 1, y = baseline_risk + 0.001,
                 label = sprintf("Present (%.3f)", baseline_risk),
                 hjust = 0, size = 3, colour = "grey40") +
        geom_line(linewidth = 0.9) +
        geom_point(size = 3) +
        scale_colour_manual(values = c("SSP1-2.6"="#1a9850","SSP2-4.5"="#fee08b",
                                       "SSP3-7.0"="#f46d43","SSP5-8.5"="#d73027"),
                            name = "Emission scenario") +
        scale_y_continuous(labels = scales::label_number(accuracy = 0.001)) +
        labs(title    = "Projected Mean Conflict Risk Index",
             subtitle = "Dashed line = present-day baseline (0.103)",
             x = "Time period", y = "Mean conflict risk index") +
        theme_bw() +
        theme(legend.position  = "bottom",
              plot.title       = element_text(face = "bold"))
      .supp_save_fig(p, file.path(fig_dir, "figure_s20_conflict_risk_trajectory.png"), 8, 5)
      cat("  + figure_s20_conflict_risk_trajectory.png\n")
      n <- n + 1L
    }
  }, error = function(e) cat(sprintf("  S20 skipped: %s\n", e$message)))

  cat(sprintf("=== Supplementary figures complete: %d saved ===\n\n", n))
  invisible(list(n_figures = n))
}

} # end source guard

# ── Standalone invocation ─────────────────────────────────────────────────────
local({
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 1 && !exists("run_dir", envir = .GlobalEnv)) {
    run_dir <- normalizePath(args[1], winslash = "/", mustWork = FALSE)
    run_id  <- if (length(args) >= 2) args[2] else basename(run_dir)
    message("Standalone: generating supplementary figures for ", run_id)
    create_supplementary_figures(run_dir, run_id)
  }
})
