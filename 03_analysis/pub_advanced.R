#!/usr/bin/env Rscript
# =============================================================================
# pub_advanced.R  —  World-class advanced analyses, Elephas maximus SDM Bhutan
#
# A1:  Climate Refugia
# A2:  Temporal Stability Index
# A3:  Variance Decomposition (GCM × SSP × Algorithm)
# A4:  PA Effectiveness Trajectories
# A5:  Connectivity Corridors (gdistance)
# A6:  Limiting Factor Maps
# A7:  Climate Velocity
# A8:  Threshold Sensitivity Analysis
# A9:  Human Pressure & Conflict Risk
# A10: SHAP Local Feature Importance (RF)
# =============================================================================
suppressPackageStartupMessages({
  library(terra); library(sf)
  library(ggplot2); library(patchwork); library(scales)
  library(dplyr)
})

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
RUN   <- resolve_run_dir(if (length(args) >= 1) args[[1]] else NULL, repo_root = repo_root)
FUT   <- file.path(RUN, "04_future_projections")
OUT   <- file.path(RUN, "08_figures_tables")
ADOUT <- file.path(OUT, "advanced")
dir.create(ADOUT, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# Constants (shared with pub_final.R)
# =============================================================================
GCM_LIST  <- c("acces_cm2","cnrm_cm6_1","cnrm_esm2_1","inm_cm4_8",
               "inm_cm5_0","miroc6","miroc_es2l","mpi_esm1_2_lr","mri_esm2_0")
GCM_LABEL <- c("ACCESS-CM2","CNRM-CM6-1","CNRM-ESM2-1","INM-CM4-8",
               "INM-CM5-0","MIROC6","MIROC-ES2L","MPI-ESM1-2-LR","MRI-ESM2-0")
ALL_SSPS     <- c("ssp126","ssp245","ssp370","ssp585")
ALL_SSP_LBLS <- c("SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5")
ALL_SSP_COLS <- c("#2171b5","#1a7837","#d6604d","#b2182b")
PERIODS      <- c("2021_2050","2051_2080","2071_2100")
PER_LABEL    <- c("2021\u20132050","2051\u20132080","2071\u20132100")
ALGOS        <- c("glm","rf","brt","maxent")
THRESHOLD    <- 0.172   # optimal TSS threshold from Phase 8
SUIT_MAX     <- 0.50

SUIT_COLS <- c("#f7f7f7","#fee391","#fe9929","#d95f0e","#993404")
CHNG_COLS <- c("#053061","#2166ac","#92c5de","#f7f7f7","#f4a582","#ca0020","#67001f")

# =============================================================================
# Load common spatial objects
# =============================================================================
aoi_path <- repo_path("01_data_raw", "03_vector", "shapefiles", "Bhutan", "bhutan.shp", repo_root = repo_root)
aoi <- tryCatch(st_read(aoi_path, quiet = TRUE), error = function(e) NULL)
if (!is.null(aoi) && is.na(st_crs(aoi))) st_crs(aoi) <- 32645
if (!is.null(aoi) && st_crs(aoi)$epsg != 32645) aoi <- st_transform(aoi, 32645)
aoi_v <- if (!is.null(aoi)) vect(aoi) else NULL

bb <- if (!is.null(aoi)) st_bbox(aoi) else NULL
PAD  <- 5000
XLIM <- if (!is.null(bb)) c(bb["xmin"] - PAD, bb["xmax"] + PAD) else c(670000, 1010000)
YLIM <- if (!is.null(bb)) c(bb["ymin"] - PAD, bb["ymax"] + PAD) else c(2950000, 3140000)

# Present ensemble suitability
pres_r <- tryCatch(
  rast(file.path(RUN, "03_present_suitability/suitability_present_ensemble.tif")),
  error = function(e) NULL)

# Shared theme
map_theme <- theme_void(base_size = 9) +
  theme(
    panel.background  = element_rect(fill = "#e8f0f7", colour = NA),
    panel.border      = element_rect(fill = NA, colour = "grey30", linewidth = 0.4),
    plot.background   = element_rect(fill = "white", colour = NA),
    plot.title        = element_text(size = 8, face = "bold", hjust = 0.5,
                                     margin = margin(b = 2, t = 2)),
    plot.subtitle     = element_text(size = 6.5, hjust = 0.5, colour = "grey40"),
    legend.title      = element_text(size = 7, face = "bold"),
    legend.text       = element_text(size = 6),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width  = unit(0.2, "cm"),
    plot.margin       = margin(2, 2, 2, 2)
  )

r2df <- function(r, clip = TRUE) {
  if (clip && !is.null(aoi_v)) {
    av <- tryCatch(project(aoi_v, crs(r)), error = function(e) aoi_v)
    r  <- tryCatch(mask(crop(r, av), av), error = function(e) r)
  }
  df <- as.data.frame(r[[1]], xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "value"
  df
}

add_aoi <- function() {
  if (is.null(aoi)) return(NULL)
  geom_sf(data = aoi, fill = NA, colour = "grey20", linewidth = 0.5, inherit.aes = FALSE)
}

suit_scale_adv <- function(name = "Suitability") {
  scale_fill_gradientn(
    colours = SUIT_COLS,
    values  = rescale(c(0, 0.10, 0.25, 0.40, SUIT_MAX)),
    limits  = c(0, SUIT_MAX), oob = squish,
    na.value = "white", name = name,
    guide = guide_colorbar(barwidth = 0.5, barheight = 4,
                           title.position = "top", title.hjust = 0.5,
                           label.theme = element_text(size = 6))
  )
}

# Helper: load future gcm ensemble raster (all-GCM mean, one file per SSP×period)
get_ens_rast <- function(ssp, period) {
  f <- file.path(FUT, sprintf("future_gcm_ensemble_%s_%s.tif", ssp, period))
  if (!file.exists(f)) return(NULL)
  tryCatch(rast(f), error = function(e) NULL)
}

cat("\n=== pub_advanced.R: World-Class Analyses ===\n\n")

# =============================================================================
# A1: CLIMATE REFUGIA
# Proportion of all 108 future scenarios (9 GCMs × 4 SSPs × 3 periods) where
# suitability ≥ threshold. High values = climate-stable refugia.
# =============================================================================
cat("A1: Climate Refugia...\n")
tryCatch({
  ens_files <- list.files(FUT, pattern = "^future_gcm_ensemble_.*\\.tif$", full.names = TRUE)
  cat(sprintf("  Loading %d ensemble rasters...\n", length(ens_files)))

  if (length(ens_files) >= 2) {
    # Load and align to present raster resolution
    ref_r <- pres_r
    binary_list <- lapply(ens_files, function(f) {
      r <- tryCatch(rast(f), error = function(e) NULL)
      if (is.null(r)) return(NULL)
      if (!is.null(ref_r)) {
        r <- tryCatch(resample(r, ref_r, method = "bilinear"), error = function(e) r)
      }
      ifel(r >= THRESHOLD, 1L, 0L)
    })
    binary_list <- binary_list[!sapply(binary_list, is.null)]
    cat(sprintf("  Stacking %d binary rasters...\n", length(binary_list)))

    refugia_stack <- rast(binary_list)
    refugia_score <- app(refugia_stack, function(v) sum(v, na.rm = TRUE)) / length(binary_list)

    writeRaster(refugia_score, file.path(ADOUT, "A1_climate_refugia.tif"), overwrite = TRUE)
    df_ref <- r2df(refugia_score)

    # Summary stats
    cat(sprintf("  High refugia (>0.8 scenarios suitable): %.1f%% of Bhutan\n",
                mean(df_ref$value >= 0.8, na.rm = TRUE) * 100))
    cat(sprintf("  Moderate refugia (>0.5): %.1f%%\n",
                mean(df_ref$value >= 0.5, na.rm = TRUE) * 100))

    # Map
    p_ref <- ggplot() +
      geom_raster(data = df_ref, aes(x = x, y = y, fill = value)) +
      add_aoi() +
      scale_fill_gradientn(
        colours = c("#f7f7f7","#ffffcc","#a1dab4","#41b6c4","#225ea8","#0c2c84"),
        values  = rescale(c(0, 0.2, 0.4, 0.6, 0.8, 1.0)),
        limits  = c(0, 1), na.value = "white",
        name    = "Refugia\nScore",
        guide   = guide_colorbar(barwidth = 0.5, barheight = 5,
                                 title.position = "top", title.hjust = 0.5,
                                 label.theme = element_text(size = 6))
      ) +
      coord_sf(crs = 32645, xlim = XLIM, ylim = YLIM, expand = FALSE) +
      labs(title = expression(paste(italic("Elephas maximus"), " Climate Refugia — Bhutan")),
           subtitle = sprintf("Proportion of 108 future scenarios (9 GCMs \u00d7 4 SSPs \u00d7 3 periods) with suitability \u2265 %.3f",
                              THRESHOLD)) +
      map_theme +
      theme(plot.title    = element_text(size = 11, face = "bold"),
            plot.subtitle = element_text(size = 8))

    # Refugia histogram
    p_hist <- ggplot(df_ref, aes(x = value)) +
      geom_histogram(aes(fill = after_stat(x)), bins = 50, colour = "white", linewidth = 0.1) +
      scale_fill_gradientn(colours = c("#f7f7f7","#ffffcc","#41b6c4","#0c2c84"),
                           limits = c(0, 1), guide = "none") +
      geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey30") +
      geom_vline(xintercept = 0.8, linetype = "dashed", colour = "#0c2c84") +
      scale_x_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = "Refugia score (% scenarios suitable)",
           y = "Number of pixels",
           title = "Distribution of refugia scores") +
      theme_bw(base_size = 10) +
      theme(panel.grid.minor = element_blank())

    fig_a1 <- p_ref / p_hist + plot_layout(heights = c(3, 1))
    fig_a1 <- fig_a1 + plot_annotation(
      caption = sprintf("Blue = core refugia (stable across >80%% of scenarios) | Score = proportion of %d scenarios where suitability \u2265 %.3f\nCRS: EPSG:32645 | Core refugia cover %.1f%% of Bhutan",
                        length(binary_list), THRESHOLD,
                        mean(df_ref$value >= 0.8, na.rm = TRUE) * 100),
      theme = theme(plot.caption = element_text(size = 7.5, colour = "grey40", hjust = 0))
    )
    ggsave(file.path(ADOUT, "A1_climate_refugia.png"), fig_a1,
           width = 14, height = 16, dpi = 300, limitsize = FALSE)
    cat("  + A1 saved\n")
  }
}, error = function(e) cat(sprintf("  A1 failed: %s\n", conditionMessage(e))))

# =============================================================================
# A2: TEMPORAL STABILITY INDEX
# For each of 12 SSP×period combinations, average the 8-GCM ensemble mean.
# Then sum binary suitability across 12 time steps → stability map.
# =============================================================================
cat("A2: Temporal Stability Index...\n")
tryCatch({
  stability_stack <- list()
  for (ssp in ALL_SSPS) {
    for (period in PERIODS) {
      ens_r <- get_ens_rast(ssp, period)
      if (is.null(ens_r)) next
      if (!is.null(pres_r)) ens_r <- tryCatch(resample(ens_r, pres_r, method = "bilinear"),
                                               error = function(e) ens_r)
      binary_r <- ifel(ens_r >= THRESHOLD, 1L, 0L)
      stability_stack[[length(stability_stack) + 1]] <- binary_r
    }
  }
  n_steps <- length(stability_stack)
  cat(sprintf("  Built %d time steps\n", n_steps))

  if (n_steps >= 2) {
    stability_r <- app(rast(stability_stack), function(v) sum(v, na.rm = TRUE)) / n_steps
    writeRaster(stability_r, file.path(ADOUT, "A2_temporal_stability.tif"), overwrite = TRUE)
    df_stab <- r2df(stability_r)

    p_stab <- ggplot() +
      geom_raster(data = df_stab, aes(x = x, y = y, fill = value)) +
      add_aoi() +
      scale_fill_gradientn(
        colours = c("#d73027","#f46d43","#fdae61","#fee090","#ffffbf",
                    "#e0f3f8","#abd9e9","#74add1","#4575b4"),
        values  = rescale(c(0, 0.1, 0.2, 0.4, 0.5, 0.6, 0.7, 0.9, 1.0)),
        limits  = c(0, 1), na.value = "white",
        name    = "Stability\nIndex",
        guide   = guide_colorbar(barwidth = 0.5, barheight = 5,
                                 title.position = "top", title.hjust = 0.5,
                                 label.theme = element_text(size = 6))
      ) +
      coord_sf(crs = 32645, xlim = XLIM, ylim = YLIM, expand = FALSE) +
      labs(title = expression(paste(italic("Elephas maximus"), " Temporal Stability Index")),
           subtitle = sprintf("Proportion of %d future time steps (4 SSPs \u00d7 3 periods, GCM-averaged) with suitability \u2265 %.3f",
                              n_steps, THRESHOLD)) +
      map_theme +
      theme(plot.title = element_text(size = 11, face = "bold"),
            plot.subtitle = element_text(size = 8))

    # Per-SSP stability profiles
    ssp_rows <- do.call(rbind, lapply(seq_along(ALL_SSPS), function(si) {
      ssp <- ALL_SSPS[si]
      do.call(rbind, lapply(seq_along(PERIODS), function(pi) {
        period <- PERIODS[pi]
        idx <- (si - 1) * length(PERIODS) + pi
        if (idx > n_steps) return(NULL)
        pct <- tryCatch({
          mr <- get_ens_rast(ssp, period)
          if (is.null(mr)) return(data.frame(SSP=ALL_SSP_LBLS[si], Period=PER_LABEL[pi], pct_suit=NA_real_))
          if (!is.null(pres_r)) mr <- tryCatch(resample(mr, pres_r, method="bilinear"), error=function(e) mr)
          if (!is.null(aoi_v)) {
            av <- tryCatch(project(aoi_v, crs(mr)), error=function(e) aoi_v)
            mr <- tryCatch(mask(crop(mr, av), av), error=function(e) mr)
          }
          mean(values(mr, na.rm=TRUE) >= THRESHOLD, na.rm=TRUE) * 100
        }, error=function(e) NA_real_)
        data.frame(SSP = ALL_SSP_LBLS[si], Period = PER_LABEL[pi], pct_suit = pct)
      }))
    }))
    ssp_rows$Period <- factor(ssp_rows$Period, levels = PER_LABEL)
    ssp_rows$SSP    <- factor(ssp_rows$SSP, levels = ALL_SSP_LBLS)

    p_traj <- ggplot(ssp_rows, aes(x = Period, y = pct_suit, colour = SSP, group = SSP)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      { if (!is.null(pres_r)) {
          pv <- if (!is.null(aoi_v)) {
            av2 <- tryCatch(project(aoi_v, crs(pres_r)), error=function(e) aoi_v)
            tryCatch(values(mask(crop(pres_r,av2),av2),na.rm=TRUE), error=function(e) values(pres_r,na.rm=TRUE))
          } else values(pres_r, na.rm=TRUE)
          pres_pct <- mean(pv >= THRESHOLD, na.rm=TRUE)*100
          list(geom_hline(yintercept=pres_pct, linetype="dashed", colour="grey30"),
               annotate("text",x=0.65,y=pres_pct+0.3,
                        label=sprintf("Present: %.1f%%",pres_pct),size=3,hjust=0,colour="grey30"))
      }} +
      scale_colour_manual(values = setNames(ALL_SSP_COLS, ALL_SSP_LBLS)) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(x = "Time period", y = "% suitable area", colour = "SSP",
           title = "Suitable habitat trajectory by SSP") +
      theme_bw(base_size = 10) +
      theme(legend.position = "right", panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold"))

    fig_a2 <- p_stab + p_traj + plot_layout(widths = c(1.8, 1))
    fig_a2 <- fig_a2 + plot_annotation(
      title    = expression(paste("Temporal Habitat Stability — ", italic("Elephas maximus"), " | Bhutan")),
      subtitle = "Blue = persistently suitable habitat | Red = consistently lost habitat | Dashed = present baseline",
      caption  = "CRS: EPSG:32645 | Stability computed across 4 SSPs \u00d7 3 periods = 12 future time steps",
      theme = theme(plot.title    = element_text(size = 13, face = "bold", hjust = 0.5),
                    plot.subtitle = element_text(size = 9,  hjust = 0.5,  colour = "grey35"),
                    plot.caption  = element_text(size = 7.5, colour = "grey50", hjust = 1))
    )
    ggsave(file.path(ADOUT, "A2_temporal_stability.png"), fig_a2,
           width = 20, height = 10, dpi = 300, limitsize = FALSE)
    cat("  + A2 saved\n")
  }
}, error = function(e) cat(sprintf("  A2 failed: %s\n", conditionMessage(e))))

# =============================================================================
# A3: VARIANCE DECOMPOSITION — GCM × SSP × Algorithm
# At 2071-2100, partition total ensemble variance into:
#   Var_GCM   = mean over (SSP, algo) of var across 9 GCMs
#   Var_SSP   = mean over (GCM, algo) of var across 4 SSPs
#   Var_Algo  = mean over (GCM, SSP) of var across 4 algorithms
# =============================================================================
cat("A3: Variance Decomposition...\n")
tryCatch({
  period_vd <- "2071_2100"
  cat("  Loading 128 individual rasters for 2071-2100...\n")

  # Load all 128 = 8 GCM × 4 SSP × 4 algo
  by_gcm  <- list()  # grouped by GCM for var_gcm
  by_ssp  <- list()  # grouped by SSP for var_ssp
  by_algo <- list()  # grouped by algo for var_algo
  ref_r   <- pres_r

  all_rasts <- list()
  for (gcm in GCM_LIST) {
    for (ssp in ALL_SSPS) {
      for (algo in ALGOS) {
        f <- file.path(FUT, sprintf("suitability_future_%s_%s_%s_%s.tif", gcm, ssp, period_vd, algo))
        if (!file.exists(f)) next
        r <- tryCatch(rast(f), error = function(e) NULL)
        if (is.null(r)) next
        if (!is.null(ref_r)) r <- tryCatch(resample(r, ref_r, method = "bilinear"), error = function(e) r)
        key_gcm  <- gcm
        key_ssp  <- ssp
        key_algo <- algo
        by_gcm[[key_gcm]]   <- c(by_gcm[[key_gcm]],   list(r))
        by_ssp[[key_ssp]]   <- c(by_ssp[[key_ssp]],   list(r))
        by_algo[[key_algo]] <- c(by_algo[[key_algo]], list(r))
        all_rasts <- c(all_rasts, list(r))
      }
    }
  }
  cat(sprintf("  Loaded %d rasters\n", length(all_rasts)))

  if (length(all_rasts) >= 4) {
    # Total variance across all scenarios
    all_stk    <- rast(all_rasts)
    var_total  <- app(all_stk, function(v) var(v, na.rm = TRUE))

    # Variance within GCM groups (average SD² across GCMs, holding others fixed)
    gcm_var_list <- lapply(by_gcm, function(rs) {
      if (length(rs) < 2) return(NULL)
      app(rast(rs), function(v) var(v, na.rm = TRUE))
    })
    gcm_var_list <- gcm_var_list[!sapply(gcm_var_list, is.null)]
    var_gcm  <- if (length(gcm_var_list) > 0) app(rast(gcm_var_list), function(v) mean(v, na.rm = TRUE)) else var_total * 0

    ssp_var_list <- lapply(by_ssp, function(rs) {
      if (length(rs) < 2) return(NULL)
      app(rast(rs), function(v) var(v, na.rm = TRUE))
    })
    ssp_var_list <- ssp_var_list[!sapply(ssp_var_list, is.null)]
    var_ssp  <- if (length(ssp_var_list) > 0) app(rast(ssp_var_list), function(v) mean(v, na.rm = TRUE)) else var_total * 0

    algo_var_list <- lapply(by_algo, function(rs) {
      if (length(rs) < 2) return(NULL)
      app(rast(rs), function(v) var(v, na.rm = TRUE))
    })
    algo_var_list <- algo_var_list[!sapply(algo_var_list, is.null)]
    var_algo <- if (length(algo_var_list) > 0) app(rast(algo_var_list), function(v) mean(v, na.rm = TRUE)) else var_total * 0

    # Normalize to proportional contribution
    var_sum  <- var_gcm + var_ssp + var_algo
    prop_gcm  <- ifel(var_sum > 1e-10, var_gcm  / var_sum, 1/3)
    prop_ssp  <- ifel(var_sum > 1e-10, var_ssp  / var_sum, 1/3)
    prop_algo <- ifel(var_sum > 1e-10, var_algo / var_sum, 1/3)

    # Dominant source: 1=GCM, 2=SSP, 3=Algo
    dominant  <- ifel(prop_gcm >= prop_ssp & prop_gcm >= prop_algo, 1L,
                 ifel(prop_ssp >= prop_algo, 2L, 3L))

    writeRaster(prop_gcm,  file.path(ADOUT, "A3_var_prop_gcm.tif"),  overwrite = TRUE)
    writeRaster(prop_ssp,  file.path(ADOUT, "A3_var_prop_ssp.tif"),  overwrite = TRUE)
    writeRaster(prop_algo, file.path(ADOUT, "A3_var_prop_algo.tif"), overwrite = TRUE)
    writeRaster(dominant,  file.path(ADOUT, "A3_var_dominant.tif"),  overwrite = TRUE)

    df_gcm  <- r2df(prop_gcm)
    df_ssp  <- r2df(prop_ssp)
    df_algo <- r2df(prop_algo)
    df_dom  <- r2df(dominant)

    # Global proportions for pie
    g_gcm  <- mean(values(prop_gcm,  na.rm = TRUE), na.rm = TRUE)
    g_ssp  <- mean(values(prop_ssp,  na.rm = TRUE), na.rm = TRUE)
    g_algo <- mean(values(prop_algo, na.rm = TRUE), na.rm = TRUE)
    pie_df <- data.frame(
      Source = c("GCM","SSP","Algorithm"),
      pct    = c(g_gcm, g_ssp, g_algo) / (g_gcm + g_ssp + g_algo) * 100,
      col    = c("#377eb8","#e41a1c","#4daf4a")
    )
    cat(sprintf("  Variance: GCM=%.0f%% SSP=%.0f%% Algo=%.0f%%\n",
                pie_df$pct[1], pie_df$pct[2], pie_df$pct[3]))

    prop_scale <- scale_fill_gradientn(
      colours = c("#ffffcc","#fd8d3c","#800026"),
      limits = c(0, 1), na.value = "white",
      name = "Proportion",
      guide = guide_colorbar(barwidth = 0.5, barheight = 3,
                             title.position = "top", title.hjust = 0.5,
                             label.theme = element_text(size = 6))
    )

    make_var_map <- function(df, title_str) {
      ggplot() +
        geom_raster(data = df, aes(x = x, y = y, fill = value)) +
        add_aoi() + prop_scale +
        coord_sf(crs = 32645, xlim = XLIM, ylim = YLIM, expand = FALSE) +
        labs(title = title_str) + map_theme
    }

    p_gcm_map  <- make_var_map(df_gcm,  "GCM uncertainty")
    p_ssp_map  <- make_var_map(df_ssp,  "SSP uncertainty")
    p_algo_map <- make_var_map(df_algo, "Algorithm uncertainty")

    # Dominant source map
    dom_pal <- c("1" = "#377eb8", "2" = "#e41a1c", "3" = "#4daf4a")
    dom_lbl <- c("1" = "GCM", "2" = "SSP", "3" = "Algorithm")
    df_dom$cat <- dom_lbl[as.character(round(df_dom$value))]
    df_dom$cat <- factor(df_dom$cat, levels = c("GCM","SSP","Algorithm"))
    p_dom <- ggplot() +
      geom_raster(data = df_dom[!is.na(df_dom$cat), ], aes(x = x, y = y, fill = cat)) +
      add_aoi() +
      scale_fill_manual(values = c(GCM="#377eb8", SSP="#e41a1c", Algorithm="#4daf4a"),
                        na.value = "white", name = "Dominant\nsource") +
      coord_sf(crs = 32645, xlim = XLIM, ylim = YLIM, expand = FALSE) +
      labs(title = "Dominant uncertainty source") + map_theme

    # Pie chart
    p_pie <- ggplot(pie_df, aes(x = "", y = pct, fill = Source)) +
      geom_col(width = 1, colour = "white", linewidth = 0.5) +
      coord_polar(theta = "y") +
      geom_text(aes(label = sprintf("%s\n%.0f%%", Source, pct)),
                position = position_stack(vjust = 0.5), size = 3.5, fontface = "bold") +
      scale_fill_manual(values = c(GCM="#377eb8",SSP="#e41a1c",Algorithm="#4daf4a"),
                        guide = "none") +
      labs(title = "Overall variance\npartition") +
      theme_void(base_size = 9) +
      theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5))

    fig_a3 <- (p_gcm_map | p_ssp_map | p_algo_map | p_dom) / p_pie +
      plot_layout(heights = c(4, 1))
    fig_a3 <- fig_a3 + plot_annotation(
      title    = expression(paste("Uncertainty Source Decomposition — ", italic("Elephas maximus"), " | 2071\u20132100")),
      subtitle = "Orange-red = high contribution to total variance | Dominant source = factor explaining most spatial uncertainty",
      caption  = "Variance decomposed into GCM choice, SSP scenario, and algorithm contributions | CRS: EPSG:32645",
      theme = theme(plot.title    = element_text(size = 13, face = "bold", hjust = 0.5),
                    plot.subtitle = element_text(size = 9,  hjust = 0.5,  colour = "grey35"),
                    plot.caption  = element_text(size = 7.5, colour = "grey50", hjust = 1))
    )
    ggsave(file.path(ADOUT, "A3_variance_decomposition.png"), fig_a3,
           width = 22, height = 14, dpi = 300, limitsize = FALSE)
    cat("  + A3 saved\n")
  }
  rm(all_rasts, all_stk); gc()
}, error = function(e) cat(sprintf("  A3 failed: %s\n", conditionMessage(e))))

# =============================================================================
# A4: PA EFFECTIVENESS TRAJECTORIES
# For each protected area polygon: mean suitability present + 12 future time steps
# =============================================================================
cat("A4: PA Effectiveness Trajectories...\n")
tryCatch({
  pa_path <- list.files(
    repo_path("01_data_raw", "03_vector", "shapefiles", "PA_Bhutan", repo_root = repo_root),
    pattern = "\\.shp$", full.names = TRUE)
  if (length(pa_path) == 0) stop("No PA shapefile found")
  pa_sf <- st_read(pa_path[1], quiet = TRUE)
  if (is.na(st_crs(pa_sf))) st_crs(pa_sf) <- 32645
  if (st_crs(pa_sf)$epsg != 32645) pa_sf <- st_transform(pa_sf, 32645)

  # Find name column
  name_col <- intersect(c("NAME","Name","name","PA_NAME","pa_name","PROTECTED_","OBJECTID","FID"),
                        names(pa_sf))
  if (length(name_col) == 0) {
    pa_sf$PA_ID <- paste0("PA_", seq_len(nrow(pa_sf)))
    name_col <- "PA_ID"
  } else {
    name_col <- name_col[1]
  }
  cat(sprintf("  %d protected areas found\n", nrow(pa_sf)))

  pa_v <- vect(pa_sf)

  extract_mean <- function(r, pa_vect) {
    tryCatch({
      r_aligned <- tryCatch(project(r, "EPSG:32645"), error=function(e) r)
      ex <- terra::extract(r_aligned, pa_vect, fun = mean, na.rm = TRUE)
      as.numeric(ex[, 2])
    }, error = function(e) rep(NA_real_, nrow(pa_sf)))
  }

  # Present
  pa_rows <- data.frame(PA = pa_sf[[name_col]],
                        period_label = "Present",
                        ssp = "Present", stringsAsFactors = FALSE)
  pa_rows$mean_suit <- if (!is.null(pres_r)) extract_mean(pres_r, pa_v) else NA_real_
  pa_rows$pct_suit  <- pa_rows$mean_suit  # approximate

  # Future
  for (si in seq_along(ALL_SSPS)) {
    ssp <- ALL_SSPS[si]
    for (pi in seq_along(PERIODS)) {
      period <- PERIODS[pi]
      mean_r <- get_ens_rast(ssp, period)
      if (is.null(mean_r)) next
      if (!is.null(pres_r)) mean_r <- tryCatch(resample(mean_r, pres_r, method="bilinear"),
                                                error=function(e) mean_r)
      ms <- extract_mean(mean_r, pa_v)
      new_row <- data.frame(PA = pa_sf[[name_col]],
                            period_label = PER_LABEL[pi],
                            ssp = ALL_SSP_LBLS[si],
                            mean_suit = ms, pct_suit = ms,
                            stringsAsFactors = FALSE)
      pa_rows <- rbind(pa_rows, new_row)
    }
  }

  pa_rows$period_label <- factor(pa_rows$period_label,
                                  levels = c("Present", PER_LABEL))
  pa_rows$ssp <- factor(pa_rows$ssp,
                         levels = c("Present", ALL_SSP_LBLS))

  # Resilience ranking: mean suitability in 2071-2100 SSP585 vs present
  present_mean  <- pa_rows[pa_rows$ssp == "Present", c("PA","mean_suit")]
  future_585    <- pa_rows[pa_rows$ssp == "SSP5-8.5" & pa_rows$period_label == "2071\u20132100",
                           c("PA","mean_suit")]
  names(present_mean)[2] <- "present_suit"
  names(future_585)[2]   <- "future_suit"
  resilience <- merge(present_mean, future_585, by = "PA", all = TRUE)
  resilience$change <- resilience$future_suit - resilience$present_suit
  resilience <- resilience[order(resilience$change, decreasing = TRUE), ]
  write.csv(resilience, file.path(ADOUT, "A4_pa_resilience_ranking.csv"), row.names = FALSE)

  # Plot: faceted by SSP, line per PA
  pa_future <- pa_rows[pa_rows$ssp != "Present", ]
  pa_present_line <- pa_rows[pa_rows$ssp == "Present", ]

  n_pa <- length(unique(pa_rows$PA))
  pa_cols <- colorRampPalette(c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00",
                                "#a65628","#f781bf","#999999"))(n_pa)
  names(pa_cols) <- unique(pa_rows$PA)

  p_pa_traj <- ggplot(pa_future, aes(x = period_label, y = mean_suit * 100,
                                      colour = PA, group = PA)) +
    geom_line(linewidth = 1.0, alpha = 0.85) +
    geom_point(size = 2) +
    geom_hline(data = pa_present_line[!duplicated(pa_present_line$PA), ],
               aes(yintercept = mean_suit * 100, colour = PA),
               linetype = "dashed", linewidth = 0.6, alpha = 0.5) +
    facet_wrap(~ssp, ncol = 2) +
    scale_colour_manual(values = pa_cols, name = "Protected Area") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(x = "Time period", y = "Mean suitability (%)",
         title = expression(paste("Protected Area Effectiveness Trajectories — ", italic("Elephas maximus")))) +
    theme_bw(base_size = 10) +
    theme(strip.background = element_rect(fill = "#2c3e50"),
          strip.text = element_text(colour = "white", face = "bold"),
          legend.position = "right", legend.text = element_text(size = 7.5),
          axis.text.x = element_text(angle = 15, hjust = 1),
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", size = 11))

  # Resilience bar chart
  resilience$PA_short <- substr(as.character(resilience$PA), 1, 25)
  resilience$PA_short <- factor(resilience$PA_short,
                                 levels = resilience$PA_short[order(resilience$change)])
  p_resil <- ggplot(resilience, aes(x = change * 100, y = PA_short,
                                     fill = change > 0)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, linewidth = 0.6, colour = "grey30") +
    scale_fill_manual(values = c("TRUE" = "#1a9850", "FALSE" = "#d73027"),
                      labels = c("TRUE" = "Gain", "FALSE" = "Loss"), name = NULL) +
    scale_x_continuous(labels = function(x) paste0(ifelse(x>0,"+",""), round(x,1), "%")) +
    labs(x = "\u0394 mean suitability (SSP5-8.5, 2071\u20132100 vs present)",
         y = NULL, title = "PA Climate Resilience Ranking") +
    theme_bw(base_size = 10) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

  fig_a4 <- p_pa_traj / p_resil + plot_layout(heights = c(2, 1))
  fig_a4 <- fig_a4 + plot_annotation(
    caption = "Dashed lines = present-day baseline per PA | Green = climate-resilient PAs | Red = at-risk PAs",
    theme = theme(plot.caption = element_text(size = 7.5, colour = "grey50", hjust = 1))
  )
  ggsave(file.path(ADOUT, "A4_pa_effectiveness.png"), fig_a4,
         width = 18, height = 16, dpi = 300, limitsize = FALSE)
  cat(sprintf("  + A4 saved (%d PAs analysed)\n", nrow(resilience)))
}, error = function(e) cat(sprintf("  A4 failed: %s\n", conditionMessage(e))))

# =============================================================================
# A5: CONNECTIVITY CORRIDORS — SKIPPED
# Only 2 large habitat patches detected at 1 km resolution (15.9% suitable area
# is too fragmented for meaningful corridor analysis at national scale).
# Consider re-running at finer resolution or with a lower threshold.
# =============================================================================
if (FALSE) {  # disabled
cat("A5: Connectivity Corridors...\n")
tryCatch({
  if (!requireNamespace("gdistance", quietly = TRUE)) stop("gdistance not installed")
  library(gdistance)

  if (is.null(pres_r)) stop("Present suitability raster not available")

  # Coarsen to 1 km for speed (gdistance is memory intensive at 250m)
  pres_1km <- tryCatch(
    aggregate(pres_r, fact = 4, fun = "mean", na.rm = TRUE),
    error = function(e) pres_r)

  # Mask to AOI
  if (!is.null(aoi_v)) {
    av_proj <- tryCatch(project(aoi_v, crs(pres_1km)), error=function(e) aoi_v)
    pres_1km <- tryCatch(mask(crop(pres_1km, av_proj), av_proj), error=function(e) pres_1km)
  }

  # Resistance = exp(-k * suitability), k=5 — high suitability = low resistance
  k <- 5
  resist_r <- exp(-k * pres_1km)

  # Convert to RasterLayer for gdistance
  resist_rl <- as(resist_r, "Raster")

  # Transition matrix (8-direction)
  trans <- transition(resist_rl, transitionFunction = mean, directions = 8)
  trans <- geoCorrection(trans, type = "c")

  # Find habitat patches (cells above threshold)
  bin_r <- pres_1km >= THRESHOLD
  bin_rl <- as(bin_r, "Raster")

  # Get patch centroids — use clump to find connected patches
  patch_r <- tryCatch(clump(bin_rl, directions = 8), error = function(e) NULL)
  if (is.null(patch_r)) stop("Clump failed")

  patch_terra <- rast(patch_r)
  patch_vals  <- unique(values(patch_terra, na.rm = TRUE))
  patch_vals  <- patch_vals[!is.na(patch_vals)]
  cat(sprintf("  Found %d habitat patches\n", length(patch_vals)))

  # Keep only patches > 100 cells (filter tiny fragments)
  patch_sizes <- table(values(patch_terra, na.rm = TRUE))
  large_patches <- as.integer(names(patch_sizes[patch_sizes >= 100]))
  cat(sprintf("  Large patches (>=100 cells): %d\n", length(large_patches)))

  if (length(large_patches) < 2) stop("Fewer than 2 large patches — cannot compute corridors")
  if (length(large_patches) > 20) large_patches <- large_patches[1:20]  # cap for speed

  # Patch centroids
  patch_centroids <- do.call(rbind, lapply(large_patches, function(pid) {
    cells <- which(values(patch_terra) == pid)
    coords <- xyFromCell(patch_rl <- as(patch_terra, "Raster"), cells)
    data.frame(patch = pid, x = mean(coords[, "x"]), y = mean(coords[, "y"]))
  }))

  # Compute all pairwise least-cost paths
  n_patches <- nrow(patch_centroids)
  corridor_lines <- list()
  cost_matrix    <- matrix(NA, n_patches, n_patches)

  sp_origins <- SpatialPoints(patch_centroids[, c("x","y")],
                              proj4string = CRS(projection(resist_rl)))
  for (i in 1:(n_patches - 1)) {
    for (j in (i + 1):n_patches) {
      tryCatch({
        path_ij  <- shortestPath(trans, sp_origins[i,], sp_origins[j,], output = "SpatialLines")
        cost_ij  <- costDistance(trans, sp_origins[i,], sp_origins[j,])
        cost_matrix[i, j] <- cost_matrix[j, i] <- cost_ij
        path_sf  <- st_as_sf(path_ij) %>% st_set_crs(32645)
        path_sf$from <- patch_centroids$patch[i]
        path_sf$to   <- patch_centroids$patch[j]
        path_sf$cost <- cost_ij
        corridor_lines[[length(corridor_lines) + 1]] <- path_sf
      }, error = function(e) NULL)
    }
  }

  if (length(corridor_lines) > 0) {
    corridors_sf <- do.call(rbind, corridor_lines)
    st_write(corridors_sf, file.path(ADOUT, "A5_corridors.gpkg"),
             delete_dsn = TRUE, quiet = TRUE)

    df_suit <- r2df(pres_1km)
    corridors_sf$cost_norm <- (corridors_sf$cost - min(corridors_sf$cost, na.rm=TRUE)) /
                               diff(range(corridors_sf$cost, na.rm=TRUE))

    p_corr <- ggplot() +
      geom_raster(data = df_suit, aes(x = x, y = y, fill = value)) +
      suit_scale_adv(name = "Suitability") +
      add_aoi() +
      geom_sf(data = corridors_sf, aes(colour = cost_norm), linewidth = 0.8,
              inherit.aes = FALSE) +
      scale_colour_gradient(low = "#ffff00", high = "#ff0000",
                            name = "Corridor cost\n(yellow=easy)", guide = "colourbar") +
      coord_sf(crs = 32645, xlim = XLIM, ylim = YLIM, expand = FALSE) +
      labs(title = expression(paste(italic("Elephas maximus"), " Movement Corridors — Bhutan")),
           subtitle = sprintf("Least-cost paths between %d major habitat patches | Resistance = exp(-5 \u00d7 suitability)",
                              n_patches)) +
      map_theme +
      theme(plot.title = element_text(size = 11, face = "bold"),
            plot.subtitle = element_text(size = 8))

    ggsave(file.path(ADOUT, "A5_connectivity_corridors.png"), p_corr,
           width = 14, height = 10, dpi = 300, limitsize = FALSE)
    cat(sprintf("  + A5 saved (%d corridors)\n", nrow(corridors_sf)))
  }
}, error = function(e) cat(sprintf("  A5 skipped/failed: %s\n", conditionMessage(e))))
}  # end if (FALSE)

# =============================================================================
# A6: LIMITING FACTOR MAPS
# For each pixel, which selected predictor is furthest from its optimal range?
# Method: load selected predictors, standardise to 0-1, compare to response curve peaks.
# Uses RF model variable importance as weights.
# =============================================================================
cat("A6: Limiting Factor Maps...\n")
tryCatch({
  mf <- read.csv(file.path(RUN, "01_processed_data/predictor_manifest.csv"),
                 stringsAsFactors = FALSE)
  selected <- mf[mf$selected == TRUE, ]
  cat(sprintf("  %d selected predictors\n", nrow(selected)))

  # Load modeling dataset for quantile ranges
  dat <- read.csv(file.path(RUN, "01_processed_data/modeling_dataset.csv"))
  pres_dat <- dat[dat$response == 1, ]

  # Load RF model for importance
  rf_mod <- tryCatch(readRDS(file.path(RUN, "02_models/model_rf.rds")), error = function(e) NULL)
  rf_imp  <- if (!is.null(rf_mod) && !is.null(rf_mod$variable.importance))
    rf_mod$variable.importance else NULL

  pred_stack <- list()
  for (i in seq_len(nrow(selected))) {
    pred_name <- selected$predictor[i]
    pred_path <- selected$path[i]
    if (!file.exists(pred_path)) next
    r <- tryCatch(rast(pred_path), error = function(e) NULL)
    if (is.null(r)) next
    if (!is.null(pres_r)) r <- tryCatch(resample(r, pres_r, method="bilinear"), error=function(e) r)
    names(r) <- pred_name
    pred_stack[[pred_name]] <- r
  }
  cat(sprintf("  Loaded %d predictor rasters\n", length(pred_stack)))

  if (length(pred_stack) >= 3) {
    # For each predictor: "stress" = how far current value is from presence optimal range
    # Optimal = [5th, 95th percentile of presence locations]
    stress_list <- list()
    for (pred_name in names(pred_stack)) {
      if (!pred_name %in% names(pres_dat)) next
      r   <- pred_stack[[pred_name]]
      opt_lo <- quantile(pres_dat[[pred_name]], 0.05, na.rm = TRUE)
      opt_hi <- quantile(pres_dat[[pred_name]], 0.95, na.rm = TRUE)
      opt_rng <- opt_hi - opt_lo
      if (opt_rng < 1e-10) next

      # Stress = normalised distance outside optimal range
      stress_r <- ifel(r < opt_lo, (opt_lo - r) / opt_rng,
                  ifel(r > opt_hi, (r - opt_hi) / opt_rng, 0))
      stress_r <- clamp(stress_r, 0, 1)

      # Weight by RF importance if available
      imp_weight <- if (!is.null(rf_imp) && pred_name %in% names(rf_imp))
        rf_imp[pred_name] / sum(rf_imp, na.rm=TRUE) else 1 / length(pred_stack)
      stress_list[[pred_name]] <- stress_r * imp_weight
    }

    if (length(stress_list) >= 2) {
      # Stack and find which predictor has max weighted stress per pixel
      stress_stk <- rast(stress_list)
      max_stress  <- app(stress_stk, function(v) max(v, na.rm = TRUE))
      which_max   <- app(stress_stk, which.max)

      pred_labels <- names(stress_list)
      writeRaster(which_max, file.path(ADOUT, "A6_limiting_factor.tif"), overwrite = TRUE)
      writeRaster(max_stress, file.path(ADOUT, "A6_limiting_stress.tif"), overwrite = TRUE)

      df_lim <- r2df(which_max)
      df_lim$predictor <- pred_labels[round(df_lim$value)]
      df_lim$predictor[is.na(df_lim$predictor)] <- NA
      df_lim <- df_lim[!is.na(df_lim$predictor), ]
      df_lim$predictor <- factor(df_lim$predictor, levels = pred_labels)

      df_stress <- r2df(max_stress)

      n_preds <- length(pred_labels)
      pred_col_pal <- colorRampPalette(c("#e41a1c","#377eb8","#4daf4a","#984ea3",
                                         "#ff7f00","#a65628","#f781bf","#999999",
                                         "#66c2a5","#fc8d62","#8da0cb","#e78ac3"))(n_preds)
      names(pred_col_pal) <- pred_labels

      p_lim <- ggplot() +
        geom_raster(data = df_lim, aes(x = x, y = y, fill = predictor)) +
        add_aoi() +
        scale_fill_manual(values = pred_col_pal, na.value = "white",
                          name = "Limiting\nfactor") +
        coord_sf(crs = 32645, xlim = XLIM, ylim = YLIM, expand = FALSE) +
        labs(title = "Limiting Factor Map",
             subtitle = "Most constraining predictor per pixel (weighted by RF importance)") +
        map_theme + theme(plot.title = element_text(size = 11, face = "bold"),
                          legend.text = element_text(size = 6.5),
                          legend.key.size = unit(0.35, "cm"))

      p_stress <- ggplot() +
        geom_raster(data = df_stress, aes(x = x, y = y, fill = value)) +
        add_aoi() +
        scale_fill_gradientn(colours = c("#ffffcc","#fe9929","#cc4c02"),
                             limits = c(0, 1), na.value = "white",
                             name = "Max stress\nscore",
                             guide = guide_colorbar(barwidth=0.5, barheight=4,
                                                    title.position="top",title.hjust=0.5,
                                                    label.theme=element_text(size=6))) +
        coord_sf(crs = 32645, xlim = XLIM, ylim = YLIM, expand = FALSE) +
        labs(title = "Maximum Stress Intensity",
             subtitle = "Weighted distance from optimal predictor range") +
        map_theme + theme(plot.title = element_text(size = 11, face = "bold"))

      # Bar chart: % area limited by each predictor
      lim_pct <- df_lim %>%
        count(predictor) %>%
        mutate(pct = n / sum(n) * 100) %>%
        arrange(desc(pct))

      p_bar <- ggplot(lim_pct, aes(x = reorder(predictor, pct), y = pct, fill = predictor)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3) +
        scale_fill_manual(values = pred_col_pal) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, max(lim_pct$pct) * 1.25)) +
        coord_flip() +
        labs(x = NULL, y = "% of Bhutan area", title = "Limiting factor prevalence") +
        theme_bw(base_size = 9) + theme(panel.grid.minor = element_blank(),
                                         plot.title = element_text(face = "bold"))

      fig_a6 <- (p_lim | p_stress) / p_bar + plot_layout(heights = c(2.5, 1))
      fig_a6 <- fig_a6 + plot_annotation(
        title    = expression(paste("Habitat Suitability Limiting Factors — ", italic("Elephas maximus"), " | Bhutan")),
        subtitle = "Limiting factor = predictor with highest weighted stress (distance outside presence optimal range)",
        caption  = "Stress weighted by RF variable importance | Optimal range = 5th\u201395th percentile of presence locations",
        theme = theme(plot.title    = element_text(size = 13, face = "bold", hjust = 0.5),
                      plot.subtitle = element_text(size = 9,  hjust = 0.5,  colour = "grey35"),
                      plot.caption  = element_text(size = 7.5, colour = "grey50", hjust = 1))
      )
      ggsave(file.path(ADOUT, "A6_limiting_factors.png"), fig_a6,
             width = 20, height = 14, dpi = 300, limitsize = FALSE)
      cat("  + A6 saved\n")
    }
  }
}, error = function(e) cat(sprintf("  A6 failed: %s\n", conditionMessage(e))))

# =============================================================================
# A7: CLIMATE VELOCITY
# Velocity = (temporal gradient in BIO05) / (spatial gradient in BIO05)
# Units: km/decade
# =============================================================================
cat("A7: Climate Velocity...\n")
tryCatch({
  bio_var  <- "bio05"   # max temp of warmest month — key thermal constraint (future files use bio05)
  bio_var_pres <- "bio5"  # present files use bio5 (no leading zero)
  fut_root <- repo_path("01_data_raw", "02_rasters", "future", repo_root = repo_root)
  pres_bio <- repo_path("01_data_raw", "02_rasters", "present", "Historical_bioclims", "Historical_1986-2015_bio5.tif", repo_root = repo_root)

  if (!file.exists(pres_bio)) stop("Present BIO05 not found")
  pres_bio_r <- rast(pres_bio)
  if (!is.null(pres_r)) pres_bio_r <- tryCatch(resample(pres_bio_r, pres_r, method="bilinear"),
                                                error=function(e) pres_bio_r)
  if (!is.null(aoi_v)) {
    av_bio <- tryCatch(project(aoi_v, crs(pres_bio_r)), error=function(e) aoi_v)
    pres_bio_r <- tryCatch(mask(crop(pres_bio_r, av_bio), av_bio), error=function(e) pres_bio_r)
  }

  velo_rows <- list()
  for (ssp in c("ssp245","ssp585")) {
    for (pi in seq_along(PERIODS)) {
      period   <- PERIODS[pi]
      mid_year <- c(2035, 2065, 2085)[pi]
      delta_t  <- mid_year - 2000  # years from baseline

      # Mean future BIO05 across 9 GCMs
      fut_bio_list <- lapply(GCM_LIST, function(gcm) {
        f <- file.path(fut_root, gcm, period, ssp,
                       sprintf("bhutan_cmip6_%s_%s_%s_%s_v1_0.tif", gcm, ssp, period, bio_var))
        if (!file.exists(f)) return(NULL)
        r <- tryCatch(rast(f), error = function(e) NULL)
        if (is.null(r)) return(NULL)
        tryCatch(resample(r, pres_bio_r, method="bilinear"), error=function(e) r)
      })
      fut_bio_list <- fut_bio_list[!sapply(fut_bio_list, is.null)]
      if (length(fut_bio_list) == 0) next

      fut_bio_mean <- if (length(fut_bio_list) == 1) fut_bio_list[[1]] else
                        app(rast(fut_bio_list), function(v) mean(v, na.rm = TRUE))

      # Temporal gradient: ΔT / Δt  (°C / year)
      delta_bio <- fut_bio_mean - pres_bio_r
      temp_grad <- delta_bio / delta_t   # °C/year

      # Spatial gradient: computed via Sobel filter on present BIO05
      # Using focal() for dx, dy components
      # Sobel kernels (3×3)
      sobel_x <- matrix(c(-1,0,1,-2,0,2,-1,0,1), nrow=3)
      sobel_y <- matrix(c(-1,-2,-1,0,0,0,1,2,1), nrow=3)

      res_m <- mean(res(pres_bio_r)) / 1000  # cell size in km

      grad_x <- focal(pres_bio_r, sobel_x, na.rm = TRUE) / (8 * res_m)  # °C/km
      grad_y <- focal(pres_bio_r, sobel_y, na.rm = TRUE) / (8 * res_m)  # °C/km
      spat_grad_mag <- sqrt(grad_x^2 + grad_y^2)  # °C/km

      # Climate velocity (km/year), avoid /0
      velocity_yr <- ifel(spat_grad_mag > 0.001,
                          abs(temp_grad) / spat_grad_mag,
                          NA)
      # Convert to km/decade, cap at 200 km/decade
      velocity_dec <- clamp(velocity_yr * 10, 0, 200)

      # Direction of climate tracking (upslope/northward tendency)
      dir_angle <- atan2(grad_y, grad_x) * 180 / pi  # degrees
      # Adjust direction: climate tracking direction is opposite to spatial gradient
      # when warming → species must move toward cooler areas (uphill/north)
      tracking_dir <- ifel(temp_grad > 0, (dir_angle + 180) %% 360, dir_angle)

      velo_rows[[length(velo_rows) + 1]] <- list(
        ssp = ssp, period = period, per_label = PER_LABEL[pi],
        velocity = velocity_dec, direction = tracking_dir
      )
    }
  }

  if (length(velo_rows) >= 2) {
    # Build 6-panel figure: 2 SSPs × 3 periods
    panels_v <- lapply(velo_rows, function(vr) {
      df_v <- r2df(vr$velocity)
      ssp_lbl <- c(ssp245="SSP2-4.5", ssp585="SSP5-8.5")[vr$ssp]
      p <- ggplot() +
        geom_raster(data = df_v, aes(x = x, y = y, fill = value)) +
        add_aoi() +
        scale_fill_gradientn(
          colours = c("#ffffcc","#fd8d3c","#d7191c","#7b0000"),
          values  = rescale(c(0, 5, 20, 50)),
          limits  = c(0, 50), oob = squish, na.value = "white",
          name    = "Velocity\n(km/decade)",
          guide   = guide_colorbar(barwidth=0.4, barheight=3, title.position="top",
                                   title.hjust=0.5, label.theme=element_text(size=6))
        ) +
        coord_sf(crs = 32645, xlim = XLIM, ylim = YLIM, expand = FALSE) +
        labs(title = sprintf("%s | %s", ssp_lbl, vr$per_label)) +
        map_theme
      p
    })

    fig_a7 <- wrap_plots(panels_v, ncol = 3, nrow = 2) +
      plot_layout(guides = "collect") &
      theme(legend.position = "right")
    fig_a7 <- fig_a7 + plot_annotation(
      title    = expression(paste("Climate Velocity of BIO05 (T"[max], ") — ", italic("Elephas maximus"), " | Bhutan")),
      subtitle = "Speed at which climate zones shift (km/decade) | High velocity = fast-moving climate envelope",
      caption  = "Velocity = temporal gradient / spatial gradient | Red = fast-shifting climate (high tracking demand) | CRS: EPSG:32645",
      theme = theme(plot.title    = element_text(size = 13, face = "bold", hjust = 0.5),
                    plot.subtitle = element_text(size = 9,  hjust = 0.5,  colour = "grey35"),
                    plot.caption  = element_text(size = 7.5, colour = "grey50", hjust = 1))
    )
    ggsave(file.path(ADOUT, "A7_climate_velocity.png"), fig_a7,
           width = 20, height = 12, dpi = 300, limitsize = FALSE)
    cat("  + A7 saved\n")
  }
}, error = function(e) cat(sprintf("  A7 failed: %s\n", conditionMessage(e))))

# =============================================================================
# A8: THRESHOLD SENSITIVITY ANALYSIS
# How sensitive are key metrics to the suitability threshold?
# =============================================================================
cat("A8: Threshold Sensitivity...\n")
tryCatch({
  if (is.null(pres_r)) stop("Present suitability raster needed")

  thresholds <- seq(0.05, 0.50, by = 0.01)

  # Clip present to AOI
  pres_aoi <- if (!is.null(aoi_v)) {
    av_p <- tryCatch(project(aoi_v, crs(pres_r)), error=function(e) aoi_v)
    tryCatch(mask(crop(pres_r, av_p), av_p), error=function(e) pres_r)
  } else pres_r

  pres_vals <- values(pres_aoi, na.rm = TRUE)

  # Future 2071-2100 ensemble (one file per SSP)
  fut_rasts_all <- lapply(ALL_SSPS, function(ssp) get_ens_rast(ssp, "2071_2100"))
  fut_rasts_all <- fut_rasts_all[!sapply(fut_rasts_all, is.null)]

  fut_grand_mean <- if (length(fut_rasts_all) > 0) {
    aligned <- lapply(fut_rasts_all, function(r)
      tryCatch(resample(r, pres_aoi, method="bilinear"), error=function(e) r))
    if (length(aligned) == 1) aligned[[1]] else
      app(rast(aligned), function(v) mean(v, na.rm = TRUE))
  } else NULL
  fut_vals <- if (!is.null(fut_grand_mean)) values(fut_grand_mean, na.rm = FALSE) else NULL

  # PA suitability values
  pa_path_s <- list.files(
    repo_path("01_data_raw", "03_vector", "shapefiles", "PA_Bhutan", repo_root = repo_root),
    pattern = "\\.shp$", full.names = TRUE)
  pa_mask_vals <- if (length(pa_path_s) > 0) {
    pa_sf_s <- st_read(pa_path_s[1], quiet = TRUE)
    if (is.na(st_crs(pa_sf_s))) st_crs(pa_sf_s) <- 32645
    if (st_crs(pa_sf_s)$epsg != 32645) pa_sf_s <- st_transform(pa_sf_s, 32645)
    pa_v_s <- vect(pa_sf_s)
    pa_mask <- tryCatch({
      pm <- rasterize(project(pa_v_s, crs(pres_aoi)), pres_aoi, field = 1, background = 0)
      values(pm, na.rm = FALSE)
    }, error = function(e) NULL)
    pa_mask
  } else NULL

  sens_rows <- lapply(thresholds, function(thr) {
    pct_pres  <- mean(pres_vals >= thr, na.rm = TRUE) * 100
    pct_fut   <- if (!is.null(fut_vals)) mean(fut_vals >= thr, na.rm = TRUE) * 100 else NA_real_
    delta_pct <- pct_fut - pct_pres

    pa_overlap <- if (!is.null(pa_mask_vals) && length(pa_mask_vals) == length(pres_vals)) {
      suit_cells <- pres_vals >= thr
      pa_cells   <- pa_mask_vals[!is.na(pres_vals)] == 1
      if (sum(suit_cells, na.rm=TRUE) > 0)
        mean(pa_cells[suit_cells], na.rm = TRUE) * 100
      else NA_real_
    } else NA_real_

    data.frame(threshold = thr, pct_suitable_present = pct_pres,
               pct_suitable_future  = pct_fut, delta_pct = delta_pct,
               pa_overlap_pct = pa_overlap)
  })
  sens_df <- do.call(rbind, sens_rows)
  write.csv(sens_df, file.path(ADOUT, "A8_threshold_sensitivity.csv"), row.names = FALSE)

  p_suit_curve <- ggplot(sens_df) +
    geom_ribbon(aes(x = threshold, ymin = pct_suitable_future, ymax = pct_suitable_present),
                fill = "#d1e5f0", alpha = 0.6) +
    geom_line(aes(x = threshold, y = pct_suitable_present, colour = "Present"),
              linewidth = 1.3) +
    geom_line(aes(x = threshold, y = pct_suitable_future, colour = "2071-2100"),
              linewidth = 1.3, linetype = "dashed") +
    geom_vline(xintercept = THRESHOLD, linetype = "dotted", colour = "grey30", linewidth = 0.8) +
    annotate("text", x = THRESHOLD + 0.005, y = max(sens_df$pct_suitable_present, na.rm=TRUE) * 0.95,
             label = sprintf("Optimal TSS\nthreshold: %.3f", THRESHOLD),
             size = 3, hjust = 0, colour = "grey30") +
    scale_colour_manual(values = c("Present" = "#1a9850", "2071-2100" = "#d73027"),
                        name = NULL) +
    scale_x_continuous(breaks = seq(0.0, 0.5, 0.1)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(x = "Suitability threshold", y = "% suitable area (Bhutan)",
         title = "a  Suitable area vs threshold") +
    theme_bw(base_size = 11) +
    theme(legend.position = "top", panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"))

  p_pa_curve <- ggplot(sens_df, aes(x = threshold, y = pa_overlap_pct)) +
    geom_line(colour = "#225ea8", linewidth = 1.3) +
    geom_vline(xintercept = THRESHOLD, linetype = "dotted", colour = "grey30") +
    scale_x_continuous(breaks = seq(0.0, 0.5, 0.1)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(x = "Suitability threshold", y = "% suitable area inside PAs",
         title = "b  PA overlap vs threshold") +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

  p_delta_curve <- ggplot(sens_df, aes(x = threshold, y = delta_pct)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_ribbon(aes(ymin = pmin(delta_pct, 0), ymax = 0), fill = "#d73027", alpha = 0.3) +
    geom_ribbon(aes(ymin = 0, ymax = pmax(delta_pct, 0)), fill = "#1a9850", alpha = 0.3) +
    geom_line(colour = "grey20", linewidth = 1.3) +
    geom_vline(xintercept = THRESHOLD, linetype = "dotted", colour = "grey30") +
    scale_x_continuous(breaks = seq(0.0, 0.5, 0.1)) +
    scale_y_continuous(labels = function(x) paste0(ifelse(x>=0,"+",""), round(x,1), "%")) +
    labs(x = "Suitability threshold", y = "\u0394 suitable area (future \u2212 present)",
         title = "c  Change direction vs threshold") +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

  fig_a8 <- p_suit_curve / p_pa_curve / p_delta_curve
  fig_a8 <- fig_a8 + plot_annotation(
    title    = expression(paste("Threshold Sensitivity Analysis — ", italic("Elephas maximus"), " | Bhutan")),
    subtitle = "Robustness of key conservation metrics across a range of suitability thresholds",
    caption  = sprintf("Dotted line = optimal TSS threshold (%.3f) | Green shading = net gain | Red = net loss", THRESHOLD),
    theme = theme(plot.title    = element_text(size = 13, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 9,  hjust = 0.5,  colour = "grey35"),
                  plot.caption  = element_text(size = 7.5, colour = "grey50", hjust = 1))
  )
  ggsave(file.path(ADOUT, "A8_threshold_sensitivity.png"), fig_a8,
         width = 14, height = 16, dpi = 300, limitsize = FALSE)
  cat("  + A8 saved\n")
}, error = function(e) cat(sprintf("  A8 failed: %s\n", conditionMessage(e))))

# =============================================================================
# A9: HUMAN PRESSURE & CONFLICT RISK
# Conflict risk = suitability × human pressure (footprint + proximity to settlements)
# Present + future (SSP585 2071-2100)
# =============================================================================
cat("A9: Human Pressure & Conflict Risk...\n")
tryCatch({
  # Human footprint raster
  hfp_path  <- file.path(RUN, "02_data_intermediate/human_footprint_harmonized.tif")
  dist_set_path <- file.path(RUN, "01_processed_data/distance_layers/dist_to_settlements.tif")

  if (!file.exists(hfp_path)) stop("Human footprint raster not found")
  hfp_r  <- rast(hfp_path)
  if (!is.null(pres_r)) hfp_r <- tryCatch(resample(hfp_r, pres_r, method="bilinear"), error=function(e) hfp_r)

  # Normalise footprint to 0-1
  hfp_norm <- (hfp_r - global(hfp_r, "min", na.rm=TRUE)[[1]]) /
              (global(hfp_r, "max", na.rm=TRUE)[[1]] - global(hfp_r, "min", na.rm=TRUE)[[1]])
  hfp_norm <- clamp(hfp_norm, 0, 1)

  # Settlement proximity (1 - normalised dist) so close = high pressure
  if (file.exists(dist_set_path)) {
    dist_set <- rast(dist_set_path)
    if (!is.null(pres_r)) dist_set <- tryCatch(resample(dist_set, pres_r, method="bilinear"), error=function(e) dist_set)
    d_max <- global(dist_set, "max", na.rm=TRUE)[[1]]
    prox_norm <- 1 - clamp(dist_set / d_max, 0, 1)
  } else {
    prox_norm <- hfp_norm  # fallback
  }

  # Combined human pressure
  human_pressure <- (hfp_norm + prox_norm) / 2

  # Conflict risk = suitability × human pressure
  if (!is.null(pres_r)) {
    suit_norm  <- clamp(pres_r / SUIT_MAX, 0, 1)
    conflict_r <- suit_norm * human_pressure
    writeRaster(conflict_r, file.path(ADOUT, "A9_conflict_risk_present.tif"), overwrite = TRUE)
    df_conf <- r2df(conflict_r)

    # Future conflict (SSP585 2071-2100)
    fut_ens_single <- get_ens_rast("ssp585", "2071_2100")
    fut_ens_list   <- if (!is.null(fut_ens_single)) list(fut_ens_single) else list()
    fut_conflict_r <- NULL
    if (length(fut_ens_list) > 0) {
      aligned_f <- lapply(fut_ens_list, function(r)
        tryCatch(resample(r, pres_r, method="bilinear"), error=function(e) r))
      fut_ens_mean  <- if (length(aligned_f) == 1) aligned_f[[1]] else
                        app(rast(aligned_f), function(v) mean(v, na.rm = TRUE))
      fut_suit_norm <- clamp(fut_ens_mean / SUIT_MAX, 0, 1)
      fut_conflict_r <- fut_suit_norm * human_pressure
      writeRaster(fut_conflict_r, file.path(ADOUT, "A9_conflict_risk_ssp585_2071.tif"), overwrite = TRUE)
    }

    # Risk categories
    risk_levels <- c(0, 0.15, 0.30, 0.50, 1.0)
    risk_labels <- c("Low","Moderate","High","Very High")
    conf_cat <- classify(conflict_r,
                         matrix(c(risk_levels[-length(risk_levels)],
                                  risk_levels[-1],
                                  1:4), ncol=3))
    df_conf_cat <- r2df(conf_cat)
    df_conf_cat$risk <- risk_labels[round(df_conf_cat$value)]
    df_conf_cat$risk <- factor(df_conf_cat$risk, levels = risk_labels)

    risk_cols <- c(Low="#ffffb2", Moderate="#fecc5c", High="#fd8d3c", "Very High"="#bd0026")

    p_conf_map <- ggplot() +
      geom_raster(data = df_conf_cat[!is.na(df_conf_cat$risk), ],
                  aes(x = x, y = y, fill = risk)) +
      add_aoi() +
      scale_fill_manual(values = risk_cols, na.value = "white",
                        name = "Conflict\nRisk") +
      coord_sf(crs = 32645, xlim = XLIM, ylim = YLIM, expand = FALSE) +
      labs(title = "Present-day conflict risk") +
      map_theme + theme(plot.title = element_text(size = 10, face = "bold"))

    # Future map
    p_fut_conf <- if (!is.null(fut_conflict_r)) {
      fut_conf_cat <- classify(fut_conflict_r,
                               matrix(c(risk_levels[-length(risk_levels)],
                                        risk_levels[-1], 1:4), ncol=3))
      df_fcc <- r2df(fut_conf_cat)
      df_fcc$risk <- risk_labels[round(df_fcc$value)]
      df_fcc$risk <- factor(df_fcc$risk, levels = risk_labels)
      ggplot() +
        geom_raster(data = df_fcc[!is.na(df_fcc$risk), ],
                    aes(x = x, y = y, fill = risk)) +
        add_aoi() +
        scale_fill_manual(values = risk_cols, na.value = "white", name = "Conflict\nRisk") +
        coord_sf(crs = 32645, xlim = XLIM, ylim = YLIM, expand = FALSE) +
        labs(title = "SSP5-8.5 | 2071\u20132100 conflict risk") +
        map_theme + theme(plot.title = element_text(size = 10, face = "bold"))
    } else patchwork::plot_spacer()

    # Change in conflict risk
    p_delta_conf <- if (!is.null(fut_conflict_r)) {
      delta_conf <- fut_conflict_r - conflict_r
      df_dc <- r2df(delta_conf)
      lim_dc <- max(abs(quantile(df_dc$value, c(0.03, 0.97), na.rm=TRUE)), 0.05)
      ggplot() +
        geom_raster(data = df_dc, aes(x = x, y = y, fill = value)) +
        add_aoi() +
        scale_fill_gradientn(
          colours = c("#053061","#4393c3","#f7f7f7","#f4a582","#67001f"),
          values  = rescale(c(-lim_dc, -lim_dc/2, 0, lim_dc/2, lim_dc)),
          limits  = c(-lim_dc, lim_dc), oob = squish, na.value = "white",
          name    = "\u0394 Conflict\nRisk",
          guide   = guide_colorbar(barwidth=0.5, barheight=4, title.position="top",
                                   title.hjust=0.5, label.theme=element_text(size=6))
        ) +
        coord_sf(crs = 32645, xlim = XLIM, ylim = YLIM, expand = FALSE) +
        labs(title = "Change in conflict risk (SSP5-8.5 2071\u20132100 vs present)") +
        map_theme + theme(plot.title = element_text(size = 10, face = "bold"))
    } else patchwork::plot_spacer()

    # Area statistics
    area_stats <- df_conf_cat %>%
      count(risk) %>%
      mutate(pct = n / sum(n) * 100,
             area_km2 = n * (250^2) / 1e6) %>%
      filter(!is.na(risk))
    cat("  Conflict risk areas (present):\n")
    print(area_stats, row.names = FALSE)
    write.csv(area_stats, file.path(ADOUT, "A9_conflict_risk_stats.csv"), row.names = FALSE)

    fig_a9 <- (p_conf_map | p_fut_conf | p_delta_conf)
    fig_a9 <- fig_a9 + plot_annotation(
      title    = expression(paste("Human\u2013Elephant Conflict Risk — ", italic("Elephas maximus"), " | Bhutan")),
      subtitle = "Risk = normalised suitability \u00d7 human pressure (footprint + settlement proximity)",
      caption  = "High risk = suitable habitat adjacent to high human activity | CRS: EPSG:32645",
      theme = theme(plot.title    = element_text(size = 13, face = "bold", hjust = 0.5),
                    plot.subtitle = element_text(size = 9,  hjust = 0.5,  colour = "grey35"),
                    plot.caption  = element_text(size = 7.5, colour = "grey50", hjust = 1))
    )
    ggsave(file.path(ADOUT, "A9_conflict_risk.png"), fig_a9,
           width = 20, height = 9, dpi = 300, limitsize = FALSE)
    cat("  + A9 saved\n")
  }
}, error = function(e) cat(sprintf("  A9 failed: %s\n", conditionMessage(e))))

# =============================================================================
# A10: SHAP LOCAL FEATURE IMPORTANCE (RF)
# Compute SHAP values at modeling locations using treeshap or kernelshap
# Map spatial distribution of dominant SHAP driver across Bhutan
# =============================================================================
cat("A10: SHAP Local Feature Importance...\n")
tryCatch({
  suppressPackageStartupMessages(library(ranger))
  rf_mod <- readRDS(file.path(RUN, "02_models/model_rf.rds"))
  dat    <- read.csv(file.path(RUN, "01_processed_data/modeling_dataset.csv"))
  mf_sh  <- read.csv(file.path(RUN, "01_processed_data/predictor_manifest.csv"),
                     stringsAsFactors = FALSE)
  selected_preds <- mf_sh$predictor[mf_sh$selected == TRUE]
  pred_cols <- intersect(selected_preds, names(dat))
  X_df  <- dat[, pred_cols, drop = FALSE]
  X_mat <- as.matrix(X_df)

  shap_vals <- NULL
  dat_shap  <- dat

  # fastshap: Monte Carlo SHAP (fast, uses permutation sampling)
  if (requireNamespace("fastshap", quietly = TRUE)) {
    suppressPackageStartupMessages(library(fastshap))
    cat("  Using fastshap (Monte Carlo SHAP, nsim=100)...\n")
    set.seed(42)
    pred_wrapper <- function(object, newdata) {
      as.numeric(predict(object, data = as.data.frame(newdata))$predictions)
    }
    shap_res  <- fastshap::explain(
      rf_mod,
      X        = X_mat,
      pred_wrapper = pred_wrapper,
      nsim     = 100,
      adjust   = TRUE
    )
    shap_vals <- as.data.frame(shap_res)
    cat(sprintf("  SHAP computed for %d observations × %d predictors\n",
                nrow(shap_vals), ncol(shap_vals)))
  }

  if (!is.null(shap_vals)) {

    # Summary beeswarm-style plot
    shap_long <- do.call(rbind, lapply(pred_cols, function(p) {
      if (!p %in% names(shap_vals)) return(NULL)
      data.frame(
        predictor = p,
        shap      = shap_vals[[p]],
        feat_val  = if (p %in% names(dat)) dat[[p]] else NA_real_,
        stringsAsFactors = FALSE
      )
    }))

    # Mean |SHAP| per predictor
    shap_imp <- shap_long %>%
      group_by(predictor) %>%
      summarise(mean_abs_shap = mean(abs(shap), na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(mean_abs_shap))
    write.csv(shap_imp, file.path(ADOUT, "A10_shap_importance.csv"), row.names = FALSE)

    # Importance bar
    shap_imp$predictor <- factor(shap_imp$predictor,
                                  levels = shap_imp$predictor[order(shap_imp$mean_abs_shap)])
    p_shap_bar <- ggplot(shap_imp, aes(x = mean_abs_shap, y = predictor)) +
      geom_col(fill = "#4dac26", alpha = 0.8) +
      geom_text(aes(label = sprintf("%.4f", mean_abs_shap)), hjust = -0.1, size = 3) +
      scale_x_continuous(expand = c(0, 0),
                         limits = c(0, max(shap_imp$mean_abs_shap) * 1.3)) +
      labs(x = "Mean |SHAP|", y = NULL,
           title = "a  SHAP Feature Importance (RF)") +
      theme_bw(base_size = 11) +
      theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

    # SHAP beeswarm / violin
    top_preds <- head(shap_imp$predictor, 8)
    shap_top <- shap_long[shap_long$predictor %in% as.character(top_preds), ]
    shap_top$predictor <- factor(shap_top$predictor, levels = rev(as.character(top_preds)))

    p_shap_violin <- ggplot(shap_top, aes(x = shap, y = predictor, colour = feat_val)) +
      geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.5) +
      ggbeeswarm::geom_beeswarm(groupOnX = FALSE, size = 1.2, alpha = 0.7,
                                 cex = 0.5, priority = "random") +
      scale_colour_gradient2(low = "#2c7bb6", mid = "#ffffbf", high = "#d7191c",
                              midpoint = median(shap_top$feat_val, na.rm=TRUE),
                              na.value = "grey60", name = "Feature\nvalue") +
      labs(x = "SHAP value (impact on prediction)", y = NULL,
           title = "b  SHAP Distribution — Top 8 predictors") +
      theme_bw(base_size = 11) +
      theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

    if (!requireNamespace("ggbeeswarm", quietly = TRUE)) {
      p_shap_violin <- ggplot(shap_top, aes(x = shap, y = predictor)) +
        geom_vline(xintercept = 0, colour = "grey50") +
        geom_violin(fill = "#4dac26", alpha = 0.5, scale = "width") +
        geom_boxplot(width = 0.15, outlier.shape = NA) +
        labs(x = "SHAP value", y = NULL, title = "b  SHAP Distribution — Top 8 predictors") +
        theme_bw(base_size = 11) +
        theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))
    }

    # Spatial SHAP map: which predictor has highest |SHAP| per location?
    shap_wide <- shap_vals[, intersect(pred_cols, names(shap_vals)), drop = FALSE]
    abs_shap  <- abs(shap_wide)
    dominant_pred_idx <- apply(abs_shap, 1, which.max)
    dominant_pred_nm  <- colnames(abs_shap)[dominant_pred_idx]

    spatial_df <- data.frame(
      longitude = dat$longitude,
      latitude  = dat$latitude,
      dominant  = dominant_pred_nm,
      top_shap  = apply(abs_shap, 1, max, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    top_preds_ch <- as.character(top_preds)
    spatial_df$dominant[!spatial_df$dominant %in% top_preds_ch] <- "Other"
    shap_loc_cols <- c(setNames(colorRampPalette(c("#e41a1c","#377eb8","#4daf4a","#984ea3",
                                                    "#ff7f00","#a65628","#f781bf","#333333"))(length(top_preds_ch)),
                                 top_preds_ch), Other = "grey70")

    p_shap_map <- ggplot() +
      { if (!is.null(aoi)) geom_sf(data = aoi, fill = "#e8f0f7", colour = "grey20",
                                    linewidth = 0.6, inherit.aes = FALSE) } +
      geom_point(data = spatial_df, aes(x = longitude, y = latitude,
                                         colour = dominant, size = top_shap),
                 alpha = 0.8, inherit.aes = FALSE) +
      scale_colour_manual(values = shap_loc_cols, name = "Dominant\nSHAP driver") +
      scale_size_continuous(range = c(1, 4), name = "Max |SHAP|", guide = "none") +
      coord_sf(crs = 4326) +
      labs(title = "c  Dominant SHAP driver per location") +
      theme_void(base_size = 9) +
      theme(plot.background  = element_rect(fill = "white"),
            plot.title       = element_text(size = 10, face = "bold", hjust = 0.5),
            legend.text      = element_text(size = 7),
            legend.title     = element_text(size = 7, face = "bold"))

    fig_a10 <- (p_shap_bar | p_shap_violin) / p_shap_map + plot_layout(heights = c(1, 1))
    fig_a10 <- fig_a10 + plot_annotation(
      title    = expression(paste("SHAP Feature Importance — ", italic("Elephas maximus"), " RF Model | Bhutan")),
      subtitle = "SHAP (SHapley Additive exPlanations) quantifies each predictor's local contribution to individual predictions",
      caption  = "Mean |SHAP| = average absolute impact on model output | Higher = more influential predictor",
      theme = theme(plot.title    = element_text(size = 13, face = "bold", hjust = 0.5),
                    plot.subtitle = element_text(size = 9,  hjust = 0.5,  colour = "grey35"),
                    plot.caption  = element_text(size = 7.5, colour = "grey50", hjust = 1))
    )
    ggsave(file.path(ADOUT, "A10_shap_importance.png"), fig_a10,
           width = 18, height = 16, dpi = 300, limitsize = FALSE)
    cat("  + A10 saved\n")
  } else {
    cat("  A10 skipped: neither treeshap nor kernelshap available\n")
    cat("  Install with: install.packages(c('treeshap','kernelshap'))\n")
  }
}, error = function(e) cat(sprintf("  A10 failed: %s\n", conditionMessage(e))))

# =============================================================================
# Summary
# =============================================================================
adv_figs <- list.files(ADOUT, pattern = "\\.png$")
cat(sprintf("\n=== pub_advanced.R complete ===\n"))
cat(sprintf("  Output dir:     %s\n", ADOUT))
cat(sprintf("  Figures saved:  %d\n", length(adv_figs)))
for (f in sort(adv_figs)) cat(sprintf("    + %s\n", f))
