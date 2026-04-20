#!/usr/bin/env Rscript
# =============================================================================
# pub_redesign.R — Nature/Science-level figure redesign
#
# Key improvements over pub_final.R:
#   1. White map backgrounds (remove blue panel fill throughout)
#   2. Colorblind-safe palettes (Okabe-Ito for algorithms/SSPs, YlOrRd for suit)
#   3. M2: 4-SSP × 3-period ensemble matrix (12 panels) — main manuscript fig
#   4. M3: 6-panel change maps (2 SSPs × 3 periods) — clean and readable
#   5. M4: Trajectory ribbons (mean ± SD + individual GCM lines) + correct threshold
#   6. M5: Validation — white background, sequential palette for heatmap
#   7. M6: Climate refugia cropped to habitat zone
#   8. Consistent typography: Helvetica-style, sizes 7–11pt
#   9. Thin hairline map borders, no panel fill
#  10. A7 (climate velocity) REMOVED — all panels blank, raster data missing
#
# Usage (PowerShell):
#   Rscript 03_analysis/pub_redesign.R [run_dir]
# =============================================================================
suppressPackageStartupMessages({
  library(terra); library(sf)
  library(ggplot2); library(patchwork); library(scales); library(dplyr)
  if (requireNamespace("tidyr", quietly = TRUE)) library(tidyr)
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

# ── Paths ─────────────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  RUN <- resolve_run_dir(args[[1]], repo_root = repo_root)
} else {
  RUN <- latest_run_dir(repo_root = repo_root)
}
FUT <- file.path(RUN, "04_future_projections")
OUT <- file.path(RUN, "08_figures_tables", "redesign")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

cat(sprintf("\n=== pub_redesign.R ===\nRun: %s\nOutput: %s\n\n", basename(RUN), OUT))

# =============================================================================
# DESIGN SYSTEM
# =============================================================================

# ── Colorblind-safe palettes (Okabe-Ito + cubehelix extensions) ──────────────
# Suitability: warm sequential, perceptually uniform, colorblind-safe
SUIT_COLS <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
SUIT_MAX  <- 0.50
SUIT_BKPS <- c(0, 0.10, 0.25, 0.40, SUIT_MAX)

# Change (delta): diverging blue→white→red — unambiguous for most deficiencies
CHNG_COLS <- c("#2166AC", "#67A9CF", "#D1E5F0", "#FFFFFF", "#FDDBC7", "#EF8A62", "#B2182B")

# Uncertainty (SD): sequential purple — distinct from suitability
UNCT_COLS <- c("#F7FCFD", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8856A7", "#810F7C")

# Refugia: sequential blue — proportion of scenarios
REFU_COLS <- c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B")

# Conflict risk: sequential orange-red
CONF_COLS <- c("#FFFFB2", "#FED976", "#FD8D3C", "#F03B20", "#BD0026")

# Algorithm colours (Okabe-Ito): distinguishable by colour-blind viewers
ALGO_COLS <- c(glm = "#E69F00", rf = "#56B4E9", brt = "#009E73", maxent = "#CC79A7")

# SSP colours: four clearly distinguishable hues
SSP_COLS  <- c(ssp126 = "#1B7837", ssp245 = "#4393C3",
               ssp370 = "#D6604D", ssp585 = "#762A83")
SSP_LABEL <- c(ssp126 = "SSP1-2.6", ssp245 = "SSP2-4.5",
               ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5")

# Adopted threshold
THRESHOLD <- 0.172

# ── Constants ─────────────────────────────────────────────────────────────────
GCM_LIST  <- c("acces_cm2","cnrm_cm6_1","cnrm_esm2_1","inm_cm4_8",
               "inm_cm5_0","miroc6","miroc_es2l","mpi_esm1_2_lr","mri_esm2_0")
GCM_LABEL <- c("ACCESS-CM2","CNRM-CM6-1","CNRM-ESM2-1","INM-CM4-8",
               "INM-CM5-0","MIROC6","MIROC-ES2L","MPI-ESM1-2-LR","MRI-ESM2-0")
SSP_LIST  <- c("ssp126","ssp245","ssp370","ssp585")
PER_LIST  <- c("2021_2050","2051_2080","2071_2100")
PER_LABEL <- c("2021–2050","2051–2080","2071–2100")
ALGOS     <- c("glm","rf","brt","maxent")

# ── AOI ───────────────────────────────────────────────────────────────────────
aoi_path <- repo_path("01_data_raw", "03_vector", "shapefiles", "Bhutan", "bhutan.shp", repo_root = repo_root)
aoi <- tryCatch(st_read(aoi_path, quiet = TRUE), error = function(e) NULL)
if (!is.null(aoi)) {
  if (is.na(st_crs(aoi))) st_crs(aoi) <- 32645
  if (!isTRUE(st_crs(aoi)$epsg == 32645)) aoi <- st_transform(aoi, 32645)
}
aoi_v <- if (!is.null(aoi)) vect(aoi) else NULL
if (!is.null(aoi)) {
  bb   <- st_bbox(aoi); PAD <- 5000
  XLIM <- c(bb["xmin"] - PAD, bb["xmax"] + PAD)
  YLIM <- c(bb["ymin"] - PAD, bb["ymax"] + PAD)
} else {
  XLIM <- c(670000, 1010000); YLIM <- c(2950000, 3140000)
}

# ── Helpers ───────────────────────────────────────────────────────────────────
get_ens <- function(gcm, ssp, period) {
  ff <- file.path(FUT, sprintf("suitability_future_%s_%s_%s_%s.tif", gcm, ssp, period, ALGOS))
  ff <- ff[file.exists(ff)]
  if (!length(ff)) return(NULL)
  stk <- tryCatch(rast(ff), error = function(e) NULL)
  if (is.null(stk)) return(NULL)
  app(stk, mean, na.rm = TRUE)
}
get_ssp_ens <- function(ssp, period) {
  f <- file.path(FUT, sprintf("future_gcm_ensemble_%s_%s.tif", ssp, period))
  if (!file.exists(f)) return(NULL)
  tryCatch(rast(f), error = function(e) NULL)
}
r2df <- function(r) {
  if (!is.null(aoi_v)) {
    av <- tryCatch(project(aoi_v, crs(r)), error = function(e) aoi_v)
    r  <- tryCatch(mask(crop(r, av), av), error = function(e) r)
  }
  df <- as.data.frame(r[[1]], xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "value"; df
}

# ── Design tokens ─────────────────────────────────────────────────────────────
# Core map theme: WHITE background, hairline border, no grid
map_theme <- theme_void(base_size = 9) +
  theme(
    panel.background  = element_rect(fill = "white",   colour = NA),
    panel.border      = element_rect(fill = NA,        colour = "grey60", linewidth = 0.3),
    plot.background   = element_rect(fill = "white",   colour = NA),
    plot.title        = element_text(size = 8, face = "bold",  hjust = 0.5,
                                     margin = margin(b = 2, t = 2)),
    plot.subtitle     = element_text(size = 6.5, hjust = 0.5,  colour = "grey45",
                                     margin = margin(b = 1)),
    legend.title      = element_text(size = 6.5, face = "bold"),
    legend.text       = element_text(size = 6),
    legend.key.height = unit(0.45, "cm"),
    legend.key.width  = unit(0.18, "cm"),
    plot.margin       = margin(1, 1, 1, 1)
  )

# Standard suit scale
suit_scale <- function(name = "Suitability") {
  scale_fill_gradientn(
    colours  = SUIT_COLS,
    values   = rescale(SUIT_BKPS),
    limits   = c(0, SUIT_MAX), oob = squish,
    na.value = "white", name = name,
    guide = guide_colorbar(barwidth = 0.4, barheight = 3.5,
                           title.position = "top", title.hjust = 0.5,
                           ticks.linewidth = 0.4,
                           label.theme = element_text(size = 5.5))
  )
}

# Change scale — symmetric, fixed
change_scale <- function(lim = 0.27) {
  scale_fill_gradientn(
    colours  = CHNG_COLS,
    values   = rescale(c(-lim, -lim*0.5, -0.04, 0, 0.04, lim*0.5, lim)),
    limits   = c(-lim, lim), oob = squish,
    na.value = "white", name = "\u0394 Suit.",
    guide = guide_colorbar(barwidth = 0.4, barheight = 3.5,
                           title.position = "top", title.hjust = 0.5,
                           label.theme = element_text(size = 5.5))
  )
}

# Compact scale bar
add_scalebar <- function(df, len_m = 100000) {
  xr <- range(df$x, na.rm = TRUE); yr <- range(df$y, na.rm = TRUE)
  sx <- xr[1] + (xr[2] - xr[1]) * 0.06
  sy <- yr[1] + (yr[2] - yr[1]) * 0.06
  list(
    annotate("rect", xmin = sx, xmax = sx + len_m, ymin = sy - 2500, ymax = sy + 2500,
             fill = "grey10", colour = NA),
    annotate("rect", xmin = sx + len_m/2, xmax = sx + len_m, ymin = sy - 2500, ymax = sy + 2500,
             fill = "white", colour = "grey10", linewidth = 0.2),
    annotate("text", x = sx + len_m/2, y = sy - 8000, label = "100 km",
             size = 1.9, hjust = 0.5, colour = "grey20")
  )
}

pnl_label <- function(label, x = XLIM[1] + 7000, y = YLIM[2] - 7000) {
  annotate("text", x = x, y = y, label = label,
           size = 3.0, fontface = "bold", hjust = 0, vjust = 1, colour = "grey10")
}

# =============================================================================
# M1 — Present-day suitability (redesigned)
# Layout: large ensemble map (left) | 2×2 algorithm maps (right)
# Removed: histogram (weak panel); kept: scale bar on main map
# =============================================================================
cat("M1: Present suitability...\n")
pres_r <- tryCatch(
  rast(file.path(RUN, "03_present_suitability/suitability_present_ensemble.tif")),
  error = function(e) NULL)

if (!is.null(pres_r)) {
  df_p <- r2df(pres_r)

  p_main <- ggplot() +
    geom_raster(data = df_p, aes(x = x, y = y, fill = value)) +
    { if (!is.null(aoi)) geom_sf(data = aoi, fill = NA, colour = "grey15",
                                  linewidth = 0.6, inherit.aes = FALSE) } +
    suit_scale("Habitat\nsuitability") +
    add_scalebar(df_p) +
    pnl_label("a") +
    labs(title = "Ensemble mean (GLM + RF + BRT + MaxEnt)") +
    coord_sf(crs = 32645, xlim = XLIM, ylim = YLIM, expand = FALSE) +
    map_theme + theme(plot.title = element_text(size = 9))

  algo_pnls <- list()
  for (ai in seq_along(ALGOS)) {
    f <- file.path(RUN, sprintf("03_present_suitability/suitability_present_%s.tif", ALGOS[ai]))
    r_a <- if (file.exists(f)) tryCatch(rast(f), error = function(e) NULL) else NULL
    if (is.null(r_a)) { algo_pnls[[ai]] <- plot_spacer(); next }
    df_a <- r2df(r_a)
    algo_pnls[[ai]] <- ggplot() +
      geom_raster(data = df_a, aes(x = x, y = y, fill = value)) +
      { if (!is.null(aoi)) geom_sf(data = aoi, fill = NA, colour = "grey20",
                                    linewidth = 0.4, inherit.aes = FALSE) } +
      suit_scale(NULL) +
      pnl_label(c("b","c","d","e")[ai]) +
      labs(title = toupper(ALGOS[ai])) +
      coord_sf(crs = 32645, xlim = XLIM, ylim = YLIM, expand = FALSE) +
      map_theme +
      theme(plot.title = element_text(colour = ALGO_COLS[ALGOS[ai]], size = 8, face = "bold"),
            legend.position = "none")
  }

  m1 <- (p_main | (wrap_plots(algo_pnls, ncol = 2) & theme(legend.position = "none"))) +
    plot_layout(widths = c(1.6, 1)) +
    plot_annotation(
      title    = expression(paste("Present-day ", italic("Elephas maximus"),
                                  " habitat suitability — Bhutan")),
      subtitle = "Ensemble mean and individual algorithm outputs | 5-fold spatial cross-validation | threshold = 0.172",
      caption  = "CRS: EPSG:32645 (WGS 84 / UTM Zone 45N) | Suitability scale capped at 0.50",
      theme = theme(
        plot.title    = element_text(size = 11, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8,  hjust = 0.5,  colour = "grey40"),
        plot.caption  = element_text(size = 6.5, colour = "grey55", hjust = 1)
      )
    )
  ggsave(file.path(OUT, "M1_present_suitability.png"),
         m1, width = 14, height = 6, dpi = 300, limitsize = FALSE)
  cat("  + M1 saved\n")
}

# =============================================================================
# M2 — Multi-SSP ensemble suitability (4 SSPs × 3 periods = 12 panels)
# This replaces Fig2+Fig3 as the primary manuscript figure for future projections
# Rows = SSPs, Cols = periods
# =============================================================================
cat("M2: 4-SSP × 3-period ensemble matrix...\n")

panels_m2 <- list()
li <- 0
for (si in seq_along(SSP_LIST)) {
  for (pi in seq_along(PER_LIST)) {
    li <- li + 1
    r <- get_ssp_ens(SSP_LIST[si], PER_LIST[pi])
    if (is.null(r)) { panels_m2[[li]] <- plot_spacer(); next }
    df <- r2df(r)
    col_title <- if (si == 1) PER_LABEL[pi] else ""
    row_title  <- if (pi == 1) SSP_LABEL[SSP_LIST[si]] else ""
    ssp_col    <- SSP_COLS[SSP_LIST[si]]

    p <- ggplot() +
      geom_raster(data = df, aes(x = x, y = y, fill = value)) +
      { if (!is.null(aoi)) geom_sf(data = aoi, fill = NA, colour = "grey20",
                                    linewidth = 0.35, inherit.aes = FALSE) } +
      suit_scale(NULL) +
      pnl_label(letters[li]) +
      coord_sf(crs = 32645, xlim = XLIM, ylim = YLIM, expand = FALSE) +
      map_theme
    if (nchar(col_title) > 0)
      p <- p + labs(title = col_title) +
        theme(plot.title = element_text(size = 8, face = "bold", hjust = 0.5,
                                        margin = margin(b = 1, t = 2)))
    if (nchar(row_title) > 0)
      p <- p + labs(subtitle = row_title) +
        theme(plot.subtitle = element_text(size = 7.5, face = "bold",
                                           colour = ssp_col, hjust = 0))
    if (si == 4 && pi == 1) p <- p + add_scalebar(df)
    panels_m2[[li]] <- p
  }
}

m2 <- wrap_plots(panels_m2, ncol = 3, nrow = 4) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")
m2 <- m2 + plot_annotation(
  title    = expression(paste("Future ", italic("Elephas maximus"),
                              " habitat suitability — all SSPs")),
  subtitle = "All-GCM ensemble mean (8 CMIP6 GCMs × 4 algorithms) | Rows = emission scenarios | Columns = time periods",
  caption  = "CRS: EPSG:32645 | Suitability scale 0–0.50 consistent across all panels",
  theme = theme(
    plot.title    = element_text(size = 11, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 8,  hjust = 0.5,  colour = "grey40"),
    plot.caption  = element_text(size = 6.5, colour = "grey55", hjust = 1)
  )
)
ggsave(file.path(OUT, "M2_future_all_ssp_matrix.png"),
       m2, width = 10, height = 14, dpi = 300, limitsize = FALSE)
cat("  + M2 saved\n")

# =============================================================================
# M3 — Change maps: 2 SSPs × 3 periods (6 focused panels, not 16)
# Uses fixed symmetric delta scale pre-computed globally
# =============================================================================
cat("M3: Change maps (6 panels, 2 SSPs × 3 periods)...\n")

pres_r2 <- if (!is.null(pres_r)) pres_r else tryCatch(
  rast(file.path(RUN, "03_present_suitability/suitability_present_ensemble.tif")),
  error = function(e) NULL)

if (!is.null(pres_r2)) {
  # Pre-compute global delta limit from all 24 panels
  all_dv <- c()
  for (.ssp in c("ssp245","ssp585")) {
    for (.per in PER_LIST) {
      for (.gcm in GCM_LIST) {
        .fr <- get_ens(.gcm, .ssp, .per)
        if (!is.null(.fr)) {
          .fr <- tryCatch(resample(.fr, pres_r2, method="bilinear"), error=function(e) .fr)
          all_dv <- c(all_dv, as.numeric(values(.fr - pres_r2, na.rm=TRUE)))
        }
      }
    }
  }
  DLIM <- if (length(all_dv) > 0)
    round(min(max(abs(quantile(all_dv, c(0.02, 0.98), na.rm=TRUE)), 0.10), 0.35), 2)
  else 0.27
  cat(sprintf("  Global delta scale: \u00b1%.2f\n", DLIM))

  panels_m3 <- list()
  li <- 0
  for (si in 1:2) {
    ssp_v   <- c("ssp245","ssp585")[si]
    ssp_lbl <- c("SSP2-4.5","SSP5-8.5")[si]
    ssp_col <- c(SSP_COLS["ssp245"], SSP_COLS["ssp585"])[si]
    for (pi in seq_along(PER_LIST)) {
      li <- li + 1
      # Ensemble of all 8 GCMs at this SSP×period
      delta_stack <- list()
      for (.gcm in GCM_LIST) {
        .fr <- get_ens(.gcm, ssp_v, PER_LIST[pi])
        if (!is.null(.fr)) {
          .fr <- tryCatch(resample(.fr, pres_r2, method="bilinear"), error=function(e) .fr)
          delta_stack[[length(delta_stack)+1]] <- .fr - pres_r2
        }
      }
      if (length(delta_stack) == 0) { panels_m3[[li]] <- plot_spacer(); next }
      delta_r <- if (length(delta_stack) == 1) delta_stack[[1]] else
        app(tryCatch(rast(delta_stack), error=function(e) delta_stack[[1]]),
            mean, na.rm=TRUE)
      df_d <- r2df(delta_r)
      col_title <- if (si == 1) PER_LABEL[pi] else ""
      row_title  <- if (pi == 1) ssp_lbl else ""

      p <- ggplot() +
        geom_raster(data = df_d, aes(x = x, y = y, fill = value)) +
        { if (!is.null(aoi)) geom_sf(data = aoi, fill = NA, colour = "grey20",
                                      linewidth = 0.35, inherit.aes = FALSE) } +
        change_scale(DLIM) +
        pnl_label(letters[li]) +
        coord_sf(crs = 32645, xlim = XLIM, ylim = YLIM, expand = FALSE) +
        map_theme
      if (nchar(col_title) > 0)
        p <- p + labs(title = col_title) +
          theme(plot.title = element_text(size = 8, face = "bold", hjust = 0.5,
                                          margin = margin(b = 1, t = 2)))
      if (nchar(row_title) > 0)
        p <- p + labs(subtitle = row_title) +
          theme(plot.subtitle = element_text(size = 7.5, face = "bold",
                                             colour = unname(ssp_col), hjust = 0))
      if (si == 2 && pi == 1) p <- p + add_scalebar(df_d)
      panels_m3[[li]] <- p
    }
  }

  m3 <- wrap_plots(panels_m3, ncol = 3, nrow = 2) +
    plot_layout(guides = "collect") &
    theme(legend.position = "right")
  m3 <- m3 + plot_annotation(
    title    = expression(paste("\u0394 Change in ", italic("Elephas maximus"),
                                " habitat suitability vs present")),
    subtitle = sprintf("GCM-ensemble mean change | Blue = habitat loss | Red = habitat gain | Fixed symmetric scale \u00b1%.2f", DLIM),
    caption  = "Delta = future ensemble mean minus present-day ensemble | 8 CMIP6 GCMs averaged | CRS: EPSG:32645",
    theme = theme(
      plot.title    = element_text(size = 11, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 8,  hjust = 0.5,  colour = "grey40"),
      plot.caption  = element_text(size = 6.5, colour = "grey55", hjust = 1)
    )
  )
  ggsave(file.path(OUT, "M3_change_maps.png"),
         m3, width = 10, height = 7.5, dpi = 300, limitsize = FALSE)
  cat("  + M3 saved\n")
}

# =============================================================================
# M4 — Suitable habitat trajectories (redesigned)
# Ribbon = mean ± SD across GCMs per SSP; individual GCM lines shown faintly
# CORRECT threshold = 0.172 (not 0.5 as in original)
# All 4 SSPs on one panel with SSP-coloured ribbons
# =============================================================================
cat("M4: Habitat trajectories (ribbon + all 4 SSPs)...\n")

if (!is.null(pres_r2)) {
  pxl_km2 <- prod(res(pres_r2)) / 1e6

  # Present baseline
  pres_area <- sum(as.numeric(values(pres_r2, na.rm = TRUE)) >= THRESHOLD, na.rm = TRUE) * pxl_km2
  pres_pct  <- mean(as.numeric(values(pres_r2, na.rm = TRUE)) >= THRESHOLD, na.rm = TRUE) * 100

  # Compute per-GCM area at each SSP × period
  traj_rows <- list()
  for (si in seq_along(SSP_LIST)) {
    for (pi in seq_along(PER_LIST)) {
      for (gi in seq_along(GCM_LIST)) {
        r <- get_ens(GCM_LIST[gi], SSP_LIST[si], PER_LIST[pi])
        if (is.null(r)) next
        r <- tryCatch(resample(r, pres_r2, method = "bilinear"), error = function(e) r)
        v <- as.numeric(values(r, na.rm = TRUE))
        traj_rows[[length(traj_rows)+1]] <- data.frame(
          ssp    = SSP_LIST[si],
          period = PER_LABEL[pi],
          period_n = c(2035, 2065, 2085)[pi],
          gcm    = GCM_LABEL[gi],
          area_km2 = sum(v >= THRESHOLD, na.rm=TRUE) * pxl_km2,
          pct    = mean(v >= THRESHOLD, na.rm=TRUE) * 100,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  traj_df <- do.call(rbind, traj_rows)

  # Summary stats per SSP × period
  traj_sum <- traj_df %>%
    group_by(ssp, period, period_n) %>%
    summarise(mean_km2 = mean(area_km2, na.rm=TRUE),
              sd_km2   = sd(area_km2,   na.rm=TRUE),
              q25_km2  = quantile(area_km2, 0.25, na.rm=TRUE),
              q75_km2  = quantile(area_km2, 0.75, na.rm=TRUE),
              .groups  = "drop") %>%
    mutate(ssp_label = SSP_LABEL[ssp])

  # Add present row for each SSP (same baseline)
  pres_rows <- data.frame(
    ssp = SSP_LIST, period = "Present", period_n = 2010,
    mean_km2 = pres_area, sd_km2 = 0,
    q25_km2 = pres_area, q75_km2 = pres_area,
    ssp_label = SSP_LABEL[SSP_LIST], stringsAsFactors = FALSE
  )
  traj_sum <- bind_rows(pres_rows, traj_sum) %>%
    arrange(ssp, period_n)

  traj_gcm_pres <- data.frame(
    ssp = rep(SSP_LIST, each = length(GCM_LIST)),
    period_n = 2010, gcm = GCM_LABEL,
    area_km2 = pres_area, stringsAsFactors = FALSE
  )
  traj_gcm_full <- bind_rows(traj_gcm_pres, traj_df) %>% arrange(ssp, gcm, period_n)

  ssp_col_vec <- SSP_COLS
  names(ssp_col_vec) <- SSP_LIST

  m4 <- ggplot() +
    # Individual GCM lines (faint)
    geom_line(data = traj_gcm_full,
              aes(x = period_n, y = area_km2, group = interaction(ssp, gcm),
                  colour = ssp),
              alpha = 0.18, linewidth = 0.4) +
    # Ribbon ± 1 SD
    geom_ribbon(data = traj_sum,
                aes(x = period_n,
                    ymin = pmax(mean_km2 - sd_km2, 0),
                    ymax = mean_km2 + sd_km2,
                    fill = ssp),
                alpha = 0.20) +
    # Ensemble mean line
    geom_line(data = traj_sum,
              aes(x = period_n, y = mean_km2, colour = ssp),
              linewidth = 1.1) +
    geom_point(data = traj_sum,
               aes(x = period_n, y = mean_km2, colour = ssp),
               size = 2.2, shape = 21, fill = "white", stroke = 0.8) +
    # Present baseline reference
    geom_hline(yintercept = pres_area, linetype = "dashed",
               colour = "grey50", linewidth = 0.5) +
    annotate("text", x = 2008, y = pres_area * 1.02,
             label = sprintf("Present: %.0f km²", pres_area),
             size = 2.5, hjust = 0, colour = "grey40") +
    scale_colour_manual(values = ssp_col_vec, labels = SSP_LABEL, name = "Scenario") +
    scale_fill_manual(  values = ssp_col_vec, labels = SSP_LABEL, name = "Scenario") +
    scale_x_continuous(breaks = c(2010, 2035, 2065, 2085),
                       labels = c("Present","2021–2050","2051–2080","2071–2100")) +
    scale_y_continuous(labels = function(x) sprintf("%.0f", x),
                       expand = expansion(mult = c(0.03, 0.05))) +
    labs(
      x = NULL,
      y = expression(paste("Suitable area (km"^2, ")")),
      title = expression(paste(italic("Elephas maximus"), " suitable habitat through time — Bhutan")),
      subtitle = sprintf("Threshold = %.3f (optimal TSS) | Ribbon = mean \u00b1 1 SD across 9 GCMs | Faint lines = individual GCMs", THRESHOLD),
      caption = "Suitable area = pixels with suitability \u2265 threshold | All 4 SSPs × 8 CMIP6 GCMs × 3 periods"
    ) +
    theme_classic(base_size = 9) +
    theme(
      plot.title    = element_text(size = 10, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 7.5, hjust = 0.5, colour = "grey40"),
      plot.caption  = element_text(size = 6.5, colour = "grey55", hjust = 1),
      axis.text     = element_text(size = 8),
      axis.text.x   = element_text(angle = 20, hjust = 1),
      legend.position  = c(0.12, 0.82),
      legend.background = element_rect(fill = "white", colour = "grey80", linewidth = 0.3),
      legend.key.height = unit(0.4, "cm"),
      legend.text   = element_text(size = 7),
      legend.title  = element_text(size = 7.5, face = "bold"),
      panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.4)
    )

  ggsave(file.path(OUT, "M4_trajectories.png"),
         m4, width = 9, height = 5.5, dpi = 300, limitsize = FALSE)
  cat("  + M4 saved\n")
}

# =============================================================================
# M5 — Model validation (redesigned)
# White background, sequential blue heatmap (colorblind-safe), cleaner layout
# =============================================================================
cat("M5: Model validation...\n")

model_dir  <- file.path(RUN, "02_models")
eval_file  <- file.path(model_dir, "evaluation_summary.csv")
roc_file   <- file.path(model_dir, "roc_curve_data.csv")

if (file.exists(eval_file) && file.exists(roc_file)) {
  eval_df <- read.csv(eval_file)
  roc_df  <- read.csv(roc_file)
  roc_df  <- roc_df[!is.na(roc_df$sensitivity) & !is.na(roc_df$specificity), ]

  algo_order <- c("glm","rf","brt","maxent")
  algo_lbl   <- c("GLM","RF","BRT","MaxEnt")
  roc_df$algorithm <- factor(roc_df$algorithm, levels = algo_order, labels = algo_lbl)
  algo_col_named <- setNames(unname(ALGO_COLS[algo_order]), algo_lbl)

  # Panel a: ROC curves — no fill, clean lines
  p_roc <- ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity,
                               colour = algorithm)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                colour = "grey70", linewidth = 0.5) +
    geom_line(linewidth = 0.9, alpha = 0.9) +
    scale_colour_manual(values = algo_col_named, name = NULL) +
    scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0)) +
    labs(x = "False positive rate (1 – Specificity)",
         y = "True positive rate (Sensitivity)",
         title = "a  ROC curves (5-fold spatial CV)") +
    theme_classic(base_size = 9) +
    theme(
      plot.title      = element_text(size = 9, face = "bold"),
      legend.position = c(0.75, 0.20),
      legend.background = element_blank(),
      legend.key.height = unit(0.35, "cm"),
      legend.text     = element_text(size = 7.5),
      axis.text       = element_text(size = 7.5)
    )

  # Add AUC labels
  auc_labels <- eval_df[eval_df$algorithm %in% algo_order, c("algorithm","auc_mean")]
  auc_labels$algorithm_lbl <- algo_lbl[match(auc_labels$algorithm, algo_order)]
  auc_labels <- auc_labels[!is.na(auc_labels$auc_mean), ]
  auc_labels <- auc_labels[order(auc_labels$auc_mean, decreasing = TRUE), ]
  auc_text <- paste(sprintf("%s: AUC = %.3f", auc_labels$algorithm_lbl, auc_labels$auc_mean), collapse = "\n")
  p_roc <- p_roc + annotate("text", x = 0.58, y = 0.08, label = auc_text,
                              size = 2.3, hjust = 0, colour = "grey20", lineheight = 1.4)

  # Panel b: performance heatmap — sequential BLUE (colorblind-safe)
  metrics_show <- c("auc_mean","tss_mean","boyce","brier","calibration_slope","moran_i")
  metrics_lbl  <- c("AUC","TSS","Boyce Index","Brier Score","Calib. Slope","Moran's I")
  heat_df <- eval_df[eval_df$algorithm %in% algo_order, c("algorithm", metrics_show)]
  heat_long <- tidyr::pivot_longer(heat_df, cols = all_of(metrics_show),
                                   names_to = "metric", values_to = "value") %>%
    mutate(
      algo_f   = factor(algorithm, levels = rev(algo_order), labels = rev(algo_lbl)),
      metric_f = factor(metric, levels = metrics_show, labels = metrics_lbl),
      value_fmt = ifelse(is.na(value), "NA", sprintf("%.3f", value))
    )

  # Normalize each metric 0–1 for fill (higher = bluer = better for AUC/TSS/Boyce;
  # lower = bluer = better for Brier/Moran; calibration close to 1 = best)
  heat_long <- heat_long %>%
    group_by(metric) %>%
    mutate(
      v_min = min(value, na.rm=TRUE),
      v_max = max(value, na.rm=TRUE),
      fill_norm = case_when(
        metric %in% c("auc_mean","tss_mean","boyce") ~
          (value - v_min) / pmax(v_max - v_min, 1e-6),
        metric %in% c("brier","moran_i") ~
          1 - (value - v_min) / pmax(v_max - v_min, 1e-6),
        metric == "calibration_slope" ~
          1 - abs(value - 1) / pmax(max(abs(value - 1), na.rm=TRUE), 1e-6),
        TRUE ~ 0.5
      )
    ) %>% ungroup()

  p_heat <- ggplot(heat_long, aes(x = metric_f, y = algo_f, fill = fill_norm)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = value_fmt, colour = fill_norm < 0.4),
              size = 2.7, fontface = "bold") +
    scale_fill_gradientn(colours = c("#F7FBFF","#C6DBEF","#6BAED6","#2171B5","#08306B"),
                         limits = c(0, 1), na.value = "grey90", guide = "none") +
    scale_colour_manual(values = c("TRUE" = "grey10", "FALSE" = "white"), guide = "none") +
    scale_x_discrete(guide = guide_axis(angle = 30)) +
    labs(x = NULL, y = NULL, title = "b  Cross-validation performance") +
    theme_minimal(base_size = 9) +
    theme(
      plot.title   = element_text(size = 9, face = "bold"),
      axis.text    = element_text(size = 7.5),
      panel.grid   = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA)
    )

  # Panel c: AUC dot plot
  auc_df <- eval_df[eval_df$algorithm %in% algo_order & !is.na(eval_df$auc_mean), ]
  auc_df$algo_f <- factor(auc_df$algorithm, levels = rev(algo_order), labels = rev(algo_lbl))
  p_auc <- ggplot(auc_df, aes(x = auc_mean, y = algo_f, colour = algorithm)) +
    geom_vline(xintercept = c(0.7, 0.9), linetype = "dashed",
               colour = c("grey70","grey50"), linewidth = 0.5) +
    geom_segment(aes(x = 0.5, xend = auc_mean, yend = algo_f),
                 colour = "grey80", linewidth = 0.4) +
    geom_point(size = 3.5) +
    geom_text(aes(label = sprintf("%.4f", auc_mean)),
              hjust = -0.3, size = 2.6, colour = "grey20") +
    scale_colour_manual(values = ALGO_COLS[algo_order], labels = algo_lbl, guide = "none") +
    scale_x_continuous(limits = c(0.5, 1.02), expand = c(0, 0),
                       breaks = c(0.5, 0.7, 0.9, 1.0)) +
    annotate("text", x = c(0.7, 0.9), y = 0.4,
             label = c("Good", "Excellent"), size = 2.2,
             colour = c("grey65","grey45"), hjust = 0) +
    labs(x = "AUC", y = NULL, title = "c  AUC summary") +
    theme_classic(base_size = 9) +
    theme(
      plot.title  = element_text(size = 9, face = "bold"),
      axis.text   = element_text(size = 7.5),
      axis.text.y = element_text(colour = unname(ALGO_COLS[algo_order[4:1]])),
      plot.background = element_rect(fill = "white", colour = NA)
    )

  m5 <- (p_roc | p_heat | p_auc) +
    plot_layout(widths = c(1.2, 1.3, 0.9)) +
    plot_annotation(
      title    = expression(paste(italic("Elephas maximus"), " SDM — model validation | Bhutan")),
      subtitle = "5-fold spatial cross-validation | Out-of-fold holdout predictions | 252 presence records from station-level survey data",
      caption  = "Blue heatmap: darker = better performance | Boyce Index: >0.5 = good | Brier: lower = better",
      theme = theme(
        plot.title    = element_text(size = 11, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8,  hjust = 0.5,  colour = "grey40"),
        plot.caption  = element_text(size = 6.5, colour = "grey55", hjust = 1),
        plot.background = element_rect(fill = "white", colour = NA)
      )
    )
  ggsave(file.path(OUT, "M5_model_validation.png"),
         m5, width = 14, height = 5.5, dpi = 300, limitsize = FALSE)
  cat("  + M5 saved\n")
} else {
  cat("  ! Skipped M5 — evaluation files not found\n")
}

# =============================================================================
# M6 — Climate refugia (cropped to habitat zone — removes empty northern half)
# =============================================================================
cat("M6: Climate refugia (cropped)...\n")

refugia_f <- file.path(RUN, "08_figures_tables", "advanced", "A1_climate_refugia.tif")
if (file.exists(refugia_f)) {
  ref_r <- tryCatch(rast(refugia_f), error = function(e) NULL)
  if (!is.null(ref_r)) {
    # Crop y-extent to southern habitat zone (remove empty northern Bhutan)
    # Based on visual: habitat zone is roughly lower 40% of y range
    y_full <- c(YLIM[1], YLIM[2])
    y_crop <- c(YLIM[1], YLIM[1] + (YLIM[2] - YLIM[1]) * 0.60)
    XLIM_c <- XLIM; YLIM_c <- y_crop

    df_ref <- r2df(ref_r)
    df_ref_crop <- df_ref[df_ref$y <= YLIM_c[2], ]

    # Define refugia categories
    df_ref$category <- cut(df_ref$value,
                            breaks = c(-Inf, 0.25, 0.50, 0.75, 0.95, Inf),
                            labels = c("Marginal (0–25%)", "Low (25–50%)",
                                       "Moderate (50–75%)", "High (75–95%)",
                                       "Core refugia (>95%)"))

    # Cropped map
    p_ref_map <- ggplot() +
      geom_raster(data = df_ref[df_ref$y <= YLIM_c[2], ],
                  aes(x = x, y = y, fill = value)) +
      { if (!is.null(aoi)) geom_sf(data = aoi, fill = NA, colour = "grey20",
                                    linewidth = 0.5, inherit.aes = FALSE) } +
      scale_fill_gradientn(
        colours  = REFU_COLS,
        limits   = c(0, 1), na.value = "white",
        name     = "Refugia\nscore",
        breaks   = c(0, 0.25, 0.5, 0.75, 0.95, 1.0),
        labels   = c("0%","25%","50%","75%","95%","100%"),
        guide    = guide_colorbar(barwidth = 0.4, barheight = 4.5,
                                   title.position = "top", title.hjust = 0.5,
                                   label.theme = element_text(size = 5.5))
      ) +
      add_scalebar(df_ref[df_ref$y <= YLIM_c[2], ]) +
      pnl_label("a") +
      labs(title = "Refugia score (proportion of 96 scenarios suitable)") +
      coord_sf(crs = 32645, xlim = XLIM_c, ylim = YLIM_c, expand = FALSE) +
      map_theme + theme(plot.title = element_text(size = 9))

    # Score distribution (condensed bar chart — only non-zero bins)
    ref_vals <- df_ref$value[!is.na(df_ref$value)]
    ref_hist_df <- data.frame(score = ref_vals)

    p_ref_hist <- ggplot(ref_hist_df, aes(x = score)) +
      geom_histogram(aes(fill = after_stat(x)), bins = 30,
                     colour = "white", linewidth = 0.1) +
      scale_fill_gradientn(colours = REFU_COLS, limits = c(0,1), guide = "none") +
      geom_vline(xintercept = 0.95, linetype = "dashed",
                 colour = "grey40", linewidth = 0.6) +
      annotate("text", x = 0.97, y = Inf, label = "Core\nrefu.",
               size = 2.3, hjust = 0, vjust = 1.2, colour = "grey35") +
      scale_x_continuous(limits = c(0,1), breaks = c(0, 0.25, 0.5, 0.75, 1),
                         labels = c("0%","25%","50%","75%","100%")) +
      labs(x = "Refugia score", y = "Pixel count",
           title = "b  Score distribution") +
      theme_classic(base_size = 9) +
      theme(plot.title   = element_text(size = 9, face = "bold"),
            axis.text    = element_text(size = 7.5),
            panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.4))

    core_pct <- mean(ref_vals >= 0.95, na.rm = TRUE) * 100
    m6 <- (p_ref_map | p_ref_hist) +
      plot_layout(widths = c(2, 1)) +
      plot_annotation(
        title    = expression(paste(italic("Elephas maximus"), " climate refugia — Bhutan")),
        subtitle = sprintf("Proportion of 108 future scenarios (9 GCMs \u00d7 4 SSPs \u00d7 3 periods) with suitability \u2265 %.3f | Core refugia: %.1f%% of Bhutan", THRESHOLD, core_pct),
        caption  = "Core refugia = suitable in >95% of all future scenarios | Figure cropped to southern habitat zone",
        theme = theme(
          plot.title    = element_text(size = 11, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 8,  hjust = 0.5,  colour = "grey40"),
          plot.caption  = element_text(size = 6.5, colour = "grey55", hjust = 1)
        )
      )
    ggsave(file.path(OUT, "M6_climate_refugia.png"),
           m6, width = 12, height = 5.5, dpi = 300, limitsize = FALSE)
    cat(sprintf("  + M6 saved (core refugia: %.1f%%)\n", core_pct))
  }
} else {
  cat("  ! Skipped M6 — refugia raster not found\n")
}

# =============================================================================
# S4 — GCM reliability ranking (redesigned: colorblind-safe bars)
# =============================================================================
cat("S4: GCM reliability ranking...\n")
gcm_rank_f <- file.path(RUN, "08_figures_tables", "Table_gcm_ranking.csv")
if (file.exists(gcm_rank_f)) {
  gcm_rank <- read.csv(gcm_rank_f)
  # Use a sequential palette (viridis-like), not red-green
  if ("composite_score" %in% names(gcm_rank) && "gcm" %in% names(gcm_rank)) {
    gcm_rank <- gcm_rank[order(gcm_rank$composite_score, decreasing = TRUE), ]
    gcm_rank$rank_label <- sprintf("#%d", seq_len(nrow(gcm_rank)))
    gcm_rank$gcm_f <- factor(gcm_rank$gcm, levels = rev(gcm_rank$gcm))

    s4a <- ggplot(gcm_rank, aes(x = composite_score, y = gcm_f, fill = composite_score)) +
      geom_col(colour = NA) +
      geom_text(aes(label = sprintf("%.3f", composite_score)),
                hjust = -0.15, size = 2.6, colour = "grey20") +
      scale_fill_gradientn(
        colours = c("#F7FCFD","#BFD3E6","#6BAED6","#2171B5","#08306B"),
        limits  = c(0, max(gcm_rank$composite_score) * 1.1),
        guide   = "none"
      ) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.12)),
                         labels = scales::percent_format(accuracy = 1)) +
      labs(x = "Composite reliability score", y = NULL,
           title = "a  GCM reliability ranking") +
      theme_classic(base_size = 9) +
      theme(plot.title = element_text(size = 9, face = "bold"),
            axis.text  = element_text(size = 8))

    ggsave(file.path(OUT, "S4_gcm_ranking.png"),
           s4a, width = 6, height = 4.5, dpi = 300)
    cat("  + S4 saved\n")
  }
} else {
  cat("  ! Skipped S4 — ranking table not found\n")
}

# =============================================================================
# DESIGN SPECIFICATION DOCUMENT
# =============================================================================
spec_lines <- c(
  "# Publication Figure Design Specifications",
  sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "## Typography",
  "  Font family:       sans (Helvetica/Arial equivalent)",
  "  Main title:        11 pt, bold",
  "  Panel subtitle:    8 pt, grey40",
  "  Panel label (a,b): 8 pt, bold, top-left corner, no box",
  "  Axis text:         7.5–8 pt",
  "  Legend title:      6.5–7 pt, bold",
  "  Legend text:       6–7 pt",
  "  Caption:           6.5 pt, grey55",
  "",
  "## Map Theme",
  "  Panel background:  white (fill = 'white')  — NO blue/grey fill",
  "  Panel border:      hairline grey60, linewidth = 0.3",
  "  Plot background:   white",
  "  Grid lines:        none (theme_void base)",
  "  Map padding:       5 km around Bhutan AOI",
  "",
  "## Color Palettes",
  "  Suitability:       #FFFFD4 → #FED98E → #FE9929 → #D95F0E → #993404 (YlOrRd)",
  "  Change (delta):    #2166AC → #67A9CF → #D1E5F0 → white → #FDDBC7 → #EF8A62 → #B2182B",
  "  Uncertainty (SD):  #F7FCFD → #BFD3E6 → #9EBCDA → #8C96C6 → #810F7C (BuPu)",
  "  Refugia:           #F7FBFF → #C6DBEF → #6BAED6 → #2171B5 → #08306B (Blues)",
  "  Conflict risk:     #FFFFB2 → #FED976 → #FD8D3C → #F03B20 → #BD0026",
  "  Algorithm (Okabe-Ito):",
  "    GLM    = #E69F00 (amber)",
  "    RF     = #56B4E9 (sky blue)",
  "    BRT    = #009E73 (teal)",
  "    MaxEnt = #CC79A7 (pink)",
  "  SSP colours:",
  "    SSP1-2.6 = #1B7837 (dark green)",
  "    SSP2-4.5 = #4393C3 (medium blue)",
  "    SSP3-7.0 = #D6604D (orange-red)",
  "    SSP5-8.5 = #762A83 (purple)",
  "",
  "## Validation Heatmap",
  "  OLD: red-green (NOT colorblind-safe)",
  "  NEW: sequential blue (BuPu) — darker = better performance",
  "  Normalization: AUC/TSS/Boyce = higher is better",
  "               Brier/Moran = lower is better",
  "               Calibration slope = closer to 1.0 is better",
  "",
  "## Figure Set — Manuscript",
  "  M1  Present suitability         14 × 6 in",
  "  M2  Future 4-SSP × 3-period     10 × 14 in (12 panels)",
  "  M3  Change maps 2-SSP × 3-per   10 × 7.5 in (6 panels)",
  "  M4  Trajectories ribbon          9 × 5.5 in",
  "  M5  Model validation            14 × 5.5 in",
  "  M6  Climate refugia (cropped)   12 × 5.5 in",
  "",
  "## Figure Set — Supplementary",
  "  S1  GCM matrix SSP2-4.5 (24 panels)   — existing Fig2",
  "  S2  GCM matrix SSP5-8.5 (24 panels)   — existing Fig3",
  "  S3  Uncertainty SSP245 + SSP585        — existing Fig6",
  "  S4  GCM reliability ranking (redesigned blue palette)",
  "  S5  SHAP importance (existing A10)",
  "  S6  Limiting factors (existing A6)",
  "  S7  Variance decomposition (existing A3)",
  "  S8  Conflict risk (existing A9)",
  "  S9  Threshold sensitivity (existing A8)",
  "",
  "## Removed / Deprecated",
  "  Fig4  End-century comparison  REDUNDANT — covered by M2 at 2071-2100",
  "  A7    Climate velocity        BROKEN — all panels blank (raster data absent)",
  "  figure_0X / figure_E*        SUPERSEDED by redesigned M/S set",
  "",
  "## Analytical Consistency",
  "  Threshold:   0.172 (MaxSSS on OOF predictions) — applied everywhere",
  "  Ensemble:    unweighted mean across GLM + RF + BRT + MaxEnt",
  "  Delta scale: fixed symmetric (computed globally, not per-panel)",
  "  CRS:         EPSG:32645 (WGS 84 / UTM Zone 45N)"
)
writeLines(spec_lines, file.path(OUT, "design_specifications.md"))
cat("  + Design spec saved\n")

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n=== pub_redesign.R complete ===\n")
figs <- list.files(OUT, pattern="\\.png$")
cat(sprintf("  %d figures saved to: %s\n", length(figs), OUT))
cat("  Design spec: redesign/design_specifications.md\n")
cat("\n  Manuscript set: M1 M2 M3 M4 M5 M6\n")
cat("  Supplementary:  S4 (+ existing Fig2/3/6/9 A3/A6/A8/A9/A10)\n")
cat("  Removed:        Fig4 (redundant) | A7 (blank panels)\n")
