#!/usr/bin/env Rscript
# =============================================================================
# pub_figures_gcm_maps.R
# World-class publication figures: 9 GCMs × SSP245 + SSP585 + GCM ranking
# =============================================================================
suppressPackageStartupMessages({
  library(terra); library(sf)
  library(ggplot2); library(patchwork); library(scales)
  library(dplyr); library(tidyr)
})

RUN_DIR <- "E:/Elephas_maximus_SDM_Project_v4/04_outputs/runs/RUN_20260317_203608_b990"
FUT_DIR <- file.path(RUN_DIR, "04_future_projections")
OUT_DIR <- file.path(RUN_DIR, "08_figures_tables")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# Colour system
# =============================================================================
SUIT_COLS  <- c("#f7f7f7","#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
SUIT_VALS  <- c(0, 0.05, 0.20, 0.40, 0.65, 1.00)
CHNG_COLS  <- c("#053061","#2166ac","#92c5de","#f7f7f7","#f4a582","#ca0020","#67001f")
MAP_BG     <- "#cfe2f3"
SSP_COLS   <- c("SSP2-4.5"="#2ca25f","SSP5-8.5"="#de2d26")
ALGO_COLS  <- c(brt="#e41a1c",glm="#377eb8",maxent="#4daf4a",rf="#984ea3")

# =============================================================================
# Themes
# =============================================================================
map_theme <- theme_bw(base_size = 9) +
  theme(
    panel.background   = element_rect(fill = MAP_BG, colour = NA),
    panel.grid.major   = element_line(colour = "white", linewidth = 0.15),
    panel.grid.minor   = element_blank(),
    panel.border       = element_rect(colour = "grey40", linewidth = 0.4),
    axis.title         = element_blank(),
    axis.text          = element_text(size = 5.5, colour = "grey30"),
    axis.ticks         = element_line(linewidth = 0.3),
    plot.title         = element_text(size = 8.5, face = "bold", hjust = 0.5,
                                      margin = margin(b = 1.5)),
    plot.subtitle      = element_text(size = 7, hjust = 0.5, colour = "grey35",
                                      margin = margin(b = 1)),
    legend.title       = element_text(size = 7.5, face = "bold"),
    legend.text        = element_text(size = 6.5),
    legend.key.height  = unit(0.55, "cm"),
    legend.key.width   = unit(0.22, "cm"),
    plot.margin        = margin(2, 2, 2, 2)
  )

suit_scale <- function(name = "Suitability", ...) {
  scale_fill_gradientn(
    colours = SUIT_COLS, values = SUIT_VALS, limits = c(0, 1),
    na.value = "grey85", name = name,
    guide = guide_colorbar(barwidth = 0.6, barheight = 4.5,
                           title.position = "top", title.hjust = 0.5,
                           ticks.linewidth = 0.4,
                           label.theme = element_text(size = 6)),
    ...
  )
}
change_scale <- function(lim = 0.5, name = "\u0394 Suit.") {
  scale_fill_gradientn(
    colours = CHNG_COLS, limits = c(-lim, lim), oob = squish,
    na.value = "grey85", name = name,
    guide = guide_colorbar(barwidth = 0.6, barheight = 4.5,
                           title.position = "top", title.hjust = 0.5,
                           ticks.linewidth = 0.4,
                           label.theme = element_text(size = 6))
  )
}

pub_title <- function(title, subtitle = "", caption = "") {
  plot_annotation(
    title = title, subtitle = subtitle, caption = caption,
    theme = theme(
      plot.title    = element_text(size = 15, face = "bold", hjust = 0.5,
                                   margin = margin(b = 5)),
      plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey30",
                                   margin = margin(b = 8)),
      plot.caption  = element_text(size = 7.5, hjust = 1, colour = "grey50",
                                   margin = margin(t = 6))
    )
  )
}

# =============================================================================
# Data setup
# =============================================================================
GCM_LIST <- c("acces_cm2","cnrm_cm6_1","cnrm_esm2_1","inm_cm4_8",
              "inm_cm5_0","miroc6","miroc_es2l","mpi_esm1_2_lr","mri_esm2_0")
GCM_LABEL <- c("ACCESS-CM2","CNRM-CM6-1","CNRM-ESM2-1","INM-CM4-8",
               "INM-CM5-0","MIROC6","MIROC-ES2L","MPI-ESM1-2-LR","MRI-ESM2-0")
GCM_MAP  <- setNames(GCM_LABEL, GCM_LIST)

PERIODS     <- c("2021_2050","2051_2080","2071_2100")
PER_LABELS  <- c("2021\u20132050","2051\u20132080","2071\u20132100")
ALGOS_ENS   <- c("glm","rf","brt","maxent")

# AOI
aoi <- tryCatch(
  st_read("E:/Elephas_maximus_SDM_Project_v4/01_data_raw/03_vector/shapefiles/Bhutan/bhutan.shp",
          quiet = TRUE),
  error = function(e) NULL
)
if (!is.null(aoi) && is.na(st_crs(aoi))) st_crs(aoi) <- 32645
aoi_wgs <- if (!is.null(aoi)) tryCatch(st_transform(aoi, 4326), error = function(e) NULL) else NULL
aoi_v   <- if (!is.null(aoi_wgs)) vect(aoi_wgs) else NULL
cat("AOI loaded\n")

# Present ensemble
pres_r <- tryCatch(
  rast(file.path(RUN_DIR, "03_present_suitability/suitability_present_ensemble.tif")),
  error = function(e) NULL
)

# =============================================================================
# Helpers
# =============================================================================
r2df <- function(r) {
  if (!is.null(aoi_v)) {
    av <- tryCatch(project(aoi_v, crs(r)), error = function(e) aoi_v)
    r  <- tryCatch(mask(crop(r, av), av), error = function(e) r)
  }
  df <- as.data.frame(r[[1]], xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "value"
  df
}

aoi_geom <- function() {
  if (!is.null(aoi_wgs))
    geom_sf(data = aoi_wgs, fill = NA, colour = "#222222", linewidth = 0.4,
            inherit.aes = FALSE)
  else list()
}

# Ensemble mean of available algorithms
get_ens <- function(gcm, ssp, period, algos = ALGOS_ENS) {
  files <- file.path(FUT_DIR,
    sprintf("suitability_future_%s_%s_%s_%s.tif", gcm, ssp, period, algos))
  files <- files[file.exists(files)]
  if (length(files) == 0) return(NULL)
  stk <- tryCatch(rast(files), error = function(e) NULL)
  if (is.null(stk)) return(NULL)
  app(stk, mean, na.rm = TRUE)
}

one_map <- function(gcm, ssp, period, scale_fn = suit_scale,
                    show_title = FALSE, title = "", subtitle = "") {
  r <- get_ens(gcm, ssp, period)
  if (is.null(r)) return(patchwork::plot_spacer())
  df <- r2df(r)
  ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) +
    aoi_geom() +
    scale_fn() +
    { if (nchar(title) > 0) labs(title = title) else labs() } +
    { if (nchar(subtitle) > 0) labs(subtitle = subtitle) else labs() } +
    coord_sf(crs = 4326, expand = FALSE) +
    map_theme
}

# =============================================================================
# FIGURE 1 — SSP2-4.5: 9 GCMs × 3 Periods (ensemble)
# Layout: 8 rows (GCMs) × 3 cols (periods) = 24 panels
# =============================================================================
cat("Building Figure 1: SSP2-4.5 | 9 GCMs x 3 periods...\n")

panels_ssp245 <- list()
for (gi in seq_along(GCM_LIST)) {
  for (pi in seq_along(PERIODS)) {
    ttl      <- if (gi == 1) PER_LABELS[pi] else ""
    row_lbl  <- if (pi == 1) GCM_LABEL[gi] else ""
    p <- one_map(GCM_LIST[gi], "ssp245", PERIODS[pi],
                 title = ttl, subtitle = row_lbl) +
      theme(
        plot.title    = element_text(size = 9, face = "bold", colour = "#2c3e50"),
        plot.subtitle = element_text(size = 7.5, face = "bold", colour = "#555555", hjust = 0)
      )
    panels_ssp245[[length(panels_ssp245) + 1]] <- p
  }
}

fig1 <- wrap_plots(panels_ssp245, ncol = 3, nrow = 8) +
  plot_layout(guides = "collect") &
  suit_scale(name = "Habitat\nSuitability") &
  theme(legend.position = "right")

fig1 <- fig1 + pub_title(
  title    = expression(paste(italic("Elephas maximus"), " Habitat Suitability — SSP2-4.5 (Intermediate Emissions)")),
  subtitle = "Bhutan | Ensemble mean (GLM + RF + BRT + MaxEnt) | Rows = GCM; Columns = time period",
  caption  = paste0("8 CMIP6 GCMs | SSP2-4.5 | EPSG:32645 (UTM Zone 45N) | ",
                    "Colour: white (low) \u2192 yellow \u2192 orange \u2192 red (high suitability)")
)

ggsave(file.path(OUT_DIR, "figpub_MAP1_ssp245_8gcm_3period.png"),
       fig1, width = 16, height = 26, dpi = 300, limitsize = FALSE)
cat("  + Saved MAP1 SSP245\n")

# =============================================================================
# FIGURE 2 — SSP5-8.5: 9 GCMs × 3 Periods (ensemble)
# =============================================================================
cat("Building Figure 2: SSP5-8.5 | 9 GCMs x 3 periods...\n")

panels_ssp585 <- list()
for (gi in seq_along(GCM_LIST)) {
  for (pi in seq_along(PERIODS)) {
    ttl     <- if (gi == 1) PER_LABELS[pi] else ""
    row_lbl <- if (pi == 1) GCM_LABEL[gi] else ""
    p <- one_map(GCM_LIST[gi], "ssp585", PERIODS[pi],
                 title = ttl, subtitle = row_lbl) +
      theme(
        plot.title    = element_text(size = 9, face = "bold", colour = "#8b0000"),
        plot.subtitle = element_text(size = 7.5, face = "bold", colour = "#555555", hjust = 0)
      )
    panels_ssp585[[length(panels_ssp585) + 1]] <- p
  }
}

fig2 <- wrap_plots(panels_ssp585, ncol = 3, nrow = 8) +
  plot_layout(guides = "collect") &
  suit_scale(name = "Habitat\nSuitability") &
  theme(legend.position = "right")

fig2 <- fig2 + pub_title(
  title    = expression(paste(italic("Elephas maximus"), " Habitat Suitability — SSP5-8.5 (Very High Emissions)")),
  subtitle = "Bhutan | Ensemble mean (GLM + RF + BRT + MaxEnt) | Rows = GCM; Columns = time period",
  caption  = paste0("8 CMIP6 GCMs | SSP5-8.5 | EPSG:32645 (UTM Zone 45N) | ",
                    "Colour: white (low) \u2192 yellow \u2192 orange \u2192 red (high suitability)")
)

ggsave(file.path(OUT_DIR, "figpub_MAP2_ssp585_8gcm_3period.png"),
       fig2, width = 16, height = 26, dpi = 300, limitsize = FALSE)
cat("  + Saved MAP2 SSP585\n")

# =============================================================================
# FIGURE 3 — SSP245 vs SSP585 at 2071-2100: 9 GCMs × 2 SSPs
# Side-by-side scenario comparison for end-of-century
# =============================================================================
cat("Building Figure 3: SSP245 vs SSP585 at 2071-2100...\n")

panels_cmp <- list()
for (gi in seq_along(GCM_LIST)) {
  for (si in seq_along(c("ssp245","ssp585"))) {
    ssp_v   <- c("ssp245","ssp585")[si]
    ssp_lbl <- c("SSP2-4.5","SSP5-8.5")[si]
    col     <- c("#2ca25f","#de2d26")[si]
    ttl     <- if (gi == 1) ssp_lbl else ""
    row_lbl <- if (si == 1) GCM_LABEL[gi] else ""
    p <- one_map(GCM_LIST[gi], ssp_v, "2071_2100",
                 title = ttl, subtitle = row_lbl) +
      theme(
        plot.title    = element_text(size = 10, face = "bold", colour = col),
        plot.subtitle = element_text(size = 7.5, face = "bold", colour = "#333333", hjust = 0)
      )
    panels_cmp[[length(panels_cmp) + 1]] <- p
  }
}

fig3 <- wrap_plots(panels_cmp, ncol = 2, nrow = 8) +
  plot_layout(guides = "collect") &
  suit_scale(name = "Habitat\nSuitability") &
  theme(legend.position = "right")

fig3 <- fig3 + pub_title(
  title    = expression(paste(italic("Elephas maximus"), " End-of-Century Habitat Suitability (2071\u20132100)")),
  subtitle = "Bhutan | SSP2-4.5 vs SSP5-8.5 | Ensemble mean (GLM + RF + BRT + MaxEnt) | Left = moderate; Right = extreme emissions",
  caption  = "8 CMIP6 GCMs | EPSG:32645 (UTM Zone 45N)"
)

ggsave(file.path(OUT_DIR, "figpub_MAP3_ssp245_vs_ssp585_2071_2100.png"),
       fig3, width = 12, height = 26, dpi = 300, limitsize = FALSE)
cat("  + Saved MAP3 comparison\n")

# =============================================================================
# FIGURE 4 — Habitat CHANGE maps (Δ vs present): 9 GCMs × 2 SSPs at 2071-2100
# =============================================================================
cat("Building Figure 4: Habitat change maps (delta vs present)...\n")

if (!is.null(pres_r)) {
  panels_delta <- list()
  for (gi in seq_along(GCM_LIST)) {
    for (si in seq_along(c("ssp245","ssp585"))) {
      ssp_v   <- c("ssp245","ssp585")[si]
      ssp_lbl <- c("SSP2-4.5","SSP5-8.5")[si]
      col     <- c("#2ca25f","#de2d26")[si]
      ttl     <- if (gi == 1) ssp_lbl else ""
      row_lbl <- if (si == 1) GCM_LABEL[gi] else ""

      fut_r <- get_ens(GCM_LIST[gi], ssp_v, "2071_2100")
      if (is.null(fut_r)) {
        panels_delta[[length(panels_delta) + 1]] <- patchwork::plot_spacer()
        next
      }
      # Resample future to present grid for clean subtraction
      fut_rs <- tryCatch(resample(fut_r, pres_r, method = "bilinear"),
                         error = function(e) fut_r)
      delta  <- fut_rs - pres_r

      df   <- r2df(delta)
      lim  <- max(abs(quantile(df$value, c(0.02, 0.98), na.rm = TRUE)), 0.10)
      lim  <- round(lim + 0.05, 2)

      p <- ggplot() +
        geom_raster(data = df, aes(x = x, y = y, fill = value)) +
        aoi_geom() +
        change_scale(lim) +
        { if (nchar(ttl) > 0) labs(title = ttl) else labs() } +
        { if (nchar(row_lbl) > 0) labs(subtitle = row_lbl) else labs() } +
        coord_sf(crs = 4326, expand = FALSE) +
        map_theme +
        theme(
          plot.title    = element_text(size = 10, face = "bold", colour = col),
          plot.subtitle = element_text(size = 7.5, face = "bold", colour = "#333333", hjust = 0)
        )
      panels_delta[[length(panels_delta) + 1]] <- p
    }
  }

  fig4 <- wrap_plots(panels_delta, ncol = 2, nrow = 8) +
    plot_layout(guides = "collect") &
    theme(legend.position = "right")

  fig4 <- fig4 + pub_title(
    title    = expression(paste("\u0394 Change in ", italic("Elephas maximus"),
                               " Habitat Suitability vs Present (2071\u20132100)")),
    subtitle = "Blue = habitat loss | White = no change | Red = habitat gain | Left = SSP2-4.5 | Right = SSP5-8.5",
    caption  = "Delta = future ensemble mean minus present-day ensemble | 8 CMIP6 GCMs | EPSG:32645"
  )

  ggsave(file.path(OUT_DIR, "figpub_MAP4_delta_change_2071_2100.png"),
         fig4, width = 12, height = 26, dpi = 300, limitsize = FALSE)
  cat("  + Saved MAP4 delta change\n")
}

# =============================================================================
# FIGURE 5 — GCM UNCERTAINTY at 2071-2100: SD across GCMs + ensemble mean
# Shows where GCMs agree vs disagree
# =============================================================================
cat("Building Figure 5: GCM uncertainty maps...\n")

build_uncertainty_panel <- function(ssp, ssp_lbl, col) {
  gcm_rasters <- list()
  for (gcm in GCM_LIST) {
    r <- get_ens(gcm, ssp, "2071_2100")
    if (!is.null(r)) gcm_rasters[[gcm]] <- r
  }
  if (length(gcm_rasters) < 2) return(list(mean = patchwork::plot_spacer(),
                                            sd   = patchwork::plot_spacer()))

  # Align all to first raster's grid
  ref <- gcm_rasters[[1]]
  stk_list <- lapply(gcm_rasters, function(r) {
    if (!isTRUE(compareGeom(r, ref, stopOnError = FALSE)))
      r <- resample(r, ref, method = "bilinear")
    r
  })
  stk  <- rast(stk_list)
  ens_mean <- app(stk, mean, na.rm = TRUE)
  ens_sd   <- app(stk, sd,   na.rm = TRUE)

  df_mean <- r2df(ens_mean)
  df_sd   <- r2df(ens_sd)

  p_mean <- ggplot() +
    geom_raster(data = df_mean, aes(x = x, y = y, fill = value)) +
    aoi_geom() +
    suit_scale(name = "Mean\nSuitability") +
    labs(title = paste(ssp_lbl, "| Ensemble Mean"),
         subtitle = paste0("Mean across ", length(gcm_rasters), " GCMs")) +
    coord_sf(crs = 4326, expand = FALSE) + map_theme +
    theme(plot.title = element_text(colour = col, size = 9, face = "bold"))

  # SD palette: white = low uncertainty, dark purple = high uncertainty
  p_sd <- ggplot() +
    geom_raster(data = df_sd, aes(x = x, y = y, fill = value)) +
    aoi_geom() +
    scale_fill_distiller(palette = "Purples", direction = 1, na.value = "grey85",
                         name = "SD\n(GCM spread)",
                         guide = guide_colorbar(barwidth = 0.6, barheight = 4.5,
                                                title.position = "top", title.hjust = 0.5,
                                                label.theme = element_text(size = 6))) +
    labs(title = paste(ssp_lbl, "| GCM Uncertainty"),
         subtitle = "SD across GCMs (purple = high disagreement)") +
    coord_sf(crs = 4326, expand = FALSE) + map_theme +
    theme(plot.title = element_text(colour = col, size = 9, face = "bold"))

  list(mean = p_mean, sd = p_sd, n = length(gcm_rasters))
}

unc_245 <- build_uncertainty_panel("ssp245", "SSP2-4.5", "#2ca25f")
unc_585 <- build_uncertainty_panel("ssp585", "SSP5-8.5", "#de2d26")

fig5 <- (unc_245$mean | unc_245$sd) / (unc_585$mean | unc_585$sd) +
  plot_layout(guides = "keep")
fig5 <- fig5 + pub_title(
  title    = expression(paste("GCM Ensemble Agreement — 2071\u20132100 | ", italic("Elephas maximus"), " | Bhutan")),
  subtitle = "Left: mean suitability across all 9 GCMs  |  Right: standard deviation (uncertainty) across GCMs",
  caption  = paste0("Top = SSP2-4.5 (intermediate) | Bottom = SSP5-8.5 (very high emissions) | ",
                    "Low SD = high GCM consensus | High SD = model disagreement")
)
ggsave(file.path(OUT_DIR, "figpub_MAP5_gcm_uncertainty.png"),
       fig5, width = 16, height = 14, dpi = 300, limitsize = FALSE)
cat("  + Saved MAP5 uncertainty\n")

# =============================================================================
# FIGURE 6 — GCM VALIDATION & RANKING PANEL
# Three components:
#   A. Algorithm performance heatmap (AUC, TSS, Boyce, Brier, Calibration)
#   B. GCM reliability metrics (MESS extrapolation + algorithm SD per GCM)
#   C. Ranked lollipop chart
# =============================================================================
cat("Building Figure 6: GCM validation and ranking panel...\n")

# --- 6A: Algorithm metrics heatmap ---
eval_df <- tryCatch(
  read.csv(file.path(RUN_DIR, "02_models/evaluation_all.csv")),
  error = function(e) NULL
)

if (!is.null(eval_df)) {
  eval_df <- eval_df[eval_df$algorithm != "ensemble", ]
  eval_df$Algorithm <- toupper(eval_df$algorithm)

  metric_meta <- data.frame(
    col   = c("auc_mean","tss_mean","boyce","brier","calibration_slope","moran_i"),
    label = c("AUC","TSS","Boyce\nIndex","Brier\nScore","Calibration\nSlope","Moran's\nI"),
    good  = c("high","high","high","low","~1","low"),
    stringsAsFactors = FALSE
  )

  metric_long <- do.call(rbind, lapply(seq_len(nrow(metric_meta)), function(i) {
    m <- metric_meta$col[i]
    if (!m %in% names(eval_df)) return(NULL)
    data.frame(
      Algorithm = eval_df$Algorithm,
      Metric    = metric_meta$label[i],
      Value     = eval_df[[m]],
      Direction = metric_meta$good[i],
      stringsAsFactors = FALSE
    )
  }))
  metric_long <- metric_long[!is.na(metric_long$Value), ]
  metric_long$Metric <- factor(metric_long$Metric,
    levels = metric_meta$label[metric_meta$label %in% unique(metric_long$Metric)])

  # Normalise 0-1 within metric (higher = better) for heat colour
  metric_long <- metric_long %>%
    group_by(Metric) %>%
    mutate(
      norm = if (unique(Direction) == "low")
               1 - (Value - min(Value, na.rm=TRUE)) / diff(range(Value, na.rm=TRUE))
             else
               (Value - min(Value, na.rm=TRUE)) / diff(range(Value, na.rm=TRUE)),
      norm = ifelse(is.nan(norm), 0.5, norm)
    ) %>%
    ungroup()

  fig6a <- ggplot(metric_long, aes(x = Metric, y = Algorithm, fill = norm)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(aes(label = sprintf("%.3f", Value)), size = 3.2, fontface = "bold",
              colour = ifelse(metric_long$norm > 0.55, "white", "grey10")) +
    scale_fill_distiller(palette = "RdYlGn", direction = 1,
                         limits = c(0, 1), guide = "none") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(title = "A  Model Performance Metrics",
         subtitle = "Green = better performance | Values from 5-fold spatial cross-validation (OOF holdout)") +
    theme_bw(base_size = 10) +
    theme(
      axis.title         = element_blank(),
      axis.text.x        = element_text(size = 9, face = "bold"),
      axis.text.y        = element_text(size = 9, face = "bold",
                                        colour = ALGO_COLS[tolower(metric_long$Algorithm[
                                          match(unique(metric_long$Algorithm),
                                                toupper(names(ALGO_COLS)))])]),
      panel.grid         = element_blank(),
      plot.title         = element_text(size = 11, face = "bold"),
      plot.subtitle      = element_text(size = 8.5, colour = "grey40"),
      plot.margin        = margin(5, 8, 5, 8)
    )
}

# --- 6B: GCM reliability — MESS extrapolation + algorithm SD ---
gcm_metrics <- do.call(rbind, lapply(seq_along(GCM_LIST), function(gi) {
  gcm <- GCM_LIST[gi]
  gcm_lbl <- GCM_LABEL[gi]

  # MESS: mean extrapolation fraction at 2071-2100 (avg SSP245 + SSP585)
  mess_frac <- mean(sapply(c("ssp245","ssp585"), function(ssp) {
    ef <- tryCatch({
      ex_files <- file.path(FUT_DIR,
        sprintf("extrapolation_%s_%s_2071_2100.tif", gcm, ssp))
      if (!file.exists(ex_files)) return(NA_real_)
      r <- rast(ex_files)
      vals <- values(r, na.rm = TRUE)
      mean(vals > 0, na.rm = TRUE)
    }, error = function(e) NA_real_)
    ef
  }), na.rm = TRUE)

  # Algorithm agreement (SD across algos) at 2071-2100 SSP370
  algo_sd <- mean(sapply(c("ssp245","ssp585"), function(ssp) {
    files <- file.path(FUT_DIR,
      sprintf("suitability_future_%s_%s_2071_2100_%s.tif", gcm, ssp, ALGOS_ENS))
    files <- files[file.exists(files)]
    if (length(files) < 2) return(NA_real_)
    stk <- tryCatch(rast(files), error = function(e) NULL)
    if (is.null(stk)) return(NA_real_)
    sd_r <- app(stk, sd, na.rm = TRUE)
    mean(values(sd_r, na.rm = TRUE), na.rm = TRUE)
  }), na.rm = TRUE)

  # Mean suitable area % at 2071-2100
  suit_pct <- mean(sapply(c("ssp245","ssp585"), function(ssp) {
    r <- get_ens(gcm, ssp, "2071_2100")
    if (is.null(r)) return(NA_real_)
    vals <- values(r, na.rm = TRUE)
    mean(vals >= 0.5, na.rm = TRUE) * 100
  }), na.rm = TRUE)

  data.frame(gcm = gcm_lbl, mess_frac = mess_frac,
             algo_sd = algo_sd, suit_pct = suit_pct,
             stringsAsFactors = FALSE)
}))

# Composite reliability score (lower extrapolation + lower SD = more reliable)
gcm_metrics$rel_extrap <- 1 - (gcm_metrics$mess_frac /
                                  max(gcm_metrics$mess_frac, na.rm = TRUE))
gcm_metrics$rel_algsd  <- 1 - (gcm_metrics$algo_sd /
                                  max(gcm_metrics$algo_sd,  na.rm = TRUE))
gcm_metrics$composite  <- rowMeans(gcm_metrics[, c("rel_extrap","rel_algsd")],
                                    na.rm = TRUE)
gcm_metrics$rank        <- rank(-gcm_metrics$composite, ties.method = "first")
gcm_metrics$gcm         <- factor(gcm_metrics$gcm,
                                   levels = gcm_metrics$gcm[order(gcm_metrics$composite)])

# Panel B1: MESS extrapolation bar
fig6b1 <- ggplot(gcm_metrics, aes(x = mess_frac * 100, y = gcm)) +
  geom_col(aes(fill = mess_frac), width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", mess_frac * 100)),
            hjust = -0.1, size = 3.2, fontface = "bold") +
  scale_fill_gradient(low = "#1a9850", high = "#d73027") +
  scale_x_continuous(limits = c(0, max(gcm_metrics$mess_frac * 100, na.rm=TRUE) * 1.2),
                     expand = c(0, 0)) +
  labs(title = "B  GCM Extrapolation Risk",
       subtitle = "% pixels with novel climate (MESS < 0) at 2071\u20132100\nLower = more reliable | Mean of SSP2-4.5 + SSP5-8.5",
       x = "Extrapolation fraction (%)", y = NULL) +
  theme_bw(base_size = 10) +
  theme(plot.title    = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 8, colour = "grey40"),
        axis.text.y   = element_text(size = 9, face = "bold"),
        panel.grid.minor = element_blank())

# Panel B2: Algorithm SD
fig6b2 <- ggplot(gcm_metrics, aes(x = algo_sd, y = gcm)) +
  geom_col(aes(fill = algo_sd), width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.3f", algo_sd)),
            hjust = -0.1, size = 3.2, fontface = "bold") +
  scale_fill_gradient(low = "#1a9850", high = "#d73027") +
  scale_x_continuous(limits = c(0, max(gcm_metrics$algo_sd, na.rm=TRUE) * 1.2),
                     expand = c(0, 0)) +
  labs(title = "C  Algorithm Disagreement per GCM",
       subtitle = "Mean SD across GLM/RF/BRT/MaxEnt predictions at 2071\u20132100\nLower = higher algorithm consensus",
       x = "Mean SD (algorithm spread)", y = NULL) +
  theme_bw(base_size = 10) +
  theme(plot.title    = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 8, colour = "grey40"),
        axis.text.y   = element_text(size = 9, face = "bold"),
        panel.grid.minor = element_blank())

# Panel D: Composite ranking lollipop
fig6d <- ggplot(gcm_metrics, aes(x = composite, y = gcm)) +
  geom_segment(aes(x = 0, xend = composite, yend = gcm),
               colour = "grey70", linewidth = 1.2) +
  geom_point(aes(colour = composite), size = 5.5) +
  geom_text(aes(label = paste0("#", rank)), hjust = -0.55,
            size = 3.5, fontface = "bold") +
  scale_colour_gradient(low = "#d73027", high = "#1a9850", guide = "none") +
  scale_x_continuous(limits = c(0, 1.15), expand = c(0, 0),
                     labels = percent_format(accuracy = 1)) +
  labs(title = "D  GCM Reliability Ranking",
       subtitle = "Composite score = mean of (1 - extrapolation fraction) + (1 - algorithm SD)\nHigher = more reliable projections",
       x = "Composite reliability score", y = NULL) +
  theme_bw(base_size = 10) +
  theme(plot.title    = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 8, colour = "grey40"),
        axis.text.y   = element_text(size = 9, face = "bold"),
        panel.grid.minor = element_blank())

# Assemble fig 6
if (!is.null(eval_df)) {
  fig6 <- (fig6a) / (fig6b1 | fig6b2) / fig6d +
    plot_layout(heights = c(1.2, 1, 1.1))
} else {
  fig6 <- (fig6b1 | fig6b2) / fig6d +
    plot_layout(heights = c(1, 1.1))
}

fig6 <- fig6 + pub_title(
  title    = expression(paste(italic("Elephas maximus"), " SDM — Model Validation & GCM Reliability Assessment")),
  subtitle = paste0("A: Algorithm cross-validation performance (5-fold OOF) | ",
                    "B\u2013D: GCM reliability based on climate novelty and model consensus"),
  caption  = paste0("MESS = Multivariate Environmental Similarity Surface (negative = novel climate outside training range) | ",
                    "Algorithm SD = spread across GLM, RF, BRT, MaxEnt")
)

ggsave(file.path(OUT_DIR, "figpub_MAP6_validation_gcm_ranking.png"),
       fig6, width = 16, height = 18, dpi = 300, limitsize = FALSE)
cat("  + Saved MAP6 validation/ranking\n")

# =============================================================================
# FIGURE 7 — Suitable area trajectories: 9 GCMs × 2 SSPs × 3 periods
# =============================================================================
cat("Building Figure 7: Suitable area trajectories...\n")

area_rows <- list()
for (gi in seq_along(GCM_LIST)) {
  for (ssp in c("ssp245","ssp585")) {
    for (pi in seq_along(PERIODS)) {
      r <- get_ens(GCM_LIST[gi], ssp, PERIODS[pi])
      if (is.null(r)) next
      vals <- if (!is.null(aoi_v)) {
        av2 <- tryCatch(project(aoi_v, crs(r)), error = function(e) aoi_v)
        tryCatch(values(mask(crop(r,av2),av2), na.rm=TRUE),
                 error=function(e) values(r, na.rm=TRUE))
      } else values(r, na.rm=TRUE)
      area_rows[[length(area_rows)+1]] <- data.frame(
        GCM      = GCM_LABEL[gi],
        ssp      = ssp,
        Period   = PER_LABELS[pi],
        pct_suit = mean(vals >= 0.5, na.rm=TRUE)*100,
        mean_suit = mean(vals, na.rm=TRUE),
        stringsAsFactors = FALSE
      )
    }
  }
}
area_df <- do.call(rbind, area_rows)
area_df$ssp_label <- ifelse(area_df$ssp == "ssp245", "SSP2-4.5", "SSP5-8.5")
area_df$Period    <- factor(area_df$Period, levels = PER_LABELS)

# Add present baseline
pres_pct <- NA
if (!is.null(pres_r)) {
  pv <- if (!is.null(aoi_v)) {
    av3 <- tryCatch(project(aoi_v, crs(pres_r)), error=function(e) aoi_v)
    tryCatch(values(mask(crop(pres_r,av3),av3), na.rm=TRUE),
             error=function(e) values(pres_r, na.rm=TRUE))
  } else values(pres_r, na.rm=TRUE)
  pres_pct <- mean(pv >= 0.5, na.rm=TRUE)*100
}

# GCM colour palette (8 distinct)
gcm_cols <- setNames(
  c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#a65628","#f781bf","#999999"),
  GCM_LABEL
)

fig7 <- ggplot(area_df, aes(x = Period, y = pct_suit,
                             colour = GCM, group = GCM)) +
  geom_line(linewidth = 1.1, alpha = 0.85) +
  geom_point(size = 2.8) +
  { if (!is.na(pres_pct))
      geom_hline(yintercept = pres_pct, linetype = "dashed",
                 colour = "grey25", linewidth = 0.8) } +
  { if (!is.na(pres_pct))
      annotate("text", x = 0.6, y = pres_pct + 0.4,
               label = sprintf("Present: %.1f%%", pres_pct),
               colour = "grey25", size = 3.2, hjust = 0) } +
  facet_wrap(~ ssp_label, ncol = 2) +
  scale_colour_manual(values = gcm_cols, name = "GCM") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = expression(paste(italic("Elephas maximus"), " Suitable Habitat Trajectory — Bhutan")),
    subtitle = "% of Bhutan with suitability \u2265 0.5 | Ensemble mean (GLM + RF + BRT + MaxEnt)",
    x        = "Time Period",
    y        = "Suitable area (% of Bhutan)",
    caption  = "Left: SSP2-4.5 (intermediate) | Right: SSP5-8.5 (very high emissions) | Dashed = present-day baseline"
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "#2c3e50"),
    strip.text       = element_text(colour = "white", face = "bold", size = 12),
    legend.position  = "right",
    legend.text      = element_text(size = 8.5),
    axis.text.x      = element_text(angle = 20, hjust = 1, size = 9),
    plot.title       = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(size = 9.5, hjust = 0.5, colour = "grey35"),
    plot.caption     = element_text(size = 8, colour = "grey50"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(OUT_DIR, "figpub_MAP7_suitable_area_trajectories.png"),
       fig7, width = 14, height = 8, dpi = 300)
cat("  + Saved MAP7 area trajectories\n")

# =============================================================================
# Export GCM ranking table
# =============================================================================
gcm_rank_out <- gcm_metrics[order(gcm_metrics$rank), c("rank","gcm","mess_frac",
                                                        "algo_sd","suit_pct","composite")]
gcm_rank_out$mess_frac <- round(gcm_rank_out$mess_frac * 100, 2)
gcm_rank_out$algo_sd   <- round(gcm_rank_out$algo_sd, 4)
gcm_rank_out$suit_pct  <- round(gcm_rank_out$suit_pct, 2)
gcm_rank_out$composite <- round(gcm_rank_out$composite, 4)
names(gcm_rank_out) <- c("Rank","GCM","Extrapolation_pct","Algorithm_SD",
                          "Suitable_area_pct","Composite_score")
write.csv(gcm_rank_out, file.path(OUT_DIR, "gcm_reliability_ranking.csv"),
          row.names = FALSE)
cat("\nGCM Reliability Ranking:\n")
print(gcm_rank_out, row.names = FALSE)

cat("\n=== All figures complete ===\n")
cat("Output:", OUT_DIR, "\n")
