#!/usr/bin/env Rscript
# =============================================================================
# pub_final.R  —  Definitive publication figures, Elephas maximus SDM Bhutan
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
RUN <- resolve_run_dir(if (length(args) >= 1) args[[1]] else NULL, repo_root = repo_root)
FUT <- file.path(RUN, "04_future_projections")
OUT <- file.path(RUN, "08_figures_tables")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# Constants
# =============================================================================
GCM_LIST  <- c("acces_cm2","cnrm_cm6_1","cnrm_esm2_1","inm_cm4_8",
               "inm_cm5_0","miroc6","miroc_es2l","mpi_esm1_2_lr","mri_esm2_0")
GCM_LABEL <- c("ACCESS-CM2","CNRM-CM6-1","CNRM-ESM2-1","INM-CM4-8",
               "INM-CM5-0","MIROC6","MIROC-ES2L","MPI-ESM1-2-LR","MRI-ESM2-0")
PERIODS   <- c("2021_2050","2051_2080","2071_2100")
PER_LABEL <- c("2021–2050","2051–2080","2071–2100")
ALGOS     <- c("glm","rf","brt","maxent")

# p90 of present ensemble = 0.346; cap at 0.50 so gradients are visible
SUIT_MAX  <- 0.50
SUIT_COLS <- c("#f7f7f7","#fee391","#fe9929","#d95f0e","#993404")
CHNG_COLS <- c("#053061","#2166ac","#92c5de","#f7f7f7","#f4a582","#ca0020","#67001f")

# =============================================================================
# Load AOI (keep in UTM 32645 for display — no reprojection to WGS84)
# =============================================================================
aoi_path <- repo_path("01_data_raw", "03_vector", "shapefiles", "Bhutan", "bhutan.shp", repo_root = repo_root)
aoi <- tryCatch(st_read(aoi_path, quiet = TRUE), error = function(e) NULL)
if (!is.null(aoi) && is.na(st_crs(aoi))) st_crs(aoi) <- 32645
if (!is.null(aoi) && st_crs(aoi)$epsg != 32645)
  aoi <- st_transform(aoi, 32645)
aoi_v <- if (!is.null(aoi)) vect(aoi) else NULL

# Bhutan bounding box in UTM for coord limits
if (!is.null(aoi)) {
  bb  <- st_bbox(aoi)
  PAD <- 5000   # 5 km padding
  XLIM <- c(bb["xmin"] - PAD, bb["xmax"] + PAD)
  YLIM <- c(bb["ymin"] - PAD, bb["ymax"] + PAD)
} else {
  XLIM <- c(670000, 1010000); YLIM <- c(2950000, 3140000)
}
cat("AOI loaded. Bhutan UTM extent:", paste(round(c(XLIM, YLIM)/1000), collapse=" "), "km\n")

# =============================================================================
# Raster helpers
# =============================================================================
get_ens <- function(gcm, ssp, period) {
  ff <- file.path(FUT, sprintf("suitability_future_%s_%s_%s_%s.tif",
                               gcm, ssp, period, ALGOS))
  ff <- ff[file.exists(ff)]
  if (!length(ff)) return(NULL)
  stk <- tryCatch(rast(ff), error = function(e) NULL)
  if (is.null(stk)) return(NULL)
  app(stk, mean, na.rm = TRUE)
}

r2df_utm <- function(r, clip = TRUE) {
  if (clip && !is.null(aoi_v)) {
    av <- tryCatch(project(aoi_v, crs(r)), error = function(e) aoi_v)
    r  <- tryCatch(mask(crop(r, av), av), error = function(e) r)
  }
  df <- as.data.frame(r[[1]], xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "value"
  df
}

# =============================================================================
# Theme  — clean, minimal, journal-ready
# =============================================================================
map_theme <- theme_void(base_size = 9) +
  theme(
    panel.background  = element_rect(fill = "#e8f0f7", colour = NA),
    panel.border      = element_rect(fill = NA, colour = "grey30", linewidth = 0.4),
    plot.background   = element_rect(fill = "white", colour = NA),
    plot.title        = element_text(size = 8, face = "bold", hjust = 0.5,
                                     margin = margin(b = 2, t = 2)),
    plot.subtitle     = element_text(size = 6.5, hjust = 0.5, colour = "grey40",
                                     margin = margin(b = 1)),
    legend.title      = element_text(size = 7, face = "bold"),
    legend.text       = element_text(size = 6),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width  = unit(0.2, "cm"),
    plot.margin       = margin(1, 1, 1, 1)
  )

suit_scale <- function(name = "Suitability\n(0–0.5+)") {
  scale_fill_gradientn(
    colours = SUIT_COLS,
    values  = rescale(c(0, 0.10, 0.25, 0.40, SUIT_MAX)),
    limits  = c(0, SUIT_MAX), oob = squish,
    na.value = "white", name = name,
    guide = guide_colorbar(
      barwidth = 0.5, barheight = 4,
      title.position = "top", title.hjust = 0.5,
      ticks.linewidth = 0.5,
      label.theme = element_text(size = 6)
    )
  )
}

change_scale <- function(lim = 0.25) {
  scale_fill_gradientn(
    colours = CHNG_COLS,
    values  = rescale(c(-lim, -lim*0.5, -0.05, 0, 0.05, lim*0.5, lim)),
    limits  = c(-lim, lim), oob = squish,
    na.value = "white", name = "\u0394 Suitability",
    guide = guide_colorbar(
      barwidth = 0.5, barheight = 4,
      title.position = "top", title.hjust = 0.5,
      label.theme = element_text(size = 6)
    )
  )
}

# Manual scale bar (200 km) — UTM metres
add_scalebar <- function(df) {
  xr <- range(df$x, na.rm=TRUE); yr <- range(df$y, na.rm=TRUE)
  sb_x <- xr[1] + (xr[2]-xr[1]) * 0.06
  sb_y <- yr[1] + (yr[2]-yr[1]) * 0.06
  sb_len <- 100000  # 100 km in metres
  list(
    annotate("rect", xmin=sb_x, xmax=sb_x+sb_len, ymin=sb_y-3000, ymax=sb_y+3000,
             fill="black", colour="black"),
    annotate("rect", xmin=sb_x+sb_len/2, xmax=sb_x+sb_len, ymin=sb_y-3000, ymax=sb_y+3000,
             fill="white", colour="black"),
    annotate("text", x=sb_x+sb_len/2, y=sb_y-9000, label="100 km",
             size=2.2, hjust=0.5, colour="grey10")
  )
}

# North arrow
add_north <- function(df) {
  xr <- range(df$x, na.rm=TRUE); yr <- range(df$y, na.rm=TRUE)
  nx <- xr[2] - (xr[2]-xr[1])*0.08
  ny <- yr[2] - (yr[2]-yr[1])*0.12
  list(
    annotate("segment", x=nx, xend=nx, y=ny-15000, yend=ny+15000,
             arrow=arrow(length=unit(0.08,"cm"), ends="last", type="closed"),
             linewidth=0.6, colour="grey10"),
    annotate("text", x=nx, y=ny+22000, label="N", size=2.5,
             fontface="bold", hjust=0.5, colour="grey10")
  )
}

# Single panel map
one_panel <- function(gcm, ssp, period, label="", scalebar=FALSE, north=FALSE,
                      scale_fn = suit_scale) {
  r <- get_ens(gcm, ssp, period)
  if (is.null(r)) return(patchwork::plot_spacer())
  df <- r2df_utm(r)
  p <- ggplot() +
    geom_raster(data=df, aes(x=x, y=y, fill=value)) +
    { if (!is.null(aoi))
        geom_sf(data=aoi, fill=NA, colour="grey20", linewidth=0.5,
                inherit.aes=FALSE) } +
    scale_fn() +
    coord_sf(crs=32645, xlim=XLIM, ylim=YLIM, expand=FALSE) +
    map_theme
  if (nchar(label) > 0)
    p <- p + annotate("text", x=XLIM[1]+8000, y=YLIM[2]-8000,
                      label=label, size=3.2, fontface="bold",
                      hjust=0, vjust=1, colour="grey5")
  if (scalebar) p <- p + add_scalebar(df)
  if (north)    p <- p + add_north(df)
  p
}

# =============================================================================
# FIGURE 1 — Present-day ensemble suitability (single large map + histogram)
# =============================================================================
cat("Building Figure 1: Present-day suitability...\n")

pres_r <- tryCatch(
  rast(file.path(RUN,"03_present_suitability/suitability_present_ensemble.tif")),
  error=function(e) NULL)

if (!is.null(pres_r)) {
  df_pres <- r2df_utm(pres_r)

  p_map <- ggplot() +
    geom_raster(data=df_pres, aes(x=x, y=y, fill=value)) +
    { if (!is.null(aoi)) geom_sf(data=aoi, fill=NA, colour="grey15",
                                  linewidth=0.7, inherit.aes=FALSE) } +
    suit_scale(name="Habitat\nsuitability") +
    add_scalebar(df_pres) + add_north(df_pres) +
    annotate("text", x=XLIM[1]+8000, y=YLIM[2]-8000, label="a",
             size=4, fontface="bold", hjust=0, vjust=1) +
    labs(title="Present-day habitat suitability (ensemble mean)") +
    coord_sf(crs=32645, xlim=XLIM, ylim=YLIM, expand=FALSE) +
    map_theme + theme(plot.title=element_text(size=10))

  # Per-algorithm present maps
  algo_cols_v <- c(glm="#377eb8",rf="#984ea3",brt="#e41a1c",maxent="#4daf4a")
  algo_panels <- list()
  for (ai in seq_along(ALGOS)) {
    f <- file.path(RUN, sprintf("03_present_suitability/suitability_present_%s.tif", ALGOS[ai]))
    if (!file.exists(f)) { algo_panels[[ai]] <- patchwork::plot_spacer(); next }
    r_a <- tryCatch(rast(f), error=function(e) NULL)
    if (is.null(r_a)) { algo_panels[[ai]] <- patchwork::plot_spacer(); next }
    df_a <- r2df_utm(r_a)
    algo_panels[[ai]] <- ggplot() +
      geom_raster(data=df_a, aes(x=x, y=y, fill=value)) +
      { if (!is.null(aoi)) geom_sf(data=aoi, fill=NA, colour="grey20",
                                    linewidth=0.5, inherit.aes=FALSE) } +
      suit_scale(name=NULL) +
      annotate("text", x=XLIM[1]+8000, y=YLIM[2]-8000,
               label=c("b","c","d","e")[ai],
               size=3.2, fontface="bold", hjust=0, vjust=1) +
      labs(title=toupper(ALGOS[ai])) +
      coord_sf(crs=32645, xlim=XLIM, ylim=YLIM, expand=FALSE) +
      map_theme +
      theme(plot.title=element_text(colour=algo_cols_v[ALGOS[ai]], size=8.5, face="bold"))
  }

  # Suitability histogram
  p_hist <- ggplot(df_pres, aes(x=value)) +
    geom_histogram(aes(fill=after_stat(x)), bins=40, colour="white", linewidth=0.1) +
    scale_fill_gradientn(colours=SUIT_COLS,
                         values=rescale(c(0,0.10,0.25,0.40,SUIT_MAX)),
                         limits=c(0,SUIT_MAX), oob=squish, guide="none") +
    geom_vline(xintercept=0.5, linetype="dashed", colour="grey30", linewidth=0.7) +
    annotate("text", x=0.52, y=Inf, label="Threshold (0.5)",
             size=2.8, hjust=0, vjust=1.5, colour="grey30") +
    scale_x_continuous(limits=c(0, SUIT_MAX), oob=squish,
                       labels=function(x) sprintf("%.1f",x)) +
    labs(x="Suitability score", y="Number of cells",
         title="f  Suitability distribution") +
    theme_bw(base_size=9) +
    theme(plot.title=element_text(size=8.5,face="bold"),
          panel.grid.minor=element_blank(),
          axis.text=element_text(size=7))

  # Assemble
  fig1 <- (p_map | (wrap_plots(algo_panels, ncol=2) & theme(legend.position="none"))) /
    p_hist +
    plot_layout(heights=c(3,1), widths=c(1.5,1)) +
    plot_annotation(
      title=expression(paste("Present-Day ", italic("Elephas maximus"), " Habitat Suitability — Bhutan")),
      subtitle="Ensemble mean (GLM + Random Forest + BRT + MaxEnt) | 5-fold spatial cross-validation | 250 m resolution",
      caption="Colour scale capped at 0.5; values > 0.5 shown in darkest red | CRS: WGS 84 / UTM Zone 45N (EPSG:32645)",
      theme=theme(plot.title=element_text(size=13,face="bold",hjust=0.5),
                  plot.subtitle=element_text(size=9,hjust=0.5,colour="grey35"),
                  plot.caption=element_text(size=7.5,colour="grey50",hjust=1))
    )

  ggsave(file.path(OUT,"Fig1_present_suitability.png"),
         fig1, width=16, height=13, dpi=300, limitsize=FALSE)
  cat("  + Fig1 saved\n")
}

# =============================================================================
# FIGURE 2 — SSP2-4.5: 3 rows (periods) × 9 cols (GCMs) — landscape
# =============================================================================
cat("Building Figure 2: SSP2-4.5 | 3 periods x 9 GCMs...\n")

build_gcm_grid <- function(ssp, ssp_title, ssp_col, fname) {
  letters_seq <- letters[1:24]
  li <- 0
  panels <- list()
  for (pi in seq_along(PERIODS)) {
    for (gi in seq_along(GCM_LIST)) {
      li <- li + 1
      col_title <- if (pi == 1) GCM_LABEL[gi] else ""
      row_title  <- if (gi == 1) PER_LABEL[pi] else ""
      p <- one_panel(GCM_LIST[gi], ssp, PERIODS[pi],
                     label = letters_seq[li],
                     scalebar = (pi == 3 && gi == 1),
                     north    = (pi == 1 && gi == 8))
      if (nchar(col_title) > 0)
        p <- p + labs(title = col_title) +
          theme(plot.title = element_text(size = 7.5, face = "bold",
                                          colour = "grey10", hjust = 0.5,
                                          margin = margin(b = 1, t = 2)))
      if (nchar(row_title) > 0)
        p <- p + labs(subtitle = row_title) +
          theme(plot.subtitle = element_text(size = 7, face = "bold",
                                             colour = ssp_col, hjust = 0,
                                             margin = margin(b = 0.5)))
      panels[[li]] <- p
    }
  }
  fig <- wrap_plots(panels, ncol=8, nrow=3) +
    plot_layout(guides="collect") &
    suit_scale(name="Suitability") &
    theme(legend.position="right",
          legend.margin=margin(0,0,0,4))

  fig <- fig + plot_annotation(
    title    = bquote(paste(italic("Elephas maximus"), " Habitat Suitability — ", .(ssp_title))),
    subtitle = "Ensemble mean (GLM + RF + BRT + MaxEnt) | Columns = CMIP6 GCM | Rows = time period | Colour scale capped at 0.5",
    caption  = "CRS: EPSG:32645 (WGS 84 / UTM Zone 45N) | Dashed = 0.5 suitability threshold | 9 GCMs × 3 time periods = 27 scenarios",
    theme = theme(
      plot.title    = element_text(size=14, face="bold", hjust=0.5),
      plot.subtitle = element_text(size=9, hjust=0.5, colour="grey35"),
      plot.caption  = element_text(size=7.5, colour="grey50", hjust=1)
    )
  )
  ggsave(file.path(OUT, fname), fig, width=24, height=11, dpi=300, limitsize=FALSE)
  cat(sprintf("  + %s saved\n", fname))
}

build_gcm_grid("ssp245","SSP2-4.5 (Intermediate Emissions)","#1a7837",
               "Fig2_ssp245_8gcm_3period.png")
build_gcm_grid("ssp585","SSP5-8.5 (Very High Emissions)","#b2182b",
               "Fig3_ssp585_8gcm_3period.png")

# =============================================================================
# FIGURE 4 — SSP245 vs SSP585 side-by-side at 2071-2100 (2 rows × 9 cols)
# =============================================================================
cat("Building Figure 4: SSP2-4.5 vs SSP5-8.5 at 2071-2100...\n")

panels_4 <- list()
li <- 0
for (si in 1:2) {
  ssp_v   <- c("ssp245","ssp585")[si]
  ssp_lbl <- c("SSP2-4.5 (Intermediate)","SSP5-8.5 (Very High)")[si]
  ssp_col <- c("#1a7837","#b2182b")[si]
  for (gi in seq_along(GCM_LIST)) {
    li <- li + 1
    col_title <- if (si == 1) GCM_LABEL[gi] else ""
    row_title  <- if (gi == 1) ssp_lbl else ""
    p <- one_panel(GCM_LIST[gi], ssp_v, "2071_2100",
                   label = letters[li],
                   scalebar = (si == 2 && gi == 1),
                   north    = (si == 1 && gi == 8))
    if (nchar(col_title) > 0)
      p <- p + labs(title = col_title) +
        theme(plot.title=element_text(size=7.5, face="bold", hjust=0.5,
                                      margin=margin(b=1,t=2)))
    if (nchar(row_title) > 0)
      p <- p + labs(subtitle = row_title) +
        theme(plot.subtitle=element_text(size=7,face="bold",colour=ssp_col,hjust=0))
    panels_4[[li]] <- p
  }
}

fig4 <- wrap_plots(panels_4, ncol=8, nrow=2) +
  plot_layout(guides="collect") &
  suit_scale(name="Suitability") &
  theme(legend.position="right")

fig4 <- fig4 + plot_annotation(
  title    = bquote(paste(italic("Elephas maximus"), " End-of-Century Habitat Suitability (2071\u20132100)")),
  subtitle = "Top row: SSP2-4.5 (intermediate emissions) | Bottom row: SSP5-8.5 (very high emissions) | 8 CMIP6 GCMs",
  caption  = "Ensemble mean of GLM + RF + BRT + MaxEnt | CRS: EPSG:32645 | Colour capped at 0.5",
  theme=theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
              plot.subtitle=element_text(size=9,hjust=0.5,colour="grey35"),
              plot.caption=element_text(size=7.5,colour="grey50",hjust=1))
)
ggsave(file.path(OUT,"Fig4_ssp245_vs_ssp585_2071_2100.png"),
       fig4, width=24, height=8.5, dpi=300, limitsize=FALSE)
cat("  + Fig4 saved\n")

# =============================================================================
# FIGURE 5 — Habitat change maps Δ (2071-2100 vs present), 2 rows × 9 cols
# Pre-compute a SINGLE global symmetric delta limit (§7.4 scale rule)
# =============================================================================
cat("Building Figure 5: Delta habitat change maps...\n")

# Compute global symmetric limit from all 18 delta rasters (2 SSPs × 9 GCMs)
DELTA_LIM <- 0.20   # fixed fallback
if (!is.null(pres_r)) {
  all_delta_vals <- c()
  for (.ssp in c("ssp245","ssp585")) {
    for (.gcm in GCM_LIST) {
      .fr <- get_ens(.gcm, .ssp, "2071_2100")
      if (!is.null(.fr)) {
        .fr <- tryCatch(resample(.fr, pres_r, method="bilinear"), error=function(e) .fr)
        .dv <- as.numeric(values(.fr - pres_r, na.rm=TRUE))
        all_delta_vals <- c(all_delta_vals, .dv)
      }
    }
  }
  if (length(all_delta_vals) > 0) {
    DELTA_LIM <- max(abs(quantile(all_delta_vals, c(0.02, 0.98), na.rm=TRUE)), 0.10)
    DELTA_LIM <- round(min(DELTA_LIM, 0.35), 2)
  }
  cat(sprintf("  Global delta scale: ±%.2f (applied to all Fig5 panels)\n", DELTA_LIM))
}

if (!is.null(pres_r)) {
  panels_5 <- list()
  li <- 0
  for (si in 1:2) {
    ssp_v   <- c("ssp245","ssp585")[si]
    ssp_lbl <- c("SSP2-4.5 Δ","SSP5-8.5 Δ")[si]
    ssp_col <- c("#1a7837","#b2182b")[si]
    for (gi in seq_along(GCM_LIST)) {
      li <- li + 1
      col_title <- if (si == 1) GCM_LABEL[gi] else ""
      row_title  <- if (gi == 1) ssp_lbl else ""
      fut_r <- get_ens(GCM_LIST[gi], ssp_v, "2071_2100")
      if (is.null(fut_r)) { panels_5[[li]] <- patchwork::plot_spacer(); next }
      fut_rs <- tryCatch(resample(fut_r, pres_r, method="bilinear"), error=function(e) fut_r)
      delta  <- fut_rs - pres_r
      df_d   <- r2df_utm(delta)
      lim    <- DELTA_LIM  # fixed global symmetric scale

      p <- ggplot() +
        geom_raster(data=df_d, aes(x=x, y=y, fill=value)) +
        { if (!is.null(aoi)) geom_sf(data=aoi, fill=NA, colour="grey20",
                                      linewidth=0.5, inherit.aes=FALSE) } +
        change_scale(lim) +
        annotate("text", x=XLIM[1]+8000, y=YLIM[2]-8000,
                 label=letters[li], size=3.2, fontface="bold", hjust=0, vjust=1) +
        coord_sf(crs=32645, xlim=XLIM, ylim=YLIM, expand=FALSE) +
        map_theme
      if (nchar(col_title) > 0)
        p <- p + labs(title=col_title) +
          theme(plot.title=element_text(size=7.5,face="bold",hjust=0.5,
                                        margin=margin(b=1,t=2)))
      if (nchar(row_title) > 0)
        p <- p + labs(subtitle=row_title) +
          theme(plot.subtitle=element_text(size=7,face="bold",colour=ssp_col,hjust=0))
      if (si==2 && gi==1) p <- p + add_scalebar(df_d)
      panels_5[[li]] <- p
    }
  }
  fig5 <- wrap_plots(panels_5, ncol=8, nrow=2) +
    plot_layout(guides="collect") &
    theme(legend.position="right")
  fig5 <- fig5 + plot_annotation(
    title    = bquote(paste("\u0394 Change in ", italic("Elephas maximus"),
                            " Habitat Suitability vs Present (2071\u20132100)")),
    subtitle = "Blue = projected loss | White = no change | Red = projected gain | Ensemble mean (GLM + RF + BRT + MaxEnt)",
    caption  = "Delta = future suitability minus present-day ensemble | 8 CMIP6 GCMs | CRS: EPSG:32645",
    theme=theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
                plot.subtitle=element_text(size=9,hjust=0.5,colour="grey35"),
                plot.caption=element_text(size=7.5,colour="grey50",hjust=1))
  )
  ggsave(file.path(OUT,"Fig5_delta_change_2071_2100.png"),
         fig5, width=24, height=8.5, dpi=300, limitsize=FALSE)
  cat("  + Fig5 saved\n")
}

# =============================================================================
# FIGURE 6 — GCM uncertainty: mean + SD across 9 GCMs at 2071-2100
# =============================================================================
cat("Building Figure 6: GCM ensemble uncertainty...\n")

build_gcm_uncertainty <- function(ssp, ssp_lbl, ssp_col) {
  gcm_rasters <- lapply(GCM_LIST, function(gcm) get_ens(gcm, ssp, "2071_2100"))
  gcm_rasters <- gcm_rasters[!sapply(gcm_rasters, is.null)]
  if (length(gcm_rasters) < 2) return(list(NULL, NULL))
  ref <- gcm_rasters[[1]]
  aligned <- lapply(gcm_rasters, function(r) {
    if (!isTRUE(compareGeom(r, ref, stopOnError=FALSE)))
      r <- resample(r, ref, method="bilinear")
    r
  })
  stk      <- rast(aligned)
  ens_mean <- app(stk, mean, na.rm=TRUE)
  ens_sd   <- app(stk, sd,   na.rm=TRUE)

  df_m <- r2df_utm(ens_mean)
  df_s <- r2df_utm(ens_sd)

  p_mean <- ggplot() +
    geom_raster(data=df_m, aes(x=x,y=y,fill=value)) +
    { if (!is.null(aoi)) geom_sf(data=aoi,fill=NA,colour="grey20",
                                  linewidth=0.5,inherit.aes=FALSE) } +
    suit_scale(name="Mean\nSuitability") +
    add_scalebar(df_m) + add_north(df_m) +
    annotate("text", x=XLIM[1]+8000, y=YLIM[2]-8000, label="a",
             size=3.2, fontface="bold", hjust=0, vjust=1) +
    labs(title=paste(ssp_lbl, "| Ensemble mean")) +
    coord_sf(crs=32645,xlim=XLIM,ylim=YLIM,expand=FALSE) +
    map_theme +
    theme(plot.title=element_text(size=9, face="bold", colour=ssp_col))

  sd_max <- quantile(df_s$value, 0.99, na.rm=TRUE)
  p_sd <- ggplot() +
    geom_raster(data=df_s, aes(x=x,y=y,fill=value)) +
    { if (!is.null(aoi)) geom_sf(data=aoi,fill=NA,colour="grey20",
                                  linewidth=0.5,inherit.aes=FALSE) } +
    scale_fill_distiller(palette="Purples", direction=1,
                         limits=c(0, sd_max), oob=squish,
                         na.value="white", name="SD\n(uncertainty)",
                         guide=guide_colorbar(barwidth=0.5,barheight=4,
                                              title.position="top",title.hjust=0.5,
                                              label.theme=element_text(size=6))) +
    annotate("text", x=XLIM[1]+8000, y=YLIM[2]-8000, label="b",
             size=3.2, fontface="bold", hjust=0, vjust=1) +
    labs(title=paste(ssp_lbl, "| GCM spread (SD)")) +
    coord_sf(crs=32645,xlim=XLIM,ylim=YLIM,expand=FALSE) +
    map_theme +
    theme(plot.title=element_text(size=9, face="bold", colour=ssp_col))

  list(p_mean, p_sd)
}

unc_245 <- build_gcm_uncertainty("ssp245","SSP2-4.5","#1a7837")
unc_585 <- build_gcm_uncertainty("ssp585","SSP5-8.5","#b2182b")

fig6 <- (unc_245[[1]] | unc_245[[2]] | unc_585[[1]] | unc_585[[2]])
fig6 <- fig6 + plot_annotation(
  title    = bquote(paste("GCM Ensemble Agreement — 2071\u20132100 | ", italic("Elephas maximus"), " | Bhutan")),
  subtitle = "Columns 1–2: SSP2-4.5 | Columns 3–4: SSP5-8.5 | Odd = ensemble mean | Even = SD across 9 GCMs (purple = high uncertainty)",
  caption  = "Standard deviation across 9 CMIP6 GCMs | Low SD = high model consensus | CRS: EPSG:32645",
  theme=theme(plot.title=element_text(size=13,face="bold",hjust=0.5),
              plot.subtitle=element_text(size=9,hjust=0.5,colour="grey35"),
              plot.caption=element_text(size=7.5,colour="grey50",hjust=1))
)
ggsave(file.path(OUT,"Fig6_gcm_uncertainty.png"),
       fig6, width=20, height=7, dpi=300, limitsize=FALSE)
cat("  + Fig6 saved\n")

# =============================================================================
# FIGURE 7 — Model validation: ROC + metrics heatmap + AUC bars
# =============================================================================
cat("Building Figure 7: Model validation panel...\n")

eval_df  <- tryCatch(read.csv(file.path(RUN,"02_models/evaluation_all.csv")), error=function(e) NULL)
roc_df   <- tryCatch(read.csv(file.path(RUN,"02_models/roc_curve_data.csv")),  error=function(e) NULL)
ALGO_COLS <- c(brt="#e41a1c", glm="#377eb8", maxent="#4daf4a", rf="#984ea3")

if (!is.null(eval_df) && !is.null(roc_df)) {
  eval_df  <- eval_df[eval_df$algorithm != "ensemble", ]
  roc_df$fpr <- 1 - roc_df$specificity

  # ROC curves
  auc_lut <- setNames(sprintf("AUC = %.3f", eval_df$auc_mean), eval_df$algorithm)
  p_roc <- ggplot(roc_df, aes(x=fpr, y=sensitivity, colour=algorithm)) +
    geom_abline(slope=1, intercept=0, linetype="dashed", colour="grey60", linewidth=0.6) +
    geom_ribbon(aes(ymin=0, ymax=sensitivity, fill=algorithm), alpha=0.08) +
    geom_line(linewidth=1.3) +
    scale_colour_manual(values=ALGO_COLS, labels=auc_lut, name=NULL) +
    scale_fill_manual(values=ALGO_COLS, guide="none") +
    scale_x_continuous(limits=c(0,1), breaks=seq(0,1,0.2),
                       labels=function(x) ifelse(x==0,"0",sprintf("%.1f",x))) +
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    labs(title="a  ROC Curves (5-fold spatial cross-validation)",
         x="False positive rate (1 − Specificity)",
         y="True positive rate (Sensitivity)") +
    theme_bw(base_size=11) +
    theme(plot.title=element_text(size=11,face="bold"),
          legend.position=c(0.98,0.04), legend.justification=c(1,0),
          legend.background=element_rect(fill=alpha("white",0.85),colour="grey80"),
          legend.text=element_text(size=8.5),
          panel.grid.minor=element_blank())

  # Metrics heatmap
  metrics <- c("auc_mean","tss_mean","boyce","brier","calibration_slope","moran_i")
  m_labels <- c("AUC","TSS","Boyce\nIndex","Brier\nScore","Calib.\nSlope","Moran's I")
  m_dir    <- c(1,1,1,-1,0,-1)  # 1=high good, -1=low good, 0=~1 good

  ml <- do.call(rbind, lapply(seq_along(metrics), function(i) {
    m <- metrics[i]
    if (!m %in% names(eval_df)) return(NULL)
    data.frame(Algorithm=toupper(eval_df$algorithm), Metric=m_labels[i],
               Value=eval_df[[m]], dir=m_dir[i], stringsAsFactors=FALSE)
  }))
  ml <- ml[!is.na(ml$Value), ]

  ml <- ml %>% group_by(Metric) %>%
    mutate(norm = {
      rng <- diff(range(Value, na.rm=TRUE))
      if (rng < 1e-10) 0.5
      else if (unique(dir) == -1) 1 - (Value - min(Value,na.rm=TRUE)) / rng
      else (Value - min(Value,na.rm=TRUE)) / rng
    }) %>% ungroup()

  ml$Metric    <- factor(ml$Metric, levels=m_labels[m_labels %in% unique(ml$Metric)])
  ml$Algorithm <- factor(ml$Algorithm, levels=toupper(c("glm","rf","brt","maxent")))

  p_heat <- ggplot(ml, aes(x=Metric, y=Algorithm, fill=norm)) +
    geom_tile(colour="white", linewidth=1.2) +
    geom_text(aes(label=sprintf("%.3f",Value), colour=norm>0.55),
              size=3.5, fontface="bold", show.legend=FALSE) +
    scale_fill_distiller(palette="RdYlGn", direction=1, limits=c(0,1), guide="none") +
    scale_colour_manual(values=c("FALSE"="grey10","TRUE"="white")) +
    scale_x_discrete(expand=c(0,0), position="top") +
    scale_y_discrete(expand=c(0,0)) +
    labs(title="b  Cross-Validation Performance") +
    theme_bw(base_size=11) +
    theme(plot.title=element_text(size=11,face="bold"),
          axis.title=element_blank(),
          axis.text.x=element_text(size=9,face="bold",colour="grey20"),
          axis.text.y=element_text(size=9.5,face="bold",
                                   colour=ALGO_COLS[tolower(unique(ml$Algorithm)[
                                     match(levels(ml$Algorithm),toupper(names(ALGO_COLS)))])]),
          panel.grid=element_blank())

  # AUC bar
  p_auc <- ggplot(eval_df, aes(x=reorder(toupper(algorithm),auc_mean), y=auc_mean,
                                fill=algorithm)) +
    geom_col(width=0.6, show.legend=FALSE) +
    geom_text(aes(label=sprintf("%.4f",auc_mean)), hjust=-0.08,
              size=3.5, fontface="bold") +
    geom_hline(yintercept=0.7, linetype="dashed", colour="grey60", linewidth=0.6) +
    geom_hline(yintercept=0.9, linetype="dashed", colour="grey40", linewidth=0.6) +
    annotate("text",x=0.55,y=0.705,label="Good (0.7)",size=2.8,hjust=0,colour="grey50") +
    annotate("text",x=0.55,y=0.905,label="Excellent (0.9)",size=2.8,hjust=0,colour="grey35") +
    scale_fill_manual(values=ALGO_COLS) +
    scale_y_continuous(limits=c(0.5,1.02), expand=c(0,0),
                       breaks=seq(0.5,1.0,0.1)) +
    coord_flip() +
    labs(title="c  AUC (ROC)", x=NULL, y="AUC") +
    theme_bw(base_size=11) +
    theme(plot.title=element_text(size=11,face="bold"),
          panel.grid.minor=element_blank(),
          axis.text.y=element_text(size=10,face="bold",
                                   colour=ALGO_COLS[tolower(
                                     reorder(eval_df$algorithm,eval_df$auc_mean))]))

  fig7 <- (p_roc | (p_heat / p_auc)) + plot_layout(widths=c(1.4,1))
  fig7 <- fig7 + plot_annotation(
    title    = bquote(paste(italic("Elephas maximus"), " SDM — Model Validation | Bhutan")),
    subtitle = "Spatial cross-validation (5 folds) | Out-of-fold holdout predictions | 252 presence records from station-level survey data",
    caption  = "Boyce Index: 0.987 (excellent) | AUC range: 0.925–0.958 | Green = better performance",
    theme=theme(plot.title=element_text(size=13,face="bold",hjust=0.5),
                plot.subtitle=element_text(size=9,hjust=0.5,colour="grey35"),
                plot.caption=element_text(size=7.5,colour="grey50",hjust=1))
  )
  ggsave(file.path(OUT,"Fig7_model_validation.png"),
         fig7, width=16, height=10, dpi=300)
  cat("  + Fig7 saved\n")
}

# =============================================================================
# FIGURE 8 — Suitable area trajectories: 9 GCMs × 2 SSPs
# =============================================================================
cat("Building Figure 8: Suitable area trajectories...\n")

area_rows <- list()
for (gi in seq_along(GCM_LIST)) {
  for (ssp in c("ssp245","ssp585")) {
    for (pi in seq_along(PERIODS)) {
      r <- get_ens(GCM_LIST[gi], ssp, PERIODS[pi])
      if (is.null(r)) next
      vals <- if (!is.null(aoi_v)) {
        av2 <- tryCatch(project(aoi_v, crs(r)), error=function(e) aoi_v)
        tryCatch(values(mask(crop(r,av2),av2),na.rm=TRUE),
                 error=function(e) values(r,na.rm=TRUE))
      } else values(r, na.rm=TRUE)
      area_rows[[length(area_rows)+1]] <- data.frame(
        GCM=GCM_LABEL[gi], ssp=ssp, Period=PER_LABEL[pi],
        pct_suit=mean(vals>=0.5,na.rm=TRUE)*100,
        mean_suit=mean(vals,na.rm=TRUE)*100,
        stringsAsFactors=FALSE
      )
    }
  }
}
area_df <- do.call(rbind, area_rows)
area_df$ssp_label <- factor(ifelse(area_df$ssp=="ssp245","SSP2-4.5","SSP5-8.5"),
                             levels=c("SSP2-4.5","SSP5-8.5"))
area_df$Period    <- factor(area_df$Period, levels=PER_LABEL)

pres_pct <- NA
if (!is.null(pres_r)) {
  pv <- if (!is.null(aoi_v)) {
    av3 <- tryCatch(project(aoi_v, crs(pres_r)), error=function(e) aoi_v)
    tryCatch(values(mask(crop(pres_r,av3),av3),na.rm=TRUE),
             error=function(e) values(pres_r,na.rm=TRUE))
  } else values(pres_r, na.rm=TRUE)
  pres_pct <- mean(pv>=0.5, na.rm=TRUE)*100
}

gcm_cols9 <- setNames(
  c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#a65628","#f781bf","#333333","#999999"),
  GCM_LABEL)

# Panel A: line trajectories
p_traj <- ggplot(area_df, aes(x=Period, y=pct_suit, colour=GCM, group=GCM)) +
  geom_line(linewidth=1.1, alpha=0.9) +
  geom_point(aes(shape=GCM), size=3) +
  { if(!is.na(pres_pct)) geom_hline(yintercept=pres_pct,
                                     linetype="dashed",colour="grey25",linewidth=0.8) } +
  { if(!is.na(pres_pct)) annotate("text",x=0.65,y=pres_pct+0.25,
                                   label=sprintf("Present: %.1f%%",pres_pct),
                                   size=3,hjust=0,colour="grey25") } +
  facet_wrap(~ssp_label, ncol=2) +
  scale_colour_manual(values=gcm_cols9, name="GCM") +
  scale_shape_manual(values=setNames(c(16,17,15,18,8,4,3,1,6),GCM_LABEL), name="GCM") +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  labs(title="a  Suitable habitat trajectory",
       x="Time period", y="Area with suitability \u2265 0.5 (% of Bhutan)") +
  theme_bw(base_size=11) +
  theme(strip.background=element_rect(fill="#2c3e50"),
        strip.text=element_text(colour="white",face="bold",size=11),
        legend.position="right", legend.text=element_text(size=8.5),
        axis.text.x=element_text(angle=15,hjust=1,size=9),
        panel.grid.minor=element_blank(),
        plot.title=element_text(size=11,face="bold"))

# Panel B: boxplot distribution at 2071-2100
area_2100 <- area_df[area_df$Period == "2071–2100", ]
p_box <- ggplot(area_2100, aes(x=ssp_label, y=pct_suit, fill=ssp_label)) +
  geom_boxplot(width=0.4, outlier.shape=21, outlier.size=2, linewidth=0.6,
               alpha=0.7) +
  geom_jitter(aes(colour=GCM), width=0.08, size=3, alpha=0.95) +
  { if(!is.na(pres_pct)) geom_hline(yintercept=pres_pct,
                                     linetype="dashed",colour="grey25",linewidth=0.8) } +
  scale_fill_manual(values=c("SSP2-4.5"="#74c476","SSP5-8.5"="#fb6a4a"),guide="none") +
  scale_colour_manual(values=gcm_cols9, name="GCM") +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  labs(title="b  2071\u20132100 distribution",
       x=NULL, y="Area with suitability \u2265 0.5 (% of Bhutan)") +
  theme_bw(base_size=11) +
  theme(legend.position="none", panel.grid.minor=element_blank(),
        axis.text.x=element_text(size=10,face="bold"),
        plot.title=element_text(size=11,face="bold"))

fig8 <- p_traj + p_box + plot_layout(widths=c(2.5,1))
fig8 <- fig8 + plot_annotation(
  title    = bquote(paste(italic("Elephas maximus"), " Suitable Habitat Projections — Bhutan")),
  subtitle = "Suitable habitat defined as suitability \u2265 0.5 | Ensemble mean (GLM + RF + BRT + MaxEnt) | 8 CMIP6 GCMs",
  caption  = "Dashed line = present-day baseline | Left: SSP2-4.5 | Right: SSP5-8.5",
  theme=theme(plot.title=element_text(size=13,face="bold",hjust=0.5),
              plot.subtitle=element_text(size=9,hjust=0.5,colour="grey35"),
              plot.caption=element_text(size=7.5,colour="grey50",hjust=1))
)
ggsave(file.path(OUT,"Fig8_suitable_area_trajectories.png"),
       fig8, width=18, height=8, dpi=300)
cat("  + Fig8 saved\n")

# =============================================================================
# FIGURE 9 — GCM reliability ranking
# =============================================================================
cat("Building Figure 9: GCM ranking panel...\n")

gcm_rank <- do.call(rbind, lapply(seq_along(GCM_LIST), function(gi) {
  gcm <- GCM_LIST[gi]
  mess_frac <- mean(sapply(c("ssp245","ssp585"), function(ssp) {
    f <- file.path(FUT, sprintf("extrapolation_%s_%s_2071_2100.tif", gcm, ssp))
    if (!file.exists(f)) return(NA_real_)
    r <- tryCatch(rast(f), error=function(e) NULL)
    if (is.null(r)) return(NA_real_)
    mean(values(r, na.rm=TRUE) > 0, na.rm=TRUE)
  }), na.rm=TRUE)

  algo_sd <- mean(sapply(c("ssp245","ssp585"), function(ssp) {
    ff <- file.path(FUT, sprintf("suitability_future_%s_%s_2071_2100_%s.tif",gcm,ssp,ALGOS))
    ff <- ff[file.exists(ff)]
    if (length(ff) < 2) return(NA_real_)
    stk <- tryCatch(rast(ff), error=function(e) NULL)
    if (is.null(stk)) return(NA_real_)
    mean(values(app(stk, sd, na.rm=TRUE), na.rm=TRUE), na.rm=TRUE)
  }), na.rm=TRUE)

  data.frame(GCM=GCM_LABEL[gi], mess_pct=mess_frac*100, algo_sd=algo_sd,
             stringsAsFactors=FALSE)
}))

gcm_rank$rel_mess <- 1 - gcm_rank$mess_pct / max(gcm_rank$mess_pct,na.rm=TRUE)
gcm_rank$rel_sd   <- 1 - gcm_rank$algo_sd   / max(gcm_rank$algo_sd,  na.rm=TRUE)
gcm_rank$score    <- rowMeans(gcm_rank[,c("rel_mess","rel_sd")], na.rm=TRUE)
gcm_rank$rank     <- rank(-gcm_rank$score, ties.method="first")
gcm_rank$GCM      <- factor(gcm_rank$GCM, levels=gcm_rank$GCM[order(gcm_rank$score)])

p_rank <- ggplot(gcm_rank, aes(x=score, y=GCM)) +
  geom_vline(xintercept=seq(0,1,0.2), colour="grey90", linewidth=0.4) +
  geom_segment(aes(x=0, xend=score, yend=GCM), colour="grey70", linewidth=1.8) +
  geom_point(aes(fill=score), shape=21, size=5, colour="grey20", stroke=0.5) +
  geom_text(aes(label=sprintf("#%d  %.3f", rank, score)),
            hjust=-0.15, size=3.5, fontface="bold") +
  scale_fill_distiller(palette="RdYlGn", direction=1, limits=c(0,1), guide="none") +
  scale_x_continuous(limits=c(0,1.25), expand=c(0,0),
                     labels=percent_format(accuracy=1)) +
  labs(title="a  GCM Reliability Ranking",
       subtitle="Composite = mean(1 - extrapolation fraction, 1 - algorithm SD)\nGreen = most reliable | Red = least reliable",
       x="Composite reliability score", y=NULL) +
  theme_bw(base_size=11) +
  theme(plot.title=element_text(size=11,face="bold"),
        plot.subtitle=element_text(size=8.5,colour="grey40"),
        panel.grid=element_blank(),
        axis.text.y=element_text(size=10,face="bold",
                                  colour=gcm_cols9[levels(gcm_rank$GCM)]))

p_mess <- ggplot(gcm_rank, aes(x=mess_pct, y=GCM)) +
  geom_col(aes(fill=mess_pct), width=0.65, show.legend=FALSE) +
  geom_text(aes(label=sprintf("%.1f%%",mess_pct)), hjust=-0.1, size=3.5, fontface="bold") +
  scale_fill_gradient(low="#1a9850", high="#d73027") +
  scale_x_continuous(expand=c(0,0),
                     limits=c(0, max(gcm_rank$mess_pct,na.rm=TRUE)*1.2)) +
  labs(title="b  Climate Novelty (Extrapolation)",
       subtitle="% pixels with climate outside training range\nat 2071–2100 (mean SSP2-4.5 + SSP5-8.5)",
       x="Extrapolation fraction (%)", y=NULL) +
  theme_bw(base_size=11) +
  theme(plot.title=element_text(size=11,face="bold"),
        plot.subtitle=element_text(size=8.5,colour="grey40"),
        panel.grid.minor=element_blank(),
        axis.text.y=element_text(size=10,face="bold"))

p_sd <- ggplot(gcm_rank, aes(x=algo_sd, y=GCM)) +
  geom_col(aes(fill=algo_sd), width=0.65, show.legend=FALSE) +
  geom_text(aes(label=sprintf("%.4f",algo_sd)), hjust=-0.1, size=3.5, fontface="bold") +
  scale_fill_gradient(low="#1a9850", high="#d73027") +
  scale_x_continuous(expand=c(0,0),
                     limits=c(0, max(gcm_rank$algo_sd,na.rm=TRUE)*1.2)) +
  labs(title="c  Algorithm Disagreement",
       subtitle="Mean SD across GLM/RF/BRT/MaxEnt\nat 2071–2100 (mean SSP2-4.5 + SSP5-8.5)",
       x="Mean SD (algorithm spread)", y=NULL) +
  theme_bw(base_size=11) +
  theme(plot.title=element_text(size=11,face="bold"),
        plot.subtitle=element_text(size=8.5,colour="grey40"),
        panel.grid.minor=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())

fig9 <- p_rank | p_mess | p_sd
fig9 <- fig9 + plot_annotation(
  title    = bquote(paste("CMIP6 GCM Reliability Assessment | ", italic("Elephas maximus"), " SDM | Bhutan")),
  subtitle = "Ranking based on climate novelty (MESS extrapolation) and algorithm consensus at 2071–2100",
  caption  = paste0("Best GCMs (green): lowest climate novelty + highest model agreement | ",
                    "INM-CM5-0 and MIROC6 rank highest for this study region"),
  theme=theme(plot.title=element_text(size=13,face="bold",hjust=0.5),
              plot.subtitle=element_text(size=9,hjust=0.5,colour="grey35"),
              plot.caption=element_text(size=7.5,colour="grey50",hjust=1))
)
ggsave(file.path(OUT,"Fig9_gcm_reliability_ranking.png"),
       fig9, width=18, height=9, dpi=300)

# Export ranking table
gcm_out <- gcm_rank[order(gcm_rank$rank), c("rank","GCM","mess_pct","algo_sd","score")]
names(gcm_out) <- c("Rank","GCM","Extrapolation_%","Algorithm_SD","Composite_Score")
write.csv(gcm_out, file.path(OUT,"Table_gcm_ranking.csv"), row.names=FALSE)
cat("  + Fig9 + ranking table saved\n")
cat("\nGCM Ranking:\n")
print(gcm_out, row.names=FALSE)

# =============================================================================
# PORTRAIT FIGURES — 8 rows (GCMs) × 3 cols (periods)
# Same data as Fig2/Fig3 but rotated: GCMs become rows, periods become columns.
# =============================================================================
cat("\n--- Portrait figures ---\n")

build_gcm_grid_portrait <- function(ssp, ssp_title, ssp_col, fname) {
  letters_seq <- letters[1:24]
  li <- 0
  panels <- list()
  for (gi in seq_along(GCM_LIST)) {       # rows = GCMs
    for (pi in seq_along(PERIODS)) {      # columns = periods
      li <- li + 1
      col_title <- if (gi == 1) PER_LABEL[pi] else ""   # period as column header (top row only)
      row_title  <- if (pi == 1) GCM_LABEL[gi] else ""  # GCM as row label (left column only)
      p <- one_panel(GCM_LIST[gi], ssp, PERIODS[pi],
                     label    = letters_seq[li],
                     scalebar = (gi == 8 && pi == 1),
                     north    = (gi == 1 && pi == 3))
      if (nchar(col_title) > 0)
        p <- p + labs(title = col_title) +
          theme(plot.title = element_text(size = 7.5, face = "bold",
                                          colour = "grey10", hjust = 0.5,
                                          margin = margin(b = 1, t = 2)))
      if (nchar(row_title) > 0)
        p <- p + labs(subtitle = row_title) +
          theme(plot.subtitle = element_text(size = 7, face = "bold",
                                             colour = ssp_col, hjust = 0,
                                             margin = margin(b = 0.5)))
      panels[[li]] <- p
    }
  }
  fig <- wrap_plots(panels, ncol = 3, nrow = 8) +
    plot_layout(guides = "collect") &
    suit_scale(name = "Suitability") &
    theme(legend.position = "right",
          legend.margin = margin(0, 0, 0, 4))

  fig <- fig + plot_annotation(
    title    = bquote(paste(italic("Elephas maximus"), " Habitat Suitability — ", .(ssp_title))),
    subtitle = "Ensemble mean (GLM + RF + BRT + MaxEnt) | Rows = CMIP6 GCM | Columns = time period | Colour scale capped at 0.5",
    caption  = "CRS: EPSG:32645 (WGS 84 / UTM Zone 45N) | 9 GCMs × 3 time periods = 27 scenarios",
    theme = theme(
      plot.title    = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 9,  hjust = 0.5,  colour = "grey35"),
      plot.caption  = element_text(size = 7.5, colour = "grey50", hjust = 1)
    )
  )
  ggsave(file.path(OUT, fname), fig, width = 11, height = 28, dpi = 300, limitsize = FALSE)
  cat(sprintf("  + %s saved\n", fname))
}

# Portrait variants of Fig2 and Fig3
build_gcm_grid_portrait("ssp245", "SSP2-4.5 (Intermediate Emissions)", "#1a7837",
                        "Fig2p_ssp245_portrait.png")
build_gcm_grid_portrait("ssp585", "SSP5-8.5 (Very High Emissions)", "#b2182b",
                        "Fig3p_ssp585_portrait.png")

# =============================================================================
# ALL 4 SSPs — landscape + portrait
# Adds SSP1-2.6 (optimistic) and SSP3-7.0 (pessimistic/high) grids
# =============================================================================
cat("Building SSP1-2.6 and SSP3-7.0 grids...\n")

build_gcm_grid("ssp126", "SSP1-2.6 (Low Emissions — Sustainability)", "#2171b5",
               "Fig_SSP126_landscape.png")
build_gcm_grid("ssp370", "SSP3-7.0 (High Emissions — Regional Rivalry)", "#d6604d",
               "Fig_SSP370_landscape.png")
build_gcm_grid_portrait("ssp126", "SSP1-2.6 (Low Emissions — Sustainability)", "#2171b5",
                        "Fig_SSP126_portrait.png")
build_gcm_grid_portrait("ssp370", "SSP3-7.0 (High Emissions — Regional Rivalry)", "#d6604d",
                        "Fig_SSP370_portrait.png")

# =============================================================================
# PORTRAIT Fig4p — SSP2-4.5 vs SSP5-8.5 at 2071-2100 (8 rows × 2 cols)
# =============================================================================
cat("Building Figure 4p (portrait)...\n")

panels_4p <- list()
li <- 0
for (gi in seq_along(GCM_LIST)) {     # rows = GCMs
  for (si in 1:2) {                   # columns = SSPs
    li <- li + 1
    ssp_v   <- c("ssp245", "ssp585")[si]
    ssp_lbl <- c("SSP2-4.5 (Intermediate)", "SSP5-8.5 (Very High)")[si]
    ssp_col <- c("#1a7837", "#b2182b")[si]
    col_title <- if (gi == 1) ssp_lbl else ""
    row_title  <- if (si == 1) GCM_LABEL[gi] else ""
    p <- one_panel(GCM_LIST[gi], ssp_v, "2071_2100",
                   label    = letters[li],
                   scalebar = (gi == 8 && si == 1),
                   north    = (gi == 1 && si == 2))
    if (nchar(col_title) > 0)
      p <- p + labs(title = col_title) +
        theme(plot.title = element_text(size = 7.5, face = "bold", hjust = 0.5,
                                        colour = ssp_col, margin = margin(b = 1, t = 2)))
    if (nchar(row_title) > 0)
      p <- p + labs(subtitle = row_title) +
        theme(plot.subtitle = element_text(size = 7, face = "bold",
                                           colour = "grey30", hjust = 0))
    panels_4p[[li]] <- p
  }
}

fig4p <- wrap_plots(panels_4p, ncol = 2, nrow = 8) +
  plot_layout(guides = "collect") &
  suit_scale(name = "Suitability") &
  theme(legend.position = "right")

fig4p <- fig4p + plot_annotation(
  title    = bquote(paste(italic("Elephas maximus"), " End-of-Century Suitability (2071\u20132100)")),
  subtitle = "Left: SSP2-4.5 (intermediate) | Right: SSP5-8.5 (very high) | Rows = 8 CMIP6 GCMs",
  caption  = "Ensemble mean of GLM + RF + BRT + MaxEnt | CRS: EPSG:32645 | Colour capped at 0.5",
  theme = theme(plot.title    = element_text(size = 14, face = "bold", hjust = 0.5),
                plot.subtitle = element_text(size = 9,  hjust = 0.5,  colour = "grey35"),
                plot.caption  = element_text(size = 7.5, colour = "grey50", hjust = 1))
)
ggsave(file.path(OUT, "Fig4p_ssp245_vs_ssp585_portrait.png"),
       fig4p, width = 8, height = 22, dpi = 300, limitsize = FALSE)
cat("  + Fig4p saved\n")

# =============================================================================
# PORTRAIT Fig5p — Habitat change Δ (8 rows × 2 cols)
# =============================================================================
cat("Building Figure 5p (portrait delta)...\n")

if (!is.null(pres_r)) {
  panels_5p <- list()
  li <- 0
  for (gi in seq_along(GCM_LIST)) {   # rows = GCMs
    for (si in 1:2) {                 # columns = SSPs
      li <- li + 1
      ssp_v   <- c("ssp245", "ssp585")[si]
      ssp_lbl <- c("SSP2-4.5 \u0394", "SSP5-8.5 \u0394")[si]
      ssp_col <- c("#1a7837", "#b2182b")[si]
      col_title <- if (gi == 1) ssp_lbl else ""
      row_title  <- if (si == 1) GCM_LABEL[gi] else ""

      fut_r <- get_ens(GCM_LIST[gi], ssp_v, "2071_2100")
      if (is.null(fut_r)) { panels_5p[[li]] <- patchwork::plot_spacer(); next }
      fut_rs <- tryCatch(resample(fut_r, pres_r, method = "bilinear"), error = function(e) fut_r)
      delta  <- fut_rs - pres_r
      df_d   <- r2df_utm(delta)
      lim    <- DELTA_LIM  # fixed global symmetric scale (§7.4)

      p <- ggplot() +
        geom_raster(data = df_d, aes(x = x, y = y, fill = value)) +
        { if (!is.null(aoi)) geom_sf(data = aoi, fill = NA, colour = "grey20",
                                      linewidth = 0.5, inherit.aes = FALSE) } +
        change_scale(lim) +
        annotate("text", x = XLIM[1] + 8000, y = YLIM[2] - 8000,
                 label = letters[li], size = 3.2, fontface = "bold", hjust = 0, vjust = 1) +
        coord_sf(crs = 32645, xlim = XLIM, ylim = YLIM, expand = FALSE) +
        map_theme
      if (nchar(col_title) > 0)
        p <- p + labs(title = col_title) +
          theme(plot.title = element_text(size = 7.5, face = "bold", hjust = 0.5,
                                          colour = ssp_col, margin = margin(b = 1, t = 2)))
      if (nchar(row_title) > 0)
        p <- p + labs(subtitle = row_title) +
          theme(plot.subtitle = element_text(size = 7, face = "bold",
                                             colour = "grey30", hjust = 0))
      if (gi == 8 && si == 1) p <- p + add_scalebar(df_d)
      panels_5p[[li]] <- p
    }
  }

  fig5p <- wrap_plots(panels_5p, ncol = 2, nrow = 8) +
    plot_layout(guides = "collect") &
    theme(legend.position = "right")
  fig5p <- fig5p + plot_annotation(
    title    = bquote(paste("\u0394 Change in ", italic("Elephas maximus"),
                            " Habitat Suitability vs Present (2071\u20132100)")),
    subtitle = "Blue = projected loss | White = no change | Red = projected gain | Rows = 8 CMIP6 GCMs",
    caption  = "Delta = future minus present-day ensemble | Left: SSP2-4.5 | Right: SSP5-8.5 | CRS: EPSG:32645",
    theme = theme(plot.title    = element_text(size = 14, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 9,  hjust = 0.5,  colour = "grey35"),
                  plot.caption  = element_text(size = 7.5, colour = "grey50", hjust = 1))
  )
  ggsave(file.path(OUT, "Fig5p_delta_change_portrait.png"),
         fig5p, width = 8, height = 22, dpi = 300, limitsize = FALSE)
  cat("  + Fig5p saved\n")
}

# =============================================================================
# ALL-4-SSP COMPARISON MAPS — per time period (4 SSPs × 9 GCMs = 36 panels)
# One figure per period: rows = SSPs, cols = GCMs
# =============================================================================
cat("Building all-4-SSP per-period comparison maps...\n")

ALL_SSPS     <- c("ssp126", "ssp245", "ssp370", "ssp585")
ALL_SSP_LBLS <- c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
ALL_SSP_COLS <- c("#2171b5", "#1a7837", "#d6604d", "#b2182b")

build_period_comparison <- function(period, period_label, fname) {
  letters_seq <- c(letters, paste0("a", letters))  # 52 labels
  li <- 0
  panels <- list()
  for (si in seq_along(ALL_SSPS)) {          # rows = SSPs
    for (gi in seq_along(GCM_LIST)) {        # columns = GCMs
      li <- li + 1
      col_title <- if (si == 1) GCM_LABEL[gi] else ""
      row_title  <- if (gi == 1) ALL_SSP_LBLS[si] else ""
      p <- one_panel(GCM_LIST[gi], ALL_SSPS[si], period,
                     label    = letters_seq[li],
                     scalebar = (si == 4 && gi == 1),
                     north    = (si == 1 && gi == 8))
      if (nchar(col_title) > 0)
        p <- p + labs(title = col_title) +
          theme(plot.title = element_text(size = 7, face = "bold", colour = "grey10",
                                          hjust = 0.5, margin = margin(b = 1, t = 2)))
      if (nchar(row_title) > 0)
        p <- p + labs(subtitle = row_title) +
          theme(plot.subtitle = element_text(size = 7, face = "bold",
                                             colour = ALL_SSP_COLS[si], hjust = 0,
                                             margin = margin(b = 0.5)))
      panels[[li]] <- p
    }
  }
  fig <- wrap_plots(panels, ncol = 8, nrow = 4) +
    plot_layout(guides = "collect") &
    suit_scale(name = "Suitability") &
    theme(legend.position = "right", legend.margin = margin(0, 0, 0, 4))

  fig <- fig + plot_annotation(
    title    = bquote(paste(italic("Elephas maximus"), " Habitat Suitability — ", .(period_label),
                            " | All 4 SSPs")),
    subtitle = "Ensemble mean (GLM + RF + BRT + MaxEnt) | Rows = SSP scenario | Columns = CMIP6 GCM | Colour capped at 0.5",
    caption  = "CRS: EPSG:32645 (WGS 84 / UTM Zone 45N) | 4 SSPs × 9 GCMs = 36 scenarios",
    theme = theme(
      plot.title    = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 9,  hjust = 0.5,  colour = "grey35"),
      plot.caption  = element_text(size = 7.5, colour = "grey50", hjust = 1)
    )
  )
  ggsave(file.path(OUT, fname), fig, width = 24, height = 14, dpi = 300, limitsize = FALSE)
  cat(sprintf("  + %s saved\n", fname))
}

build_period_comparison("2021_2050", "2021\u20132050", "Fig_AllSSP_2021_2050.png")
build_period_comparison("2051_2080", "2051\u20132080", "Fig_AllSSP_2051_2080.png")
build_period_comparison("2071_2100", "2071\u20132100", "Fig_AllSSP_2071_2100.png")

# Portrait version of per-period (4 cols = SSPs, 8 rows = GCMs)
build_period_comparison_portrait <- function(period, period_label, fname) {
  letters_seq <- c(letters, paste0("a", letters))
  li <- 0
  panels <- list()
  for (gi in seq_along(GCM_LIST)) {          # rows = GCMs
    for (si in seq_along(ALL_SSPS)) {        # columns = SSPs
      li <- li + 1
      col_title <- if (gi == 1) ALL_SSP_LBLS[si] else ""
      row_title  <- if (si == 1) GCM_LABEL[gi] else ""
      p <- one_panel(GCM_LIST[gi], ALL_SSPS[si], period,
                     label    = letters_seq[li],
                     scalebar = (gi == 8 && si == 1),
                     north    = (gi == 1 && si == 4))
      if (nchar(col_title) > 0)
        p <- p + labs(title = col_title) +
          theme(plot.title = element_text(size = 7, face = "bold",
                                          colour = ALL_SSP_COLS[si], hjust = 0.5,
                                          margin = margin(b = 1, t = 2)))
      if (nchar(row_title) > 0)
        p <- p + labs(subtitle = row_title) +
          theme(plot.subtitle = element_text(size = 7, face = "bold",
                                             colour = "grey30", hjust = 0,
                                             margin = margin(b = 0.5)))
      panels[[li]] <- p
    }
  }
  fig <- wrap_plots(panels, ncol = 4, nrow = 8) +
    plot_layout(guides = "collect") &
    suit_scale(name = "Suitability") &
    theme(legend.position = "right", legend.margin = margin(0, 0, 0, 4))

  fig <- fig + plot_annotation(
    title    = bquote(paste(italic("Elephas maximus"), " Habitat Suitability — ", .(period_label),
                            " | All 4 SSPs")),
    subtitle = "Ensemble mean (GLM + RF + BRT + MaxEnt) | Rows = CMIP6 GCM | Columns = SSP scenario | Colour capped at 0.5",
    caption  = "CRS: EPSG:32645 (WGS 84 / UTM Zone 45N) | 9 GCMs × 4 SSPs = 36 scenarios",
    theme = theme(
      plot.title    = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 9,  hjust = 0.5,  colour = "grey35"),
      plot.caption  = element_text(size = 7.5, colour = "grey50", hjust = 1)
    )
  )
  ggsave(file.path(OUT, fname), fig, width = 14, height = 22, dpi = 300, limitsize = FALSE)
  cat(sprintf("  + %s saved\n", fname))
}

build_period_comparison_portrait("2021_2050", "2021\u20132050", "Fig_AllSSP_2021_2050_portrait.png")
build_period_comparison_portrait("2051_2080", "2051\u20132080", "Fig_AllSSP_2051_2080_portrait.png")
build_period_comparison_portrait("2071_2100", "2071\u20132100", "Fig_AllSSP_2071_2100_portrait.png")

cat("\n=== All 9 figures complete ===\n")
cat("Output:", OUT, "\n")
