#!/usr/bin/env Rscript
# ============================================================
# Publication-Quality Figures — Elephas maximus SDM, Bhutan
# 9 GCMs x 4 SSPs x 3 Periods x 4 Algorithms
# ============================================================
suppressPackageStartupMessages({
  library(terra); library(sf); library(ggplot2)
  library(patchwork); library(scales)
})

RUN_DIR <- "E:/Elephas_maximus_SDM_Project_v4/04_outputs/runs/RUN_20260317_203608_b990"
OUT_DIR <- file.path(RUN_DIR, "08_figures_tables")
FUT_DIR <- file.path(RUN_DIR, "04_future_projections")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
setwd("E:/Elephas_maximus_SDM_Project_v4")

# ---- Colour palettes & themes ----
SUIT_COLS <- c("#f7f7f7","#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
SUIT_VALS <- c(0, 0.05, 0.20, 0.40, 0.65, 1.00)
CHANGE_COLS <- c("#2166ac","#92c5de","#f7f7f7","#f4a582","#ca0020")
MAP_BG <- "#d6e8f5"
ALGO_COLS <- c(brt="#e41a1c", glm="#377eb8", maxent="#4daf4a", rf="#984ea3")
SSP_COLS  <- c("SSP1-2.6"="#1a9850","SSP2-4.5"="#fee08b","SSP3-7.0"="#f46d43","SSP5-8.5"="#d73027")

suit_scale <- function(name="Suitability") {
  scale_fill_gradientn(
    colours=SUIT_COLS, values=SUIT_VALS, limits=c(0,1),
    na.value="grey88", name=name,
    guide=guide_colorbar(barwidth=0.8, barheight=5.5, ticks.linewidth=0.4,
                         title.theme=element_text(size=8,face="bold"),
                         label.theme=element_text(size=7))
  )
}
change_scale <- function(lim=0.5) {
  scale_fill_gradientn(
    colours=CHANGE_COLS, limits=c(-lim,lim), oob=squish,
    na.value="grey88", name="\u0394 Suit.",
    guide=guide_colorbar(barwidth=0.8, barheight=5.5, ticks.linewidth=0.4,
                         title.theme=element_text(size=8,face="bold"),
                         label.theme=element_text(size=7))
  )
}
map_theme <- theme_bw() + theme(
  panel.background  = element_rect(fill=MAP_BG),
  panel.grid.major  = element_line(colour="white", linewidth=0.2),
  plot.title        = element_text(size=9, face="bold", hjust=0.5, margin=margin(b=2)),
  plot.subtitle     = element_text(size=7.5, hjust=0.5, colour="grey40"),
  axis.title        = element_blank(),
  axis.text         = element_text(size=6),
  legend.title      = element_text(size=8, face="bold"),
  legend.text       = element_text(size=7),
  plot.margin       = margin(3,3,3,3)
)
pub_annotation <- function(title, subtitle="", caption="") {
  plot_annotation(
    title=title, subtitle=subtitle, caption=caption,
    theme=theme(
      plot.title    = element_text(size=14, face="bold", hjust=0.5, margin=margin(b=4)),
      plot.subtitle = element_text(size=10, hjust=0.5, colour="grey30", margin=margin(b=8)),
      plot.caption  = element_text(size=7.5, hjust=1, colour="grey50", margin=margin(t=6))
    )
  )
}

# ---- Load AOI ----
load_aoi <- function() {
  p <- "E:/Elephas_maximus_SDM_Project_v4/01_data_raw/03_vector/shapefiles/Bhutan/bhutan.shp"
  aoi <- tryCatch(st_read(p, quiet=TRUE), error=function(e) NULL)
  if (is.null(aoi)) {
    aoi <- tryCatch(st_read(file.path(RUN_DIR,"02_data_intermediate/m_area_vector.gpkg"),
                            quiet=TRUE), error=function(e) NULL)
  }
  if (!is.null(aoi) && is.na(st_crs(aoi))) st_crs(aoi) <- 32645
  aoi
}
aoi    <- load_aoi()
aoi_wgs <- if (!is.null(aoi)) st_transform(aoi, 4326) else NULL
cat("AOI loaded\n")

# ---- Raster helpers ----
r2df <- function(r, aoi_sf=NULL) {
  if (!is.null(aoi_sf)) {
    av <- vect(st_transform(aoi_sf, crs(r)))
    r  <- tryCatch(mask(crop(r, av), av), error=function(e) r)
  }
  df <- as.data.frame(r[[1]], xy=TRUE, na.rm=TRUE)
  names(df)[3] <- "value"
  df
}
aoi_layer <- function() {
  if (!is.null(aoi_wgs))
    geom_sf(data=aoi_wgs, fill=NA, colour="black", linewidth=0.35)
  else NULL
}
one_map <- function(df, title, scale_fn=suit_scale) {
  ggplot() +
    geom_raster(data=df, aes(x=x, y=y, fill=value)) +
    aoi_layer() + scale_fn() +
    labs(title=title) +
    coord_sf(crs=4326) + map_theme
}

# Discover GCMs dynamically from available output files
gcms_available <- unique(gsub("suitability_future_([a-z0-9_]+?)_(ssp[0-9]+).*", "\\1",
  basename(list.files(FUT_DIR, pattern="suitability_future_.*_ssp.*_glm\\.tif$"))))
gcm_name_map <- c(
  acces_cm2="ACCESS-CM2", cnrm_cm6_1="CNRM-CM6-1", cnrm_esm2_1="CNRM-ESM2-1",
  inm_cm4_8="INM-CM4-8", inm_cm5_0="INM-CM5-0", miroc6="MIROC6",
  miroc_es2l="MIROC-ES2L", mpi_esm1_2_lr="MPI-ESM1-2-LR", mri_esm2_0="MRI-ESM2-0"
)
gcms_list  <- gcms_available
gcm_labels <- ifelse(gcms_list %in% names(gcm_name_map), gcm_name_map[gcms_list], toupper(gcms_list))
cat(sprintf("Found %d GCMs: %s\n", length(gcms_list), paste(gcm_labels, collapse=", ")))
gcm_label_str <- paste(gcm_labels, collapse=", ")
ssps_list   <- c("ssp126","ssp245","ssp370","ssp585")
ssp_labels  <- c("SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5")
ssp_sub     <- c("(Low emissions)","(Intermediate)","(High emissions)","(Very high)")
periods     <- c("2021_2050","2051_2080","2071_2100")
per_labels  <- c("2021-2050","2051-2080","2071-2100")
algos_ens   <- c("glm","rf","brt")
algo_all    <- c("glm","rf","brt","maxent")
algo_labels <- c("GLM","Random Forest","BRT","MaxEnt")

get_ens <- function(gcm=NULL, ssp, period, algos=algos_ens) {
  if (is.null(gcm)) {
    pat <- paste0("suitability_future_.*_",ssp,"_",period,"_(", paste(algos,collapse="|"),").tif$")
  } else {
    pat <- paste0("suitability_future_",gcm,"_",ssp,"_",period,"_(", paste(algos,collapse="|"),").tif$")
  }
  files <- list.files(FUT_DIR, pattern=pat, full.names=TRUE)
  if (length(files)==0) return(NULL)
  stk <- tryCatch(rast(files), error=function(e) NULL)
  if (is.null(stk)) return(NULL)
  app(stk, mean, na.rm=TRUE)
}

# ==============================================================
# FIG A  — All GCMs x 4 SSPs x 3 Periods (ensemble per GCM)
# Layout: rows=GCMs, cols=SSPs, facet within each = period
# Full grid: 3 rows x 4 SSP groups x 3 periods = massive
# Better: 3-page figure, one page per GCM (4 SSPs x 3 periods = 12 panels per page)
# ==============================================================
cat(sprintf("Building Fig A: Full %d-GCM x 4-SSP x 3-Period grid...\n", length(gcms_list)))
for (gi in seq_along(gcms_list)) {
  gcm <- gcms_list[gi]; gcm_lbl <- gcm_labels[gi]
  cat(sprintf("  GCM: %s\n", gcm_lbl))
  panels <- list()
  for (pi in seq_along(periods)) {
    for (si in seq_along(ssps_list)) {
      ens <- get_ens(gcm, ssps_list[si], periods[pi])
      if (is.null(ens)) { panels[[length(panels)+1]] <- patchwork::plot_spacer(); next }
      df  <- r2df(ens, aoi)
      ttl <- if (pi==1) ssp_labels[si] else ""
      p   <- one_map(df, ttl) +
             theme(plot.title=element_text(size=9, face="bold",
                                           colour=SSP_COLS[ssp_labels[si]]))
      if (si==1) {
        p <- p + labs(subtitle=per_labels[pi]) +
             theme(plot.subtitle=element_text(size=8,face="bold",hjust=0,colour="#2c3e50"))
      }
      panels[[length(panels)+1]] <- p
    }
  }
  # 3 rows (periods) x 4 cols (SSPs)
  fig <- wrap_plots(panels, ncol=4, nrow=3) &
    suit_scale()
  fig <- fig + pub_annotation(
    title    = sprintf("Future Habitat Suitability — %s (GLM+RF+BRT Ensemble)", gcm_lbl),
    subtitle = expression(paste(italic("Elephas maximus"), " | Bhutan | Rows = time period; Columns = emission scenario")),
    caption  = sprintf("GCM: %s  |  Ensemble mean of GLM, Random Forest, and BRT  |  CRS: EPSG:32645 (UTM Zone 45N)", gcm_lbl)
  )
  fname <- sprintf("figpub_A_%s_ssp_period.png", gcm)
  ggsave(file.path(OUT_DIR, fname), fig, width=18, height=14, dpi=300, limitsize=FALSE)
  cat(sprintf("  + Saved %s\n", fname))
}

# ==============================================================
# FIG B — GCM comparison: all 3 GCMs side-by-side
# One panel per SSP x Period, showing 3 GCMs as columns
# Pick end-of-century (2071-2100) for all 4 SSPs
# ==============================================================
cat("Building Fig B: 3-GCM comparison per SSP (2071-2100)...\n")
per_b <- "2071_2100"
all_panels_B <- list()
for (si in seq_along(ssps_list)) {
  row_panels <- list()
  for (gi in seq_along(gcms_list)) {
    ens <- get_ens(gcms_list[gi], ssps_list[si], per_b)
    if (is.null(ens)) { row_panels[[gi]] <- patchwork::plot_spacer(); next }
    df  <- r2df(ens, aoi)
    ttl <- if (si==1) gcm_labels[gi] else ""
    sub <- if (gi==1) paste(ssp_labels[si], ssp_sub[si]) else ""
    p   <- one_map(df, ttl) +
           labs(subtitle=sub) +
           theme(plot.subtitle=element_text(size=8,face="bold",colour=SSP_COLS[ssp_labels[si]],hjust=0))
    row_panels[[gi]] <- p
  }
  all_panels_B <- c(all_panels_B, row_panels)
}
fig_B <- wrap_plots(all_panels_B, ncol=length(gcms_list), nrow=4) &
  suit_scale()
fig_B <- fig_B + pub_annotation(
  title    = "GCM Comparison — 2071-2100 Habitat Suitability across Emission Scenarios",
  subtitle = bquote(paste(italic("Elephas maximus"), " | Bhutan | Ensemble mean (GLM+RF+BRT) | Columns = GCM; Rows = SSP")),
  caption  = paste0("GCMs: ", gcm_label_str, "  |  CRS: EPSG:32645 (WGS 84 / UTM Zone 45N)")
)
ggsave(file.path(OUT_DIR, "figpub_B_gcm_comparison.png"),
       fig_B, width=16, height=20, dpi=300, limitsize=FALSE)
cat("+ Fig B saved\n")

# ==============================================================
# FIG C — Habitat change maps: delta suitability vs present
# All 4 SSPs x 3 periods, ensemble across all GCMs
# ==============================================================
cat("Building Fig C: Habitat change maps...\n")
present_file <- file.path(RUN_DIR, "03_present_suitability/suitability_present_ensemble.tif")
if (file.exists(present_file)) {
  pres_r <- rast(present_file)
  panels_C <- list()
  for (pi in seq_along(periods)) {
    for (si in seq_along(ssps_list)) {
      ens <- get_ens(NULL, ssps_list[si], periods[pi])
      if (is.null(ens)) { panels_C[[length(panels_C)+1]] <- patchwork::plot_spacer(); next }
      fut_r <- tryCatch(resample(ens, pres_r, method="bilinear"), error=function(e) ens)
      delta  <- fut_r - pres_r
      df     <- r2df(delta, aoi)
      lim    <- max(abs(quantile(df$value, c(0.02,0.98), na.rm=TRUE)), 0.15)
      ttl <- if (pi==1) ssp_labels[si] else ""
      sub <- if (si==1) per_labels[pi] else ""
      p   <- ggplot() +
             geom_raster(data=df, aes(x=x, y=y, fill=value)) +
             aoi_layer() +
             change_scale(round(lim+0.05, 1)) +
             labs(title=ttl, subtitle=sub) +
             coord_sf(crs=4326) + map_theme +
             theme(plot.title=element_text(colour=SSP_COLS[ssp_labels[si]]),
                   plot.subtitle=element_text(face="bold"))
      panels_C[[length(panels_C)+1]] <- p
    }
  }
  # 3 rows (periods) x 4 cols (SSPs)
  fig_C <- wrap_plots(panels_C, ncol=4, nrow=3)
  fig_C <- fig_C + pub_annotation(
    title    = "Projected Change in Habitat Suitability vs. Present-Day Baseline",
    subtitle = expression(paste(italic("Elephas maximus"), " | Bhutan | Ensemble mean (all GCMs, GLM+RF+BRT) | Blue = loss; Red = gain")),
    caption  = paste0("Delta = future ensemble mean minus present ensemble  |  GCMs: ", gcm_label_str)
  )
  ggsave(file.path(OUT_DIR, "figpub_C_habitat_change.png"),
         fig_C, width=18, height=14, dpi=300, limitsize=FALSE)
  cat("+ Fig C saved\n")
}

# ==============================================================
# FIG D — Suitable area summary chart
# % area > 0.5 for all GCMs x SSPs x Periods
# ==============================================================
cat("Building Fig D: Suitable area summary...\n")
all_files <- list.files(FUT_DIR, pattern="suitability_future_.*_(glm|rf|brt)\\.tif$", full.names=TRUE)
area_rows <- list()
for (f in all_files) {
  bn   <- tools::file_path_sans_ext(basename(f))
  pts  <- strsplit(bn,"_")[[1]]
  ssp_i <- grep("^ssp[0-9]+$", pts)
  if (length(ssp_i)==0) next
  gcm_str  <- paste(toupper(pts[3:(ssp_i-1)]), collapse="-")
  ssp_v    <- pts[ssp_i]
  per_v    <- paste(pts[(ssp_i+1):(ssp_i+2)], collapse="-")
  algo     <- pts[length(pts)]
  r        <- tryCatch(rast(f), error=function(e) NULL)
  if (is.null(r)) next
  vals <- if (!is.null(aoi)) {
    av <- vect(st_transform(aoi, crs(r)))
    tryCatch(values(mask(crop(r,av),av), na.rm=TRUE), error=function(e) values(r,na.rm=TRUE))
  } else values(r, na.rm=TRUE)
  area_rows[[length(area_rows)+1]] <- data.frame(
    gcm=gcm_str, ssp=ssp_v, period=per_v, algo=algo,
    pct_suit=mean(vals >= 0.5, na.rm=TRUE)*100,
    mean_suit=mean(vals, na.rm=TRUE),
    stringsAsFactors=FALSE
  )
}

if (length(area_rows) > 0) {
  area_df <- do.call(rbind, area_rows)
  area_df$ssp_label <- factor(area_df$ssp,
    levels=c("ssp126","ssp245","ssp370","ssp585"),
    labels=c("SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5"))
  area_df$period_label <- factor(area_df$period,
    levels=c("2021-2050","2051-2080","2071-2100"))
  area_df$algo_label <- factor(area_df$algo,
    levels=c("glm","rf","brt"),
    labels=c("GLM","Random Forest","BRT"))

  # Add present baseline
  if (file.exists(present_file)) {
    pr   <- rast(present_file)
    pv   <- if (!is.null(aoi)) {
      av2 <- vect(st_transform(aoi, crs(pr)))
      tryCatch(values(mask(crop(pr,av2),av2),na.rm=TRUE), error=function(e) values(pr,na.rm=TRUE))
    } else values(pr, na.rm=TRUE)
    pres_pct <- mean(pv >= 0.5, na.rm=TRUE)*100
  } else pres_pct <- NA

  fig_D1 <- ggplot(area_df, aes(x=period_label, y=pct_suit,
                                 colour=ssp_label, group=interaction(ssp_label,gcm,algo))) +
    geom_line(aes(linetype=gcm), linewidth=0.8, alpha=0.7) +
    geom_point(aes(shape=gcm), size=2.5, alpha=0.9) +
    { if (!is.na(pres_pct)) geom_hline(yintercept=pres_pct, linetype="dashed",
                                        colour="grey30", linewidth=0.7) } +
    { if (!is.na(pres_pct)) annotate("text", x=0.65, y=pres_pct+0.8,
                                      label="Present", colour="grey30", size=3, hjust=0) } +
    scale_colour_manual(values=SSP_COLS, name="Scenario") +
    scale_linetype_manual(
      values=setNames(c("solid","dashed","dotted","twodash","longdash","dotdash","dashed","solid"),
                      unique(area_df$gcm)), name="GCM") +
    scale_shape_manual(
      values=setNames(c(16,17,15,18,8,4,3,1), unique(area_df$gcm)),
      name="GCM") +
    facet_wrap(~algo_label, ncol=3) +
    labs(x="Time Period", y="Suitable area (% of Bhutan)") +
    theme_bw() +
    theme(
      strip.background = element_rect(fill="#2c3e50"),
      strip.text = element_text(colour="white", face="bold", size=10),
      axis.text.x = element_text(angle=25, hjust=1, size=9),
      plot.title = element_text(size=11, face="bold"),
      legend.position = "right"
    )

  fig_D2 <- ggplot(area_df, aes(x=ssp_label, y=pct_suit, fill=ssp_label)) +
    geom_violin(alpha=0.4, scale="width") +
    geom_boxplot(width=0.25, outlier.shape=21, outlier.size=1.5, linewidth=0.5) +
    geom_jitter(width=0.08, size=1.8, alpha=0.7, colour="grey20") +
    { if (!is.na(pres_pct)) geom_hline(yintercept=pres_pct, linetype="dashed",
                                        colour="grey30", linewidth=0.8) } +
    scale_fill_manual(values=SSP_COLS, guide="none") +
    facet_wrap(~period_label, ncol=3) +
    labs(x="Emission Scenario", y="Suitable area (% of Bhutan)") +
    theme_bw() +
    theme(
      strip.background = element_rect(fill="#2c3e50"),
      strip.text = element_text(colour="white", face="bold", size=10),
      axis.text.x = element_text(angle=20, hjust=1, size=9)
    )

  fig_D <- fig_D1 / fig_D2 + plot_layout(heights=c(1,1)) +
    pub_annotation(
      title    = expression(paste("Projected Change in ", italic("Elephas maximus"), " Suitable Habitat — Bhutan")),
      subtitle = "Top: trajectory per algorithm/GCM  |  Bottom: distribution across GCMs + algorithms per period",
      caption  = paste0("Suitable habitat: suitability \u2265 0.5  |  GCMs: ", gcm_label_str, "  |  Dashed = present baseline")
    )
  ggsave(file.path(OUT_DIR, "figpub_D_area_summary.png"), fig_D, width=15, height=16, dpi=300)
  write.csv(area_df, file.path(OUT_DIR, "suitable_area_all_scenarios.csv"), row.names=FALSE)
  cat("+ Fig D saved\n")
}

# ==============================================================
# FIG E — Algorithm comparison: 4 algos x 3 GCMs (SSP370, 2071-2100)
# ==============================================================
cat("Building Fig E: 4 algorithms x 3 GCMs comparison...\n")
ssp_e <- "ssp370"; per_e <- "2071_2100"
panels_E <- list()
for (gi in seq_along(gcms_list)) {
  for (ai in seq_along(algo_all)) {
    f <- file.path(FUT_DIR, sprintf("suitability_future_%s_%s_%s_%s.tif",
                                    gcms_list[gi], ssp_e, per_e, algo_all[ai]))
    if (!file.exists(f)) { panels_E[[length(panels_E)+1]] <- patchwork::plot_spacer(); next }
    r  <- tryCatch(rast(f), error=function(e) NULL)
    if (is.null(r)) { panels_E[[length(panels_E)+1]] <- patchwork::plot_spacer(); next }
    df <- r2df(r, aoi)
    ttl <- if (gi==1) algo_labels[ai] else ""
    sub <- if (ai==1) gcm_labels[gi] else ""
    p   <- one_map(df, ttl) +
           labs(subtitle=sub) +
           theme(plot.subtitle=element_text(size=8,face="bold",colour="#2c3e50",hjust=0))
    panels_E[[length(panels_E)+1]] <- p
  }
}
# n_gcms rows x 4 cols (algorithms)
fig_E <- wrap_plots(panels_E, ncol=4, nrow=length(gcms_list)) &
  suit_scale()
fig_E <- fig_E + pub_annotation(
  title    = "Algorithm Comparison across GCMs — SSP3-7.0 | 2071-2100",
  subtitle = expression(paste(italic("Elephas maximus"), " | Bhutan | Rows = GCM; Columns = modelling algorithm")),
  caption  = "CRS: EPSG:32645 (WGS 84 / UTM Zone 45N)"
)
ggsave(file.path(OUT_DIR, "figpub_E_algorithm_gcm_comparison.png"),
       fig_E, width=18, height=14, dpi=300, limitsize=FALSE)
cat("+ Fig E saved\n")

# ==============================================================
# FIG F — Model Validation: ROC + metrics heatmap
# ==============================================================
cat("Building Fig F: Model validation panel...\n")
roc_file  <- file.path(RUN_DIR, "02_models/roc_curve_data.csv")
eval_file <- file.path(RUN_DIR, "02_models/evaluation_all.csv")
if (file.exists(roc_file) && file.exists(eval_file)) {
  roc_df  <- read.csv(roc_file)
  eval_df <- read.csv(eval_file)
  eval_df  <- eval_df[eval_df$algorithm != "ensemble",]
  roc_df$fpr <- 1 - roc_df$specificity
  auc_lut    <- setNames(round(eval_df$auc_mean,3), eval_df$algorithm)

  fig_F1 <- ggplot(roc_df, aes(x=fpr, y=sensitivity, colour=algorithm)) +
    geom_abline(slope=1, intercept=0, linetype="dashed", colour="grey60", linewidth=0.6) +
    geom_ribbon(aes(ymin=0, ymax=sensitivity, fill=algorithm), alpha=0.07) +
    geom_line(linewidth=1.2) +
    scale_colour_manual(values=ALGO_COLS, guide="none") +
    scale_fill_manual(values=ALGO_COLS, guide="none") +
    annotate("text",
      x=rep(0.52,4), y=seq(0.45,0.15,length.out=4),
      label=sprintf("%s  AUC = %.3f", toupper(names(auc_lut)), auc_lut),
      hjust=0, size=3.8, fontface="bold",
      colour=ALGO_COLS[names(auc_lut)]) +
    labs(title="ROC Curves (5-fold spatial cross-validation)",
         x="False Positive Rate (1 - Specificity)", y="True Positive Rate (Sensitivity)") +
    theme_bw() +
    theme(plot.title=element_text(size=11,face="bold",hjust=0.5),
          axis.title=element_text(size=10), axis.text=element_text(size=9))

  metric_long <- do.call(rbind, lapply(
    c("auc_mean","tss_mean","boyce","brier","calibration_slope","moran_i"), function(m) {
      if (!m %in% names(eval_df)) return(NULL)
      data.frame(Algorithm=toupper(eval_df$algorithm), Metric=m,
                 Value=eval_df[[m]], stringsAsFactors=FALSE)
  }))
  metric_long <- metric_long[!is.na(metric_long$Value),]
  metric_long$Metric <- factor(metric_long$Metric,
    levels=c("auc_mean","tss_mean","boyce","brier","calibration_slope","moran_i"),
    labels=c("AUC","TSS","Boyce Index","Brier Score","Calibration Slope","Moran's I"))

  fig_F2 <- ggplot(metric_long, aes(x=Algorithm, y=Value, fill=Algorithm)) +
    geom_col(width=0.65, show.legend=FALSE) +
    geom_text(aes(label=sprintf("%.3f",Value)), vjust=-0.35, size=3.2, fontface="bold") +
    scale_fill_manual(values=c(BRT=ALGO_COLS["brt"],GLM=ALGO_COLS["glm"],
                                MAXENT=ALGO_COLS["maxent"],RF=ALGO_COLS["rf"])) +
    facet_wrap(~Metric, scales="free_y", ncol=3) +
    labs(x=NULL, y="Value") +
    theme_bw() +
    theme(
      strip.background=element_rect(fill="#2c3e50"),
      strip.text=element_text(colour="white",face="bold",size=9),
      axis.text.x=element_text(size=9,face="bold"),
      plot.margin=margin(4,4,4,4)
    )

  fig_F <- fig_F1 / fig_F2 + plot_layout(heights=c(2,1.5)) +
    pub_annotation(
      title   = expression(paste(italic("Elephas maximus"), " SDM Performance — Bhutan")),
      subtitle= "Spatial cross-validation metrics (out-of-fold holdout predictions, 5 folds)",
      caption = "Moran's I < 0.3 indicates acceptable residual spatial autocorrelation  |  Boyce Index > 0.8 = excellent discrimination"
    )
  ggsave(file.path(OUT_DIR, "figpub_F_model_validation.png"),
         fig_F, width=14, height=14, dpi=300)
  cat("+ Fig F saved\n")
}

cat("\n=== All publication figures complete ===\n")
cat("Output:", OUT_DIR, "\n")
