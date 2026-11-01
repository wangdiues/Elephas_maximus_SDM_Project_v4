#!/usr/bin/env Rscript
# regen_E05_figures.R — Regenerate figure_E05_pa_overlay.png and
# figure_E05_pa_overlay_labelled.png from actual analysis rasters.
# Usage: Rscript 03_analysis/regen_E05_figures.R [run_dir]

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(ggplot2)
})

has_repel    <- requireNamespace("ggrepel",   quietly = TRUE)
has_ggspatial <- requireNamespace("ggspatial", quietly = TRUE)
has_patchwork <- requireNamespace("patchwork", quietly = TRUE)

args    <- commandArgs(trailingOnly = TRUE)
run_dir <- if (length(args) >= 1) normalizePath(args[1], mustWork = TRUE) else {
  # Auto-detect latest run
  repo_root <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), ".."), mustWork = FALSE)
  runs <- list.dirs(file.path(repo_root, "04_outputs", "runs"), recursive = FALSE, full.names = TRUE)
  if (length(runs) == 0) stop("No run directories found")
  normalizePath(tail(sort(runs), 1), mustWork = TRUE)
}

repo_root <- normalizePath(file.path(run_dir, "..", "..", ".."), mustWork = FALSE)
fig_dir   <- file.path(run_dir, "08_figures_tables")
pres_dir  <- file.path(run_dir, "03_present_suitability")

cat(sprintf("Run dir  : %s\n", run_dir))
cat(sprintf("Fig dir  : %s\n", fig_dir))
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ── Paths ─────────────────────────────────────────────────────────────────────
pa_shp  <- file.path(repo_root, "01_data_raw", "03_vector", "shapefiles",
                     "PA_Bhutan", "PA_Bnd_Final_20230316.shp")
aoi_shp <- file.path(repo_root, "01_data_raw", "03_vector", "shapefiles",
                     "Bhutan", "bhutan.shp")
dzong_shp <- file.path(repo_root, "01_data_raw", "03_vector", "shapefiles",
                       "Dzongkhag Boundary", "Dzongkhag Boundary.shp")
ens_tif <- file.path(pres_dir, "suitability_present_ensemble.tif")

for (p in c(pa_shp, aoi_shp, ens_tif)) {
  if (!file.exists(p)) stop("Required file not found: ", p)
}

# ── Threshold from evaluation_all.csv ─────────────────────────────────────────
threshold <- 0.5
eval_csv  <- file.path(run_dir, "02_models", "evaluation_all.csv")
if (file.exists(eval_csv)) {
  ev <- tryCatch(read.csv(eval_csv), error = function(e) NULL)
  if (!is.null(ev) && "threshold" %in% names(ev)) {
    tv <- mean(ev$threshold, na.rm = TRUE)
    if (is.finite(tv)) threshold <- tv
  }
}
cat(sprintf("Threshold: %.4f\n", threshold))

# ── Load raster ───────────────────────────────────────────────────────────────
suit <- rast(ens_tif)

# ── Load AOI ──────────────────────────────────────────────────────────────────
aoi_sf     <- st_make_valid(st_read(aoi_shp, quiet = TRUE))
aoi_sf_utm <- st_transform(aoi_sf, 32645L)
aoi_v      <- vect(aoi_sf_utm)

# ── Load PA shapefile ──────────────────────────────────────────────────────────
pa_sf     <- st_make_valid(st_read(pa_shp, quiet = TRUE))
pa_sf_utm <- st_transform(pa_sf, 32645L)

# Detect park-type field (used to split PAs vs biological corridors)
park_field <- if ("park" %in% names(pa_sf_utm)) "park" else names(pa_sf_utm)[1]
name_field <- if ("PA_name" %in% names(pa_sf_utm)) "PA_name" else
              if ("Name"    %in% names(pa_sf_utm)) "Name" else names(pa_sf_utm)[2]
cat(sprintf("PA fields: park='%s', name='%s'\n", park_field, name_field))

pa_only <- pa_sf_utm[!grepl("^Biological Corridor", pa_sf_utm[[park_field]], ignore.case = FALSE), ]
bc_only <- pa_sf_utm[ grepl("^Biological Corridor", pa_sf_utm[[park_field]], ignore.case = FALSE), ]
cat(sprintf("PAs: %d national parks/sanctuaries, %d biological corridors\n",
            nrow(pa_only), nrow(bc_only)))

# ── PA statistics ─────────────────────────────────────────────────────────────
pa_v    <- vect(pa_sf_utm)
pa_mask <- rasterize(pa_v, suit, field = 1, background = 0)
inside  <- mask(suit, pa_mask, maskvalues = 0)
outside <- mask(suit, pa_mask, maskvalues = 1)

total_valid <- sum(!is.na(values(suit)))
pct_in   <- round(100 * sum(values(inside,  na.rm = TRUE) >= threshold) / total_valid, 1)
pct_out  <- round(100 * sum(values(outside, na.rm = TRUE) >= threshold) / total_valid, 1)
pct_all  <- round(100 * sum(values(suit,    na.rm = TRUE) >= threshold) / total_valid, 1)
cat(sprintf("Inside PAs:  %.1f%% suitable\n", pct_in))
cat(sprintf("Outside PAs: %.1f%% suitable\n", pct_out))
cat(sprintf("All Bhutan:  %.1f%% suitable\n", pct_all))

# Write summary CSV
pa_summary <- data.frame(
  zone         = c("Inside PAs", "Outside PAs", "All Bhutan"),
  pct_suitable = c(pct_in, pct_out, pct_all),
  threshold    = threshold
)
write.csv(pa_summary, file.path(fig_dir, "table_E05_pa_coverage.csv"), row.names = FALSE)

# ── Raster → data frame ───────────────────────────────────────────────────────
r_trim <- tryCatch(terra::trim(suit), error = function(e) suit)
e      <- as.vector(ext(r_trim))
pad    <- 0.04
bbox   <- list(xlim = c(e[1] - (e[2]-e[1])*pad, e[2] + (e[2]-e[1])*pad),
               ylim = c(e[3] - (e[4]-e[3])*pad, e[4] + (e[4]-e[3])*pad))

suit_crop <- tryCatch(mask(crop(suit, aoi_v), aoi_v), error = function(e) suit)
df_suit   <- as.data.frame(suit_crop, xy = TRUE, na.rm = TRUE)
names(df_suit)[3] <- "value"

# ── Shared theme & scale ──────────────────────────────────────────────────────
map_theme <- function() {
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "#d6e8f5"),
    panel.grid.major = element_line(colour = "white", linewidth = 0.3),
    plot.title       = element_text(size = 13, face = "bold"),
    plot.subtitle    = element_text(size = 9,  colour = "grey40"),
    axis.title       = element_blank(),
    legend.title     = element_text(size = 9,  face = "bold"),
    legend.text      = element_text(size = 8),
    plot.caption     = element_text(size = 7,  colour = "grey50")
  )
}

suit_scale <- function(name = "Suitability") {
  scale_fill_gradientn(
    colours  = c("#f7f7f7","#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),
    values   = c(0, 0.05, 0.2, 0.4, 0.65, 1),
    limits   = c(0, 1), na.value = "grey88",
    name     = name,
    guide    = guide_colorbar(barwidth = unit(0.45, "cm"), barheight = unit(5, "cm")))
}

add_cartographic <- function(p) {
  if (has_ggspatial)
    p <- p +
      ggspatial::annotation_scale(location = "bl", width_hint = 0.25,
                                  height = unit(0.15, "cm")) +
      ggspatial::annotation_north_arrow(
        location = "tl", which_north = "true",
        style    = ggspatial::north_arrow_fancy_orienteering(text_size = 7),
        height   = unit(1.0, "cm"), width = unit(1.0, "cm"))
  p + labs(caption = "CRS: EPSG:32645 (WGS 84 / UTM Zone 45N)")
}

# PA label layer using ggrepel or fallback text
pa_label_layer <- function(pa_sf, col, size, colour, fontface) {
  if (nrow(pa_sf) == 0) return(NULL)
  ctr    <- suppressWarnings(st_centroid(pa_sf))
  coords <- sf::st_coordinates(ctr)
  df_lbl <- data.frame(x = coords[,1], y = coords[,2],
                        label = pa_sf[[col]], stringsAsFactors = FALSE)
  if (has_repel)
    ggrepel::geom_label_repel(
      data = df_lbl, aes(x = x, y = y, label = label), inherit.aes = FALSE,
      size = size, colour = colour, fill = scales::alpha("white", 0.78), fontface = fontface,
      label.size = 0.18, label.padding = unit(0.10, "lines"),
      min.segment.length = 0.25, segment.colour = colour,
      segment.size = 0.3, max.overlaps = 50, seed = 42)
  else
    geom_text(data = df_lbl, aes(x = x, y = y, label = label),
              inherit.aes = FALSE, size = size, colour = colour, fontface = fontface)
}

# ── Base map layer list ───────────────────────────────────────────────────────
aoi_layer <- geom_sf(data = aoi_sf_utm, fill = NA, color = "black", linewidth = 0.55,
                     inherit.aes = FALSE)
bc_layer  <- if (nrow(bc_only) > 0)
  geom_sf(data = bc_only, fill = NA, color = "#4dac26", linewidth = 0.5,
          linetype = "dashed", inherit.aes = FALSE) else NULL
pa_layer  <- geom_sf(data = pa_only, fill = NA, color = "#1b7837", linewidth = 0.7,
                     inherit.aes = FALSE)

# ── Build base map ────────────────────────────────────────────────────────────
p_map_base <- ggplot() +
  geom_raster(data = df_suit, aes(x = x, y = y, fill = value)) +
  suit_scale() +
  aoi_layer + bc_layer + pa_layer +
  pa_label_layer(pa_only, name_field, 2.5, "#1b7837", "bold") +
  pa_label_layer(bc_only, name_field, 2.2, "#4dac26", "italic") +
  annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.5,
           label = sprintf("Inside PAs:  %.1f%% suitable\nOutside PAs: %.1f%% suitable",
                           pct_in, pct_out),
           size = 3, fontface = "bold", colour = "#1b7837") +
  labs(title    = "Present Habitat Suitability with Protected Area Boundaries",
       subtitle = "Solid green = national parks/sanctuaries | Dashed green = biological corridors",
       x = NULL, y = NULL) +
  map_theme() +
  coord_sf(xlim = bbox$xlim, ylim = bbox$ylim, expand = FALSE, crs = st_crs(32645L))

p_map_base <- add_cartographic(p_map_base)

# ── Figure 1: PA overlay with bar chart (patchwork) ──────────────────────────
cat("\nGenerating figure_E05_pa_overlay.png ...\n")
{
  area_km2_in  <- round(sum(values(inside,  na.rm = TRUE) >= threshold) * prod(res(suit)) / 1e6)
  area_km2_out <- round(sum(values(outside, na.rm = TRUE) >= threshold) * prod(res(suit)) / 1e6)
  bar_df <- data.frame(
    zone = factor(c("Inside PAs", "Outside PAs"), levels = c("Inside PAs", "Outside PAs")),
    pct  = c(pct_in, pct_out),
    area = c(area_km2_in, area_km2_out)
  )
  p_bar <- ggplot(bar_df, aes(x = zone, y = pct, fill = zone)) +
    geom_col(width = 0.55, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.1f%%\n(%d km²)", pct, area)),
              vjust = -0.3, size = 4.0, fontface = "bold", lineheight = 0.9) +
    scale_fill_manual(values = c("Inside PAs" = "#2d6a4f", "Outside PAs" = "#e07b39")) +
    coord_cartesian(ylim = c(0, max(bar_df$pct) * 1.35)) +
    labs(title = "% Bhutan with Suitability ≥ Threshold",
         x = NULL, y = "% of Bhutan") +
    theme_bw() +
    theme(plot.title = element_text(face = "bold", size = 11))

  if (has_patchwork) {
    combined <- p_map_base + p_bar +
      patchwork::plot_layout(widths = c(3, 1)) +
      patchwork::plot_annotation(
        title    = "Protected Area Coverage of Suitable Habitat",
        subtitle = sprintf("Threshold = %.3f | PA file: PA_Bnd_Final_20230316.shp", threshold))
    ggsave(file.path(fig_dir, "figure_E05_pa_overlay.png"), combined,
           width = 14, height = 8, dpi = 300, bg = "white")
    cat("  Saved figure_E05_pa_overlay.png\n")
  } else {
    ggsave(file.path(fig_dir, "figure_E05_pa_overlay.png"), p_map_base,
           width = 10, height = 8, dpi = 300, bg = "white")
    cat("  Saved figure_E05_pa_overlay.png (map only — patchwork not available)\n")
  }
}

# ── Figure 2: Labelled map only ───────────────────────────────────────────────
cat("\nGenerating figure_E05_pa_overlay_labelled.png ...\n")
{
  p_labelled <- ggplot() +
    geom_raster(data = df_suit, aes(x = x, y = y, fill = value)) +
    suit_scale() +
    aoi_layer + bc_layer + pa_layer +
    pa_label_layer(pa_only, name_field, 2.5, "#1b7837", "bold") +
    pa_label_layer(bc_only, name_field, 2.2, "#4dac26", "italic") +
    annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.5,
             label = sprintf("Inside PAs:  %.1f%% suitable\nOutside PAs: %.1f%% suitable",
                             pct_in, pct_out),
             size = 3, fontface = "bold", colour = "#1b7837") +
    labs(title    = "Present Habitat Suitability with Protected Area Boundaries",
         subtitle = "Green outlines = protected areas",
         x = NULL, y = NULL) +
    map_theme() +
    coord_sf(xlim = bbox$xlim, ylim = bbox$ylim, expand = FALSE, crs = st_crs(32645L))

  p_labelled <- add_cartographic(p_labelled)
  ggsave(file.path(fig_dir, "figure_E05_pa_overlay_labelled.png"), p_labelled,
         width = 14, height = 9, dpi = 350, bg = "white")
  cat("  Saved figure_E05_pa_overlay_labelled.png\n")
}

# ── Copy to canonical output location ─────────────────────────────────────────
canon_dir <- normalizePath(file.path(run_dir, "..", "..", "08_figures_tables"),
                           mustWork = FALSE)
if (dir.exists(canon_dir)) {
  for (f in c("figure_E05_pa_overlay.png",
              "figure_E05_pa_overlay_labelled.png",
              "table_E05_pa_coverage.csv")) {
    src <- file.path(fig_dir, f)
    dst <- file.path(canon_dir, f)
    if (file.exists(src)) {
      file.copy(src, dst, overwrite = TRUE)
      cat(sprintf("  Copied to canonical: %s\n", f))
    }
  }
}

cat("\nDone.\n")
