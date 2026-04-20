#!/usr/bin/env Rscript
# generate_fig1_study_area.R
# Standalone script: Figure 1 — Study area map with spatial CV block fold layout
#
# Usage (PowerShell):
#   Rscript 03_analysis/generate_fig1_study_area.R <run_dir> [out_png]
#
# Defaults:
#   run_dir = most recent RUN_* in 04_outputs/runs/
#   out_png = <run_dir>/08_figures_tables/figure_01_study_area_cv.png
#
# Reads:
#   <run_dir>/01_processed_data/fold_assignments.csv
#   01_data_raw/03_vector/shapefiles/Bhutan/bhutan.shp
#   01_data_raw/03_vector/shapefiles/PA_Bhutan/PA_Bnd_Final_20230316.shp
# ============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(terra)
})

repo_root <- normalizePath(
  if (file.exists("00_governance/config.yaml")) "." else
  if (file.exists("../00_governance/config.yaml")) ".." else
  stop("Run from project root"), mustWork = FALSE)

args    <- commandArgs(trailingOnly = TRUE)

# ── Resolve run_dir ───────────────────────────────────────────────────────────
if (length(args) >= 1) {
  run_dir <- normalizePath(args[[1]], mustWork = TRUE)
} else {
  runs    <- list.dirs(file.path(repo_root, "04_outputs", "runs"),
                       full.names = TRUE, recursive = FALSE)
  runs    <- runs[grepl("^RUN_", basename(runs))]
  if (length(runs) == 0) stop("No RUN_* directories found in 04_outputs/runs/")
  run_dir <- runs[which.max(file.info(runs)$mtime)]
  cat(sprintf("Using run: %s\n", basename(run_dir)))
}

fig_dir <- file.path(run_dir, "08_figures_tables")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

out_png <- if (length(args) >= 2) args[[2]] else
  file.path(fig_dir, "figure_01_study_area_cv.png")

# ── Load fold assignments ─────────────────────────────────────────────────────
folds_csv <- file.path(run_dir, "01_processed_data", "fold_assignments.csv")
if (!file.exists(folds_csv)) stop("fold_assignments.csv not found: ", folds_csv)

folds_df <- read.csv(folds_csv, stringsAsFactors = FALSE)
# Columns: id, type, longitude, latitude, fold
folds_df$fold <- as.integer(folds_df$fold)

# ── Load boundaries ────────────────────────────────────────────────────────────
bhutan_shp <- file.path(repo_root, "01_data_raw", "03_vector", "shapefiles",
                         "Bhutan", "bhutan.shp")
pa_shp     <- file.path(repo_root, "01_data_raw", "03_vector", "shapefiles",
                         "PA_Bhutan", "PA_Bnd_Final_20230316.shp")

bhutan <- st_read(bhutan_shp, quiet = TRUE)
bhutan <- st_transform(bhutan, 32645L)

if (file.exists(pa_shp)) {
  pa <- st_read(pa_shp, quiet = TRUE)
  pa <- st_transform(pa, 32645L)
} else {
  pa <- NULL
  cat("  PA boundary not found; omitting PA layer.\n")
}

# ── Reconstruct 15 km block grid ──────────────────────────────────────────────
pts_sf <- st_as_sf(folds_df, coords = c("longitude", "latitude"),
                   crs = 4326L, remove = FALSE)
pts_utm <- st_transform(pts_sf, 32645L)
coords  <- st_coordinates(pts_utm)

bbox_pts <- list(
  xmin = min(coords[, 1]), xmax = max(coords[, 1]),
  ymin = min(coords[, 2]), ymax = max(coords[, 2])
)

block_size_m <- 15000  # 15 km in metres
n_bx <- max(1L, round((bbox_pts$xmax - bbox_pts$xmin) / block_size_m))
n_by <- max(1L, round((bbox_pts$ymax - bbox_pts$ymin) / block_size_m))

x_breaks <- seq(bbox_pts$xmin, bbox_pts$xmax, length.out = n_bx + 1L)
y_breaks <- seq(bbox_pts$ymin, bbox_pts$ymax, length.out = n_by + 1L)

# Assign points to blocks (replicate pipeline logic at seed 123458)
block_x <- findInterval(coords[, 1], x_breaks, all.inside = TRUE, rightmost.closed = TRUE)
block_y <- findInterval(coords[, 2], y_breaks, all.inside = TRUE, rightmost.closed = TRUE)
block_id <- (block_y - 1L) * n_bx + block_x

pts_utm$block_id <- block_id
pts_utm$fold     <- folds_df$fold

# Derive block-level fold by majority vote among presences; tie → smallest fold
block_folds <- pts_utm |>
  st_drop_geometry() |>
  group_by(block_id) |>
  summarise(
    fold = {
      tb <- sort(table(fold[type == "presence"]), decreasing = TRUE)
      if (length(tb) > 0) as.integer(names(tb)[1L])
      else as.integer(names(sort(table(fold), decreasing = TRUE))[1L])
    },
    .groups = "drop"
  )

# Build block polygon grid
grid_polys <- vector("list", n_bx * n_by)
k <- 1L
for (iy in seq_len(n_by)) {
  for (ix in seq_len(n_bx)) {
    bid <- (iy - 1L) * n_bx + ix
    bb <- c(x_breaks[ix], y_breaks[iy], x_breaks[ix + 1L], y_breaks[iy + 1L])
    grid_polys[[k]] <- st_sf(
      block_id = bid,
      geometry = st_sfc(st_polygon(list(matrix(
        c(bb[1], bb[2],
          bb[3], bb[2],
          bb[3], bb[4],
          bb[1], bb[4],
          bb[1], bb[2]),
        ncol = 2, byrow = TRUE))),
        crs = 32645L)
    )
    k <- k + 1L
  }
}
grid_sf <- do.call(rbind, grid_polys)
grid_sf <- left_join(grid_sf, block_folds, by = "block_id")
grid_sf$fold[is.na(grid_sf$fold)] <- 0L  # unoccupied blocks

# Clip grid to Bhutan boundary for clean display
grid_clipped <- tryCatch(
  st_intersection(grid_sf[!is.na(grid_sf$fold) & grid_sf$fold > 0, ], bhutan),
  error = function(e) grid_sf[!is.na(grid_sf$fold) & grid_sf$fold > 0, ]
)

# ── Prepare occurrence points ─────────────────────────────────────────────────
occ_sf <- pts_utm |>
  mutate(
    Point_type = ifelse(type == "presence", "Presence (n=252)", "Absence (n=837)"),
    Fold_label = paste0("Fold ", fold)
  )

fold_pal <- c(
  "1" = "#e41a1c",  # red
  "2" = "#377eb8",  # blue
  "3" = "#4daf4a",  # green
  "4" = "#ff7f00",  # orange
  "5" = "#984ea3"   # purple
)

# ── Build plot ────────────────────────────────────────────────────────────────
bhutan_extent <- st_bbox(bhutan)

p <- ggplot() +
  # Bhutan background
  geom_sf(data = bhutan, fill = "#f5f5f0", colour = "black", linewidth = 0.7) +
  # CV blocks (coloured by fold, semi-transparent)
  geom_sf(data = grid_clipped,
          aes(fill = factor(fold)),
          colour = "white", linewidth = 0.2, alpha = 0.40) +
  scale_fill_manual(
    values = fold_pal,
    name   = "Spatial CV fold\n(15 km blocks)",
    labels = paste0("Fold ", 1:5),
    na.value = "grey90"
  ) +
  # PA boundaries
  { if (!is.null(pa))
      geom_sf(data = pa, fill = NA, colour = "#2b7a2b", linewidth = 0.55,
              linetype = "dashed")
    else list() } +
  # Absence points (plotted first, under presences)
  geom_sf(data = occ_sf[occ_sf$type == "background", ],
          shape = 1, size = 1.2, colour = "grey40", stroke = 0.5, alpha = 0.55) +
  # Presence points (coloured by fold)
  geom_sf(data = occ_sf[occ_sf$type == "presence", ],
          aes(colour = factor(fold)),
          shape = 16, size = 2.0, alpha = 0.90) +
  scale_colour_manual(
    values = fold_pal,
    name   = "Presence fold",
    labels = paste0("Fold ", 1:5)
  ) +
  # Spatial extent: Bhutan + small buffer
  coord_sf(
    xlim   = c(bhutan_extent["xmin"] - 5000, bhutan_extent["xmax"] + 5000),
    ylim   = c(bhutan_extent["ymin"] - 5000, bhutan_extent["ymax"] + 5000),
    expand = FALSE
  ) +
  # Annotation: point counts
  annotate("label", x = bhutan_extent["xmax"] + 4000, y = bhutan_extent["ymin"] - 3000,
           label = paste0("Presences: 252\nAbsences: 837\nCV folds: 5 spatial blocks\nBlock size: 15 km"),
           hjust = 1, vjust = 0, size = 2.9, fill = "white", alpha = 0.9,
           linewidth = 0.3, label.padding = unit(0.3, "lines")) +
  labs(
    title    = expression(italic("Elephas maximus") ~ "SDM \u2014 Study area and spatial cross-validation design (Bhutan)"),
    subtitle = "Grey outline = national boundary | Dashed green = PA boundaries | Coloured blocks = 5-fold spatial CV assignment (15 km) | Circles = absence records",
    x = NULL, y = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 10.5),
    plot.subtitle    = element_text(size = 8.5, colour = "grey30"),
    legend.position  = "right",
    legend.box       = "vertical",
    legend.key.size  = unit(0.5, "cm"),
    legend.text      = element_text(size = 8.5),
    legend.title     = element_text(size = 9, face = "bold"),
    panel.grid.major = element_line(colour = "grey85"),
    axis.text        = element_text(size = 8),
    plot.margin      = margin(4, 4, 4, 4, "mm")
  ) +
  guides(
    fill   = guide_legend(order = 1, override.aes = list(alpha = 0.5)),
    colour = guide_legend(order = 2, override.aes = list(size = 3))
  )

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave(out_png, plot = p, width = 12, height = 7, dpi = 300, bg = "white")
cat(sprintf("Saved: %s\n", out_png))
