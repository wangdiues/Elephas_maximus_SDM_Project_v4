#!/usr/bin/env Rscript
# 18_manuscript_figure6.R — Figure 6 (potential interaction exposure) for the
# manuscript, from the exposure raster written by 17_manuscript_numbers.R.
# Usage: Rscript 03_analysis/18_manuscript_figure6.R 04_outputs/runs/RUN_<id> [out_dir]

suppressPackageStartupMessages({ library(terra); library(sf); library(ggplot2); library(yaml) })
source(file.path("03_analysis", "00_publication_helpers.R"))
args <- commandArgs(trailingOnly = TRUE)
run_dir <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
out_dir <- if (length(args) >= 2) args[[2]] else file.path("05_manuscripts", "figures")
cfg <- yaml::read_yaml("00_governance/config.yaml")

expo <- rast(file.path("05_manuscripts", "interaction_exposure_present.tif"))
xy <- as.data.frame(expo, xy = TRUE, na.rm = TRUE); names(xy)[3] <- "exposure"
aoi <- st_transform(st_read(cfg$vectors$aoi_bhutan, quiet = TRUE), crs(expo))
pa <- st_transform(st_make_valid(st_read(cfg$vectors$protected_areas, quiet = TRUE)), crs(expo))
dz <- st_transform(st_make_valid(st_read(file.path(cfg$vectors$root, "Dzongkhag Boundary", "Dzongkhag Boundary.shp"), quiet = TRUE)), crs(expo))
hw <- read.csv(file.path("01_data_raw", "04_conflicts", "HWC_Species_Elephant_clean.csv"), stringsAsFactors = FALSE)
hw <- hw[is.finite(hw$longitude) & is.finite(hw$latitude) & hw$longitude != 0, ]
hw <- st_transform(st_as_sf(hw, coords = c("longitude", "latitude"), crs = 4326), crs(expo))
hw <- hw[lengths(st_intersects(hw, st_union(aoi))) > 0, ]

p <- ggplot() +
  geom_raster(data = xy, aes(x = x, y = y, fill = exposure)) +
  geom_sf(data = dz, fill = NA, colour = "grey45", linewidth = 0.15) +
  geom_sf(data = pa, fill = NA, colour = "#009E73", linewidth = 0.3) +
  geom_sf(data = aoi, fill = NA, colour = "#111111", linewidth = 0.4) +
  geom_sf(data = hw, shape = 21, size = 1.1, stroke = 0.25, fill = "white", colour = "black") +
  scale_fill_viridis_c(option = "B", limits = c(0, 1), direction = -1, name = "Interaction\nexposure index") +
  coord_sf(crs = st_crs(aoi), expand = FALSE) +
  labs(x = NULL, y = NULL) + publication_theme(base_size = 9) +
  theme(axis.text = element_blank(), panel.grid = element_blank(), legend.position = "right")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
save_publication_figure(p, file.path(out_dir, "main_figure_06_interaction_exposure"),
                        width_mm = 178, height_mm = 110, line_art_dpi = 600)
cat("Figure 6 written\n")
