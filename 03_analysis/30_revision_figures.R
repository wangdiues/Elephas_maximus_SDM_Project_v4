#!/usr/bin/env Rscript
# 30_revision_figures.R — corrected human-pressure outputs and relabeled figures for the
# JWM revision. Rebuilds exposure from the corrected building layer, tabulates present and
# future exposure, regenerates Figures 3 and 7, and independently re-derives the corrected
# human-pressure results with polygon overlap of buildings. Never writes to the accepted run.
# Usage (PowerShell, repo root): Rscript 03_analysis/30_revision_figures.R 04_outputs/runs/RUN_<id>

suppressPackageStartupMessages({ library(terra); library(sf); library(ggplot2); library(yaml) })
source(file.path("03_analysis", "00_publication_helpers.R"))
run <- normalizePath(commandArgs(trailingOnly = TRUE)[[1]], winslash = "/", mustWork = TRUE)
d <- function(...) file.path(run, ...)
cfg <- read_yaml("00_governance/config.yaml")
out <- Sys.getenv("REV_OUT", file.path("04_outputs", "revision_jwm")); tb <- file.path(out, "tables"); fig <- file.path(out, "figures"); cache <- file.path(out, "cache")
jwm_fig <- file.path(Sys.getenv("JWM_OUT", file.path("05_manuscripts", "Journal of Wildlife Management")), "figures")

pres <- rast(d("03_present_suitability", "suitability_present_ensemble.tif"))
nr <- function(r) { v <- values(r, na.rm = TRUE); q <- quantile(v, c(0.02, 0.98)); clamp((r - q[1]) / (q[2] - q[1]), 0, 1, values = TRUE) }
fp <- resample(rast(d("02_data_intermediate", "human_footprint_harmonized.tif")), pres, method = "bilinear")
b <- rast(file.path(cache, "dist_to_settlements_touches.tif"))
if (!compareGeom(b, pres, stopOnError = FALSE)) b <- project(b, pres, method = "bilinear")
hp <- nr(nr(fp) + nr(1 / (1 + b / 1000)))
ex <- sqrt(nr(pres) * hp)
writeRaster(ex, file.path(out, "interaction_exposure_present_corrected.tif"), overwrite = TRUE)
writeRaster(hp, file.path(out, "human_pressure_corrected.tif"), overwrite = TRUE)

cells <- which(!is.na(values(pres, mat = FALSE)))
ssp <- c(ssp126 = "SSP1-2.6", ssp245 = "SSP2-4.5", ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5")
per <- c("2021_2050", "2051_2080", "2071_2100")
rows <- list(data.frame(pathway = "Present", period = "1986-2015 climate", mean_exposure = mean(values(ex, mat = FALSE)[cells], na.rm = TRUE)))
for (s in names(ssp)) for (p in per) {
  r <- rast(d("04_future_projections", sprintf("future_gcm_ensemble_%s_%s.tif", s, p)))
  if (!compareGeom(r, pres, stopOnError = FALSE)) r <- project(r, pres, method = "bilinear")
  rows[[length(rows) + 1]] <- data.frame(pathway = ssp[[s]], period = sub("_", "-", p), mean_exposure = mean(values(sqrt(nr(r) * hp), mat = FALSE)[cells], na.rm = TRUE))
}
write.csv(do.call(rbind, rows), file.path(tb, "exposure_by_scenario_corrected.csv"), row.names = FALSE)

# Figure 7, corrected exposure
xy <- as.data.frame(ex, xy = TRUE, na.rm = TRUE); names(xy)[3] <- "exposure"
aoi <- st_transform(st_read(cfg$vectors$aoi_bhutan, quiet = TRUE), crs(ex))
pa <- st_transform(st_make_valid(st_read(cfg$vectors$protected_areas, quiet = TRUE)), crs(ex))
dz <- st_read(file.path(cfg$vectors$root, "Dzongkhag Boundary", "Dzongkhag Boundary.shp"), quiet = TRUE); st_crs(dz) <- 3857L
dz <- st_transform(st_make_valid(dz), crs(ex))
hw <- read.csv(file.path("01_data_raw", "04_conflicts", "HWC_Species_Elephant_clean.csv"), stringsAsFactors = FALSE)
hw <- hw[is.finite(hw$longitude) & is.finite(hw$latitude) & hw$longitude != 0, ]
hw <- st_transform(st_as_sf(hw, coords = c("longitude", "latitude"), crs = 4326), crs(ex))
hw <- hw[lengths(st_intersects(hw, st_union(aoi))) > 0, ]
p7 <- ggplot() + geom_raster(data = xy, aes(x = x, y = y, fill = exposure)) +
  geom_sf(data = dz, fill = NA, colour = "grey45", linewidth = 0.15) + geom_sf(data = pa, fill = NA, colour = "#009E73", linewidth = 0.3) +
  geom_sf(data = aoi, fill = NA, colour = "#111111", linewidth = 0.4) +
  geom_sf(data = hw, shape = 21, size = 1.1, stroke = 0.25, fill = "white", colour = "black") +
  scale_fill_viridis_c(option = "B", limits = c(0, 1), direction = -1, name = "Interaction\nexposure index") +
  coord_sf(crs = st_crs(aoi), expand = FALSE) + labs(x = NULL, y = NULL) + publication_theme(base_size = 9) +
  theme(axis.text = element_blank(), panel.grid = element_blank(), legend.position = "right")
save_publication_figure(p7, file.path(fig, "Figure_7_interaction_exposure"), width_mm = 178, height_mm = 110, line_art_dpi = 600)

# Figure 3, relabeled: predictions are recorded presence, not detectability
long <- read.csv(file.path(jwm_fig, "Figure_3_source_data.csv"), stringsAsFactors = FALSE)
long$predictor <- factor(long$predictor, levels = unique(long$predictor))
long$model <- factor(long$model, levels = c("Generalized linear model", "Random forest", "Boosted regression trees", "Ensemble"))
p3 <- ggplot(long, aes(x, y, colour = model, linewidth = model)) + geom_line() +
  facet_wrap(~predictor, scales = "free_x", ncol = 3, strip.position = "bottom") +
  scale_colour_manual(values = c("#0072B2", "#009E73", "#E69F00", "black"), name = NULL) +
  scale_linewidth_manual(values = c(0.45, 0.45, 0.45, 1), name = NULL) + scale_y_continuous(limits = c(0, NA)) +
  labs(x = NULL, y = "Mean predicted probability of recorded presence") + publication_theme(base_size = 8.5) +
  theme(strip.placement = "outside", strip.text = element_text(size = 8), legend.position = "bottom")
save_publication_figure(p3, file.path(fig, "Figure_3_response_curves"), width_mm = 178, height_mm = 115, line_art_dpi = 600)

# Independent check: building presence from footprint centroids instead of touched cells
bv <- vect(d("02_data_intermediate", "building_footprints_harmonized.gpkg"))
bc <- distance(rasterize(centroids(bv), pres, field = 1, background = NA)) / 1000
hp2 <- nr(nr(fp) + nr(1 / (1 + bc)))
S <- readRDS(file.path(cache, "S.rds")); st <- readRDS(file.path(cache, "static.rds"))
ev <- read.csv(d("02_models", "evaluation_all.csv")); thr <- ev$threshold[ev$algorithm == "ensemble"][1]
pv <- values(pres, mat = FALSE)[st$cells]; core <- pv >= thr & rowSums(S >= thr, na.rm = TRUE) == 60
h1 <- values(hp, mat = FALSE)[st$cells]; h2 <- values(hp2, mat = FALSE)[st$cells]
t1 <- h1 >= quantile(h1, 0.75, na.rm = TRUE); t2 <- h2 >= quantile(h2, 0.75, na.rm = TRUE)
write.csv(data.frame(check = c("spearman touched vs centroid human pressure", "core top quarter touched pct", "core top quarter centroid pct", "habitat top quarter touched pct", "habitat top quarter centroid pct"),
  value = round(c(cor(h1, h2, use = "complete", method = "spearman"), 100 * mean(t1[core], na.rm = TRUE), 100 * mean(t2[core], na.rm = TRUE), 100 * mean(t1[pv >= thr], na.rm = TRUE), 100 * mean(t2[pv >= thr], na.rm = TRUE)), 3)),
  file.path(tb, "human_pressure_method_check.csv"), row.names = FALSE)
pav <- readRDS(file.path(cache, "stage2_for_variants.rds"))$pav
tt <- do.call(rbind, lapply(c(0.10, thr, 0.15, 0.20), function(x) {
  cr <- pv >= x & rowSums(S >= x, na.rm = TRUE) == 60
  data.frame(threshold = x, core_top25_km2 = sum(cr & t1, na.rm = TRUE) * prod(res(pres)) / 1e6, core_outside_top25_km2 = sum(cr & !pav & t1, na.rm = TRUE) * prod(res(pres)) / 1e6)
}))
write.csv(tt, file.path(tb, "threshold_top25_corrected.csv"), row.names = FALSE)
cat("Figures complete\n")
