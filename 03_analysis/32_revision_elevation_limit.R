#!/usr/bin/env Rscript
# 32_revision_elevation_limit.R — evidence for the 3,000-m mapping limit.
# Describes the high-elevation cells classified suitable by the run without the limit,
# and the area each run leaves out of the modeled domain. Reads both runs; writes neither.
# Usage (PowerShell, repo root):
#   Rscript 03_analysis/32_revision_elevation_limit.R 04_outputs/runs/RUN_<limited> 04_outputs/runs/RUN_<unlimited>

suppressPackageStartupMessages({ library(terra); library(sf); library(yaml); library(ranger); library(gbm) })
a <- commandArgs(trailingOnly = TRUE)
run <- normalizePath(a[[1]], winslash = "/", mustWork = TRUE); old <- normalizePath(a[[2]], winslash = "/", mustWork = TRUE)
d <- function(...) file.path(run, ...); o <- function(...) file.path(old, ...)
cfg <- read_yaml("00_governance/config.yaml")
out <- Sys.getenv("REV_OUT", file.path("04_outputs", "revision_jwm", "corrected"))
old_cache <- Sys.getenv("REV_OLD_CACHE", file.path("04_outputs", "revision_jwm", "cache"))
led <- list(); add <- function(key, value, source = "") led[[length(led) + 1]] <<- data.frame(key = key, value = as.character(value), source = source)

pres <- rast(d("03_present_suitability", "suitability_present_ensemble.tif")); cell <- prod(res(pres)) / 1e6
pres0 <- rast(o("03_present_suitability", "suitability_present_ensemble.tif"))
el <- project(rast(d("02_data_intermediate", "elevation_harmonized.tif")), pres, method = "bilinear")
bhu <- st_transform(st_read(cfg$vectors$aoi_bhutan, quiet = TRUE), crs(pres))
inb <- values(rasterize(vect(bhu), pres, field = 1, background = 0), mat = FALSE) == 1
pv <- values(pres, mat = FALSE); pv0 <- values(pres0, mat = FALSE); ev <- values(el, mat = FALSE)

# Area excluded from each domain
gone <- inb & is.na(pv); nodata <- inb & is.na(pv0); limit <- inb & !is.na(pv0) & is.na(pv)
add("domain_excluded_total_km2", round(sum(gone) * cell), "Bhutan cells outside limited domain")
add("domain_excluded_nodata_km2", round(sum(nodata) * cell), "Bhutan cells without complete predictors")
add("domain_excluded_limit_km2", round(sum(limit) * cell), "cells removed by the limit")
add("domain_excluded_median_elev_m", round(median(ev[gone], na.rm = TRUE)), "elevation")
add("domain_excluded_below_2000_pct", round(100 * mean(ev[gone] < 2000, na.rm = TRUE), 1), "elevation")

# Cells classified suitable without the limit
thr <- read.csv(o("02_models", "evaluation_all.csv")); thr <- thr$threshold[thr$algorithm == "ensemble"][1]
st <- readRDS(file.path(old_cache, "static.rds")); cells <- st$cells; x <- st$static
S <- readRDS(file.path(old_cache, "S.rds"))
xy <- xyFromCell(pres0, cells); ll <- project(xy, crs(pres0), "EPSG:4326")
hab <- pv0[cells] >= thr; core <- hab & rowSums(S >= thr, na.rm = TRUE) == 60
hi <- hab & ev[cells] > 3000; hi[is.na(hi)] <- FALSE
north <- hab & ll[, 2] > 27.4
add("unlimited_above3000_habitat_km2", round(sum(hi) * cell), "accepted run"); add("unlimited_above3000_core_km2", round(sum(hi & core) * cell), "accepted run")
add("unlimited_north_habitat_km2", round(sum(north) * cell), "accepted run, north of 27.4 N"); add("unlimited_north_core_km2", round(sum(north & core) * cell), "accepted run, north of 27.4 N")
add("unlimited_north_median_elev_m", round(median(ev[cells][north], na.rm = TRUE)), "accepted run, north of 27.4 N")

gl <- readRDS(o("02_models", "model_glm.rds")); rf <- readRDS(o("02_models", "model_rf.rds")); br <- readRDS(o("02_models", "model_brt.rds"))
n <- which(north); n <- n[complete.cases(x[n, st$pred_cols])]; n <- n[seq(1, length(n), length.out = min(3000, length(n)))]
g <- predict(gl, newdata = x[n, ], type = "response"); r <- predict(rf, data = x[n, ], num.threads = 2)$predictions; b <- predict(br$model, newdata = x[n, ], n.trees = br$best_iter, type = "response")
add("unlimited_north_glm_median", round(median(g), 3), "accepted models"); add("unlimited_north_rf_median", round(median(r), 3), "accepted models"); add("unlimited_north_brt_median", round(median(b), 3), "accepted models")

# Same algorithm comparison for habitat still mapped between the highest detection and the limit
ms <- read.csv(d("01_processed_data", "modeling_dataset.csv"))
elst <- extract(rast(d("02_data_intermediate", "elevation_harmonized.tif")), project(vect(ms, geom = c("longitude", "latitude"), crs = "EPSG:4326"), crs(rast(d("02_data_intermediate", "elevation_harmonized.tif")))), ID = FALSE)[, 1]
add("stations_above_3000_n", sum(elst > 3000, na.rm = TRUE), "stations x elevation"); add("stations_above_3000_presences", sum(elst > 3000 & ms$response == 1, na.rm = TRUE), "stations x elevation")

write.csv(do.call(rbind, led), file.path(out, "revision_numbers_limit.csv"), row.names = FALSE)
print(do.call(rbind, led)[, 1:2], row.names = FALSE)
cat("Limit stage complete\n")
