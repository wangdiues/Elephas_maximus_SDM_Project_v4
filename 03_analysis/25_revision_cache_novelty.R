#!/usr/bin/env Rscript
# 25_revision_cache_novelty.R — stage 1 of the JWM major revision.
# Builds cell-level caches for the modeled domain, confirms they reproduce the
# accepted rasters, and independently recomputes the climatic novelty score for all
# 60 projections. Never writes to the accepted run.
# Usage (PowerShell, repo root):
#   Rscript 03_analysis/25_revision_cache_novelty.R 04_outputs/runs/RUN_<id>

suppressPackageStartupMessages({ library(terra); library(sf); library(ranger); library(gbm) })
source(file.path("03_analysis", "00_spatial_alignment.R"))
run <- normalizePath(commandArgs(trailingOnly = TRUE)[[1]], winslash = "/", mustWork = TRUE)
d <- function(...) file.path(run, ...)
out <- file.path("04_outputs", "revision_jwm"); cache <- file.path(out, "cache")
dir.create(cache, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out, "tables"), showWarnings = FALSE)

pres <- rast(d("03_present_suitability", "suitability_present_ensemble.tif"))
template <- rast(d("02_data_intermediate", "template_grid.tif"))
pv <- values(pres, mat = FALSE); cells <- which(!is.na(pv))
manifest <- read.csv(d("01_processed_data", "predictor_manifest.csv"))
pred_cols <- manifest$predictor[manifest$selected]
clim_cols <- intersect(pred_cols, c("BIO05", "BIO14", "BIO15", "BIO18"))
method_of <- function(nm) if (grepl("landcover|class", tolower(nm))) "near" else "bilinear"

static <- lapply(seq_along(pred_cols), function(i) {
  p <- manifest$path[manifest$selected][i]
  r <- align_raster(rast(p)[[1]], template, method = method_of(pred_cols[i]), verbose = FALSE)
  values(r, mat = FALSE)[cells]
})
names(static) <- pred_cols
static <- as.data.frame(static)
saveRDS(list(static = static, cells = cells, pred_cols = pred_cols, clim_cols = clim_cols), file.path(cache, "static.rds"))
cat("Static cache:", nrow(static), "cells;", length(pred_cols), "predictors\n")

ff <- list.files(d("04_future_projections"), "^suitability_future_.*_ssp[0-9]{3}_[0-9]{4}_[0-9]{4}_ensemble\\.tif$", full.names = TRUE)
m <- regmatches(basename(ff), regexec("^suitability_future_(.*)_(ssp[0-9]{3})_([0-9]{4}_[0-9]{4})_ensemble", basename(ff)))
sc <- data.frame(gcm = sapply(m, `[`, 2), ssp = sapply(m, `[`, 3), period = sapply(m, `[`, 4), file = ff)
if (nrow(sc) != 60) stop("Expected 60 projections")

md <- read.csv(d("01_processed_data", "modeling_dataset.csv"))
sv <- project(vect(md, geom = c("longitude", "latitude"), crs = "EPSG:4326"), crs(pres))
cur <- rast(lapply(clim_cols, function(nm) align_raster(rast(manifest$path[manifest$predictor == nm])[[1]], template, method = "bilinear", verbose = FALSE)))
names(cur) <- clim_cols
ref <- as.matrix(extract(cur, sv, ID = FALSE)); ref <- ref[complete.cases(ref), , drop = FALSE]
rng <- data.frame(predictor = clim_cols, min = apply(ref, 2, min), max = apply(ref, 2, max), sd = apply(ref, 2, sd), n = nrow(ref))
write.csv(rng, file.path(out, "tables", "novelty_calibration_ranges.csv"), row.names = FALSE)
print(rng)

# Accepted-model checks use the same prediction route as the pipeline
gl <- readRDS(d("02_models", "model_glm.rds")); rf <- readRDS(d("02_models", "model_rf.rds")); br <- readRDS(d("02_models", "model_brt.rds"))
ens <- function(x) (as.numeric(predict(gl, newdata = x, type = "response")) + as.numeric(predict(rf, data = x)$predictions) +
                      as.numeric(predict(br$model, newdata = x, n.trees = br$best_iter, type = "response"))) / 3

S <- matrix(NA_real_, length(cells), 60); E <- matrix(FALSE, length(cells), 60)
nov_check <- vector("list", 60); lim <- vector("list", 60)
for (i in seq_len(60)) {
  s <- sc[i, ]
  fs <- vapply(clim_cols, function(nm) file.path("01_data_raw", "02_rasters", "future", s$gcm, s$period, s$ssp,
    sprintf("bhutan_cmip6_%s_%s_%s_%s_v1_0.tif", s$gcm, s$ssp, s$period, tolower(nm))), "")
  if (!all(file.exists(fs))) stop("Missing future layers: ", paste(fs[!file.exists(fs)], collapse = ", "))
  cl <- vapply(fs, function(f) values(align_raster(rast(f)[[1]], template, method = "bilinear", verbose = FALSE), mat = FALSE)[cells], numeric(length(cells)))
  colnames(cl) <- clim_cols
  saveRDS(cl, file.path(cache, sprintf("clim_%s_%s_%s.rds", s$gcm, s$ssp, s$period)))
  S[, i] <- values(rast(s$file), mat = FALSE)[cells]

  # Independent score: negative distance outside the calibration range, zero inside
  z <- vapply(clim_cols, function(nm) {
    v <- cl[, nm]; r <- rng[rng$predictor == nm, ]
    pmin((v - r$min) / r$sd, 0) + pmin((r$max - v) / r$sd, 0)
  }, numeric(length(cells)))
  mine <- apply(z, 1, min)
  acc <- values(rast(d("04_future_projections", sprintf("mess_%s_%s_%s.tif", s$gcm, s$ssp, s$period))), mat = FALSE)[cells]
  ex <- values(rast(d("04_future_projections", sprintf("extrapolation_%s_%s_%s.tif", s$gcm, s$ssp, s$period))), mat = FALSE)[cells]
  ok <- is.finite(mine) & is.finite(acc)
  E[, i] <- !is.na(ex) & ex == 1
  nov_check[[i]] <- data.frame(s[, 1:3], n = sum(ok), max_abs_diff = max(abs(mine[ok] - acc[ok])),
    binary_agree_pct = 100 * mean((mine[ok] < 0) == (ex[ok] == 1), na.rm = TRUE), min_score = min(acc[ok]), max_score = max(acc[ok]))
  lim[[i]] <- data.frame(s[, 1:3], predictor = clim_cols, above_max_n = colSums(sweep(cl, 2, rng$max, ">"), na.rm = TRUE),
                         below_min_n = colSums(sweep(cl, 2, rng$min, "<"), na.rm = TRUE))
  if (i %in% c(1, 30, 60)) {
    x <- static; x[clim_cols] <- cl[, clim_cols]
    ok2 <- complete.cases(x)
    p <- ens(x[ok2, , drop = FALSE])
    cat(sprintf("Reproduce %s %s %s: r=%.5f max|diff|=%.5f\n", s$gcm, s$ssp, s$period, cor(p, S[ok2, i]), max(abs(p - S[ok2, i]))))
    write.table(data.frame(s[, 1:3], r = cor(p, S[ok2, i]), max_abs_diff = max(abs(p - S[ok2, i]))),
                file.path(out, "tables", "reproduction_check.csv"), sep = ",", append = i != 1, col.names = i == 1, row.names = FALSE)
  }
  cat(i, "")
}
cat("\n")
saveRDS(sc, file.path(cache, "scenarios.rds")); saveRDS(S, file.path(cache, "S.rds")); saveRDS(E, file.path(cache, "E.rds"))
write.csv(do.call(rbind, nov_check), file.path(out, "tables", "novelty_recalculation_check.csv"), row.names = FALSE)
write.csv(do.call(rbind, lim), file.path(out, "tables", "novelty_by_predictor_cells.csv"), row.names = FALSE)
cat("Stage 1 complete\n")
