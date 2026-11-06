#!/usr/bin/env Rscript
# 27_revision_variants.R — stage 3 of the JWM major revision.
# Refits the ensemble under alternative data or predictor sets, keeps the accepted folds,
# settings and seed, and carries each variant through all 60 projections.
# Usage (PowerShell, repo root):
#   Rscript 03_analysis/27_revision_variants.R 04_outputs/runs/RUN_<id>

suppressPackageStartupMessages({ library(terra); library(ranger); library(gbm) })
run <- normalizePath(commandArgs(trailingOnly = TRUE)[[1]], winslash = "/", mustWork = TRUE)
d <- function(...) file.path(run, ...)
out <- Sys.getenv("REV_OUT", file.path("04_outputs", "revision_jwm")); tb <- file.path(out, "tables"); cache <- file.path(out, "cache")
st <- readRDS(file.path(cache, "static.rds")); sc <- readRDS(file.path(cache, "scenarios.rds"))
aux <- readRDS(file.path(cache, "stage2_for_variants.rds"))
cells <- st$cells; pcols <- st$pred_cols; clim <- st$clim_cols; SEED <- 123456L; FIT_SEED <- 123459L
pres <- rast(d("03_present_suitability", "suitability_present_ensemble.tif")); cell <- prod(res(pres)) / 1e6
pv <- values(pres, mat = FALSE)[cells]; pav <- aux$pav; top25 <- aux$top25; thr0 <- aux$thr; core0 <- aux$primary_core

ms <- read.csv(d("01_processed_data", "modeling_dataset.csv"))
for (p in pcols) ms[[p]][is.na(ms[[p]])] <- median(ms[[p]], na.rm = TRUE)
occ <- read.csv(file.path("01_data_raw", "01_occurrences", "elephant_PA_data.csv"), stringsAsFactors = FALSE)
book <- read.csv(file.path("01_data_raw", "01_occurrences", "Source_Files", "Book1.csv"), stringsAsFactors = FALSE)
kk <- function(lon, lat) paste(round(lon, 5), round(lat, 5))
ix <- match(kk(ms$longitude, ms$latitude), kk(occ$longitude, occ$latitude))
ms$survey <- ifelse(occ$source[ix] == "book1_survey", "elephant", "tiger")
ms$dung <- occ$station_id[ix] %in% trimws(book$station[trimws(book$camera) == "Dung"])
el_r <- rast(d("02_data_intermediate", "elevation_harmonized.tif"))
ms$elev <- extract(el_r, project(vect(ms, geom = c("longitude", "latitude"), crs = "EPSG:4326"), crs(el_r)), ID = FALSE)[, 1]
lim_el <- max(ms$elev[ms$response == 1], na.rm = TRUE)

auc <- function(y, p) { r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
tss_at <- function(y, p, t) mean(p[y == 1] >= t) + mean(p[y == 0] < t) - 1
threads <- 2L  # low memory: each ranger thread holds its own prediction buffers

fit3 <- function(x, f, np) {
  set.seed(FIT_SEED)
  g <- suppressWarnings(glm(f, data = x, family = binomial()))
  r <- ranger(f, data = x, num.trees = 1000, mtry = floor(sqrt(np)), seed = FIT_SEED, num.threads = threads)
  b <- gbm(f, data = x, distribution = "bernoulli", n.trees = 1000, interaction.depth = 3, shrinkage = 0.01, n.minobsinnode = 5, bag.fraction = 0.75, verbose = FALSE)
  it <- suppressWarnings(suppressMessages(gbm.perf(b, method = "OOB", plot.it = FALSE)))
  list(g = g, r = r, b = b, it = if (is.na(it) || it < 1) 500L else it)
}
pr3_block <- function(m, x) rowMeans(cbind(as.numeric(predict(m$g, newdata = x, type = "response")),
                                     as.numeric(predict(m$r, data = x, num.threads = threads)$predictions),
                                     as.numeric(predict(m$b, newdata = x, n.trees = m$it, type = "response"))))
# Predict in blocks so a 528,000-cell grid never sits in memory three times over
pr3 <- function(m, x, size = 50000L) {
  out <- numeric(nrow(x)); idx <- split(seq_len(nrow(x)), ceiling(seq_len(nrow(x)) / size))
  for (i in idx) out[i] <- pr3_block(m, x[i, , drop = FALSE])
  out
}
only <- commandArgs(trailingOnly = TRUE)[-1]
variants <- list(
  no_dung = list(keep = !ms$dung, drop = character()),
  tiger_only = list(keep = ms$survey == "tiger", drop = character()),
  time_aligned = list(keep = rep(TRUE, nrow(ms)), drop = c("landcover", "dynamicworld_prob", "evi", "human_footprint")),
  no_human = list(keep = rep(TRUE, nrow(ms)), drop = c("human_footprint", "dist_to_settlements", "dist_to_roads", "dist_to_private_land")),
  no_dist_pa = list(keep = rep(TRUE, nrow(ms)), drop = "dist_to_protected_areas")
)
if (length(only)) variants <- variants[only]
base0 <- st$static
res <- list(); per <- list()
for (nm in names(variants)) {
  v <- variants[[nm]]; use <- setdiff(pcols, v$drop)
  ms_v <- ms; base <- base0
  x <- ms_v[v$keep, ]; ok_cells <- complete.cases(base[, use, drop = FALSE])
  f <- as.formula(paste("response ~", paste(use, collapse = " + ")))
  cat("Variant", nm, ":", nrow(x), "stations,", length(use), "predictors\n")
  p <- rep(NA_real_, nrow(x))
  for (k in sort(unique(x$fold))) p[x$fold == k] <- pr3(fit3(x[x$fold != k, ], f, length(use)), x[x$fold == k, ])
  ct <- sort(unique(p)); tv <- vapply(ct, function(t) tss_at(x$response, p, t), 0); thr <- ct[which.max(tv)]
  w <- x$elev <= lim_el & !is.na(x$elev); e <- x$survey == "elephant"
  full <- fit3(x, f, length(use))
  mp <- rep(NA_real_, nrow(base)); mp[ok_cells] <- pr3(full, base[ok_cells, use, drop = FALSE])
  h <- !is.na(mp) & mp >= thr & !is.na(pv)
  cnt <- integer(nrow(base)); km <- numeric(60)
  for (i in seq_len(60)) {
    s <- sc[i, ]; cl <- readRDS(file.path(cache, sprintf("clim_%s_%s_%s.rds", s$gcm, s$ssp, s$period)))
    b2 <- base[, use, drop = FALSE]; b2[clim] <- cl[, clim]; rm(cl); ok <- complete.cases(b2)
    q <- rep(NA_real_, nrow(b2)); q[ok] <- pr3(full, b2[ok, , drop = FALSE]); rm(b2)
    if (i %% 5 == 0) gc(verbose = FALSE)
    sf <- !is.na(q) & q >= thr & !is.na(pv)
    km[i] <- sum(sf) * cell; cnt <- cnt + as.integer(h & sf)
    if (i %% 10 == 0) cat(" ", i)
  }
  cat("\n")
  core <- h & cnt == 60; a0 <- sum(h) * cell; chg <- 100 * (km - a0) / a0
  per[[nm]] <- data.frame(variant = nm, sc[, 1:3], area_km2 = km, change_pct = chg)
  r0 <- data.frame(variant = nm, stations = nrow(x), presences = sum(x$response), predictors = length(use), threshold = thr,
    auc = auc(x$response, p), tss = max(tv), auc_within = auc(x$response[w], p[w]), tss_within_at_threshold = tss_at(x$response[w], p[w], thr),
    auc_elephant = if (sum(e) > 0) auc(x$response[e], p[e]) else NA_real_,
    habitat_km2 = a0, habitat_outside_pa_pct = 100 * mean(!pav[h]), map_r = cor(mp, pv, use = "complete"),
    schoener_d = { i <- !is.na(mp) & !is.na(pv); 1 - 0.5 * sum(abs(mp[i] / sum(mp[i]) - pv[i] / sum(pv[i]))) },
    habitat_jaccard = sum(h & pv >= thr0, na.rm = TRUE) / sum(h | pv >= thr0, na.rm = TRUE),
    high_km2 = sum(h & cnt >= 45) * cell, core_km2 = sum(core) * cell, core_outside_pa_pct = 100 * mean(!pav[core]), core_outside_pa_km2 = sum(core & !pav) * cell,
    core_top25_pct = 100 * mean(top25[core]), core_jaccard = sum(core & core0) / sum(core | core0), primary_core_retained_pct = 100 * sum(core & core0) / sum(core0),
    n_loss = sum(chg < 0), change_min = min(chg), change_max = max(chg))
  if (nm == "no_human") {
    nrv <- function(v) { q <- quantile(v, c(0.02, 0.98), na.rm = TRUE); pmin(pmax((v - q[1]) / (q[2] - q[1]), 0), 1) }
    ex2 <- sqrt(nrv(mp) * aux$hpv); ex1 <- sqrt(aux$snv * aux$hpv)
    r0$exposure_r_vs_primary <- cor(ex1, ex2, use = "complete")
    r0$exposure_top15_overlap <- { a <- ex1 >= quantile(ex1, 0.85, na.rm = TRUE); b <- ex2 >= quantile(ex2, 0.85, na.rm = TRUE); sum(a & b, na.rm = TRUE) / sum(a, na.rm = TRUE) }
    rf <- ex2[aux$settled_foothill]; rf <- rf[is.finite(rf)]; z <- ex2[aux$cidx]; z <- z[is.finite(z)]
    r0$incident_auc_settled <- mean(vapply(z, function(u) mean(rf < u) + 0.5 * mean(rf == u), 0))
  } else { r0$exposure_r_vs_primary <- NA_real_; r0$exposure_top15_overlap <- NA_real_; r0$incident_auc_settled <- NA_real_ }
  res[[nm]] <- r0
  saveRDS(list(map = mp, core = core, threshold = thr), file.path(cache, sprintf("variant_%s_map.rds", nm)))
  write.csv(r0, file.path(tb, sprintf("variant_%s.csv", nm)), row.names = FALSE)
  write.csv(per[[nm]], file.path(tb, sprintf("variant_%s_projections.csv", nm)), row.names = FALSE)
  print(t(r0))
  rm(full, mp, h, cnt, core); gc(verbose = FALSE)
}
cat("Stage 3 complete\n")
