#!/usr/bin/env Rscript
# 28_revision_corrections.R — corrections found during the JWM revision audit.
# Rasterizing small polygons by cell center lost most buildings and much private land,
# so both distance layers and the building term of human pressure are rebuilt here with
# every touched cell. Also: land use within habitat, conflict reference comparisons,
# and buffered spatial cross-validation. Never writes to the accepted run.
# Usage (PowerShell, repo root):
#   Rscript 03_analysis/28_revision_corrections.R 04_outputs/runs/RUN_<id>

suppressPackageStartupMessages({ library(terra); library(sf); library(yaml); library(ranger); library(gbm) })
run <- normalizePath(commandArgs(trailingOnly = TRUE)[[1]], winslash = "/", mustWork = TRUE)
d <- function(...) file.path(run, ...)
cfg <- read_yaml("00_governance/config.yaml")
out <- file.path("04_outputs", "revision_jwm"); tb <- file.path(out, "tables"); cache <- file.path(out, "cache")
led <- list(); add <- function(key, value, source = "") led[[length(led) + 1]] <<- data.frame(key = key, value = as.character(value), source = source)
wr <- function(x, f) write.csv(x, file.path(tb, f), row.names = FALSE)
SEED <- 123456L; FIT_SEED <- 123459L
auc <- function(y, p) { r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
tss_at <- function(y, p, t) mean(p[y == 1] >= t) + mean(p[y == 0] < t) - 1
best_thr <- function(y, p) { ct <- sort(unique(p)); ct[which.max(vapply(ct, function(t) tss_at(y, p, t), 0))] }

pres <- rast(d("03_present_suitability", "suitability_present_ensemble.tif")); cell <- prod(res(pres)) / 1e6
template <- rast(d("02_data_intermediate", "template_grid.tif"))
st <- readRDS(file.path(cache, "static.rds")); cells <- st$cells; pcols <- st$pred_cols
S <- readRDS(file.path(cache, "S.rds")); E <- readRDS(file.path(cache, "E.rds"))
ev <- read.csv(d("02_models", "evaluation_all.csv")); thr <- ev$threshold[ev$algorithm == "ensemble"][1]
pv <- values(pres, mat = FALSE)[cells]; hab <- pv >= thr; core <- hab & rowSums(S >= thr, na.rm = TRUE) == 60
pa <- st_transform(st_make_valid(st_read(cfg$vectors$protected_areas, quiet = TRUE)), crs(pres))
pav <- values(rasterize(vect(pa), pres, field = 1, background = 0), mat = FALSE)[cells] == 1

# ── Corrected distance layers ────────────────────────────────────────────────
bv <- vect(d("02_data_intermediate", "building_footprints_harmonized.gpkg"))
lv <- vect(d("02_data_intermediate", "2022_07_01_cadastral_fixed_harmonized.gpkg"))
b_new <- distance(rasterize(bv, template, field = 1, background = NA, touches = TRUE))
l_new <- distance(rasterize(lv, template, field = 1, background = NA, touches = TRUE))
b_old <- rast(d("01_processed_data", "distance_layers", "dist_to_settlements.tif"))
l_old <- rast(d("01_processed_data", "distance_layers", "dist_to_private_land.tif"))
writeRaster(b_new, file.path(cache, "dist_to_settlements_touches.tif"), overwrite = TRUE)
writeRaster(l_new, file.path(cache, "dist_to_private_land_touches.tif"), overwrite = TRUE)
gv <- function(r) values(if (compareGeom(r, pres, stopOnError = FALSE)) r else project(r, pres, method = "bilinear"), mat = FALSE)[cells]
bn <- gv(b_new); ln <- gv(l_new); bo <- gv(b_old); lo <- gv(l_old)
add("buildings_n", nrow(bv), "footprints"); add("buildings_cells_old", sum(!is.na(values(rasterize(bv, template, field = 1, background = NA)))), "cell-center")
add("buildings_cells_new", sum(!is.na(values(rasterize(bv, template, field = 1, background = NA, touches = TRUE)))), "touches")
add("dist_buildings_median_old_km", round(median(bo, na.rm = TRUE) / 1000, 2), "domain cells"); add("dist_buildings_median_new_km", round(median(bn, na.rm = TRUE) / 1000, 2), "domain cells")
add("dist_private_median_old_km", round(median(lo, na.rm = TRUE) / 1000, 2), "domain cells"); add("dist_private_median_new_km", round(median(ln, na.rm = TRUE) / 1000, 2), "domain cells")
add("cor_old_new_buildings", round(cor(bo, bn, use = "complete", method = "spearman"), 3), "domain cells")
add("within1km_buildings_old_pct", round(100 * mean(bo <= 1000, na.rm = TRUE), 1), "domain cells"); add("within1km_buildings_new_pct", round(100 * mean(bn <= 1000, na.rm = TRUE), 1), "domain cells")

ms <- read.csv(d("01_processed_data", "modeling_dataset.csv"))
pts <- project(vect(ms, geom = c("longitude", "latitude"), crs = "EPSG:4326"), crs(pres))
ms_b <- extract(b_new, pts, ID = FALSE)[, 1]; ms_l <- extract(l_new, pts, ID = FALSE)[, 1]
add("station_dist_buildings_presence_old_m", round(median(ms$dist_to_settlements[ms$response == 1], na.rm = TRUE)), "stations")
add("station_dist_buildings_presence_new_m", round(median(ms_b[ms$response == 1], na.rm = TRUE)), "stations")
add("station_dist_buildings_nondet_new_m", round(median(ms_b[ms$response == 0], na.rm = TRUE)), "stations")
add("cor_new_settlements_private", round(cor(ms_b, ms_l, use = "complete"), 3), "stations")

# ── Corrected human pressure and exposure ────────────────────────────────────
nr <- function(r) { v <- values(r, na.rm = TRUE); q <- quantile(v, c(0.02, 0.98)); clamp((r - q[1]) / (q[2] - q[1]), 0, 1, values = TRUE) }
fp <- resample(rast(d("02_data_intermediate", "human_footprint_harmonized.tif")), pres, method = "bilinear")
b_km <- (if (compareGeom(b_new, pres, stopOnError = FALSE)) b_new else project(b_new, pres, method = "bilinear")) / 1000
fpn <- nr(fp); prox <- nr(1 / (1 + b_km)); hp <- nr(fpn + prox)
old_bl <- rasterize(bv, pres, field = 1, background = NA)
hp_old <- nr(fpn + nr(1 / (1 + distance(old_bl) / 1000)))
hpv <- values(hp, mat = FALSE)[cells]; hov <- values(hp_old, mat = FALSE)[cells]
top <- !is.na(hpv) & hpv >= quantile(hpv, 0.75, na.rm = TRUE); topo <- !is.na(hov) & hov >= quantile(hov, 0.75, na.rm = TRUE)
add("hp_old_new_spearman", round(cor(hov, hpv, use = "complete", method = "spearman"), 3), "domain cells")
add("top25_overlap_old_new_pct", round(100 * sum(top & topo) / sum(topo), 1), "domain cells")
add("hp_habitat_top25_pct", round(100 * sum(hab & top) / sum(hab), 1), "corrected"); add("hp_core_top25_pct", round(100 * sum(core & top) / sum(core), 1), "corrected")
add("hp_core_outside_top25_km2", round(sum(core & !pav & top) * cell), "corrected"); add("hp_core_top25_km2", round(sum(core & top) * cell), "corrected")
sn <- nr(pres); ex <- sqrt(sn * hp); xv <- values(ex, mat = FALSE)[cells]; snv <- values(sn, mat = FALSE)[cells]
add("exposure_mean_corrected", round(mean(xv, na.rm = TRUE), 3), "corrected"); add("cor_suit_hp_corrected", round(cor(snv, hpv, use = "complete", method = "spearman"), 3), "corrected")
ex_old <- values(sqrt(sn * hp_old), mat = FALSE)[cells]
add("exposure_old_new_r", round(cor(ex_old, xv, use = "complete"), 3), "domain cells")

# Future exposure with human pressure held at present values (GCM-mean maps)
mfs <- list.files(d("04_future_projections"), "^future_gcm_ensemble_ssp[0-9]{3}_[0-9]{4}_[0-9]{4}\\.tif$", full.names = TRUE)
fx <- vapply(mfs, function(f) { r <- rast(f); r <- if (compareGeom(r, pres, stopOnError = FALSE)) r else project(r, pres, method = "bilinear"); mean(values(sqrt(nr(r) * hp), mat = FALSE)[cells], na.rm = TRUE) }, 0)
add("exposure_future_min", round(min(fx), 3), "corrected"); add("exposure_future_max", round(max(fx), 3), "corrected")

dz <- st_read(file.path(cfg$vectors$root, "Dzongkhag Boundary", "Dzongkhag Boundary.shp"), quiet = TRUE); st_crs(dz) <- 3857L
dz <- st_transform(st_make_valid(dz), crs(pres))
dzr <- rasterize(vect(dz), pres, field = "dzongkhag"); lv2 <- levels(dzr)[[1]]; dzv <- lv2[[2]][match(values(dzr, mat = FALSE)[cells], lv2[[1]])]
ez <- sort(tapply(xv, dzv, mean, na.rm = TRUE), decreasing = TRUE)
for (i in 1:4) add(sprintf("exposure_corrected_rank%d", i), names(ez)[i], "corrected")
wr(data.frame(district = names(ez), mean_exposure = round(ez, 3)), "exposure_by_district_corrected.csv")

# ── Land use within habitat, 2020 national layer (area-weighted) ─────────────
lu <- vect(d("02_data_intermediate", "lulc2020_harmonized.gpkg"))
cl <- sort(unique(lu$class_name)); rows <- list()
for (c1 in cl) {
  v <- values(rasterize(lu[lu$class_name == c1, ], pres, cover = TRUE, background = 0), mat = FALSE)[cells]
  rows[[c1]] <- data.frame(class = c1, habitat_km2 = sum(v[hab]) * cell, outside_pa_habitat_km2 = sum(v[hab & !pav]) * cell, core_km2 = sum(v[core]) * cell, core_outside_pa_km2 = sum(v[core & !pav]) * cell)
}
lt <- do.call(rbind, rows); rownames(lt) <- NULL
for (n in c("habitat_km2", "outside_pa_habitat_km2", "core_km2", "core_outside_pa_km2")) lt[[sub("_km2", "_pct", n)]] <- 100 * lt[[n]] / sum(lt[[n]])
wr(lt[order(-lt$habitat_km2), ], "land_use_in_habitat_2020.csv"); print(lt[order(-lt$habitat_km2), c(1, 2, 6:9)], row.names = FALSE, digits = 3)

# ── Conflict records against reference sets ──────────────────────────────────
hw <- read.csv(file.path("01_data_raw", "04_conflicts", "HWC_Species_Elephant_clean.csv"), stringsAsFactors = FALSE)
hw <- hw[is.finite(hw$longitude) & is.finite(hw$latitude) & hw$longitude != 0, ]
hv <- project(vect(hw, geom = c("longitude", "latitude"), crs = "EPSG:4326"), crs(pres))
hc <- cellFromXY(pres, crds(hv)); inc <- hc %in% cells
bhu <- st_transform(st_read(cfg$vectors$aoi_bhutan, quiet = TRUE), crs(pres))
inb <- lengths(st_intersects(st_as_sf(hv), st_union(bhu))) > 0
add("hwc_total_rows", nrow(hw), "SMART"); add("hwc_inside_bhutan", sum(inb), "SMART"); add("hwc_not_in_domain", sum(!inc), "SMART")
add("hwc_year_min_domain", min(hw$year[inc]), "SMART"); add("hwc_year_max_domain", max(hw$year[inc]), "SMART"); add("hwc_2025_domain", sum(hw$year[inc] == 2025), "SMART")
add("hwc_damage_domain", paste(names(table(hw$damage_type[inc])), table(hw$damage_type[inc]), sep = ":", collapse = " "), "SMART")
cidx <- match(hc[inc], cells); xy5 <- paste(floor(crds(hv)[inc, 1] / 5000), floor(crds(hv)[inc, 2] / 5000))
elv <- values(project(rast(d("02_data_intermediate", "elevation_harmonized.tif")), pres, method = "bilinear"), mat = FALSE)[cells]
presE <- max(extract(rast(d("02_data_intermediate", "elevation_harmonized.tif")), project(pts[ms$response == 1, ], crs(rast(d("02_data_intermediate", "elevation_harmonized.tif")))), ID = FALSE)[, 1], na.rm = TRUE)
near_b <- !is.na(bn) & bn <= 1000
near_lu <- values(rasterize(lu[lu$class_name %in% c("Agriculture Land", "Built up"), ], pres, cover = TRUE, background = 0), mat = FALSE)[cells] > 0
refs <- list(`All modeled cells` = rep(TRUE, length(cells)), `Within 1 km of a building` = near_b,
             `Within 1 km of a building, at or below highest detection` = near_b & !is.na(elv) & elv <= presE,
             `Within 1 km of a building, inside suitable habitat` = near_b & hab,
             `Agriculture or built-up cells, inside suitable habitat` = near_lu & hab)
set.seed(SEED); rows <- list()
pct <- function(sc, ri) { r <- sc[ri]; r <- r[is.finite(r)]; vapply(sc[cidx], function(z) if (is.finite(z)) mean(r < z) + 0.5 * mean(r == z) else NA_real_, 0) }
for (nm in names(refs)) for (sc in c("Exposure index", "Suitability", "Human pressure")) {
  s2 <- switch(sc, `Exposure index` = xv, Suitability = snv, `Human pressure` = hpv); b <- pct(s2, refs[[nm]])
  sp <- split(b, xy5); ug <- names(sp); bs <- replicate(2000, mean(unlist(sp[sample(ug, replace = TRUE)], use.names = FALSE), na.rm = TRUE))
  rows[[length(rows) + 1]] <- data.frame(reference = nm, reference_cells = sum(refs[[nm]]), score = sc, auc = round(mean(b, na.rm = TRUE), 3), ci_low = round(quantile(bs, 0.025), 3), ci_high = round(quantile(bs, 0.975), 3))
}
ia <- do.call(rbind, rows); rownames(ia) <- NULL; wr(ia, "conflict_reference_comparison_corrected.csv"); print(ia, row.names = FALSE)
add("hwc_in_suitable_pct", round(100 * mean(hab[cidx]), 1), "SMART x habitat")
add("hwc_near_building_pct", round(100 * mean(near_b[cidx]), 1), "SMART x corrected buildings")
q85 <- quantile(xv, 0.85, na.rm = TRUE); add("hwc_top15_corrected_pct", round(100 * mean(xv[cidx] >= q85, na.rm = TRUE), 1), "corrected")
add("hwc_median_pctile_corrected", round(100 * median(ecdf(xv)(xv[cidx]), na.rm = TRUE)), "corrected")

saveRDS(list(hpv = hpv, snv = snv, cidx = cidx, settled_foothill = refs[[3]], pav = pav, top25 = top, thr = thr, primary_core = core,
             static_settle = bn, static_private = ln, st_settle = ms_b, st_private = ms_l), file.path(cache, "stage2_for_variants.rds"))

# ── Buffered spatial cross-validation (accepted folds) ───────────────────────
for (p in pcols) ms[[p]][is.na(ms[[p]])] <- median(ms[[p]], na.rm = TRUE)
fm <- as.formula(paste("response ~", paste(pcols, collapse = " + ")))
fit3 <- function(x) { set.seed(FIT_SEED)
  g <- suppressWarnings(glm(fm, data = x, family = binomial())); r <- ranger(fm, data = x, num.trees = 1000, mtry = 4, seed = FIT_SEED, num.threads = 6)
  b <- gbm(fm, data = x, distribution = "bernoulli", n.trees = 1000, interaction.depth = 3, shrinkage = 0.01, n.minobsinnode = 5, bag.fraction = 0.75, verbose = FALSE)
  it <- suppressWarnings(suppressMessages(gbm.perf(b, method = "OOB", plot.it = FALSE))); list(g = g, r = r, b = b, it = if (is.na(it) || it < 1) 500L else it) }
pr3 <- function(m, x) rowMeans(cbind(as.numeric(predict(m$g, newdata = x, type = "response")), as.numeric(predict(m$r, data = x, num.threads = 6)$predictions), as.numeric(predict(m$b, newdata = x, n.trees = m$it, type = "response"))))
XY <- crds(pts); D <- as.matrix(dist(XY)) / 1000
el_st <- extract(rast(d("02_data_intermediate", "elevation_harmonized.tif")), project(pts, crs(rast(d("02_data_intermediate", "elevation_harmonized.tif")))), ID = FALSE)[, 1]
wr_ok <- !is.na(el_st) & el_st <= presE
rows <- list()
for (bf in c(0, 5, 10, 20)) {
  p <- rep(NA_real_, nrow(ms)); dropped <- 0
  for (k in 1:5) { te <- ms$fold == k; tr <- !te & apply(D[, te, drop = FALSE], 1, min) >= bf; dropped <- dropped + sum(!te) - sum(tr); p[te] <- pr3(fit3(ms[tr, ]), ms[te, ]) }
  y <- ms$response
  ts <- vapply(1:5, function(k) best_thr(y[ms$fold != k], p[ms$fold != k]), 0)
  crossfit <- { tp <- fn <- tn <- fp2 <- 0; for (k in 1:5) { i <- ms$fold == k; tp <- tp + sum(y[i] == 1 & p[i] >= ts[k]); fn <- fn + sum(y[i] == 1 & p[i] < ts[k]); tn <- tn + sum(y[i] == 0 & p[i] < ts[k]); fp2 <- fp2 + sum(y[i] == 0 & p[i] >= ts[k]) }; tp / (tp + fn) + tn / (tn + fp2) - 1 }
  rows[[length(rows) + 1]] <- data.frame(buffer_km = bf, mean_training_removed = round(dropped / 5), auc = auc(y, p), tss_pooled_opt = max(vapply(sort(unique(p)), function(t) tss_at(y, p, t), 0)),
    tss_crossfit = crossfit, auc_within = auc(y[wr_ok], p[wr_ok]), brier = mean((p - y)^2))
}
bt <- do.call(rbind, rows); wr(bt, "cv_buffered.csv"); print(bt)
write.csv(do.call(rbind, led), file.path(out, "revision_numbers_stage2b.csv"), row.names = FALSE)
cat("Corrections stage complete\n")
