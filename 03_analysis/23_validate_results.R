#!/usr/bin/env Rscript
# 23_validate_results.R — independent recalculation of manuscript results.
# Recomputes results from station files, held-out predictions and rasters without
# using the claim ledgers, then compares them with the values the manuscript reports.
# Usage (PowerShell, repo root):
#   Rscript 03_analysis/23_validate_results.R 04_outputs/runs/RUN_<id> 04_outputs/runs/RUN_<sensitivity_id>

suppressPackageStartupMessages({ library(terra); library(sf); library(yaml) })
args <- commandArgs(trailingOnly = TRUE)
run <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
d <- function(...) file.path(run, ...)
cfg <- read_yaml("00_governance/config.yaml")
out_dir <- Sys.getenv("JWM_OUT", file.path("05_manuscripts", "Journal of Wildlife Management"))
rev_dir <- Sys.getenv("REV_OUT", file.path("04_outputs", "revision_jwm"))

led <- read.csv(file.path(Sys.getenv("LEDGER_DIR", "05_manuscripts"), "manuscript_claim_ledger.csv"), stringsAsFactors = FALSE)
xtr <- read.csv(file.path(out_dir, "jwm_extra_numbers.csv"), stringsAsFactors = FALSE)
sen <- read.csv(file.path(rev_dir, "tables", "variant_no_dist_pa.csv"), stringsAsFactors = FALSE)
rep_v <- function(k) {
  for (t in list(led, xtr)) if (k %in% t$key) return(as.numeric(t$value[t$key == k]))
  stop("No reported value: ", k)
}
res <- list()
chk <- function(group, claim, reported, recomputed, tol) {
  res[[length(res) + 1]] <<- data.frame(group = group, claim = claim, reported = reported,
    recomputed = round(recomputed, 4), tolerance = tol,
    status = if (is.finite(recomputed) && abs(reported - recomputed) <= tol + 1e-9) "PASS" else "FAIL")
}
k <- function(group, key, value, tol) chk(group, key, rep_v(key), value, tol)

# ── Station data ─────────────────────────────────────────────────────────────
occ <- read.csv(file.path("01_data_raw", "01_occurrences", "elephant_PA_data.csv"), stringsAsFactors = FALSE)
exc <- read.csv(file.path("01_data_raw", "01_occurrences", "occurrence_exclusions.csv"), stringsAsFactors = FALSE)
chk("Stations", "modeled stations", 1063, nrow(occ), 0)
chk("Stations", "presences", 239, sum(occ$presence == 1), 0)
chk("Stations", "absences", 824, sum(occ$presence == 0), 0)
chk("Stations", "records before exclusion", 1089, nrow(occ) + nrow(exc), 0)
chk("Stations", "records excluded", 26, nrow(exc), 0)
chk("Stations", "prevalence", 0.225, sum(occ$presence == 1) / nrow(occ), 0.0005)
out <- exc[exc$exclusion_reason == "outside_bhutan_boundary", ]
chk("Stations", "outside boundary", 24, nrow(out), 0)
chk("Stations", "outside boundary: presences", 11, sum(out$presence == 1), 0)
chk("Stations", "outside boundary: absences", 13, sum(out$presence == 0), 0)
chk("Stations", "unverified coordinates (presences)", 2, sum(exc$exclusion_reason == "unverified_coordinate" & exc$presence == 1), 0)
aoi <- st_transform(st_read(cfg$vectors$aoi_bhutan, quiet = TRUE), 32645)
od <- as.numeric(st_distance(st_transform(st_as_sf(out, coords = c("longitude", "latitude"), crs = 4326), 32645), st_union(aoi))) / 1000
bad_lat <- out$latitude < 25
chk("Stations", "outside: latitude 21-24 N", 3, sum(bad_lat), 0)
chk("Stations", "outside: within 1.5 km", 10, sum(od[!bad_lat] <= 1.5), 0)
chk("Stations", "outside: 6-18 km", 2, sum(od[!bad_lat] >= 6 & od[!bad_lat] <= 18), 0)
far <- !bad_lat & od >= 58.5 & od <= 66.5
chk("Stations", "outside: about 59-66 km", 9, sum(far), 0)
chk("Stations", "outside: Royal Manas stations among those", 8, sum(grepl("^RMNP", out$station_id[far])), 0)

# ── Held-out evaluation ──────────────────────────────────────────────────────
oof <- read.csv(d("01_processed_data", "oof_predictions.csv"))
auc <- function(y, p) { r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
tss_at <- function(y, p, t) mean(p[y == 1] >= t) + mean(p[y == 0] < t) - 1
y <- oof$response; p <- oof$pred_ensemble
ct <- sort(unique(p)); tt <- vapply(ct, function(t) tss_at(y, p, t), 0)
thr <- ct[which.max(tt)]
k("Evaluation", "threshold", thr, 0.0006)
k("Evaluation", "ensemble_auc_mean", auc(y, p), 0.0006)
k("Evaluation", "ensemble_tss_mean", max(tt), 0.0006)
k("Evaluation", "ensemble_brier", mean((p - y)^2), 0.0006)
chk("Evaluation", "Brier score of constant prediction", 0.174, mean(y) * (1 - mean(y)), 0.0006)
pc <- pmin(pmax(p, 1e-6), 1 - 1e-6)
k("Evaluation", "ensemble_calibration_slope", coef(glm(y ~ qlogis(pc), family = binomial))[[2]], 0.002)
for (a in c("glm", "rf", "brt")) {
  fa <- vapply(sort(unique(oof$fold)), function(f) { s <- oof$fold == f; auc(y[s], oof[[paste0("pred_", a)]][s]) }, 0)
  k("Evaluation", paste0(a, "_auc_mean"), mean(fa), 0.0006)
}
fe <- vapply(sort(unique(oof$fold)), function(f) { s <- oof$fold == f; auc(y[s], p[s]) }, 0)
k("Evaluation", "ensemble_fold_auc_min", min(fe), 0.0006)
k("Evaluation", "ensemble_fold_auc_max", max(fe), 0.0006)
xy <- st_coordinates(st_transform(st_as_sf(oof, coords = c("longitude", "latitude"), crs = 4326), 32645))
D <- as.matrix(dist(xy)); diag(D) <- Inf
nn <- t(apply(D, 1, function(v) order(v)[1:5]))
e <- (y - p) - mean(y - p)
wsum <- sum(vapply(seq_len(nrow(nn)), function(i) sum(e[i] * e[nn[i, ]]), 0))
chk("Evaluation", "ensemble Moran's I, projected distance", rep_v("ensemble_moran_projected"), (length(e) / (5 * length(e))) * wsum / sum(e^2), 0.0006)
D0 <- as.matrix(dist(cbind(oof$longitude, oof$latitude))); diag(D0) <- Inf
nn0 <- t(apply(D0, 1, function(v) order(v)[1:5]))
k("Evaluation", "ensemble_moran_i", sum(vapply(seq_len(nrow(nn0)), function(i) sum(e[i] * e[nn0[i, ]]), 0)) / (5 * sum(e^2)), 0.0006)

el_r <- rast(d("02_data_intermediate", "elevation_harmonized.tif"))
st_el <- extract(el_r, project(vect(oof, geom = c("longitude", "latitude"), crs = "EPSG:4326"), crs(el_r)), ID = FALSE)[, 1]
mx <- max(st_el[y == 1], na.rm = TRUE)
k("Evaluation", "station_elev_max_presence_m", mx, 0.6)
k("Evaluation", "presence_elevation_max_m", mx, 0.6)
s <- !is.na(st_el) & st_el <= mx
k("Within range", "eval_below_max_presence_n", sum(s), 0)
k("Within range", "eval_below_max_presence_auc", auc(y[s], p[s]), 0.0006)
k("Within range", "eval_below_max_presence_tss_at_threshold", tss_at(y[s], p[s], thr), 0.0006)
s <- !is.na(st_el) & st_el <= 1000
k("Within range", "eval_below_1000m_n", sum(s), 0)
k("Within range", "eval_below_1000m_auc", auc(y[s], p[s]), 0.0006)
k("Within range", "eval_below_1000m_tss_at_threshold", tss_at(y[s], p[s], thr), 0.0006)
k("Within range", "eval_below_1000m_above_threshold_pct", 100 * mean(p[s] >= thr), 0.06)
kk <- function(lon, lat) paste(round(lon, 5), round(lat, 5))
src <- occ$source[match(kk(oof$longitude, oof$latitude), kk(occ$longitude, occ$latitude))]
s <- src == "book1_survey"
k("Within range", "eval_elephant_n", sum(s), 0)
k("Within range", "eval_elephant_auc", auc(y[s], p[s]), 0.0006)
k("Within range", "eval_elephant_tss_at_threshold", tss_at(y[s], p[s], thr), 0.0006)
k("Within range", "eval_elephant_above_threshold_pct", 100 * mean(p[s] >= thr), 0.06)
k("Within range", "eval_tiger_auc", auc(y[!s], p[!s]), 0.0006)
k("Within range", "eval_absences_above_max_presence_pct", 100 * sum(y == 0 & st_el > mx, na.rm = TRUE) / sum(y == 0), 0.06)

# ── Present habitat ──────────────────────────────────────────────────────────
pres <- rast(d("03_present_suitability", "suitability_present_ensemble.tif"))
thr_l <- read.csv(d("02_models", "evaluation_all.csv")); thr_l <- thr_l$threshold[thr_l$algorithm == "ensemble"][1]
cell <- prod(res(pres)) / 1e6
grid <- function(f, m = "near") { r <- rast(f); if (compareGeom(r, pres, stopOnError = FALSE)) r else project(r, pres, method = m) }
pv <- values(pres, mat = FALSE); dom <- !is.na(pv); suit <- dom & pv >= thr_l
k("Present habitat", "domain_km2", sum(dom) * cell, 1)
k("Present habitat", "present_suitable_km2", sum(suit) * cell, 1)
k("Present habitat", "present_suitable_pct_domain", 100 * mean(suit[dom]), 0.06)
pa <- st_transform(st_make_valid(st_read(cfg$vectors$protected_areas, quiet = TRUE)), crs(pres))
pav <- values(rasterize(vect(pa), pres, field = 1, background = 0), mat = FALSE) == 1
k("Present habitat", "present_suitable_in_pa_pct", 100 * mean(pav[suit]), 0.5)
k("Present habitat", "pa_area_suitable_pct", 100 * sum(suit & pav) / sum(dom & pav), 0.5)
el <- values(project(el_r, pres, method = "bilinear"), mat = FALSE)
k("Present habitat", "present_suitable_below_1000m_pct", 100 * sum(suit & el <= 1000, na.rm = TRUE) / sum(suit), 0.3)
k("Present habitat", "present_suitable_above_3000m_km2", sum(suit & el > 3000, na.rm = TRUE) * cell, 3)
k("Present habitat", "pa_above_2000m_pct", 100 * mean(el[pav & !is.na(el)] > 2000), 0.5)
k("Present habitat", "absence_stations_above_3000m", sum(y == 0 & st_el > 3000, na.rm = TRUE), 0)
bh <- values(rasterize(vect(st_transform(aoi, crs(pres))), pres, field = 1, background = 0), mat = FALSE) == 1
ex_c <- bh & !dom
lim <- read.csv(file.path(rev_dir, "revision_numbers_limit.csv")); lv <- function(x) as.numeric(lim$value[lim$key == x])
chk("Present habitat", "excluded area: median elevation (m)", lv("domain_excluded_median_elev_m"), median(el[ex_c], na.rm = TRUE), 30)
chk("Present habitat", "excluded area: share below 2,000 m (%)", lv("domain_excluded_below_2000_pct"), 100 * mean(el[ex_c] < 2000, na.rm = TRUE), 0.2)
# Input layers actually used by the run
chk("Input layers", "highest elevation in modeled domain (m)", 3000, max(el[dom], na.rm = TRUE), 25)
tmpl <- rast(d("02_data_intermediate", "template_grid.tif"))
for (lyr in list(c("dist_to_settlements", "building_footprints_harmonized.gpkg"), c("dist_to_private_land", "2022_07_01_cadastral_fixed_harmonized.gpkg"))) {
  dl <- rast(d("01_processed_data", "distance_layers", paste0(lyr[1], ".tif")))
  touched <- rasterize(vect(d("02_data_intermediate", lyr[2])), tmpl, field = 1, background = NA, touches = TRUE)
  chk("Input layers", paste0(lyr[1], ": zero-distance cells equal touched cells"), sum(!is.na(values(touched))), sum(values(dl, mat = FALSE) == 0, na.rm = TRUE), 0)
}
all_stg <- read.csv(file.path(rev_dir, "revision_numbers_stage2b.csv"))
chk("Input layers", "building cells reported", as.numeric(all_stg$value[all_stg$key == "buildings_cells_new"]), sum(!is.na(values(rasterize(vect(d("02_data_intermediate", "building_footprints_harmonized.gpkg")), tmpl, field = 1, background = NA, touches = TRUE)))), 0)

# ── 60 projections, recalculated from each raster ────────────────────────────
fut <- d("04_future_projections")
ff <- list.files(fut, "^suitability_future_.*_ssp[0-9]{3}_[0-9]{4}_[0-9]{4}_ensemble\\.tif$", full.names = TRUE)
if (length(ff) != 60) stop("Expected 60 projections")
meta <- regmatches(basename(ff), regexec("^suitability_future_(.*)_(ssp[0-9]{3})_([0-9]{4}_[0-9]{4})_ensemble", basename(ff)))
tab <- data.frame(gcm = sapply(meta, `[`, 2), ssp = sapply(meta, `[`, 3), period = sapply(meta, `[`, 4))
cnt <- integer(length(pv)); tab$km2 <- NA_real_
for (i in seq_along(ff)) {
  v <- values(grid(ff[i], "bilinear"), mat = FALSE); s <- dom & !is.na(v) & v >= thr_l
  tab$km2[i] <- sum(s) * cell; cnt <- cnt + as.integer(s & suit)
}
area0 <- sum(suit) * cell
tab$chg <- 100 * (tab$km2 - area0) / area0
k("Future", "per_gcm_change_pct_min", min(tab$chg), 0.6)
k("Future", "per_gcm_change_pct_max", max(tab$chg), 0.6)
k("Future", "per_gcm_n_projections_loss", sum(tab$chg < 0), 0)
for (g in unique(tab$gcm)) k("Future", sprintf("gcm_%s_n_loss", g), sum(tab$chg[tab$gcm == g] < 0), 0)
k("Future", "gcm_acces_cm2_change_pct_min", min(tab$chg[tab$gcm == "acces_cm2"]), 0.06)
k("Future", "gcm_acces_cm2_change_pct_max", max(tab$chg[tab$gcm == "acces_cm2"]), 0.06)
k("Future", "gcm_mpi_esm1_2_lr_change_pct_min", min(tab$chg[tab$gcm == "mpi_esm1_2_lr"]), 0.6)
chk("Future", "SSP5-8.5 projections with net loss", 8, sum(tab$chg[tab$ssp == "ssp585"] < 0), 0)
gain <- loss <- mean_chg <- numeric(0)
for (sp in unique(tab$ssp)) for (pe in unique(tab$period)) {
  x <- tab[tab$ssp == sp & tab$period == pe, ]
  key <- paste(sp, pe, sep = "_")
  k("Future", paste0("future_gcm_median_change_pct_", key), 100 * (median(x$km2) - area0) / area0, 0.06)
  k("Future", paste0("future_gcm_n_loss_", key), sum(x$chg < 0), 0)
  m <- values(grid(file.path(fut, sprintf("future_gcm_ensemble_%s.tif", key)), "bilinear"), mat = FALSE)
  ms <- dom & !is.na(m) & m >= thr_l
  k("Future", paste0("future_area_km2_", key), sum(ms) * cell, 1)
  mean_chg <- c(mean_chg, 100 * (sum(ms) - sum(suit)) / sum(suit))
  gain <- c(gain, sum(ms & !suit) * cell); loss <- c(loss, sum(suit & !ms) * cell)
}
chk("Future", "GCM-mean change: minimum (%)", round(min(as.numeric(led$value[grepl("^future_change_pct_", led$key)])), 1), min(mean_chg), 0.06)
chk("Future", "GCM-mean change: maximum (%)", round(max(as.numeric(led$value[grepl("^future_change_pct_", led$key)])), 1), max(mean_chg), 0.06)
hs_t <- read.csv(d("05_change_metrics", "habitat_area_change_summary.csv"))
chk("Future", "GCM-mean loss: minimum (km2)", min(hs_t$loss_km2), min(loss), 1)
chk("Future", "GCM-mean loss: maximum (km2)", max(hs_t$loss_km2), max(loss), 1)
chk("Future", "GCM-mean gain: minimum (km2)", min(hs_t$gain_km2), min(gain), 1)
chk("Future", "GCM-mean gain: maximum (km2)", max(hs_t$gain_km2), max(gain), 1)

# ── Refugia counted directly from the 60 projections ─────────────────────────
core <- suit & cnt == 60; high <- suit & cnt >= 45
k("Refugia", "refugia_core_km2", sum(core) * cell, 1)
k("Refugia", "refugia_core_pct_present", 100 * sum(core) / sum(suit), 0.06)
k("Refugia", "refugia_high_km2", sum(high) * cell, 1)
k("Refugia", "refugia_high_pct_present", 100 * sum(high) / sum(suit), 0.06)
k("Refugia", "refugia_core_in_pa_pct", 100 * mean(pav[core]), 0.5)
k("Refugia", "refugia_high_in_pa_pct", 100 * mean(pav[high]), 0.5)
k("Refugia", "refugia_core_outside_pa_km2", sum(core & !pav) * cell, 15)
k("Refugia", "present_suitable_below_45_km2", sum(suit & cnt < 45) * cell, 1)
k("Refugia", "present_suitable_zero_of_60_km2", sum(suit & cnt == 0) * cell, 1)

# ── Climatic novelty ─────────────────────────────────────────────────────────
ef <- list.files(fut, "^extrapolation_.*_ssp[0-9]{3}_[0-9]{4}_[0-9]{4}\\.tif$", full.names = TRUE)
if (length(ef) != 60) stop("Expected 60 extrapolation layers")
dm <- hb <- cr <- numeric(length(ef))
for (i in seq_along(ef)) {
  v <- values(grid(ef[i]), mat = FALSE)
  dm[i] <- mean(v[dom] == 1, na.rm = TRUE); hb[i] <- mean(v[suit] == 1, na.rm = TRUE); cr[i] <- mean(v[core] == 1, na.rm = TRUE)
}
k("Novelty", "extrap_frac_per_projection_min_pct", 100 * min(dm), 0.06)
k("Novelty", "extrap_frac_per_projection_max_pct", 100 * max(dm), 0.06)
k("Novelty", "present_suitable_extrap_per_projection_median_pct", 100 * median(hb), 0.06)
k("Novelty", "present_suitable_extrap_per_projection_min_pct", 100 * min(hb), 0.6)
k("Novelty", "present_suitable_extrap_per_projection_max_pct", 100 * max(hb), 0.6)
k("Novelty", "refugia_core_extrap_per_projection_median_pct", 100 * median(cr), 0.06)
md <- read.csv(d("01_processed_data", "modeling_dataset.csv"))
k("Novelty", "bio05_station_max", max(md$BIO05, na.rm = TRUE), 0.06)

# ── Human pressure, exposure and conflict records ────────────────────────────
nr <- function(r) { v <- values(r, na.rm = TRUE); q <- quantile(v, c(0.02, 0.98)); clamp((r - q[1]) / (q[2] - q[1]), 0, 1, values = TRUE) }
fp <- resample(rast(d("02_data_intermediate", "human_footprint_harmonized.tif")), pres, method = "bilinear")
b <- rasterize(vect(d("02_data_intermediate", "building_footprints_harmonized.gpkg")), pres, field = 1, background = NA, touches = TRUE)
hp <- nr(nr(fp) + nr(1 / (1 + distance(b) / 1000)))
hpv <- values(hp, mat = FALSE); top <- dom & !is.na(hpv) & hpv >= quantile(hpv[dom], 0.75, na.rm = TRUE)
k("Human pressure", "present_suitable_in_top25_human_pressure_pct", 100 * sum(suit & top) / sum(suit), 0.06)
k("Human pressure", "refugia_core_in_top25_human_pressure_pct", 100 * sum(core & top) / sum(core), 0.06)
k("Human pressure", "refugia_core_outside_pa_top25_human_pressure_km2", sum(core & !pav & top) * cell, 15)
ex <- sqrt(nr(pres) * nr(hp)); xv <- values(ex, mat = FALSE)
k("Human pressure", "exposure_present_mean", mean(xv[dom], na.rm = TRUE), 0.0006)
hw <- read.csv(file.path("01_data_raw", "04_conflicts", "HWC_Species_Elephant_clean.csv"), stringsAsFactors = FALSE)
hw <- hw[is.finite(hw$longitude) & is.finite(hw$latitude) & hw$longitude != 0, ]
hv <- project(vect(hw, geom = c("longitude", "latitude"), crs = "EPSG:4326"), crs(pres))
hx <- extract(ex, hv, ID = FALSE)[, 1]; hs <- extract(pres, hv, ID = FALSE)[, 1]; ok <- !is.na(hx)
k("Human pressure", "hwc_incidents_in_domain", sum(ok), 0)
k("Human pressure", "hwc_year_min", min(hw$year, na.rm = TRUE), 0)
k("Human pressure", "hwc_year_max", max(hw$year, na.rm = TRUE), 0)
k("Human pressure", "hwc_in_suitable_habitat_pct", 100 * mean(hs[ok] >= thr_l, na.rm = TRUE), 0.06)
k("Human pressure", "hwc_in_top15_exposure_pct", 100 * mean(hx[ok] >= quantile(xv[dom], 0.85, na.rm = TRUE)), 0.06)

# ── Sensitivity run without distance to protected areas ──────────────────────
vm <- readRDS(file.path(rev_dir, "cache", "variant_no_dist_pa_map.rds"))
vc <- readRDS(file.path(rev_dir, "cache", "static.rds"))$cells
sv <- rep(NA_real_, length(pv)); sv[vc] <- vm$map; sthr <- vm$threshold
ss <- !is.na(sv) & sv >= sthr & !is.na(pv)
chk("Sensitivity", "habitat without distance to protected areas (km2)", sen$habitat_km2, sum(ss) * cell, 1)
chk("Sensitivity", "outside network without that predictor (%)", sen$habitat_outside_pa_pct, 100 * mean(!pav[ss]), 0.5)
ok2 <- is.finite(pv) & is.finite(sv)
chk("Sensitivity", "map Pearson r", sen$map_r, cor(pv[ok2], sv[ok2]), 0.0006)
chk("Sensitivity", "map Jaccard", sen$habitat_jaccard, sum(suit & ss, na.rm = TRUE) / sum(suit | ss, na.rm = TRUE), 0.0006)
a <- pv[ok2]; b2 <- sv[ok2]
chk("Sensitivity", "map Schoener D", sen$schoener_d, 1 - 0.5 * sum(abs(a / sum(a) - b2 / sum(b2))), 0.0006)

# ── Predictor statistics quoted in the text ──────────────────────────────────
pres_i <- md$response == 1
med <- function(v) median(md[[v]][pres_i], na.rm = TRUE); med0 <- function(v) median(md[[v]][!pres_i], na.rm = TRUE)
chk("Predictors", "warmest-month maximum, presences (C)", 29.6, med("BIO05"), 0.06)
chk("Predictors", "warmest-month maximum, absences (C)", 21.0, med0("BIO05"), 0.06)
chk("Predictors", "warmest-quarter precipitation, presences (mm)", 2024, med("BIO18"), 3)
chk("Predictors", "warmest-quarter precipitation, absences (mm)", 886, med0("BIO18"), 3)
chk("Predictors", "driest-month precipitation, presences (mm)", 13.1, med("BIO14"), 0.06)
chk("Predictors", "driest-month precipitation, absences (mm)", 5.0, med0("BIO14"), 0.06)
chk("Predictors", "terrain ruggedness, presences", 44, med("tri"), 1)
chk("Predictors", "terrain ruggedness, absences", 72, med0("tri"), 1)
chk("Predictors", "distance to protected areas, presences (m)", 0, med("dist_to_protected_areas"), 0.5)
k("Predictors", "dist_pa_nondetect_median_m", med0("dist_to_protected_areas"), 1)
gl <- readRDS(d("02_models", "model_glm.rds")); co <- summary(gl)$coefficients
k("Predictors", "glm_coef_BIO18", co["BIO18", "Estimate"], 0.000006)
k("Predictors", "glm_se_BIO18", co["BIO18", "Std. Error"], 0.000006)

r <- do.call(rbind, res)
write.csv(r, file.path(out_dir, "Validation_Report_JWM.csv"), row.names = FALSE)
cat(sprintf("Checks: %d  PASS: %d  FAIL: %d\n", nrow(r), sum(r$status == "PASS"), sum(r$status == "FAIL")))
print(r[r$status == "FAIL", c("group", "claim", "reported", "recomputed", "tolerance")], row.names = FALSE)
