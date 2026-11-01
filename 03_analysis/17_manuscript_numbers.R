#!/usr/bin/env Rscript
# 17_manuscript_numbers.R — every number quoted in the manuscript, computed
# from one accepted run directory. Read-only with respect to the run: writes
# 05_manuscripts/manuscript_claim_ledger.csv.
#
# Usage (PowerShell, repo root):
#   Rscript 03_analysis/17_manuscript_numbers.R 04_outputs/runs/RUN_<id>

suppressPackageStartupMessages({ library(terra); library(sf) })

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("run_dir is required")
run_dir <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
out_csv <- if (length(args) >= 2) args[[2]] else file.path("05_manuscripts", "manuscript_claim_ledger.csv")
run_id <- basename(run_dir)

d <- function(...) file.path(run_dir, ...)
ledger <- list()
add <- function(key, value, unit = "", source = "") {
  ledger[[length(ledger) + 1L]] <<- data.frame(key = key, value = value, unit = unit,
                                               source = source, stringsAsFactors = FALSE)
}

ev <- read.csv(d("02_models", "evaluation_all.csv"), stringsAsFactors = FALSE)
thr <- ev$threshold[ev$algorithm == "ensemble"]
stopifnot(length(thr) == 1, is.finite(thr))
add("threshold", round(thr, 3), "", "02_models/evaluation_all.csv")

for (i in seq_len(nrow(ev))) {
  a <- ev$algorithm[i]
  for (m in c("auc_mean", "tss_mean", "boyce", "brier", "calibration_slope", "moran_i")) {
    if (m %in% names(ev) && is.finite(ev[[m]][i]))
      add(sprintf("%s_%s", a, m), round(ev[[m]][i], 3), "", "02_models/evaluation_all.csv")
  }
}
fm <- read.csv(d("02_models", "fold_metrics.csv"), stringsAsFactors = FALSE)
for (a in unique(fm$algorithm)) {
  x <- fm[fm$algorithm == a, ]
  add(sprintf("%s_fold_auc_min", a), round(min(x$auc), 3), "", "02_models/fold_metrics.csv")
  add(sprintf("%s_fold_auc_max", a), round(max(x$auc), 3), "", "02_models/fold_metrics.csv")
  add(sprintf("%s_fold_auc_sd", a), round(sd(x$auc), 3), "", "02_models/fold_metrics.csv")
  add(sprintf("%s_fold_tss_min", a), round(min(x$tss), 3), "", "02_models/fold_metrics.csv")
  add(sprintf("%s_fold_tss_max", a), round(max(x$tss), 3), "", "02_models/fold_metrics.csv")
}
add("fold_presence_min", min(fm$n_presence), "stations", "02_models/fold_metrics.csv")
add("fold_presence_max", max(fm$n_presence), "stations", "02_models/fold_metrics.csv")
add("fold_absence_min", min(fm$n_reference), "stations", "02_models/fold_metrics.csv")
add("fold_absence_max", max(fm$n_reference), "stations", "02_models/fold_metrics.csv")

# ── Present habitat ──────────────────────────────────────────────────────────
pres <- rast(d("03_present_suitability", "suitability_present_ensemble.tif"))
cell_km2 <- prod(res(pres)) / 1e6
pv <- values(pres, mat = FALSE)
dom <- !is.na(pv)
suit <- dom & pv >= thr
add("domain_cells", sum(dom), "cells", "03_present_suitability")
add("domain_km2", round(sum(dom) * cell_km2), "km2", "03_present_suitability")
add("present_suitable_km2", round(sum(suit) * cell_km2), "km2", "03_present_suitability")
add("present_suitable_pct_domain", round(100 * mean(suit[dom]), 1), "%", "03_present_suitability")

pa_shp <- file.path("01_data_raw", "03_vector", "shapefiles", "PA_Bhutan", "PA_Bnd_Final_20230316.shp")
pa <- st_transform(st_make_valid(st_read(pa_shp, quiet = TRUE)), crs(pres))
pa_r <- rasterize(vect(pa), pres, field = 1, background = 0)
pav <- values(pa_r, mat = FALSE) == 1 & dom
add("pa_km2_on_grid", round(sum(pav) * cell_km2), "km2", pa_shp)
add("pa_pct_domain", round(100 * mean(pav[dom]), 1), "%", pa_shp)
add("present_suitable_in_pa_km2", round(sum(suit & pav) * cell_km2), "km2", "present x PA")
add("present_suitable_in_pa_pct", round(100 * sum(suit & pav) / sum(suit), 1), "%", "present x PA")
add("pa_area_suitable_pct", round(100 * sum(suit & pav) / sum(pav), 1), "%", "present x PA")
is_bc <- grepl("^Biological Corridor", pa$park)
bc_r <- rasterize(vect(pa[is_bc, ]), pres, field = 1, background = 0)
bcv <- values(bc_r, mat = FALSE) == 1 & dom
core_pa <- pav & !bcv
add("present_suitable_in_core_pa_pct", round(100 * sum(suit & core_pa) / sum(suit), 1), "%", "present x PA (parks, sanctuaries, reserve)")
add("present_suitable_in_bc_pct", round(100 * sum(suit & bcv) / sum(suit), 1), "%", "present x biological corridors")
bhu <- st_transform(st_make_valid(st_read(file.path("01_data_raw", "03_vector", "shapefiles", "Bhutan", "bhutan.shp"), quiet = TRUE)), crs(pres))
add("bhutan_km2_on_grid", round(sum(values(rasterize(vect(bhu), pres, field = 1, background = 0), mat = FALSE) == 1) * cell_km2), "km2", "bhutan.shp")
by_unit <- do.call(rbind, lapply(seq_len(nrow(pa)), function(i) {
  u <- values(rasterize(vect(pa[i, ]), pres, field = 1, background = 0), mat = FALSE) == 1 & dom
  data.frame(unit = pa$park[i], unit_km2 = sum(u) * cell_km2, suitable_km2 = sum(u & suit) * cell_km2)
}))
by_unit <- by_unit[order(-by_unit$suitable_km2), ]

# ── Future habitat (GCM-mean ensemble and per-GCM spread) ────────────────────
fut_dir <- d("04_future_projections")
ssps <- c("ssp126", "ssp245", "ssp370", "ssp585")
periods <- c("2021_2050", "2051_2080", "2071_2100")
on_grid <- function(f) {
  r <- rast(f)
  if (!isTRUE(compareGeom(r, pres, stopOnError = FALSE))) stop("Not on the present grid: ", f)
  values(r, mat = FALSE)
}
area_of <- function(f) { v <- on_grid(f); sum(dom & !is.na(v) & v >= thr) * cell_km2 }
traj <- list()
for (s in ssps) for (p in periods) {
  f_mean <- file.path(fut_dir, sprintf("future_gcm_ensemble_%s_%s.tif", s, p))
  f_gcm <- list.files(fut_dir, sprintf("^suitability_future_.*_%s_%s_ensemble\\.tif$", s, p), full.names = TRUE)
  stopifnot(file.exists(f_mean), length(f_gcm) == 5)
  a_mean <- area_of(f_mean); a_gcm <- vapply(f_gcm, area_of, numeric(1))
  key <- sprintf("%s_%s", s, p)
  add(paste0("future_area_km2_", key), round(a_mean), "km2", basename(f_mean))
  add(paste0("future_change_pct_", key), round(100 * (a_mean / (sum(suit) * cell_km2) - 1), 1), "%", basename(f_mean))
  add(paste0("future_gcm_area_min_km2_", key), round(min(a_gcm)), "km2", "per-GCM ensembles")
  add(paste0("future_gcm_area_max_km2_", key), round(max(a_gcm)), "km2", "per-GCM ensembles")
  add(paste0("future_gcm_median_change_pct_", key), round(100 * (median(a_gcm) / (sum(suit) * cell_km2) - 1), 1), "%", "per-GCM ensembles")
  add(paste0("future_gcm_n_loss_", key), sum(a_gcm < sum(suit) * cell_km2), "of 5 GCMs", "per-GCM ensembles")
  traj[[key]] <- data.frame(ssp = s, period = p, gcm = sub(sprintf("^suitability_future_(.*)_%s_%s_ensemble\\.tif$", s, p), "\\1", basename(f_gcm)),
                            area_km2 = a_gcm, stringsAsFactors = FALSE)
}
traj <- do.call(rbind, traj)
traj$change_pct <- 100 * (traj$area_km2 / (sum(suit) * cell_km2) - 1)
add("per_gcm_change_pct_min", round(min(traj$change_pct), 1), "%", "per-GCM ensembles (60)")
add("per_gcm_change_pct_max", round(max(traj$change_pct), 1), "%", "per-GCM ensembles (60)")
add("per_gcm_n_projections_loss", sum(traj$change_pct < 0), "projections", "per-GCM ensembles (60)")
for (g in unique(traj$gcm)) {
  x <- traj$change_pct[traj$gcm == g]
  add(sprintf("gcm_%s_change_pct_min", g), round(min(x), 1), "%", "per-GCM ensembles")
  add(sprintf("gcm_%s_change_pct_max", g), round(max(x), 1), "%", "per-GCM ensembles")
  add(sprintf("gcm_%s_n_loss", g), sum(x < 0), "of 12", "per-GCM ensembles")
}

# ── Extrapolation ───────────────────────────────────────────────────────────
ex_files <- list.files(fut_dir, "^extrapolation_.*_ssp[0-9]{3}_[0-9]{4}_[0-9]{4}\\.tif$", full.names = TRUE)
stopifnot(length(ex_files) == 60)
ex_any <- rep(FALSE, length(pv)); ex_frac <- numeric(0)
for (f in ex_files) {
  v <- on_grid(f)
  ex_frac <- c(ex_frac, mean(v[dom] == 1, na.rm = TRUE))
  ex_any <- ex_any | (!is.na(v) & v == 1)
}
add("extrap_frac_per_projection_min_pct", round(100 * min(ex_frac), 1), "%", "extrapolation_*.tif")
add("extrap_frac_per_projection_max_pct", round(100 * max(ex_frac), 1), "%", "extrapolation_*.tif")
add("extrap_any_of_60_pct_domain", round(100 * mean(ex_any[dom]), 1), "%", "extrapolation_*.tif")
add("present_suitable_extrap_any_pct", round(100 * sum(suit & ex_any) / sum(suit), 1), "%", "extrapolation_*.tif")
ex_hab <- vapply(ex_files, function(f) { v <- on_grid(f); mean(v[suit] == 1, na.rm = TRUE) }, numeric(1))
add("present_suitable_extrap_per_projection_median_pct", round(100 * median(ex_hab), 1), "%", "extrapolation_*.tif")
add("present_suitable_extrap_per_projection_min_pct", round(100 * min(ex_hab), 1), "%", "extrapolation_*.tif")
add("present_suitable_extrap_per_projection_max_pct", round(100 * max(ex_hab), 1), "%", "extrapolation_*.tif")

md <- read.csv(d("01_processed_data", "modeling_dataset.csv"))
# Which climate variables leave the calibration range (SSP5-8.5, 2071-2100)
st_clim <- md[, c("BIO05", "BIO15")]
add("bio05_station_max", round(max(st_clim$BIO05, na.rm = TRUE), 1), "degC", "modeling_dataset.csv")
bio_share <- function(g, b) {
  f <- Sys.glob(file.path("01_data_raw", "02_rasters", "future", g, "2071_2100", "ssp585", sprintf("*_%s_v1_0.tif", tolower(b))))[1]
  v <- values(project(rast(f), pres), mat = FALSE)[suit]
  rg <- range(st_clim[[b]], na.rm = TRUE)
  100 * mean(v < rg[1] | v > rg[2], na.rm = TRUE)
}
gcm_dirs <- c("acces_cm2", "cnrm_cm6_1", "inm_cm5_0", "miroc6", "mpi_esm1_2_lr")
b05 <- vapply(gcm_dirs, bio_share, numeric(1), b = "BIO05")
add("bio05_extrap_ssp585_2071_min", round(min(b05)), "%", "future BIO05 vs station range")
add("bio05_extrap_ssp585_2071_max", round(max(b05)), "%", "future BIO05 vs station range")
add("bio15_extrap_ssp585_2071_acces_cm2", round(bio_share("acces_cm2", "BIO15")), "%", "future BIO15 vs station range")

# ── Refugia ─────────────────────────────────────────────────────────────────
chg <- d("05_change_metrics")
core <- on_grid(file.path(chg, "refugia_core.tif")) == 1
high <- on_grid(file.path(chg, "refugia_high_stability.tif")) == 1
cnt <- on_grid(file.path(chg, "refugia_stability_count.tif"))
core[is.na(core)] <- FALSE; high[is.na(high)] <- FALSE
for (nm in c("core", "high")) {
  r <- get(nm)
  add(sprintf("refugia_%s_km2", nm), round(sum(r) * cell_km2), "km2", "05_change_metrics")
  add(sprintf("refugia_%s_pct_present", nm), round(100 * sum(r) / sum(suit), 1), "%", "05_change_metrics")
  add(sprintf("refugia_%s_in_pa_pct", nm), round(100 * sum(r & pav) / sum(r), 1), "%", "refugia x PA")
  add(sprintf("refugia_%s_outside_pa_km2", nm), round(sum(r & !pav) * cell_km2), "km2", "refugia x PA")
  add(sprintf("refugia_%s_extrap_any_pct", nm), round(100 * sum(r & ex_any) / sum(r), 1), "%", "refugia x extrapolation")
}
ex_core <- vapply(ex_files, function(f) { v <- on_grid(f); mean(v[core] == 1, na.rm = TRUE) }, numeric(1))
add("refugia_core_extrap_per_projection_median_pct", round(100 * median(ex_core), 1), "%", "refugia x extrapolation")
elev <- on_grid(d("02_data_intermediate", "elevation_harmonized.tif"))
sv <- project(vect(md, geom = c("longitude", "latitude"), crs = "EPSG:4326"), crs(pres))
st_elev <- extract(rast(d("02_data_intermediate", "elevation_harmonized.tif")), sv, ID = FALSE)[, 1]
add("presence_elevation_max_m", round(max(st_elev[md$response == 1], na.rm = TRUE)), "m", "stations x elevation")
add("absence_stations_above_3000m", sum(st_elev[md$response == 0] > 3000, na.rm = TRUE), "stations", "stations x elevation")
add("present_suitable_above_3000m_km2", round(sum(suit & elev > 3000, na.rm = TRUE) * cell_km2), "km2", "present x elevation")
add("refugia_core_above_3000m_km2", round(sum(core & elev > 3000, na.rm = TRUE) * cell_km2), "km2", "refugia x elevation")
add("present_suitable_below_1000m_pct", round(100 * sum(suit & elev <= 1000, na.rm = TRUE) / sum(suit), 1), "%", "present x elevation")
lost_any <- suit & !is.na(cnt) & cnt < 45
add("present_suitable_below_45_km2", round(sum(lost_any) * cell_km2), "km2", "refugia_stability_count")
add("present_suitable_zero_of_60_km2", round(sum(suit & !is.na(cnt) & cnt == 0) * cell_km2), "km2", "refugia_stability_count")

# ── Potential interaction exposure (same construction as Figure 6, E10) ──────
norm_rast <- function(r) {
  v <- values(r, na.rm = TRUE); lo <- quantile(v, 0.02); hi <- quantile(v, 0.98)
  clamp((r - lo) / (hi - lo), 0, 1, values = TRUE)
}
fp <- resample(rast(d("02_data_intermediate", "human_footprint_harmonized.tif")), pres, method = "bilinear")
hp <- norm_rast(fp)
bldg <- d("02_data_intermediate", "building_footprints_harmonized.gpkg")
if (file.exists(bldg)) {
  b <- rasterize(vect(bldg), pres, field = 1, background = NA)
  hp <- norm_rast(hp + norm_rast(1 / (1 + distance(b) / 1000)))
}
hpv <- values(hp, mat = FALSE)
hp75 <- quantile(hpv[dom], 0.75, na.rm = TRUE)
hi_hp <- dom & !is.na(hpv) & hpv >= hp75
add("present_suitable_in_top25_human_pressure_pct", round(100 * sum(suit & hi_hp) / sum(suit), 1), "%", "present x human pressure")
add("refugia_core_in_top25_human_pressure_pct", round(100 * sum(core & hi_hp) / sum(core), 1), "%", "refugia x human pressure")
add("refugia_core_outside_pa_top25_human_pressure_km2", round(sum(core & !pav & hi_hp) * cell_km2), "km2", "refugia x PA x human pressure")
expo <- sqrt(norm_rast(pres) * norm_rast(hp))
writeRaster(expo, file.path(dirname(out_csv), "interaction_exposure_present.tif"), overwrite = TRUE)
xv <- values(expo, mat = FALSE)
q85 <- quantile(xv[dom], 0.85, na.rm = TRUE)
hi_x <- dom & !is.na(xv) & xv >= q85
add("exposure_present_mean", round(mean(xv[dom], na.rm = TRUE), 3), "", "Figure 6 construction")
add("exposure_top15_threshold", round(q85, 3), "", "85th percentile")
add("present_suitable_in_top15_exposure_pct", round(100 * sum(suit & hi_x) / sum(suit), 1), "%", "present x exposure")
add("refugia_core_in_top15_exposure_pct", round(100 * sum(core & hi_x) / sum(core), 1), "%", "refugia x exposure")
add("refugia_core_outside_pa_top15_exposure_km2", round(sum(core & !pav & hi_x) * cell_km2), "km2", "refugia x PA x exposure")

hwc_file <- file.path("01_data_raw", "04_conflicts", "HWC_Species_Elephant_clean.csv")
if (file.exists(hwc_file)) {
  hw <- read.csv(hwc_file, stringsAsFactors = FALSE)
  hw <- hw[is.finite(hw$longitude) & is.finite(hw$latitude) & hw$longitude != 0, ]
  hv <- project(vect(hw, geom = c("longitude", "latitude"), crs = "EPSG:4326"), crs(pres))
  hx <- extract(expo, hv, ID = FALSE)[, 1]
  hs <- extract(pres, hv, ID = FALSE)[, 1]
  ok <- !is.na(hx)
  add("hwc_incidents_in_domain", sum(ok), "incidents", hwc_file)
  add("hwc_year_min", min(hw$year, na.rm = TRUE), "", hwc_file)
  add("hwc_year_max", max(hw$year, na.rm = TRUE), "", hwc_file)
  add("hwc_in_top15_exposure_pct", round(100 * mean(hx[ok] >= q85), 1), "%", "incidents x exposure")
  add("hwc_exposure_percentile_median", round(100 * median(ecdf(xv[dom])(hx[ok]))), "percentile", "incidents x exposure")
  add("hwc_in_suitable_habitat_pct", round(100 * mean(hs[ok] >= thr, na.rm = TRUE), 1), "%", "incidents x present habitat")
}

ledger <- do.call(rbind, ledger)
ledger$run_id <- run_id
dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
write.csv(ledger, out_csv, row.names = FALSE)
write.csv(traj, sub("\\.csv$", "_gcm_trajectories.csv", out_csv), row.names = FALSE)
write.csv(by_unit, sub("\\.csv$", "_pa_units.csv", out_csv), row.names = FALSE)
cat(sprintf("Wrote %d values to %s\n", nrow(ledger), out_csv))
