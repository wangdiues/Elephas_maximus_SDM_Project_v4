#!/usr/bin/env Rscript
# 26_revision_sensitivity.R — stage 2 of the JWM major revision.
# Threshold robustness, extrapolation classes, numeric audit, predictor-screening audit,
# survey heterogeneity, occupancy check, spatial cross-validation audit, nested thresholds,
# and the interaction-exposure audit. Reads stage 1 caches; never writes to the accepted run.
# Usage (PowerShell, repo root):
#   Rscript 03_analysis/26_revision_sensitivity.R 04_outputs/runs/RUN_<id>

suppressPackageStartupMessages({ library(terra); library(sf); library(yaml); library(ranger); library(gbm); library(ggplot2); library(unmarked) })
source(file.path("03_analysis", "00_sdm_helpers.R"))
source(file.path("03_analysis", "00_publication_helpers.R"))
run <- normalizePath(commandArgs(trailingOnly = TRUE)[[1]], winslash = "/", mustWork = TRUE)
d <- function(...) file.path(run, ...)
cfg <- read_yaml("00_governance/config.yaml")
out <- file.path("04_outputs", "revision_jwm"); tb <- file.path(out, "tables"); cache <- file.path(out, "cache")
fig <- file.path(out, "figures"); dir.create(fig, showWarnings = FALSE)
led <- list()
add <- function(key, value, source = "") led[[length(led) + 1]] <<- data.frame(key = key, value = as.character(value), source = source)
wr <- function(x, f) write.csv(x, file.path(tb, f), row.names = FALSE)
SEED <- 123456L; FIT_SEED <- 123459L

auc <- function(y, p) { r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
tss_at <- function(y, p, t) mean(p[y == 1] >= t) + mean(p[y == 0] < t) - 1
best_thr <- function(y, p) { ct <- sort(unique(p)); ct[which.max(vapply(ct, function(t) tss_at(y, p, t), 0))] }

ev <- read.csv(d("02_models", "evaluation_all.csv")); thr <- ev$threshold[ev$algorithm == "ensemble"][1]
add("threshold_exact", signif(thr, 6), "evaluation_all.csv")

# ── Shared cell data ─────────────────────────────────────────────────────────
st <- readRDS(file.path(cache, "static.rds")); cells <- st$cells
S <- readRDS(file.path(cache, "S.rds")); E <- readRDS(file.path(cache, "E.rds"))
pres <- rast(d("03_present_suitability", "suitability_present_ensemble.tif"))
pv <- values(pres, mat = FALSE)[cells]; cell <- prod(res(pres)) / 1e6
pa <- st_transform(st_make_valid(st_read(cfg$vectors$protected_areas, quiet = TRUE)), crs(pres))
pav <- values(rasterize(vect(pa), pres, field = 1, background = 0), mat = FALSE)[cells] == 1
nr <- function(r) { v <- values(r, na.rm = TRUE); q <- quantile(v, c(0.02, 0.98)); clamp((r - q[1]) / (q[2] - q[1]), 0, 1, values = TRUE) }
fp <- resample(rast(d("02_data_intermediate", "human_footprint_harmonized.tif")), pres, method = "bilinear")
bl <- rasterize(vect(d("02_data_intermediate", "building_footprints_harmonized.gpkg")), pres, field = 1, background = NA)
db <- distance(bl) / 1000
prox <- nr(1 / (1 + db)); fpn <- nr(fp); hp <- nr(fpn + prox)
hpv <- values(hp, mat = FALSE)[cells]; top25 <- !is.na(hpv) & hpv >= quantile(hpv, 0.75, na.rm = TRUE)

# ── 2.2 Threshold robustness and 2.1 extrapolation classes ───────────────────
primary_core <- pv >= thr & rowSums(S >= thr, na.rm = TRUE) == 60
never <- rowSums(E) == 0
thr_rows <- list(); core_by <- list()
for (t in c(0.10, thr, 0.15, 0.20)) {
  h <- pv >= t; cnt <- rowSums(S >= t, na.rm = TRUE); core <- h & cnt == 60
  core_by[[as.character(round(t, 6))]] <- core
  thr_rows[[length(thr_rows) + 1]] <- data.frame(
    threshold = t, habitat_km2 = sum(h) * cell, habitat_outside_pa_pct = 100 * mean(!pav[h]),
    core_km2 = sum(core) * cell, core_pct_of_habitat = 100 * sum(core) / sum(h), core_outside_pa_pct = 100 * mean(!pav[core]),
    core_outside_pa_km2 = sum(core & !pav) * cell, core_top25_km2 = sum(core & top25) * cell, core_outside_pa_top25_km2 = sum(core & !pav & top25) * cell,
    core_jaccard_vs_primary = sum(core & primary_core) / sum(core | primary_core),
    primary_core_retained_pct = 100 * sum(core & primary_core) / sum(primary_core),
    core_never_extrapolated_km2 = sum(core & never) * cell, core_never_extrapolated_pct = 100 * sum(core & never) / sum(core),
    core_never_outside_pa_pct = 100 * mean(!pav[core & never]))
}
tt <- do.call(rbind, thr_rows); wr(tt, "threshold_refugia_sensitivity.csv"); print(tt)

n_ex <- rowSums(E)
core <- primary_core; h <- pv >= thr
add("core_never_extrap_km2", round(sum(core & n_ex == 0) * cell), "S,E caches")
add("core_never_extrap_pct", round(100 * mean(n_ex[core] == 0), 1), "S,E caches")
add("core_extrap_any_km2", round(sum(core & n_ex > 0) * cell), "S,E caches")
add("core_extrap_any_pct", round(100 * mean(n_ex[core] > 0), 1), "S,E caches")
add("core_extrap_gt30_pct", round(100 * mean(n_ex[core] > 30), 1), "S,E caches")
add("core_extrap_all60_pct", round(100 * mean(n_ex[core] == 60), 1), "S,E caches")
add("core_never_extrap_outside_pa_pct", round(100 * mean(!pav[core & n_ex == 0]), 1), "S,E caches")
add("core_never_extrap_outside_pa_km2", round(sum(core & n_ex == 0 & !pav) * cell), "S,E caches")
add("habitat_never_extrap_pct", round(100 * mean(n_ex[h] == 0), 1), "S,E caches")

# Persistence counted only where the cell lies within sampled climate
inr <- !E; ok_n <- rowSums(inr); suit_in <- rowSums((S >= thr) & inr, na.rm = TRUE)
cons <- h & ok_n > 0 & suit_in == ok_n
add("core_in_range_only_rule_km2", round(sum(cons) * cell), "S,E caches")
add("habitat_no_in_range_projection_km2", round(sum(h & ok_n == 0) * cell), "S,E caches")
ex_tab <- data.frame(class = c("Core, within sampled climate in all 60", "Core, outside sampled climate in 1-30", "Core, outside sampled climate in 31-59", "Core, outside sampled climate in all 60"),
  km2 = c(sum(core & n_ex == 0), sum(core & n_ex >= 1 & n_ex <= 30), sum(core & n_ex >= 31 & n_ex <= 59), sum(core & n_ex == 60)) * cell)
ex_tab$pct_of_core <- 100 * ex_tab$km2 / (sum(core) * cell)
ex_tab$outside_pa_pct <- c(100 * mean(!pav[core & n_ex == 0]), 100 * mean(!pav[core & n_ex >= 1 & n_ex <= 30]), 100 * mean(!pav[core & n_ex >= 31 & n_ex <= 59]), 100 * mean(!pav[core & n_ex == 60]))
wr(ex_tab, "core_refugia_extrapolation_classes.csv"); print(ex_tab)

# Uncertainty map: threshold agreement and reliance on extrapolation
cls <- rep(NA_integer_, length(cells))
all_t <- core_by[[1]] & core_by[[2]] & core_by[[3]] & core_by[[4]]
any_t <- core_by[[1]] | core_by[[2]] | core_by[[3]] | core_by[[4]]
cls[any_t] <- 1L; cls[all_t & n_ex > 0] <- 2L; cls[all_t & n_ex == 0] <- 3L
map <- rast(pres); values(map) <- NA_real_; map[cells] <- cls
add("core_all_thresholds_km2", round(sum(all_t) * cell), "threshold sweep"); add("core_any_threshold_km2", round(sum(any_t) * cell), "threshold sweep")
add("core_all_thresholds_never_extrap_km2", round(sum(all_t & n_ex == 0) * cell), "threshold sweep")
add("core_all_thresholds_outside_pa_pct", round(100 * mean(!pav[all_t]), 1), "threshold sweep")
xy <- as.data.frame(map, xy = TRUE, na.rm = TRUE); names(xy)[3] <- "class"
xy$class <- factor(xy$class, 1:3, c("Core at some thresholds only", "Core at all thresholds; relies on extrapolation", "Core at all thresholds; within sampled climate"))
aoi <- st_transform(st_read(cfg$vectors$aoi_bhutan, quiet = TRUE), crs(pres))
pm <- ggplot() + geom_sf(data = aoi, fill = "grey95", colour = "#111111", linewidth = 0.35) +
  geom_tile(data = xy, aes(x, y, fill = class), width = 250, height = 250) +
  geom_sf(data = pa, fill = NA, colour = "grey30", linewidth = 0.18) +
  scale_fill_manual(values = c("#E8C75A", "#D55E00", "#0072B2"), name = NULL) + coord_sf(crs = st_crs(aoi), expand = FALSE) +
  labs(x = NULL, y = NULL) + publication_theme(base_size = 9) +
  theme(panel.grid = element_blank(), legend.position = "bottom", legend.direction = "vertical")
save_publication_figure(pm, file.path(fig, "Figure_S2_refugia_uncertainty"), width_mm = 178, height_mm = 125, line_art_dpi = 300)

# ── Numeric audit ────────────────────────────────────────────────────────────
dzo <- st_read(file.path(cfg$vectors$root, "Dzongkhag Boundary", "Dzongkhag Boundary.shp"), quiet = TRUE)
st_crs(dzo) <- 3857L; dzo <- st_transform(st_make_valid(dzo), crs(pres))
ext_df <- extract(pres, vect(dzo), ID = TRUE, cells = TRUE); names(ext_df)[2] <- "s"
ins <- unique(ext_df$cell[!is.na(ext_df$s)])
dom_cells <- cells; suit_cells <- cells[pv >= thr]
add("habitat_total_km2_exact", round(length(suit_cells) * cell, 2), "present map")
add("habitat_cells", length(suit_cells), "present map")
add("district_sum_km2", round(sum(ext_df$s >= thr, na.rm = TRUE) * cell, 2), "cell-centre extract by district")
add("habitat_outside_all_districts_km2", round(sum(!suit_cells %in% ins) * cell, 2), "cell-centre extract by district")
add("habitat_cells_in_two_districts", sum(duplicated(ext_df$cell[!is.na(ext_df$s) & ext_df$s >= thr])), "cell-centre extract by district")
add("area_at_rounded_threshold_km2", round(sum(pv >= round(thr, 3)) * cell, 2), "present map")
add("area_at_exact_threshold_km2", round(sum(pv >= thr) * cell, 2), "present map")
pu <- read.csv(file.path("05_manuscripts", "manuscript_claim_ledger_pa_units.csv"))
add("pa_unit_sum_raw_km2", round(sum(pu$suitable_km2), 2), "pa_units ledger")
add("pa_unit_sum_rounded_rows_km2", sum(round(pu$suitable_km2)), "pa_units ledger")
add("pa_union_km2", round(sum(pv >= thr & pav) * cell, 2), "rasterized union")
add("pa_unit_overlap_check_cells", sum(lengths(st_intersects(st_point_on_surface(pa), pa)) > 1), "sf intersects")

# Predictor screening
ms <- read.csv(d("02_models", "../01_processed_data/modeling_dataset.csv"))
pcols <- st$pred_cols
fill <- ms; for (p in pcols) fill[[p]][is.na(fill[[p]])] <- median(fill[[p]], na.rm = TRUE)
Rm <- abs(cor(fill[, pcols])); diag(Rm) <- 0
vif <- vapply(pcols, function(p) 1 / (1 - summary(lm(reformulate(setdiff(pcols, p), p), data = fill))$r.squared), 0)
ps <- data.frame(predictor = pcols, vif = round(vif, 2), max_abs_r = round(apply(Rm, 1, max), 3), most_correlated = pcols[apply(Rm, 1, which.max)])
wr(ps[order(-ps$vif), ], "retained_predictor_diagnostics.csv"); print(ps[order(-ps$vif), ], row.names = FALSE)
add("final_vif_max", round(max(vif), 2), "modeling dataset"); add("final_max_abs_r", round(max(Rm), 3), "modeling dataset")
add("landcover_class_values", paste(names(table(ms$landcover)), table(ms$landcover), sep = ":", collapse = " "), "modeling dataset")

# ── 2.3 Survey heterogeneity ─────────────────────────────────────────────────
oof <- read.csv(d("01_processed_data", "oof_predictions.csv"))
occ <- read.csv(file.path("01_data_raw", "01_occurrences", "elephant_PA_data.csv"), stringsAsFactors = FALSE)
book <- read.csv(file.path("01_data_raw", "01_occurrences", "Source_Files", "Book1.csv"), stringsAsFactors = FALSE, check.names = FALSE)
tig <- read.csv(file.path("01_data_raw", "01_occurrences", "Source_Files", "elephant_presence_absence_by_station.csv"), stringsAsFactors = FALSE)
kk <- function(lon, lat) paste(round(lon, 5), round(lat, 5))
ix <- match(kk(ms$longitude, ms$latitude), kk(occ$longitude, occ$latitude)); ms$survey <- ifelse(occ$source[ix] == "book1_survey", "elephant", "tiger"); ms$station <- occ$station_id[ix]
ok_ix <- match(kk(ms$longitude, ms$latitude), kk(oof$longitude, oof$latitude)); ms$oof <- oof$pred_ensemble[ok_ix]
fill$survey <- ms$survey; fill$station <- ms$station; fill$oof <- ms$oof
el_r <- rast(d("02_data_intermediate", "elevation_harmonized.tif"))
ms$elev <- extract(el_r, project(vect(ms, geom = c("longitude", "latitude"), crs = "EPSG:4326"), crs(el_r)), ID = FALSE)[, 1]
fill$elev <- ms$elev
occ_cols <- names(book)[grepl("^[0-9]+$", names(book))]
bm <- as.matrix(sapply(book[, occ_cols], function(v) suppressWarnings(as.numeric(v))))
book$n_valid <- rowSums(!is.na(bm)); book$n_det <- rowSums(bm > 0, na.rm = TRUE); book$cam <- trimws(book$camera)
tig$tn <- suppressWarnings(as.numeric(tig$trap_nights)); tig$tn_ok <- is.finite(tig$tn) & tig$tn >= 1 & tig$tn <= 150
sv <- data.frame(
  quantity = c("Stations modeled", "Presences", "Presence frequency", "Median elevation (m)", "Elevation range (m)", "Median warmest-month maximum (C)",
               "Effort available (stations)", "Median active occasions or trap nights", "Effort range", "Dung records"),
  elephant = c(sum(fill$survey == "elephant"), sum(fill$response[fill$survey == "elephant"]), round(mean(fill$response[fill$survey == "elephant"]), 3),
               round(median(fill$elev[fill$survey == "elephant"], na.rm = TRUE)), paste(round(range(fill$elev[fill$survey == "elephant"], na.rm = TRUE)), collapse = "-"),
               round(median(fill$BIO05[fill$survey == "elephant"]), 1),
               sum(book$cam != "Dung" & book$n_valid > 0), median(book$n_valid[book$cam != "Dung" & book$n_valid > 0]),
               paste(range(book$n_valid[book$cam != "Dung" & book$n_valid > 0]), collapse = "-"), sum(book$cam == "Dung")),
  tiger = c(sum(fill$survey == "tiger"), sum(fill$response[fill$survey == "tiger"]), round(mean(fill$response[fill$survey == "tiger"]), 3),
            round(median(fill$elev[fill$survey == "tiger"], na.rm = TRUE)), paste(round(range(fill$elev[fill$survey == "tiger"], na.rm = TRUE)), collapse = "-"),
            round(median(fill$BIO05[fill$survey == "tiger"]), 1),
            sum(tig$tn_ok), median(tig$tn[tig$tn_ok]), paste(range(tig$tn[tig$tn_ok]), collapse = "-"), 0))
wr(sv, "survey_comparison.csv"); print(sv)
add("tiger_effort_invalid_rows", sum(!tig$tn_ok), "station table"); add("tiger_rows", nrow(tig), "station table")

# Overlap region: stations in 15-km blocks sampled by both surveys
bid <- read.csv(d("01_processed_data", "fold_assignments.csv"))
blk <- bid$block_id[match(kk(fill$longitude, fill$latitude), kk(bid$longitude, bid$latitude))]
both <- intersect(blk[fill$survey == "elephant"], blk[fill$survey == "tiger"])
ov <- blk %in% both
ovt <- data.frame(survey = c("elephant", "tiger"), blocks_shared = length(both),
  stations = c(sum(ov & fill$survey == "elephant"), sum(ov & fill$survey == "tiger")),
  presences = c(sum(fill$response[ov & fill$survey == "elephant"]), sum(fill$response[ov & fill$survey == "tiger"])))
ovt$presence_frequency <- round(ovt$presences / ovt$stations, 3); wr(ovt, "survey_overlap_blocks.csv"); print(ovt)
add("overlap_blocks", length(both), "fold assignments"); add("overlap_elephant_freq", ovt$presence_frequency[1], "fold assignments"); add("overlap_tiger_freq", ovt$presence_frequency[2], "fold assignments")
ovm <- glm(response ~ survey, binomial, fill[ov, ])
add("overlap_survey_or_elephant_vs_tiger", round(exp(-coef(ovm)[[2]]), 2), "GLM in shared blocks")
add("overlap_survey_or_ci", paste(round(exp(-rev(confint.default(ovm)[2, ])), 2), collapse = "-"), "GLM in shared blocks")

# Effort versus detection within elephant elevations, tiger stations with usable effort
tig$key <- kk(tig$longitude, tig$latitude)
fz <- fill[fill$survey == "tiger" & fill$elev <= max(fill$elev[fill$response == 1], na.rm = TRUE), ]
fz$tn <- tig$tn[match(kk(fz$longitude, fz$latitude), tig$key)]; fz$tn[!(is.finite(fz$tn) & fz$tn >= 1 & fz$tn <= 150)] <- NA
fz2 <- fz[!is.na(fz$tn), ]
em <- glm(response ~ log(tn) + qlogis(pmin(pmax(oof, 1e-4), 1 - 1e-4)), binomial, fz2); ec <- summary(em)$coefficients
add("effort_n", nrow(fz2), "tiger stations with usable effort below highest detection")
add("effort_or_per_doubling", round(exp(ec[2, 1] * log(2)), 2), "GLM with OOF offset-like covariate")
add("effort_or_ci", paste(round(exp((ec[2, 1] + c(-1.96, 1.96) * ec[2, 2]) * log(2)), 2), collapse = "-"), "GLM")
add("effort_p", signif(ec[2, 4], 2), "GLM")

# Single-season occupancy, elephant survey camera detection histories only
cam <- book[book$cam != "Dung" & book$n_valid > 0, ]
ids <- trimws(cam$station); keep <- ids %in% occ$station_id[occ$source == "book1_survey"]
Y <- (bm[match(ids[keep], trimws(book$station)), ] > 0) * 1
weekly <- function(row, w = 7) { idx <- split(seq_along(row), ceiling(seq_along(row) / w)); vapply(idx, function(i) { v <- row[i]; if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE) }, 0) }
Yw <- t(apply(Y, 1, weekly))
um <- unmarkedFrameOccu(y = Yw)
o0 <- occu(~1 ~ 1, um, se = TRUE)
psi <- backTransform(o0, "state"); p_w <- backTransform(o0, "det")
pc <- p_w@estimate; nw <- rowSums(!is.na(Yw))
add("occu_stations", nrow(Yw), "Book1 camera stations in modeled data"); add("occu_naive", round(mean(rowSums(Yw, na.rm = TRUE) > 0), 3), "Book1")
add("occu_psi", round(psi@estimate, 3), "unmarked occu ~1~1, weekly occasions")
add("occu_psi_ci", paste(round(plogis(coef(o0, "state") + c(-1.96, 1.96) * sqrt(diag(vcov(o0, "state")))), 3), collapse = "-"), "unmarked")
add("occu_p_week", round(pc, 3), "unmarked"); add("occu_p_week_ci", paste(round(plogis(coef(o0, "det") + c(-1.96, 1.96) * sqrt(diag(vcov(o0, "det")))), 3), collapse = "-"), "unmarked")
add("occu_median_weeks", median(nw), "Book1"); add("occu_cum_detect", round(1 - (1 - pc)^median(nw), 3), "unmarked")
add("occu_false_nondetect_share", round((psi@estimate * (1 - pc)^median(nw)) / (1 - psi@estimate + psi@estimate * (1 - pc)^median(nw)), 3), "share of non-detection stations expected occupied, constant p")

# ── Common refit functions ───────────────────────────────────────────────────
fm <- as.formula(paste("response ~", paste(pcols, collapse = " + ")))
fit3 <- function(x, f = fm, mtry = floor(sqrt(length(pcols)))) {
  set.seed(SEED)
  g <- suppressWarnings(glm(f, data = x, family = binomial()))
  r <- ranger(f, data = x, num.trees = 1000, mtry = mtry, seed = FIT_SEED, num.threads = 2)
  b <- gbm(f, data = x, distribution = "bernoulli", n.trees = 1000, interaction.depth = 3, shrinkage = 0.01, n.minobsinnode = 5, bag.fraction = 0.75, verbose = FALSE)
  it <- suppressWarnings(suppressMessages(gbm.perf(b, method = "OOB", plot.it = FALSE)))
  list(g = g, r = r, b = b, it = if (is.na(it) || it < 1) 500L else it)
}
pr3 <- function(m, x) rowMeans(cbind(as.numeric(predict(m$g, newdata = x, type = "response")), as.numeric(predict(m$r, data = x)$predictions),
                                     as.numeric(predict(m$b, newdata = x, n.trees = m$it, type = "response"))))

# Survey transfer: train on 1 survey, score on the other (within shared elevations)
ti <- fill$survey == "tiger"; lim_el <- max(fill$elev[fill$response == 1], na.rm = TRUE)
m_t <- fit3(fill[ti, ]); p_e <- pr3(m_t, fill[!ti, ])
m_e <- fit3(fill[!ti, ]); p_t <- pr3(m_e, fill[ti, ])
tin <- ti & fill$elev <= lim_el
add("transfer_tiger_to_elephant_auc", round(auc(fill$response[!ti], p_e), 3), "train tiger, test elephant")
add("transfer_elephant_to_tiger_auc", round(auc(fill$response[ti], p_t), 3), "train elephant, test tiger")
add("transfer_elephant_to_tiger_within_auc", round(auc(fill$response[tin], p_t[fill$elev[ti] <= lim_el & !is.na(fill$elev[ti])]), 3), "test tiger stations at or below highest detection")

# Survey identity as a covariate
sv_f <- update(fm, . ~ . + survey_e); fx <- fill; fx$survey_e <- as.numeric(fill$survey == "elephant")
ov_fold <- rep(NA_real_, nrow(fx))
for (k in sort(unique(fx$fold))) ov_fold[fx$fold == k] <- pr3(fit3(fx[fx$fold != k, ], sv_f, floor(sqrt(length(pcols) + 1))), fx[fx$fold == k, ])
add("survey_covariate_auc", round(auc(fx$response, ov_fold), 3), "spatial CV with survey identity")
add("survey_covariate_within_auc", round(auc(fx$response[fx$elev <= lim_el], ov_fold[fx$elev <= lim_el]), 3), "spatial CV with survey identity")
add("survey_covariate_elephant_auc", round(auc(fx$response[!ti], ov_fold[!ti]), 3), "spatial CV with survey identity")
gs <- suppressWarnings(glm(sv_f, binomial, fx)); gc0 <- summary(gs)$coefficients["survey_e", ]
add("survey_covariate_glm_or", round(exp(gc0[1]), 2), "full GLM"); add("survey_covariate_glm_ci", paste(round(exp(gc0[1] + c(-1.96, 1.96) * gc0[2]), 2), collapse = "-"), "full GLM")

# ── 2.4 Spatial cross-validation audit ───────────────────────────────────────
XY <- st_coordinates(st_transform(st_as_sf(fill, coords = c("longitude", "latitude"), crs = 4326), 32645))
elev_ok <- fill$elev <= lim_el & !is.na(fill$elev)
moran5 <- function(e) {
  D <- as.matrix(dist(XY)); diag(D) <- Inf; nn <- t(apply(D, 1, function(v) order(v)[1:5])); e <- e - mean(e)
  sum(vapply(seq_along(e), function(i) sum(e[i] * e[nn[i, ]]), 0)) / (5 * sum(e^2))
}
inner_thr <- function(x, inner) {
  p <- rep(NA_real_, nrow(x))
  for (j in sort(unique(inner))) p[inner == j] <- pr3(fit3(x[inner != j, ]), x[inner == j, ])
  best_thr(x$response, p)
}
evaluate <- function(bs) {
  df <- data.frame(longitude = fill$longitude, latitude = fill$latitude, type = ifelse(fill$response == 1, "presence", "absence"))
  sb <- create_spatial_blocks(df, bs, 5, SEED); f <- as.integer(sb)
  bid_b <- attr(sb, "block_id"); x_b <- attr(sb, "x_breaks"); y_b <- attr(sb, "y_breaks")
  p <- rep(NA_real_, nrow(fill)); nd <- rep(NA_real_, nrow(fill)); t_nest <- numeric(5)
  for (k in 1:5) {
    tr <- fill[f != k, ]; te <- f == k
    p[te] <- pr3(fit3(tr), fill[te, ])
    dm <- as.matrix(dist(rbind(XY[te, ], XY[f != k, ])))[seq_len(sum(te)), sum(te) + seq_len(sum(f != k))]
    nd[te] <- apply(dm, 1, min) / 1000
    inner <- as.integer(create_spatial_blocks(df[f != k, ], bs, 4, SEED))
    t_nest[k] <- inner_thr(tr, inner)
  }
  conf <- function(ts) { tp <- fn <- tn <- fp <- 0
    for (k in 1:5) { te <- f == k; yk <- fill$response[te]; pk <- p[te]
      tp <- tp + sum(yk == 1 & pk >= ts[k]); fn <- fn + sum(yk == 1 & pk < ts[k]); tn <- tn + sum(yk == 0 & pk < ts[k]); fp <- fp + sum(yk == 0 & pk >= ts[k]) }
    tp / (tp + fn) + tn / (tn + fp) - 1 }
  t_cross <- vapply(1:5, function(k) best_thr(fill$response[f != k], p[f != k]), 0)
  pc <- pmin(pmax(p, 1e-3), 1 - 1e-3); y <- fill$response
  data.frame(block_km = bs, width_km = round(diff(x_b)[1] / 1000, 2), height_km = round(diff(y_b)[1] / 1000, 2), n_blocks = length(unique(bid_b)),
    presences_min = min(tabulate(f[y == 1], 5)), presences_max = max(tabulate(f[y == 1], 5)), nondetections_min = min(tabulate(f[y == 0], 5)), nondetections_max = max(tabulate(f[y == 0], 5)),
    auc = auc(y, p), tss_pooled_opt = max(vapply(sort(unique(p)), function(t) tss_at(y, p, t), 0)),
    tss_crossfit = conf(t_cross), tss_nested = conf(t_nest), threshold_nested_min = min(t_nest), threshold_nested_max = max(t_nest),
    brier = mean((p - y)^2), calibration_slope = coef(suppressWarnings(glm(y ~ qlogis(pc), binomial)))[[2]], moran5 = moran5(y - p),
    auc_within = auc(y[elev_ok], p[elev_ok]), auc_elephant = auc(y[!ti], p[!ti]), auc_tiger = auc(y[ti], p[ti]),
    nearest_train_km_median = median(nd), nearest_train_lt5km_pct = 100 * mean(nd < 5), nearest_train_lt10km_pct = 100 * mean(nd < 10))
}
cvt <- do.call(rbind, lapply(c(10, 15, 20, 25, 30), function(b) { cat("blocks", b, "\n"); r <- evaluate(b); write.csv(r, file.path(tb, sprintf("cv_block_%d.csv", b)), row.names = FALSE); r }))
wr(cvt, "cv_block_size_sensitivity.csv"); print(t(cvt))

# Spatial block bootstrap of pooled held-out metrics (accepted predictions)
bb <- fill$fold; blk_id <- blk
boot_ci <- function(sel, B = 2000) {
  ub <- unique(blk_id[sel]); idx <- split(which(sel), blk_id[sel]); set.seed(SEED)
  v <- t(replicate(B, { s <- unlist(idx[sample(as.character(ub), replace = TRUE)], use.names = FALSE); y <- fill$response[s]; p <- fill$oof[s]
    if (length(unique(y)) < 2) c(NA, NA) else c(auc(y, p), tss_at(y, p, thr)) }))
  round(c(apply(v, 2, quantile, c(0.025, 0.975), na.rm = TRUE)), 3)
}
for (nm in c("all", "within", "below1000", "elephant", "tiger")) {
  sel <- switch(nm, all = rep(TRUE, nrow(fill)), within = elev_ok, below1000 = fill$elev <= 1000 & !is.na(fill$elev), elephant = !ti, tiger = ti)
  ci <- boot_ci(sel)
  add(paste0("boot_auc_", nm), paste(ci[1:2], collapse = "-"), "2,000 block bootstrap"); add(paste0("boot_tss_", nm), paste(ci[3:4], collapse = "-"), "2,000 block bootstrap")
}

# Residual autocorrelation by distance band (projected)
D <- as.matrix(dist(XY)) / 1000; e <- (fill$response - fill$oof); e <- e - mean(e)
mb <- do.call(rbind, lapply(list(c(0, 5), c(5, 10), c(10, 20), c(20, 40), c(40, 80)), function(b) {
  W <- D > b[1] & D <= b[2]; diag(W) <- FALSE
  data.frame(band_km = paste(b, collapse = "-"), pairs = sum(W) / 2, moran = round((length(e) / sum(W)) * sum(W * outer(e, e)) / sum(e^2), 3))
})); wr(mb, "residual_moran_by_distance.csv"); print(mb)

# ── 2.7 Interaction-exposure audit ───────────────────────────────────────────
sn <- nr(pres); ex <- sqrt(sn * hp); xv <- values(ex, mat = FALSE)[cells]
fx2 <- values(rast(file.path("05_manuscripts", "interaction_exposure_present.tif")), mat = FALSE)[cells]
add("exposure_recalc_r", round(cor(xv, fx2, use = "complete"), 4), "rebuilt versus manuscript raster")
snv <- values(sn, mat = FALSE)[cells]
add("cor_suitability_humanpressure", round(cor(snv, hpv, use = "complete", method = "spearman"), 3), "domain cells")
add("cor_hp_components", round(cor(values(fpn, mat = FALSE)[cells], values(prox, mat = FALSE)[cells], use = "complete", method = "spearman"), 3), "domain cells")
add("prox_at_1km", round(1 / 2, 2), "formula"); add("db_median_km", round(median(values(db, mat = FALSE)[cells], na.rm = TRUE), 2), "domain cells")
hw <- read.csv(file.path("01_data_raw", "04_conflicts", "HWC_Species_Elephant_clean.csv"), stringsAsFactors = FALSE)
hw <- hw[is.finite(hw$longitude) & is.finite(hw$latitude) & hw$longitude != 0, ]
hv <- project(vect(hw, geom = c("longitude", "latitude"), crs = "EPSG:4326"), crs(pres))
hc <- cellFromXY(pres, crds(hv)); inc <- hc %in% cells
add("hwc_rows_geolocated", nrow(hw), "SMART export"); add("hwc_in_domain", sum(inc), "SMART export")
add("hwc_unique_cells", length(unique(hc[inc])), "SMART export")
tab5 <- table(paste(floor(crds(hv)[inc, 1] / 5000), floor(crds(hv)[inc, 2] / 5000)))
add("hwc_occupied_5km_cells", length(tab5), "SMART export"); add("hwc_top5km_share_pct", round(100 * max(tab5) / sum(tab5), 1), "SMART export")
add("hwc_by_year", paste(names(table(hw$year[inc])), table(hw$year[inc]), sep = ":", collapse = " "), "SMART export")
add("hwc_by_district", paste(names(sort(table(hw$dzongkhag[inc]), decreasing = TRUE))[1:5], sort(table(hw$dzongkhag[inc]), decreasing = TRUE)[1:5], sep = ":", collapse = " "), "SMART export")

set.seed(SEED)
# Reference: occupied landscape, cells within 1 km of a building in southern elevations
dbv <- values(db, mat = FALSE)[cells]; elv <- values(project(el_r, pres, method = "bilinear"), mat = FALSE)[cells]
refs <- list(all_domain = rep(TRUE, length(cells)), settled = !is.na(dbv) & dbv <= 1, settled_foothill = !is.na(dbv) & dbv <= 1 & !is.na(elv) & elv <= lim_el,
             settled_suitable = !is.na(dbv) & dbv <= 1 & pv >= thr)
cidx <- match(hc[inc], cells); labs <- paste(floor(crds(hv)[inc, 1] / 5000), floor(crds(hv)[inc, 2] / 5000))
inc_auc <- function(score, ri) {
  r <- score[ri]; r <- r[is.finite(r)]; x <- score[cidx]; x <- x[is.finite(x)]
  mean(vapply(x, function(z) mean(r < z) + 0.5 * mean(r == z), 0))
}
rows <- list()
for (nm in names(refs)) for (sc in c("index", "suitability", "human_pressure")) {
  sv2 <- switch(sc, index = xv, suitability = snv, human_pressure = hpv)
  a <- inc_auc(sv2, refs[[nm]])
  ug <- unique(labs); refv <- sv2[refs[[nm]]]; refv <- refv[is.finite(refv)]; base <- vapply(sv2[cidx], function(z) mean(refv < z) + 0.5 * mean(refv == z), 0)
  sp <- split(base, labs); b <- replicate(2000, mean(unlist(sp[sample(ug, replace = TRUE)], use.names = FALSE), na.rm = TRUE))
  rows[[length(rows) + 1]] <- data.frame(reference = nm, n_reference_cells = sum(refs[[nm]]), score = sc, auc = round(a, 3), ci_low = round(quantile(b, 0.025), 3), ci_high = round(quantile(b, 0.975), 3))
}
ia <- do.call(rbind, rows); rownames(ia) <- NULL; wr(ia, "conflict_reference_comparison.csv"); print(ia)

saveRDS(list(hpv = hpv, snv = snv, cidx = cidx, settled_foothill = refs$settled_foothill, pav = pav, top25 = top25, thr = thr, primary_core = primary_core),
        file.path(cache, "stage2_for_variants.rds"))
write.csv(do.call(rbind, led), file.path(out, "revision_numbers_stage2.csv"), row.names = FALSE)
cat("Stage 2 complete\n")
