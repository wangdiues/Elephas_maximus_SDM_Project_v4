#!/usr/bin/env Rscript
# 21_jwm_numbers_figures.R — numbers and figures added for the Journal of Wildlife
# Management version: station source audit, evaluation within elephant range,
# study-area climate, partial dependence, plain-language Figures 2-3.
# Usage (PowerShell, repo root): Rscript 03_analysis/21_jwm_numbers_figures.R 04_outputs/runs/RUN_<id>

suppressPackageStartupMessages({ library(terra); library(sf); library(ggplot2); library(yaml); library(ranger); library(gbm) })
source(file.path("03_analysis", "00_publication_helpers.R"))
run <- normalizePath(commandArgs(trailingOnly = TRUE)[[1]], winslash = "/", mustWork = TRUE)
d <- function(...) file.path(run, ...)
cfg <- read_yaml("00_governance/config.yaml")
out_dir <- file.path("05_manuscripts", "Journal of Wildlife Management")
fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

led <- list()
add <- function(key, value, unit = "", source = "") led[[length(led) + 1]] <<-
  data.frame(key = key, value = value, unit = unit, source = source, run_id = basename(run))

ledger <- read.csv(file.path("05_manuscripts", "manuscript_claim_ledger.csv"), stringsAsFactors = FALSE)
thr <- as.numeric(ledger$value[ledger$key == "threshold"])

# ── Station sources and evaluation subsets ───────────────────────────────────
oof <- read.csv(d("01_processed_data", "oof_predictions.csv"))
occ <- read.csv(file.path("01_data_raw", "01_occurrences", "elephant_PA_data.csv"), stringsAsFactors = FALSE)
book <- read.csv(file.path("01_data_raw", "01_occurrences", "Source_Files", "Book1.csv"), stringsAsFactors = FALSE)
dung_ids <- trimws(book$station[trimws(book$camera) == "Dung"])
key <- function(lon, lat) paste(round(lon, 5), round(lat, 5))
m <- match(key(oof$longitude, oof$latitude), key(occ$longitude, occ$latitude))
if (anyNA(m)) stop("OOF stations do not all match occurrence table")
oof$station_id <- occ$station_id[m]; oof$source <- occ$source[m]
oof$dung <- oof$station_id %in% dung_ids
pts <- vect(oof, geom = c("longitude", "latitude"), crs = "EPSG:4326")
elev_r <- rast(d("02_data_intermediate", "elevation_harmonized.tif"))
oof$elev <- extract(elev_r, project(pts, crs(elev_r)), ID = FALSE)[, 1]
max_pres <- max(oof$elev[oof$response == 1], na.rm = TRUE)

auc <- function(y, p) {
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
tss <- function(y, p, t) mean(p[y == 1] >= t) + mean(p[y == 0] < t) - 1
best_tss <- function(y, p) max(vapply(sort(unique(p)), function(t) tss(y, p, t), numeric(1)))

subs <- list(
  all = rep(TRUE, nrow(oof)),
  no_dung = !oof$dung,
  tiger = oof$source == "station_survey",
  elephant = oof$source == "book1_survey",
  elephant_no_dung = oof$source == "book1_survey" & !oof$dung,
  below_max_presence = !is.na(oof$elev) & oof$elev <= max_pres,
  below_1000m = !is.na(oof$elev) & oof$elev <= 1000,
  tiger_below_1000m = oof$source == "station_survey" & !is.na(oof$elev) & oof$elev <= 1000
)
for (s in names(subs)) {
  x <- oof[subs[[s]], ]
  add(paste0("eval_", s, "_n"), nrow(x), "stations", "oof_predictions.csv")
  add(paste0("eval_", s, "_presences"), sum(x$response == 1), "stations", "oof_predictions.csv")
  add(paste0("eval_", s, "_prevalence"), round(mean(x$response == 1), 3), "", "oof_predictions.csv")
  add(paste0("eval_", s, "_auc"), round(auc(x$response, x$pred_ensemble), 3), "", "oof_predictions.csv")
  add(paste0("eval_", s, "_tss_at_threshold"), round(tss(x$response, x$pred_ensemble, thr), 3), "", "oof_predictions.csv")
  add(paste0("eval_", s, "_tss_best"), round(best_tss(x$response, x$pred_ensemble), 3), "", "oof_predictions.csv")
  for (a in c("glm", "rf", "brt")) add(sprintf("eval_%s_auc_%s", s, a),
    round(auc(x$response, x[[paste0("pred_", a)]]), 3), "", "oof_predictions.csv")
}
add("eval_elephant_above_threshold_pct", round(100 * mean(oof$pred_ensemble[subs$elephant] >= thr), 1), "%", "oof_predictions.csv")
add("eval_below_1000m_above_threshold_pct", round(100 * mean(oof$pred_ensemble[subs$below_1000m] >= thr), 1), "%", "oof_predictions.csv")
add("eval_absences_above_max_presence_pct", round(100 * sum(oof$response == 0 & oof$elev > max_pres, na.rm = TRUE) / sum(oof$response == 0), 1), "%", "stations x elevation")
add("n_dung_presences", sum(oof$dung & oof$response == 1), "stations", "Book1.csv camera column")
add("n_dung_absences", sum(oof$dung & oof$response == 0), "stations", "Book1.csv camera column")
add("n_camera_presences", sum(!oof$dung & oof$response == 1), "stations", "Book1.csv camera column")
add("n_tiger_stations", sum(oof$source == "station_survey"), "stations", "elephant_PA_data.csv")
add("n_tiger_presences", sum(oof$source == "station_survey" & oof$response == 1), "stations", "elephant_PA_data.csv")
add("n_elephant_stations", sum(oof$source == "book1_survey"), "stations", "elephant_PA_data.csv")
add("n_elephant_presences", sum(oof$source == "book1_survey" & oof$response == 1), "stations", "elephant_PA_data.csv")
add("station_elev_max_presence_m", round(max_pres), "m", "stations x elevation")
add("station_absences_above_max_presence", sum(oof$response == 0 & oof$elev > max_pres, na.rm = TRUE), "stations", "stations x elevation")

# ── Study-area climate (present habitat, 250 m template) ─────────────────────
pres <- rast(d("03_present_suitability", "suitability_present_ensemble.tif"))
suit <- pres >= thr
clim <- function(n) {
  r <- rast(sprintf("01_data_raw/02_rasters/present/Historical_bioclims/Historical_1986-2015_bio%d.tif", n))
  project(r, pres, method = "bilinear")
}
bio1 <- clim(1); bio12 <- clim(12)
rng <- function(r, mask_r, name, unit, digits = 1) {
  v <- values(r, mat = FALSE)[values(mask_r, mat = FALSE) %in% TRUE]
  v <- v[is.finite(v)]
  q <- quantile(v, c(0.05, 0.5, 0.95))
  add(paste0(name, "_p05"), round(q[[1]], digits), unit, "bioclim x present habitat")
  add(paste0(name, "_median"), round(q[[2]], digits), unit, "bioclim x present habitat")
  add(paste0(name, "_p95"), round(q[[3]], digits), unit, "bioclim x present habitat")
}
dom <- !is.na(pres)
rng(bio1, suit, "habitat_bio1", "C"); rng(bio12, suit, "habitat_bio12", "mm", 0)
rng(bio1, dom, "domain_bio1", "C"); rng(bio12, dom, "domain_bio12", "mm", 0)
add("domain_bio12_max", round(global(mask(bio12, pres), "max", na.rm = TRUE)[[1]]), "mm", "bioclim x domain")
elev <- project(elev_r, pres, method = "bilinear")
add("domain_elev_min", round(global(mask(elev, pres), "min", na.rm = TRUE)[[1]]), "m", "elevation x domain")
add("domain_elev_max", round(global(mask(elev, pres), "max", na.rm = TRUE)[[1]]), "m", "elevation x domain")

pa <- st_transform(st_make_valid(st_read(cfg$vectors$protected_areas, quiet = TRUE)), crs(pres))
pa_r <- rasterize(vect(pa), pres, field = 1, background = 0)
hab <- values(suit, mat = FALSE) %in% TRUE
pav <- values(pa_r, mat = FALSE)
add("habitat_outside_pa_check_pct", round(100 * mean(pav[hab] == 0), 1), "%", "rasterized PA x present habitat")

# ── Partial dependence and GLM signs ─────────────────────────────────────────
dat <- read.csv(d("01_processed_data", "modeling_dataset.csv"))
preds <- setdiff(names(dat), c("id", "type", "response", "longitude", "latitude", "fold"))
for (p in preds) if (anyNA(dat[[p]])) dat[[p]][is.na(dat[[p]])] <- median(dat[[p]], na.rm = TRUE)
glm_m <- readRDS(d("02_models", "model_glm.rds"))
rf_m <- readRDS(d("02_models", "model_rf.rds"))
brt <- readRDS(d("02_models", "model_brt.rds"))
pr <- function(nd) {
  rf_p <- predict(rf_m, data = nd)$predictions
  rf_p <- if (is.matrix(rf_p)) rf_p[, "1"] else as.numeric(rf_p)
  out <- cbind(GLM = as.numeric(predict(glm_m, newdata = nd, type = "response")), RF = rf_p,
               BRT = predict(brt$model, newdata = nd, n.trees = brt$best_iter, type = "response"))
  cbind(out, Ensemble = rowMeans(out))
}
fit <- pr(dat)
dat_pts <- project(vect(dat, geom = c("longitude", "latitude"), crs = "EPSG:4326"), crs(pres))
add("fit_vs_map_cor", round(cor(fit[, "Ensemble"], extract(pres, dat_pts, ID = FALSE)[, 1], use = "complete"), 3), "", "full-data fit vs present map")
pd_curve <- function(p, n = 40) {
  x <- seq(quantile(dat[[p]], 0.02), quantile(dat[[p]], 0.98), length.out = n)
  do.call(rbind, lapply(x, function(v) {
    nd <- dat; nd[[p]] <- v
    data.frame(predictor = p, x = v, t(colMeans(pr(nd))))
  }))
}
show <- c("BIO18", "BIO14", "BIO05", "BIO15", "dist_to_major_rivers", "dist_to_protected_areas")
curves <- do.call(rbind, lapply(show, pd_curve))
anchors <- list(BIO18 = c(1000, 2000), BIO14 = c(5, 15), BIO05 = c(22, 30), BIO15 = c(90, 110),
                dist_to_major_rivers = c(1000, 10000), dist_to_protected_areas = c(0, 10000))
for (p in names(anchors)) for (v in anchors[[p]]) {
  nd <- dat; nd[[p]] <- v; o <- colMeans(pr(nd))
  for (a in names(o)) add(sprintf("pd_%s_%g_%s", p, v, tolower(a)), round(o[[a]], 3), "probability", "partial dependence")
}
co <- summary(glm_m)$coefficients
for (p in c("BIO18", "BIO14", "BIO15", "BIO05")) {
  add(paste0("glm_coef_", p), signif(co[p, "Estimate"], 3), "logit", "model_glm.rds")
  add(paste0("glm_se_", p), signif(co[p, "Std. Error"], 3), "logit", "model_glm.rds")
  uni <- coef(glm(reformulate(p, "response"), binomial, dat))[[p]]
  add(paste0("glm_univariate_coef_", p), signif(uni, 3), "logit", "single-predictor GLM")
}
add("cor_BIO18_BIO14", round(cor(dat$BIO18, dat$BIO14), 2), "r", "modeling_dataset.csv")
add("cor_BIO18_BIO15", round(cor(dat$BIO18, dat$BIO15), 2), "r", "modeling_dataset.csv")
add("cor_BIO18_BIO05", round(cor(dat$BIO18, dat$BIO05), 2), "r", "modeling_dataset.csv")

# ── Refit without dung records (same folds, settings and seed; present day only) ──
manifest <- read.csv(d("01_processed_data", "predictor_manifest.csv"))
pred_cols <- manifest$predictor[manifest$selected]
frm <- as.formula(paste("response ~", paste(pred_cols, collapse = " + ")))
keep <- !(key(dat$longitude, dat$latitude) %in% key(oof$longitude[oof$dung], oof$latitude[oof$dung]))
if (sum(!keep) != sum(oof$dung)) stop("Dung stations do not match the modeling dataset")
d2 <- dat[keep, ]
seed <- 123456L
fit_all <- function(x) {
  set.seed(seed)
  g <- glm(frm, data = x, family = binomial())
  r <- ranger(frm, data = x, num.trees = 1000, mtry = floor(sqrt(length(pred_cols))), seed = seed)
  b <- gbm(frm, data = x, distribution = "bernoulli", n.trees = 1000, interaction.depth = 3,
           shrinkage = 0.01, n.minobsinnode = 5, bag.fraction = 0.75, verbose = FALSE)
  it <- gbm.perf(b, method = "OOB", plot.it = FALSE)
  list(g = g, r = r, b = b, it = if (is.na(it) || it < 1) 500L else it)
}
pred3 <- function(m, x) rowMeans(cbind(as.numeric(predict(m$g, newdata = x, type = "response")),
                                       as.numeric(predict(m$r, data = x)$predictions),
                                       as.numeric(predict(m$b, newdata = x, n.trees = m$it, type = "response"))))
ens2 <- rep(NA_real_, nrow(d2))
for (k in sort(unique(d2$fold))) ens2[d2$fold == k] <- pred3(fit_all(d2[d2$fold != k, ]), d2[d2$fold == k, ])
cuts <- sort(unique(ens2)); tss_cut <- vapply(cuts, function(t) tss(d2$response, ens2, t), numeric(1))
thr2 <- cuts[which.max(tss_cut)]
d2_src <- occ$source[match(key(d2$longitude, d2$latitude), key(occ$longitude, occ$latitude))]
d2_elev <- extract(elev_r, project(vect(d2, geom = c("longitude", "latitude"), crs = "EPSG:4326"), crs(elev_r)), ID = FALSE)[, 1]
add("dung_refit_n", nrow(d2), "stations", "refit without dung")
add("dung_refit_auc", round(auc(d2$response, ens2), 3), "", "refit without dung")
add("dung_refit_tss", round(max(tss_cut), 3), "", "refit without dung")
add("dung_refit_threshold", round(thr2, 3), "", "refit without dung")
e_ok <- d2_src == "book1_survey"
add("dung_refit_elephant_auc", round(auc(d2$response[e_ok], ens2[e_ok]), 3), "", "refit without dung")
w_ok <- !is.na(d2_elev) & d2_elev <= max_pres
add("dung_refit_within_range_auc", round(auc(d2$response[w_ok], ens2[w_ok]), 3), "", "refit without dung")

stk <- rast(lapply(manifest$path[manifest$selected], function(p) {
  r <- rast(p)
  if (compareGeom(r, pres, stopOnError = FALSE)) r else project(r, pres, method = if (grepl("label|landcover", p)) "near" else "bilinear")
}))
names(stk) <- pred_cols
map_of <- function(m) {
  v <- as.data.frame(stk, cells = TRUE, na.rm = TRUE)
  out <- rast(pres); values(out) <- NA_real_
  out[v$cell] <- pred3(m, v)
  mask(out, pres)
}
stack_map <- map_of(list(g = glm_m, r = rf_m, b = brt$model, it = brt$best_iter))
add("stack_check_map_r", round(cor(values(stack_map, mat = FALSE), values(pres, mat = FALSE), use = "complete"), 3), "", "rebuilt stack vs accepted map")
map2 <- map_of(fit_all(d2))
hab2 <- values(map2 >= thr2, mat = FALSE) %in% TRUE
cell_km2 <- prod(res(pres)) / 1e6
core_v <- values(project(rast(d("05_change_metrics", "refugia_core.tif")), pres, method = "near") == 1, mat = FALSE) %in% TRUE
add("dung_refit_habitat_km2", round(sum(hab2) * cell_km2), "km2", "refit without dung")
add("dung_refit_outside_pa_pct", round(100 * mean(pav[hab2] == 0), 1), "%", "refit without dung")
add("dung_refit_map_r", round(cor(values(map2, mat = FALSE), values(pres, mat = FALSE), use = "complete"), 3), "", "refit without dung")
add("dung_refit_jaccard", round(sum(hab & hab2) / sum(hab | hab2), 3), "", "refit without dung")
add("dung_refit_core_suitable_pct", round(100 * mean(hab2[core_v]), 1), "%", "accepted core refugia x refit map")

# ── Protected-area elevation and exposure by district ────────────────────────
el_v <- values(elev, mat = FALSE); in_pa <- pav == 1 & !is.na(values(pres, mat = FALSE))
add("pa_above_2000m_pct", round(100 * mean(el_v[in_pa] > 2000, na.rm = TRUE), 1), "%", "elevation x PA network")
add("pa_below_1000m_pct", round(100 * mean(el_v[in_pa] <= 1000, na.rm = TRUE), 1), "%", "elevation x PA network")
expo <- project(rast(file.path("05_manuscripts", "interaction_exposure_present.tif")), pres, method = "near")
dz <- st_read(file.path(cfg$vectors$root, "Dzongkhag Boundary", "Dzongkhag Boundary.shp"), quiet = TRUE)
st_crs(dz) <- 3857L
dz <- st_transform(st_make_valid(dz), crs(pres))
nm <- names(dz)[vapply(st_drop_geometry(dz), function(z) is.character(z) && length(unique(z)) >= 15, logical(1))][1]
dz_v <- values(rasterize(vect(dz), pres, field = nm), mat = FALSE)
labs_dz <- levels(rasterize(vect(dz), pres, field = nm))[[1]]
dz_name <- labs_dz[[2]][match(dz_v, labs_dz[[1]])]
ex_v <- values(expo, mat = FALSE); top15 <- ex_v >= quantile(ex_v, 0.85, na.rm = TRUE)
ez <- sort(tapply(ex_v, dz_name, mean, na.rm = TRUE), decreasing = TRUE)
tz <- sort(100 * tapply(top15, dz_name, sum, na.rm = TRUE) / sum(top15, na.rm = TRUE), decreasing = TRUE)
print(round(ez[1:7], 3)); print(round(tz[1:7], 1))
for (i in 1:5) add(sprintf("exposure_mean_rank%d", i), names(ez)[i], "district", "exposure x Dzongkhag")
for (i in 1:5) add(sprintf("exposure_top15_rank%d", i), names(tz)[i], "district", "exposure x Dzongkhag")

write.csv(do.call(rbind, led), file.path(out_dir, "jwm_extra_numbers.csv"), row.names = FALSE)

# ── Figure 2: importance with plain labels ───────────────────────────────────
plain <- c(BIO05 = "Maximum temperature of warmest month", BIO14 = "Precipitation of driest month",
           BIO15 = "Precipitation seasonality", BIO18 = "Precipitation of warmest quarter",
           tri = "Terrain ruggedness", slope = "Slope", aspect = "Aspect", evi = "Enhanced vegetation index",
           dynamicworld_prob = "Land-cover class probability", landcover = "Land-cover class",
           human_footprint = "Human Impact Index", dist_to_settlements = "Distance to buildings")
algo_names <- c(GLM = "Generalized linear model", RF = "Random forest", BRT = "Boosted regression trees")
vi <- read.csv(d("08_figures_tables", "table_E07_variable_importance.csv"), stringsAsFactors = FALSE)
vi <- vi[toupper(vi$algorithm) %in% names(algo_names), ]
vi$algorithm <- toupper(vi$algorithm)
vi <- do.call(rbind, lapply(split(vi, vi$algorithm), function(z) z[order(-z$importance)[seq_len(min(8, nrow(z)))], ]))
vi$label <- ifelse(vi$predictor %in% names(plain), plain[vi$predictor], sub("^dist to ", "Distance to ", gsub("_", " ", vi$predictor)))
vi$group <- sub("Climate \\(BIO\\)", "Climate", vi$pred_type)
vi$group <- sub("Vegetation/LULC", "Vegetation", vi$group)
vi$panel <- reorder(paste(vi$algorithm, vi$label, sep = "|"), vi$importance)
vi$algorithm <- factor(algo_names[vi$algorithm], levels = algo_names)
pal <- c(Climate = "#0072B2", Anthropogenic = "#D55E00", Vegetation = "#009E73", Topography = "#CC79A7", Distance = "#E69F00", Other = "#777777")
p2 <- ggplot(vi, aes(panel, importance, fill = group)) + geom_col(width = 0.72) + coord_flip() +
  facet_wrap(~algorithm, scales = "free_y", ncol = 2) +
  scale_x_discrete(labels = function(z) sub("^[^|]+\\|", "", z)) +
  scale_fill_manual(values = pal, name = "Predictor group", drop = TRUE) +
  labs(x = NULL, y = "Relative importance within algorithm") +
  publication_theme(base_size = 8.5) + theme(legend.position = c(0.78, 0.2))
save_publication_figure(p2, file.path(fig_dir, "Figure_2_predictor_importance"), width_mm = 178, height_mm = 150, line_art_dpi = 600)

# ── Figure 3: partial dependence, all ensemble members ───────────────────────
axis_lab <- c(BIO18 = "Precipitation of
warmest quarter (mm)", BIO14 = "Precipitation of
driest month (mm)",
              BIO05 = "Maximum temperature of
warmest month (°C)", BIO15 = "Precipitation seasonality
(coefficient of variation, %)",
              dist_to_major_rivers = "Distance to
major rivers (km)", dist_to_protected_areas = "Distance to
protected areas (km)")
long <- do.call(rbind, lapply(c("GLM", "RF", "BRT", "Ensemble"), function(a)
  data.frame(predictor = curves$predictor, x = curves$x, y = curves[[a]], model = a)))
is_dist <- grepl("^dist_", long$predictor)
long$x[is_dist] <- long$x[is_dist] / 1000
long$predictor <- factor(axis_lab[long$predictor], levels = axis_lab[show])
long$model <- factor(c(GLM = "Generalized linear model", RF = "Random forest", BRT = "Boosted regression trees", Ensemble = "Ensemble")[long$model],
                     levels = c(algo_names, "Ensemble"))
p3 <- ggplot(long, aes(x, y, colour = model, linewidth = model)) + geom_line() +
  facet_wrap(~predictor, scales = "free_x", ncol = 3, strip.position = "bottom") +
  scale_colour_manual(values = c("#0072B2", "#009E73", "#E69F00", "black"), name = NULL) +
  scale_linewidth_manual(values = c(0.45, 0.45, 0.45, 1), name = NULL) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = NULL, y = "Mean predicted probability of detection") +
  publication_theme(base_size = 8.5) +
  theme(strip.placement = "outside", strip.text = element_text(size = 8), legend.position = "bottom")
save_publication_figure(p3, file.path(fig_dir, "Figure_3_response_curves"), width_mm = 178, height_mm = 115, line_art_dpi = 600)
write.csv(long, file.path(fig_dir, "Figure_3_source_data.csv"), row.names = FALSE)

# ── Graphical abstract (60 × 50 mm) ──────────────────────────────────────────
core <- project(rast(d("05_change_metrics", "refugia_core.tif")), pres, method = "near") == 1
ga <- as.data.frame(c(suit, core, pa_r), xy = TRUE, na.rm = FALSE)
names(ga) <- c("x", "y", "suit", "core", "pa")
ga <- ga[ga$suit %in% TRUE, ]
ga$class <- ifelse(ga$core %in% TRUE & ga$pa == 0, "Refugia, unprotected",
                   ifelse(ga$core %in% TRUE, "Refugia, protected", "Other habitat"))
aoi <- st_transform(st_read(cfg$vectors$aoi_bhutan, quiet = TRUE), crs(pres))
pct <- as.numeric(ledger$value[ledger$key == "refugia_core_in_pa_pct"])
pg <- ggplot() +
  geom_sf(data = aoi, fill = "#F2EFE6", colour = "grey35", linewidth = 0.2) +
  geom_tile(data = ga, aes(x, y, fill = class), width = 250, height = 250) +
  scale_fill_manual(values = c("Refugia, unprotected" = "#D55E00", "Refugia, protected" = "#009E73",
                               "Other habitat" = "#E8C75A"), name = NULL) +
  coord_sf(crs = st_crs(aoi), expand = FALSE) +
  labs(title = sprintf("%.0f%% of elephant climate refugia\nlie outside protected areas", 100 - pct)) +
  theme_void(base_size = 6, base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 7.5, hjust = 0.5, lineheight = 0.95),
        legend.position = "bottom", legend.key.size = grid::unit(2, "mm"),
        legend.margin = margin(0, 0, 0, 0), plot.margin = margin(2, 2, 2, 2))
save_publication_figure(pg, file.path(fig_dir, "Graphical_abstract"), width_mm = 60, height_mm = 50, line_art_dpi = 600)
cat("Done\n")
