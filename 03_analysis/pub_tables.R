#!/usr/bin/env Rscript
# =============================================================================
# pub_tables.R — Publication-grade summary tables
#
# Generates the 4 tables mandated by plots_target.md §8:
#   T1. Suitable area by GCM, SSP, and period (km² and %)
#   T2. Percentage change from present per scenario
#   T3. Trajectory summary (mean / SD / min / max across GCMs per SSP × period)
#   T4. Final threshold summary
#
# Usage (PowerShell):
#   Rscript 03_analysis/pub_tables.R [run_dir]
#
# Defaults to latest run in 04_outputs/ or 04_outputs/runs/.
# =============================================================================

suppressPackageStartupMessages({ library(terra); library(sf) })

# ── Resolve paths ─────────────────────────────────────────────────────────────
args     <- commandArgs(trailingOnly = TRUE)
repo_root <- normalizePath(".", winslash = "/")
source(file.path(repo_root, "03_analysis", "00_contract_helpers.R"), local = TRUE)

if (length(args) >= 1) {
  RUN <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
} else {
  RUN <- find_latest_run_dir(repo_root)
}
run_id <- read_run_id_from_manifest(RUN)
OUT    <- file.path(RUN, "08_figures_tables")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

cat(sprintf("\n=== pub_tables.R ===\nRun: %s\nOutput: %s\n\n", run_id, OUT))

# ── Constants ─────────────────────────────────────────────────────────────────
THRESHOLD  <- 0.172  # optimal TSS threshold (Phase 8)
THR_METHOD <- "MaxSSS (maximum sensitivity + specificity) on OOF predictions"

GCM_LIST  <- c("acces_cm2", "cnrm_cm6_1", "cnrm_esm2_1", "inm_cm4_8",
               "inm_cm5_0", "miroc6", "miroc_es2l", "mpi_esm1_2_lr")
GCM_LABEL <- c("ACCESS-CM2", "CNRM-CM6-1", "CNRM-ESM2-1", "INM-CM4-8",
               "INM-CM5-0", "MIROC6", "MIROC-ES2L", "MPI-ESM1-2-LR", "MRI-ESM2-0")
SSP_LIST  <- c("ssp126", "ssp245", "ssp370", "ssp585")
SSP_LABEL <- c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
PER_LIST  <- c("2021_2050", "2051_2080", "2071_2100")
PER_LABEL <- c("2021-2050", "2051-2080", "2071-2100")
ALGO_LIST <- c("glm", "rf", "brt", "maxent")

FUT_DIR   <- file.path(RUN, "04_future_projections")
PRES_FILE <- file.path(RUN, "03_present_suitability", "suitability_present_ensemble.tif")

# ── Helper: pixel area in km² ─────────────────────────────────────────────────
pixel_area_km2 <- function(r) {
  res_m <- res(r)
  prod(res_m) / 1e6
}

# ── Helper: % suitable area and km² at threshold ──────────────────────────────
suit_stats <- function(r, thr = THRESHOLD) {
  v   <- as.numeric(values(r, na.rm = TRUE))
  v   <- v[!is.na(v)]
  if (length(v) == 0) return(c(pct = NA_real_, area_km2 = NA_real_,
                                mean_suit = NA_real_, median_suit = NA_real_))
  pxl <- pixel_area_km2(r)
  c(
    pct        = mean(v >= thr, na.rm = TRUE) * 100,
    area_km2   = sum(v >= thr, na.rm = TRUE) * pxl,
    mean_suit  = mean(v, na.rm = TRUE),
    median_suit= median(v, na.rm = TRUE)
  )
}

# ── Load present raster ───────────────────────────────────────────────────────
pres_r <- tryCatch(rast(PRES_FILE), error = function(e) NULL)
if (is.null(pres_r)) stop("Present suitability raster not found: ", PRES_FILE)
pres_stats <- suit_stats(pres_r)
cat(sprintf("Present: %.1f%% suitable (%.0f km²)\n",
            pres_stats["pct"], pres_stats["area_km2"]))

# ── Helper: load per-GCM ensemble raster (mean across 4 algorithms) ───────────
get_gcm_ens <- function(gcm, ssp, period) {
  files <- file.path(FUT_DIR,
    sprintf("suitability_future_%s_%s_%s_%s.tif", gcm, ssp, period, ALGO_LIST))
  files <- files[file.exists(files)]
  if (length(files) == 0) return(NULL)
  rasters <- lapply(files, function(f) tryCatch(rast(f), error = function(e) NULL))
  rasters <- Filter(Negate(is.null), rasters)
  if (length(rasters) == 0) return(NULL)
  if (length(rasters) == 1) return(rasters[[1]])
  stk <- tryCatch(rast(rasters), error = function(e) NULL)
  if (is.null(stk)) return(rasters[[1]])
  app(stk, fun = "mean", na.rm = TRUE)
}

# ── Helper: load all-GCM ensemble raster ─────────────────────────────────────
get_all_gcm_ens <- function(ssp, period) {
  f <- file.path(FUT_DIR, sprintf("future_gcm_ensemble_%s_%s.tif", ssp, period))
  if (!file.exists(f)) return(NULL)
  tryCatch(rast(f), error = function(e) NULL)
}

# =============================================================================
# TABLE 1 — Suitable area by GCM, SSP, and period
# Rows: 9 GCMs × 4 SSPs × 3 periods = 108 rows + 12 all-GCM ensemble rows
# Cols: gcm, gcm_label, ssp, ssp_label, period, pct_suitable, area_km2,
#       mean_suitability, median_suitability
# =============================================================================
cat("Building Table 1: Suitable area by GCM × SSP × period...\n")

rows_t1 <- list()

# Per-GCM rows
for (gi in seq_along(GCM_LIST)) {
  for (si in seq_along(SSP_LIST)) {
    for (pi in seq_along(PER_LIST)) {
      r <- get_gcm_ens(GCM_LIST[gi], SSP_LIST[si], PER_LIST[pi])
      if (is.null(r)) {
        ss <- c(pct = NA_real_, area_km2 = NA_real_,
                mean_suit = NA_real_, median_suit = NA_real_)
      } else {
        r  <- tryCatch(resample(r, pres_r, method = "bilinear"), error = function(e) r)
        ss <- suit_stats(r)
      }
      rows_t1[[length(rows_t1) + 1]] <- data.frame(
        gcm             = GCM_LIST[gi],
        gcm_label       = GCM_LABEL[gi],
        ssp             = SSP_LIST[si],
        ssp_label       = SSP_LABEL[si],
        period          = PER_LABEL[pi],
        pct_suitable    = round(ss["pct"],    2),
        area_km2        = round(ss["area_km2"], 1),
        mean_suitability   = round(ss["mean_suit"], 4),
        median_suitability = round(ss["median_suit"], 4),
        source          = "per_gcm_algo_mean",
        stringsAsFactors = FALSE
      )
    }
  }
}

# All-GCM ensemble rows
for (si in seq_along(SSP_LIST)) {
  for (pi in seq_along(PER_LIST)) {
    r <- get_all_gcm_ens(SSP_LIST[si], PER_LIST[pi])
    if (is.null(r)) {
      ss <- c(pct = NA_real_, area_km2 = NA_real_,
              mean_suit = NA_real_, median_suit = NA_real_)
    } else {
      r  <- tryCatch(resample(r, pres_r, method = "bilinear"), error = function(e) r)
      ss <- suit_stats(r)
    }
    rows_t1[[length(rows_t1) + 1]] <- data.frame(
      gcm             = "ensemble",
      gcm_label       = "All-GCM Ensemble",
      ssp             = SSP_LIST[si],
      ssp_label       = SSP_LABEL[si],
      period          = PER_LABEL[pi],
      pct_suitable    = round(ss["pct"],    2),
      area_km2        = round(ss["area_km2"], 1),
      mean_suitability   = round(ss["mean_suit"], 4),
      median_suitability = round(ss["median_suit"], 4),
      source          = "all_gcm_ensemble",
      stringsAsFactors = FALSE
    )
  }
}

table1 <- do.call(rbind, rows_t1)
rownames(table1) <- NULL

f1 <- file.path(OUT, "Table_suitable_area_by_gcm_ssp_period.csv")
write.csv(table1, f1, row.names = FALSE)
cat(sprintf("  + Table 1 saved: %d rows → %s\n", nrow(table1), basename(f1)))

# =============================================================================
# TABLE 2 — Percentage change from present per scenario
# Δpct = future pct_suitable − present pct_suitable
# Δarea_km2 = future area_km2 − present area_km2
# pct_change = 100 * (future − present) / present
# =============================================================================
cat("Building Table 2: Percentage change from present...\n")

pres_pct  <- pres_stats["pct"]
pres_area <- pres_stats["area_km2"]

table2 <- table1
table2$present_pct      <- round(pres_pct,  2)
table2$present_area_km2 <- round(pres_area, 1)
table2$delta_pct        <- round(table2$pct_suitable - pres_pct, 2)
table2$delta_area_km2   <- round(table2$area_km2 - pres_area, 1)
table2$pct_change       <- round(100 * (table2$pct_suitable - pres_pct) / pres_pct, 2)

# Keep only columns relevant to change
table2 <- table2[, c("gcm", "gcm_label", "ssp", "ssp_label", "period",
                     "present_pct", "present_area_km2",
                     "pct_suitable", "area_km2",
                     "delta_pct", "delta_area_km2", "pct_change", "source")]

f2 <- file.path(OUT, "Table_pct_change_from_present.csv")
write.csv(table2, f2, row.names = FALSE)
cat(sprintf("  + Table 2 saved: %d rows → %s\n", nrow(table2), basename(f2)))

# =============================================================================
# TABLE 3 — Trajectory summary: mean / SD / min / max across GCMs per SSP × period
# Includes all-GCM ensemble as reference row
# =============================================================================
cat("Building Table 3: Trajectory summary...\n")

# Only per-GCM rows (exclude ensemble rows) for statistics
per_gcm_rows <- table1[table1$gcm != "ensemble", ]
ens_rows     <- table1[table1$gcm == "ensemble", ]

rows_t3 <- list()

for (si in seq_along(SSP_LIST)) {
  for (pi in seq_along(PER_LIST)) {
    sub <- per_gcm_rows[per_gcm_rows$ssp == SSP_LIST[si] &
                        per_gcm_rows$period == PER_LABEL[pi], ]
    sub_ens <- ens_rows[ens_rows$ssp == SSP_LIST[si] &
                        ens_rows$period == PER_LABEL[pi], ]

    pct_v <- sub$pct_suitable[!is.na(sub$pct_suitable)]
    km2_v <- sub$area_km2[!is.na(sub$area_km2)]

    rows_t3[[length(rows_t3) + 1]] <- data.frame(
      ssp           = SSP_LIST[si],
      ssp_label     = SSP_LABEL[si],
      period        = PER_LABEL[pi],
      n_gcms        = length(pct_v),
      mean_pct      = round(mean(pct_v),   2),
      sd_pct        = round(sd(pct_v),     2),
      min_pct       = round(min(pct_v),    2),
      max_pct       = round(max(pct_v),    2),
      mean_area_km2 = round(mean(km2_v),   1),
      sd_area_km2   = round(sd(km2_v),     1),
      min_area_km2  = round(min(km2_v),    1),
      max_area_km2  = round(max(km2_v),    1),
      ensemble_pct  = if (nrow(sub_ens) > 0) round(sub_ens$pct_suitable[1], 2) else NA_real_,
      ensemble_km2  = if (nrow(sub_ens) > 0) round(sub_ens$area_km2[1], 1) else NA_real_,
      delta_mean_pct = round(mean(pct_v) - pres_pct, 2),
      pct_change    = round(100 * (mean(pct_v) - pres_pct) / pres_pct, 2),
      stringsAsFactors = FALSE
    )
  }
}

# Add present row
rows_t3 <- c(
  list(data.frame(
    ssp = "present", ssp_label = "Present", period = "1981-2010",
    n_gcms = NA_integer_,
    mean_pct = round(pres_pct, 2), sd_pct = 0,
    min_pct = round(pres_pct, 2), max_pct = round(pres_pct, 2),
    mean_area_km2 = round(pres_area, 1), sd_area_km2 = 0,
    min_area_km2 = round(pres_area, 1), max_area_km2 = round(pres_area, 1),
    ensemble_pct = round(pres_pct, 2), ensemble_km2 = round(pres_area, 1),
    delta_mean_pct = 0, pct_change = 0,
    stringsAsFactors = FALSE
  )),
  rows_t3
)

table3 <- do.call(rbind, rows_t3)
rownames(table3) <- NULL

f3 <- file.path(OUT, "Table_trajectory_summary.csv")
write.csv(table3, f3, row.names = FALSE)
cat(sprintf("  + Table 3 saved: %d rows → %s\n", nrow(table3), basename(f3)))

# =============================================================================
# TABLE 4 — Final threshold summary
# Documents the adopted threshold and its derivation
# =============================================================================
cat("Building Table 4: Final threshold summary...\n")

# Load evaluation data for context
eval_file <- file.path(RUN, "02_models", "evaluation_summary.csv")
eval_df   <- if (file.exists(eval_file)) read.csv(eval_file) else NULL

# Per-algorithm thresholds from evaluation
algo_thresholds <- if (!is.null(eval_df) && "threshold" %in% names(eval_df)) {
  eval_df[, c("algorithm", "auc_mean", "tss_mean", "threshold")]
} else {
  data.frame(algorithm = character(0), auc_mean = numeric(0),
             tss_mean = numeric(0), threshold = numeric(0))
}

# Compute % suitable at the adopted threshold for present raster
pres_pct_at_thr <- mean(as.numeric(values(pres_r, na.rm = TRUE)) >= THRESHOLD, na.rm = TRUE) * 100
pres_km2_at_thr <- sum(as.numeric(values(pres_r, na.rm = TRUE)) >= THRESHOLD, na.rm = TRUE) *
                   pixel_area_km2(pres_r)

# Build threshold summary document
thr_lines <- c(
  "# Final Threshold Summary",
  "",
  sprintf("Run ID:              %s", run_id),
  sprintf("Adopted threshold:   %.4f", THRESHOLD),
  sprintf("Threshold method:    %s", THR_METHOD),
  sprintf("CRS:                 EPSG:32645 (WGS 84 / UTM Zone 45N)"),
  "",
  "## Present-day area at adopted threshold",
  sprintf("  Percent suitable:  %.2f%%", pres_pct_at_thr),
  sprintf("  Area suitable:     %.0f km²", pres_km2_at_thr),
  "",
  "## Per-algorithm thresholds (for reference)",
  ""
)

if (nrow(algo_thresholds) > 0) {
  thr_lines <- c(thr_lines,
    sprintf("  %-12s  AUC=%.3f  TSS=%.3f  threshold=%.4f",
            algo_thresholds$algorithm,
            ifelse(is.na(algo_thresholds$auc_mean), NA, round(algo_thresholds$auc_mean, 3)),
            ifelse(is.na(algo_thresholds$tss_mean), NA, round(algo_thresholds$tss_mean, 3)),
            ifelse(is.na(algo_thresholds$threshold), NA, round(algo_thresholds$threshold, 4))))
}

thr_lines <- c(thr_lines, "",
  "## Application scope",
  "The adopted threshold is applied consistently in:",
  "  - Suitable area calculations (Tables 1-3)",
  "  - Gain/loss/persistence maps (S3)",
  "  - PA coverage summaries (A4)",
  "  - Conflict risk overlays (A9)",
  "  - All trajectory statistics",
  "",
  "## Notes",
  "  - MaxEnt threshold may differ (evaluated separately via raster extraction)",
  "  - All threshold-based figures and tables reference this document"
)

writeLines(thr_lines, file.path(OUT, "Table_threshold_summary.txt"))

# Also save as CSV for programmatic access
table4 <- data.frame(
  run_id            = run_id,
  adopted_threshold = THRESHOLD,
  method            = THR_METHOD,
  present_pct_suitable = round(pres_pct_at_thr, 2),
  present_area_km2     = round(pres_km2_at_thr, 1),
  applied_to        = "suitable_area, gain_loss, PA_coverage, conflict_risk, trajectories",
  stringsAsFactors  = FALSE
)

f4 <- file.path(OUT, "Table_threshold_summary.csv")
write.csv(table4, f4, row.names = FALSE)
cat(sprintf("  + Table 4 saved → %s\n", basename(f4)))
cat(sprintf("  + Table 4 doc   → Table_threshold_summary.txt\n"))

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n=== pub_tables.R complete ===\n")
cat(sprintf("  Table 1 (area by GCM×SSP×period):    %d rows → %s\n", nrow(table1), basename(f1)))
cat(sprintf("  Table 2 (pct change from present):   %d rows → %s\n", nrow(table2), basename(f2)))
cat(sprintf("  Table 3 (trajectory summary):        %d rows → %s\n", nrow(table3), basename(f3)))
cat(sprintf("  Table 4 (threshold summary):         1 row  → %s\n", basename(f4)))
cat(sprintf("  All outputs in: %s\n", OUT))
