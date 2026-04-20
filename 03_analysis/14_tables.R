#!/usr/bin/env Rscript
# =============================================================================
# 14_tables.R — Publication-quality tables for Elephas maximus SDM (Bhutan)
#
# Tables generated (22 total, after deduplication):
#
# MAIN TEXT
#   T01  table_01_model_performance.csv     — AUC, TSS, Boyce, Brier, Calib, Moran's I
#   T02  table_02_predictor_selection.csv   — All candidates: kept/removed + reason
#   T03  table_03_habitat_trajectories.csv  — % suitable + km² by SSP × period
#   T14  table_14_occurrence_summary.csv    — Data processing chain (raw → final)
#   T15  table_15_confusion_matrix.csv      — TP/TN/FP/FN + sensitivity/specificity/PPV/NPV
#
# SUPPLEMENTARY
#   T04  table_04_pa_coverage.csv           — Suitable habitat inside/outside PAs
#   T06  table_06_gain_loss_persistence.csv — km² gain/loss/persistence by SSP × period
#   S1   table_S1_variable_importance.csv   — Relative importance, all 4 algorithms
#   S2   table_S2_gcm_reliability.csv       — GCM ranking: algo SD + ensemble deviation
#   S3   table_S3_cv_fold_summary.csv       — Per-fold AUC from spatial CV
#   S4   table_S4_pa_suitability.csv        — Per-protected-area suitability stats
#   S5   table_S5_gcm_ensemble_spread.csv   — GCM spread (mean SD) by SSP × period
#   T16  table_16_threshold_sensitivity.csv — % suitable at key threshold values
#   T17  table_17_niche_overlap.csv         — Schoener's D + Hellinger's I: present vs future
#   T18  table_18_cv_block_details.csv      — Spatial fold details: n, balance, %
#   T19  table_19_predictor_statistics.csv  — Predictor mean/SD at presence vs background
#   T20  table_20_algorithm_hyperparams.csv — Model hyperparameters and settings
#   T21  table_21_gcm_trajectories.csv      — % suitable per GCM × SSP × period
#   T22  table_22_conflict_risk_zones.csv   — Present-day conflict risk class areas
#   T23  table_23_refugia_summary.csv       — Refugia area stats (populated after refugia built)
#   T24  table_24_dzongkhag_suitability.csv — Per-Dzongkhag habitat area, %, mean suitability
#
# Standalone usage (PowerShell):
#   Rscript 03_analysis/14_tables.R <run_dir> [config_path]
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
})

generate_publication_tables <- function(run_dir, config_path = NULL) {
  run_dir  <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)
  fig_dir  <- file.path(run_dir, "08_figures_tables")
  mod_dir  <- file.path(run_dir, "02_models")
  proc_dir <- file.path(run_dir, "01_processed_data")
  fut_dir  <- file.path(run_dir, "04_future_projections")
  chg_dir  <- file.path(run_dir, "05_change_metrics")
  unc_dir  <- file.path(run_dir, "06_uncertainty")
  pres_dir <- file.path(run_dir, "03_present_suitability")
  ovl_dir  <- file.path(run_dir, "07_overlays")
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

  repo_root <- normalizePath(
    file.path(run_dir, "..", "..", ".."), mustWork = FALSE, winslash = "/"
  )

  cat("=== Publication Tables (22 tables) ===\n")
  n <- 0L

  # ── Shared helpers ──────────────────────────────────────────────────────────

  # TSS-optimal threshold (mean across algorithms, excluding ensemble)
  threshold <- 0.5
  eval_csv  <- file.path(mod_dir, "evaluation_all.csv")
  eval_df   <- tryCatch(read.csv(eval_csv), error = function(e) NULL)
  if (!is.null(eval_df) && "threshold" %in% names(eval_df)) {
    non_ens <- eval_df[eval_df$algorithm != "ensemble", ]
    tv      <- mean(non_ens$threshold, na.rm = TRUE)
    if (is.finite(tv)) threshold <- tv
  }

  # SSP label lookup
  ssp_lbl <- c(
    SSP126 = "SSP1-2.6", SSP245 = "SSP2-4.5",
    SSP370 = "SSP3-7.0", SSP585 = "SSP5-8.5",
    ssp126 = "SSP1-2.6", ssp245 = "SSP2-4.5",
    ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5"
  )
  period_lbl <- c(
    "2021-2050" = "2021\u20132050",
    "2051-2080" = "2051\u20132080",
    "2071-2100" = "2071\u20132100"
  )

  # Predictor type classifier
  pred_type <- function(p) {
    if      (grepl("^BIO", p, ignore.case = TRUE))                    "Climate (BIO)"
    else if (grepl("footprint|hii", p, ignore.case = TRUE))           "Anthropogenic"
    else if (grepl("evi|ndvi|landcover|dynamicworld", p, ignore.case = TRUE)) "Vegetation/LULC"
    else if (grepl("slope|aspect|tri|elevation", p, ignore.case = TRUE))      "Topography"
    else if (grepl("dist_", p, ignore.case = TRUE))                   "Distance"
    else "Other"
  }

  # Trapezoid AUC
  auc_fn <- function(y, p) {
    y <- as.numeric(y); p <- as.numeric(p)
    ok <- !is.na(y) & !is.na(p)
    y <- y[ok]; p <- p[ok]
    if (length(unique(y)) < 2) return(NA_real_)
    ord <- order(p, decreasing = TRUE)
    y   <- y[ord]; n1 <- sum(y); n0 <- length(y) - n1
    if (n1 == 0L || n0 == 0L) return(NA_real_)
    tp  <- cumsum(y); fp <- cumsum(1 - y)
    tpr <- c(0, tp / n1); fpr <- c(0, fp / n0)
    sum(diff(fpr) * (tpr[-1] + tpr[-length(tpr)]) / 2, na.rm = TRUE)
  }

  # PA shapefile path
  pa_shp <- file.path(
    repo_root, "01_data_raw", "03_vector", "shapefiles",
    "PA_Bhutan", "PA_Bnd_Final_20230316.shp"
  )

  # ── T01: Model Performance ──────────────────────────────────────────────────
  tryCatch({
    ms <- read.csv(file.path(mod_dir, "metrics_summary.csv"))
    t1 <- data.frame(
      Algorithm           = toupper(ms$algorithm),
      AUC                 = round(ms$auc,               3),
      TSS                 = round(ms$tss,               3),
      Boyce_Index         = round(ms$boyce,             3),
      Brier_Score         = round(ms$brier,             3),
      Calibration_Slope   = round(ms$calibration_slope, 3),
      Morans_I            = round(ms$moran_i,           3),
      Optimal_Threshold   = round(ms$threshold,         4),
      TSS_note            = ifelse(is.na(ms$tss),
        "TSS not computed (MaxEnt OOF probabilities not in standard range)", ""),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    # Remove note column if empty for all rows
    if (all(t1$TSS_note == "")) t1$TSS_note <- NULL
    write.csv(t1, file.path(fig_dir, "table_01_model_performance.csv"), row.names = FALSE)
    cat("  + table_01_model_performance.csv\n"); n <- n + 1L
  }, error = function(e) cat(sprintf("  T01 failed: %s\n", e$message)))

  # ── T02: Predictor Selection ────────────────────────────────────────────────
  tryCatch({
    manifest <- read.csv(file.path(proc_dir, "predictor_manifest.csv"))
    removal  <- tryCatch(
      read.csv(file.path(proc_dir, "predictor_removal_log.csv")),
      error = function(e) data.frame(predictor = character(), reason = character(),
                                     stringsAsFactors = FALSE)
    )

    manifest$Category       <- sapply(manifest$predictor, pred_type)
    manifest$Status         <- ifelse(manifest$selected, "Selected", "Removed")
    manifest$Removal_reason <- sapply(manifest$predictor, function(p) {
      idx <- which(removal$predictor == p)
      r   <- if (length(idx) > 0) as.character(removal$reason[idx[1]]) else ""
      # Fill known missing reasons
      if (nzchar(r)) r
      else if (grepl("dist_to_roads", p, ignore.case = TRUE)) "Not available in study area"
      else ""
    })

    t2 <- data.frame(
      Predictor      = manifest$predictor,
      Category       = manifest$Category,
      Status         = manifest$Status,
      Removal_reason = manifest$Removal_reason,
      stringsAsFactors = FALSE
    )
    # Sort: Selected first within each category, then removed
    t2 <- t2[order(t2$Category, t2$Status != "Selected", t2$Predictor), ]
    write.csv(t2, file.path(fig_dir, "table_02_predictor_selection.csv"), row.names = FALSE)
    cat("  + table_02_predictor_selection.csv\n"); n <- n + 1L
  }, error = function(e) cat(sprintf("  T02 failed: %s\n", e$message)))

  # ── T03: Habitat Area Trajectories ─────────────────────────────────────────
  tryCatch({
    traj_csv <- file.path(fig_dir, "table_trajectories_all_ssp.csv")
    traj     <- read.csv(traj_csv)

    pres_ens  <- file.path(pres_dir, "suitability_present_ensemble.tif")
    total_km2 <- NA_real_; pres_pct <- NA_real_
    if (file.exists(pres_ens)) {
      r_p       <- rast(pres_ens)
      n_valid   <- sum(!is.na(values(r_p)))
      cell_km2  <- prod(res(r_p)) / 1e6
      total_km2 <- round(n_valid * cell_km2, 0)
      pres_pct  <- round(100 * sum(values(r_p) >= threshold, na.rm = TRUE) / n_valid, 1)
      rm(r_p); gc(verbose = FALSE)
    }

    t3 <- data.frame(
      Scenario          = ssp_lbl[toupper(traj$ssp)],
      Period            = period_lbl[traj$period],
      Pct_suitable      = round(traj$pct_suitable, 1),
      Area_km2          = if (!is.na(total_km2)) round(traj$pct_suitable / 100 * total_km2, 0) else NA_real_,
      Delta_pct         = if (!is.na(pres_pct))  round(traj$pct_suitable - pres_pct, 1) else NA_real_,
      stringsAsFactors  = FALSE
    )
    # Replace NA scenario labels with original
    if (any(is.na(t3$Scenario))) t3$Scenario[is.na(t3$Scenario)] <- traj$ssp[is.na(t3$Scenario)]
    if (any(is.na(t3$Period)))   t3$Period[is.na(t3$Period)]     <- traj$period[is.na(t3$Period)]

    attr(t3, "Present_baseline_pct") <- pres_pct  # stored as attribute, not column
    write.csv(t3, file.path(fig_dir, "table_03_habitat_trajectories.csv"), row.names = FALSE)
    cat(sprintf("  + table_03_habitat_trajectories.csv (present baseline = %.1f%%)\n",
                if (!is.na(pres_pct)) pres_pct else -1))
    n <- n + 1L
  }, error = function(e) cat(sprintf("  T03 failed: %s\n", e$message)))

  # ── T04: PA Coverage ────────────────────────────────────────────────────────
  tryCatch({
    pa_csv <- file.path(fig_dir, "table_E05_pa_coverage.csv")
    if (!file.exists(pa_csv)) stop("table_E05_pa_coverage.csv not found")
    base_t4 <- read.csv(pa_csv)

    t4 <- data.frame(
      Zone         = base_t4$zone,
      Pct_suitable = round(base_t4$pct_suitable, 1),
      Threshold    = round(base_t4$threshold,    3),
      stringsAsFactors = FALSE
    )

    pres_ens <- file.path(pres_dir, "suitability_present_ensemble.tif")
    if (file.exists(pa_shp) && file.exists(pres_ens)) {
      suit     <- rast(pres_ens)
      pa_sf    <- tryCatch(st_transform(st_make_valid(st_read(pa_shp, quiet = TRUE)),
                                        crs(suit)), error = function(e) NULL)
      if (!is.null(pa_sf)) {
        pa_v       <- vect(pa_sf)
        pa_mask    <- rasterize(pa_v, suit, field = 1, background = 0)
        cell_km2   <- prod(res(suit)) / 1e6
        n_pa       <- sum(values(pa_mask) == 1, na.rm = TRUE)
        n_total    <- sum(!is.na(values(suit)))
        n_outside  <- n_total - n_pa
        t4$Area_km2     <- round(c(n_pa, n_outside, n_total) * cell_km2, 0)
        t4$Suitable_km2 <- round(t4$Pct_suitable / 100 * t4$Area_km2, 0)
        rm(suit, pa_mask); gc(verbose = FALSE)
      }
    }
    write.csv(t4, file.path(fig_dir, "table_04_pa_coverage.csv"), row.names = FALSE)
    cat("  + table_04_pa_coverage.csv\n"); n <- n + 1L
  }, error = function(e) cat(sprintf("  T04 failed: %s\n", e$message)))

  # ── T06: Gain / Loss / Persistence ─────────────────────────────────────────
  tryCatch({
    if (!dir.exists(chg_dir)) stop("change_metrics directory not found")
    gain_f <- list.files(chg_dir, "^gain_ensemble_.*\\.tif$",    full.names = TRUE)
    loss_f <- list.files(chg_dir, "^loss_ensemble_.*\\.tif$",    full.names = TRUE)
    pers_f <- list.files(chg_dir, "^persistence_ensemble_.*\\.tif$", full.names = TRUE)
    if (length(gain_f) == 0) stop("no gain rasters in change_metrics")

    parse_chg <- function(f) {
      b      <- tools::file_path_sans_ext(basename(f))
      m      <- regexpr("ssp[0-9]+", b, ignore.case = TRUE)
      ssp    <- if (m > 0) toupper(regmatches(b, m)) else NA_character_
      yrs    <- regmatches(b, gregexpr("[0-9]{4}", b))[[1]]
      per    <- if (length(yrs) >= 2) paste(yrs[1], yrs[2], sep = "-") else NA_character_
      data.frame(path = f, ssp = ssp, period = per, stringsAsFactors = FALSE)
    }

    gi  <- do.call(rbind, lapply(gain_f, parse_chg)); gi <- gi[!is.na(gi$ssp), ]
    li  <- do.call(rbind, lapply(loss_f, parse_chg)); li <- li[!is.na(li$ssp), ]
    pi  <- do.call(rbind, lapply(pers_f, parse_chg)); pi <- pi[!is.na(pi$ssp), ]

    pres_ens <- file.path(pres_dir, "suitability_present_ensemble.tif")
    cell_km2 <- if (file.exists(pres_ens)) {
      r <- rast(pres_ens); v <- prod(res(r)) / 1e6; rm(r); v
    } else NA_real_

    t6_rows <- list()
    for (i in seq_len(nrow(gi))) {
      lm  <- li[li$ssp == gi$ssp[i] & li$period == gi$period[i], ]
      pm  <- pi[pi$ssp == gi$ssp[i] & pi$period == gi$period[i], ]
      g   <- tryCatch({ r <- rast(gi$path[i]); v <- round(sum(values(r) == 1, na.rm = TRUE) * cell_km2, 0); rm(r); v }, error = function(e) NA_real_)
      l   <- tryCatch({ r <- rast(lm$path[1]); v <- round(sum(values(r) == 1, na.rm = TRUE) * cell_km2, 0); rm(r); v }, error = function(e) NA_real_)
      p   <- tryCatch({ r <- rast(pm$path[1]); v <- round(sum(values(r) == 1, na.rm = TRUE) * cell_km2, 0); rm(r); v }, error = function(e) NA_real_)
      net <- if (!anyNA(c(g, l))) g - l else NA_real_
      pst <- if (!anyNA(c(p, l))) p + l else NA_real_
      pct <- if (!anyNA(c(net, pst)) && pst > 0) round(net / pst * 100, 1) else NA_real_
      t6_rows[[i]] <- data.frame(
        SSP             = ssp_lbl[gi$ssp[i]],
        Period          = period_lbl[gi$period[i]],
        Gain_km2        = g,
        Loss_km2        = l,
        Persistence_km2 = p,
        Net_change_km2  = net,
        Pct_net_change  = pct,
        stringsAsFactors = FALSE
      )
      gc(verbose = FALSE)
    }
    t6 <- do.call(rbind, t6_rows)
    # Replace NA labels
    t6$SSP[is.na(t6$SSP)]       <- gi$ssp[is.na(t6$SSP)]
    t6$Period[is.na(t6$Period)] <- gi$period[is.na(t6$Period)]
    t6 <- t6[order(t6$SSP, t6$Period), ]
    write.csv(t6, file.path(fig_dir, "table_06_gain_loss_persistence.csv"), row.names = FALSE)
    cat("  + table_06_gain_loss_persistence.csv\n"); n <- n + 1L
  }, error = function(e) cat(sprintf("  T06 failed: %s\n", e$message)))

  # ── S1: Variable Importance — all algorithms ────────────────────────────────
  tryCatch({
    imp_csv <- file.path(fig_dir, "table_E07_variable_importance.csv")
    if (!file.exists(imp_csv)) stop("table_E07_variable_importance.csv not found")
    imp <- read.csv(imp_csv)
    imp$Algorithm              <- toupper(imp$algorithm)
    imp$Relative_importance_pct <- round(imp$importance, 1)
    imp$Category               <- sapply(imp$predictor, pred_type)

    # Check if we have all algorithms — if not, attempt model load
    present_algos <- unique(toupper(imp$algorithm))
    expected_algos <- c("GLM", "RF", "BRT", "MAXENT")
    missing_algos  <- setdiff(expected_algos, present_algos)

    if (length(missing_algos) > 0) {
      cat(sprintf("  S1: Missing algorithms %s — attempting model load\n",
                  paste(missing_algos, collapse = ", ")))

      # Load modeling dataset to get predictor names
      dat <- tryCatch(read.csv(file.path(proc_dir, "modeling_dataset.csv")), error = function(e) NULL)
      pred_cols <- if (!is.null(dat)) {
        setdiff(names(dat), c("id","type","response","longitude","latitude","fold"))
      } else character(0)

      extra_rows <- list()

      # RF
      if ("RF" %in% missing_algos) {
        tryCatch({
          if (!requireNamespace("ranger", quietly = TRUE)) library(ranger)
          mdl <- readRDS(file.path(mod_dir, "model_rf.rds"))
          vi  <- mdl$variable.importance
          vi  <- vi[names(vi) %in% pred_cols]
          vi_n <- vi / max(vi, na.rm = TRUE) * 100
          extra_rows[["RF"]] <- data.frame(
            Algorithm = "RF", predictor = names(vi_n),
            Relative_importance_pct = round(vi_n, 1),
            Category = sapply(names(vi_n), pred_type),
            stringsAsFactors = FALSE
          )
          rm(mdl); gc(verbose = FALSE)
        }, error = function(e) cat(sprintf("  RF importance load failed: %s\n", e$message)))
      }

      # BRT
      if ("BRT" %in% missing_algos) {
        tryCatch({
          if (!requireNamespace("gbm", quietly = TRUE)) library(gbm)
          mdl  <- readRDS(file.path(mod_dir, "model_brt.rds"))
          ri   <- tryCatch(
            { s <- summary(mdl, plotit = FALSE); setNames(s$rel.inf, s$var) },
            error = function(e) gbm::relative.influence(mdl, n.trees = mdl$n.trees)
          )
          ri   <- ri[names(ri) %in% pred_cols]
          ri_n <- ri / max(ri, na.rm = TRUE) * 100
          extra_rows[["BRT"]] <- data.frame(
            Algorithm = "BRT", predictor = names(ri_n),
            Relative_importance_pct = round(ri_n, 1),
            Category = sapply(names(ri_n), pred_type),
            stringsAsFactors = FALSE
          )
          rm(mdl); gc(verbose = FALSE)
        }, error = function(e) cat(sprintf("  BRT importance load failed: %s\n", e$message)))
      }

      if (length(extra_rows) > 0) {
        extra_df <- do.call(rbind, extra_rows)
        imp <- rbind(
          imp[, c("Algorithm","predictor","Relative_importance_pct","Category")],
          extra_df
        )
      }
    }

    t5 <- imp[, c("Algorithm","predictor","Category","Relative_importance_pct"), drop = FALSE]
    names(t5)[names(t5) == "predictor"] <- "Predictor"
    t5 <- t5[order(t5$Algorithm, -t5$Relative_importance_pct), ]
    write.csv(t5, file.path(fig_dir, "table_S1_variable_importance.csv"), row.names = FALSE)
    cat(sprintf("  + table_S1_variable_importance.csv (%d algorithms)\n",
                length(unique(t5$Algorithm))))
    n <- n + 1L
  }, error = function(e) cat(sprintf("  S1 failed: %s\n", e$message)))

  # ── S2: GCM Reliability Ranking ─────────────────────────────────────────────
  tryCatch({
    gcm_csv <- file.path(fig_dir, "table_gcm_reliability.csv")
    if (!file.exists(gcm_csv)) stop("table_gcm_reliability.csv not found")
    t7 <- read.csv(gcm_csv)
    names(t7) <- c("Rank","GCM","Algorithm_SD","Ensemble_deviation","Composite_score")
    t7$Algorithm_SD       <- round(t7$Algorithm_SD,       4)
    t7$Ensemble_deviation <- round(t7$Ensemble_deviation, 4)
    t7$Composite_score    <- round(t7$Composite_score,    3)
    write.csv(t7, file.path(fig_dir, "table_S2_gcm_reliability.csv"), row.names = FALSE)
    cat("  + table_S2_gcm_reliability.csv\n"); n <- n + 1L
  }, error = function(e) cat(sprintf("  S2 failed: %s\n", e$message)))

  # ── S3: Spatial CV Fold Summary ─────────────────────────────────────────────
  tryCatch({
    folds_csv <- file.path(proc_dir, "fold_assignments.csv")
    oof_csv   <- file.path(proc_dir, "oof_predictions.csv")
    if (!file.exists(folds_csv) || !file.exists(oof_csv))
      stop("fold_assignments.csv or oof_predictions.csv not found")

    folds     <- read.csv(folds_csv)
    oof       <- read.csv(oof_csv)
    fold_ids  <- sort(unique(folds$fold))
    algo_cols <- grep("^pred_", names(oof), value = TRUE)

    t8_rows <- list()
    for (fd in fold_ids) {
      fold_pts <- folds[folds$fold == fd, ]
      n_pres   <- sum(fold_pts$type == "presence",  na.rm = TRUE)
      n_abs    <- sum(fold_pts$type != "presence",  na.rm = TRUE)
      auc_vals <- vapply(algo_cols, function(col) {
        rows <- "fold" %in% names(oof) & oof$fold == fd & !is.na(oof[[col]])
        if (!any(rows) || sum(rows) < 5) return(NA_real_)
        auc_fn(oof$response[rows], oof[[col]][rows])
      }, numeric(1))
      names(auc_vals) <- paste0("AUC_", toupper(sub("pred_", "", algo_cols)))
      t8_rows[[length(t8_rows) + 1L]] <- c(
        list(Fold = fd, N_presences = n_pres, N_absences = n_abs),
        as.list(round(auc_vals, 3))
      )
    }
    t8 <- do.call(rbind, lapply(t8_rows, as.data.frame))
    write.csv(t8, file.path(fig_dir, "table_S3_cv_fold_summary.csv"), row.names = FALSE)
    cat("  + table_S3_cv_fold_summary.csv\n"); n <- n + 1L
  }, error = function(e) cat(sprintf("  S3 failed: %s\n", e$message)))

  # ── S4: Per-Protected-Area Suitability ──────────────────────────────────────
  tryCatch({
    pres_ens <- file.path(pres_dir, "suitability_present_ensemble.tif")
    if (!file.exists(pa_shp) || !file.exists(pres_ens))
      stop("PA shapefile or ensemble raster not found")

    suit     <- rast(pres_ens)
    pa_sf    <- st_transform(st_make_valid(st_read(pa_shp, quiet = TRUE)), crs(suit))
    cell_km2 <- prod(res(suit)) / 1e6
    name_col <- intersect(c("park","PA_name","Name","NAME","PA_Name","WDPANAME"), names(pa_sf))[1]

    t9_rows <- list()
    for (i in seq_len(nrow(pa_sf))) {
      pa_one <- pa_sf[i, ]
      tryCatch({
        r_m    <- mask(crop(suit, vect(pa_one)), vect(pa_one))
        vals   <- as.numeric(values(r_m, na.rm = TRUE))
        if (length(vals) < 3) return(NULL)
        pa_name <- if (!is.na(name_col)) as.character(pa_one[[name_col]]) else paste0("PA_", i)
        t9_rows[[i]] <- data.frame(
          PA_id        = i,
          PA_name      = pa_name,
          Area_km2     = round(length(vals) * cell_km2, 1),
          Mean_suit    = round(mean(vals), 3),
          SD_suit      = round(sd(vals),   3),
          Min_suit     = round(min(vals),  3),
          Max_suit     = round(max(vals),  3),
          Pct_suitable = round(100 * mean(vals >= threshold), 1),
          Suitable_km2 = round(sum(vals >= threshold) * cell_km2, 1),
          stringsAsFactors = FALSE
        )
      }, error = function(e) NULL)
      if (i %% 5 == 0) gc(verbose = FALSE)
    }
    t9 <- do.call(rbind, Filter(Negate(is.null), t9_rows))
    t9 <- t9[order(-t9$Mean_suit), ]
    rm(suit); gc(verbose = FALSE)
    write.csv(t9, file.path(fig_dir, "table_S4_pa_suitability.csv"), row.names = FALSE)
    cat(sprintf("  + table_S4_pa_suitability.csv (%d PAs)\n", nrow(t9))); n <- n + 1L
  }, error = function(e) cat(sprintf("  S4 failed: %s\n", e$message)))

  # ── S5: GCM Ensemble Spread ──────────────────────────────────────────────────
  tryCatch({
    if (!dir.exists(unc_dir)) stop("uncertainty directory not found")
    sd_files <- list.files(unc_dir, "^gcm_sd_.*\\.tif$", full.names = TRUE)
    if (length(sd_files) == 0) stop("no gcm_sd rasters found")

    parse_sd <- function(f) {
      b   <- tools::file_path_sans_ext(basename(f))
      b   <- sub("^gcm_sd_", "", b)
      ssp <- toupper(regmatches(b, regexpr("ssp[0-9]+", b, ignore.case = TRUE)))
      yr  <- regmatches(b, regexpr("[0-9]{4}", b))
      data.frame(path = f, ssp = ssp, year = yr, stringsAsFactors = FALSE)
    }
    sd_idx <- do.call(rbind, lapply(sd_files, parse_sd))
    period_map <- c("2021" = "2021-2050", "2051" = "2051-2080", "2071" = "2071-2100")

    t10_rows <- list()
    for (i in seq_len(nrow(sd_idx))) {
      r    <- tryCatch(rast(sd_idx$path[i]), error = function(e) NULL)
      if (is.null(r)) next
      vals   <- as.numeric(values(r, na.rm = TRUE)); rm(r)
      period <- period_map[sd_idx$year[i]]
      if (is.na(period)) period <- sd_idx$year[i]
      t10_rows[[i]] <- data.frame(
        SSP           = ssp_lbl[sd_idx$ssp[i]],
        Period        = period_lbl[period],
        Mean_GCM_SD   = round(mean(vals),   4),
        Median_GCM_SD = round(median(vals), 4),
        Max_GCM_SD    = round(max(vals),    4),
        Pct_high_SD   = round(100 * mean(vals > 0.1), 1),
        stringsAsFactors = FALSE
      )
      gc(verbose = FALSE)
    }
    t10 <- do.call(rbind, t10_rows)
    t10 <- t10[order(t10$SSP, t10$Period), ]
    write.csv(t10, file.path(fig_dir, "table_S5_gcm_ensemble_spread.csv"), row.names = FALSE)
    cat("  + table_S5_gcm_ensemble_spread.csv\n"); n <- n + 1L
  }, error = function(e) cat(sprintf("  S5 failed: %s\n", e$message)))

  # ── T14: Occurrence Data Summary ────────────────────────────────────────────
  tryCatch({
    rpt_file <- file.path(proc_dir, "occurrence_processing_report.txt")
    if (!file.exists(rpt_file)) stop("occurrence_processing_report.txt not found")
    rpt <- readLines(rpt_file)

    get_val <- function(key) {
      line <- rpt[grepl(paste0("^", key, ":"), rpt)]
      if (length(line) == 0) return(NA_character_)
      trimws(sub(paste0("^", key, ":\\s*"), "", line[1]))
    }

    n_in      <- as.integer(get_val("records_input"))
    n_qc      <- as.integer(get_val("records_after_coord_qc"))
    n_pres    <- as.integer(get_val("presences_final"))
    n_abs     <- as.integer(get_val("absences_final"))
    src_file  <- basename(get_val("input"))

    t14 <- data.frame(
      Processing_step  = c(
        "Raw input records",
        "After coordinate QC",
        "Final presences",
        "Final absences (background)",
        "Presence:absence ratio"
      ),
      Count = c(
        n_in, n_qc, n_pres, n_abs, NA_integer_
      ),
      Notes = c(
        paste0("Source: ", src_file),
        "Coordinate validity checks (bounds, duplicates, precision)",
        "Confirmed presence records retained for modelling",
        "Confirmed absence / background pseudo-absence records",
        sprintf("1 : %.1f (presence:background)", if (!is.na(n_abs) && !is.na(n_pres) && n_pres > 0) n_abs / n_pres else NA_real_)
      ),
      stringsAsFactors = FALSE
    )
    write.csv(t14, file.path(fig_dir, "table_14_occurrence_summary.csv"), row.names = FALSE)
    cat(sprintf("  + table_14_occurrence_summary.csv (%d pres, %d abs)\n", n_pres, n_abs))
    n <- n + 1L
  }, error = function(e) cat(sprintf("  T14 failed: %s\n", e$message)))

  # ── T15: Confusion Matrix per Algorithm ─────────────────────────────────────
  tryCatch({
    oof_csv <- file.path(proc_dir, "oof_predictions.csv")
    if (!file.exists(oof_csv)) stop("oof_predictions.csv not found")
    oof <- read.csv(oof_csv)

    pred_cols <- intersect(paste0("pred_", c("glm","rf","brt","maxent")), names(oof))
    if (length(pred_cols) == 0) stop("no prediction columns in OOF data")

    t15_rows <- list()
    for (pc in pred_cols) {
      algo <- toupper(sub("pred_", "", pc))
      thr  <- if (!is.null(eval_df)) {
        r <- eval_df[tolower(eval_df$algorithm) == tolower(sub("pred_", "", pc)), ]
        if (nrow(r) > 0 && "threshold" %in% names(r) && is.finite(r$threshold[1]))
          r$threshold[1] else threshold
      } else threshold

      ok  <- !is.na(oof[[pc]]) & !is.na(oof$response)
      y   <- as.integer(oof$response[ok])
      p   <- as.numeric(oof[[pc]][ok])
      bin <- as.integer(p >= thr)

      TP  <- sum(bin == 1L & y == 1L)
      TN  <- sum(bin == 0L & y == 0L)
      FP  <- sum(bin == 1L & y == 0L)
      FN  <- sum(bin == 0L & y == 1L)

      sens <- if ((TP + FN) > 0) TP / (TP + FN) else NA_real_
      spec <- if ((TN + FP) > 0) TN / (TN + FP) else NA_real_
      ppv  <- if ((TP + FP) > 0) TP / (TP + FP) else NA_real_
      npv  <- if ((TN + FN) > 0) TN / (TN + FN) else NA_real_

      t15_rows[[length(t15_rows) + 1L]] <- data.frame(
        Algorithm   = algo,
        Threshold   = round(thr, 4),
        TP = TP, TN = TN, FP = FP, FN = FN,
        Sensitivity = round(sens, 3),
        Specificity = round(spec, 3),
        PPV         = round(ppv,  3),
        NPV         = round(npv,  3),
        stringsAsFactors = FALSE
      )
    }
    t15 <- do.call(rbind, t15_rows)
    write.csv(t15, file.path(fig_dir, "table_15_confusion_matrix.csv"), row.names = FALSE)
    cat("  + table_15_confusion_matrix.csv\n"); n <- n + 1L
  }, error = function(e) cat(sprintf("  T15 failed: %s\n", e$message)))

  # ── T16: Threshold Sensitivity ──────────────────────────────────────────────
  tryCatch({
    pres_ens <- file.path(pres_dir, "suitability_present_ensemble.tif")
    if (!file.exists(pres_ens)) stop("ensemble raster not found")
    r    <- rast(pres_ens)
    vals <- as.numeric(values(r, na.rm = TRUE)); rm(r); gc(verbose = FALSE)

    key_thresholds <- c(0.05, 0.10, 0.15, round(threshold, 3), 0.20, 0.25, 0.30, 0.40, 0.50)
    key_thresholds <- sort(unique(key_thresholds))

    total_km2_thr <- NA_real_
    pres_ens2 <- rast(pres_ens)
    n_valid   <- sum(!is.na(values(pres_ens2)))
    c_km2     <- prod(res(pres_ens2)) / 1e6
    total_km2_thr <- n_valid * c_km2
    rm(pres_ens2); gc(verbose = FALSE)

    t16 <- data.frame(
      Threshold    = key_thresholds,
      Pct_suitable = round(vapply(key_thresholds,
        function(t) 100 * mean(vals >= t, na.rm = TRUE), numeric(1)), 1),
      Area_km2     = round(vapply(key_thresholds,
        function(t) sum(vals >= t, na.rm = TRUE) * c_km2, numeric(1)), 0),
      Is_current   = key_thresholds == round(threshold, 3),
      stringsAsFactors = FALSE
    )
    write.csv(t16, file.path(fig_dir, "table_16_threshold_sensitivity.csv"), row.names = FALSE)
    cat("  + table_16_threshold_sensitivity.csv\n"); n <- n + 1L
  }, error = function(e) cat(sprintf("  T16 failed: %s\n", e$message)))

  # ── T17: Niche Overlap (Schoener's D + Hellinger's I) ───────────────────────
  tryCatch({
    pres_ens <- file.path(pres_dir, "suitability_present_ensemble.tif")
    if (!file.exists(pres_ens)) stop("present ensemble raster not found")
    if (!dir.exists(fut_dir))   stop("future_projections directory not found")

    fut_files <- list.files(fut_dir, "^future_gcm_ensemble_.*\\.tif$", full.names = TRUE)
    if (length(fut_files) == 0) stop("no future ensemble rasters found")

    r_pres <- rast(pres_ens)
    p_vals <- as.numeric(values(r_pres, na.rm = FALSE))

    parse_fut <- function(f) {
      b   <- tools::file_path_sans_ext(basename(f))
      m   <- regexpr("ssp[0-9]+", b, ignore.case = TRUE)
      ssp <- if (m > 0) toupper(regmatches(b, m)) else NA_character_
      yrs <- regmatches(b, gregexpr("[0-9]{4}", b))[[1]]
      per <- if (length(yrs) >= 2) paste(yrs[1], yrs[2], sep = "-") else NA_character_
      data.frame(path = f, ssp = ssp, period = per, stringsAsFactors = FALSE)
    }
    fut_idx <- do.call(rbind, lapply(fut_files, parse_fut))
    fut_idx <- fut_idx[!is.na(fut_idx$ssp), ]

    t17_rows <- list()
    for (i in seq_len(nrow(fut_idx))) {
      tryCatch({
        r_fut   <- rast(fut_idx$path[i])
        f_vals  <- as.numeric(values(r_fut, na.rm = FALSE)); rm(r_fut)

        # Use only pixels valid in BOTH rasters
        both_ok <- !is.na(p_vals) & !is.na(f_vals) & p_vals >= 0 & f_vals >= 0
        pv      <- p_vals[both_ok]
        fv      <- f_vals[both_ok]

        sp <- sum(pv); sf <- sum(fv)
        if (sp == 0 || sf == 0) return(NULL)
        pn <- pv / sp; fn2 <- fv / sf

        # Schoener's D = 1 - 0.5 * sum(|p - q|)
        D <- 1 - 0.5 * sum(abs(pn - fn2))
        # Hellinger's I = 1 - 0.5 * sum((sqrt(p) - sqrt(q))^2)
        I <- 1 - 0.5 * sum((sqrt(pn) - sqrt(fn2))^2)

        t17_rows[[length(t17_rows) + 1L]] <- data.frame(
          Comparison  = sprintf("Present vs %s %s",
                                ssp_lbl[fut_idx$ssp[i]], period_lbl[fut_idx$period[i]]),
          SSP         = ssp_lbl[fut_idx$ssp[i]],
          Period      = period_lbl[fut_idx$period[i]],
          Schoener_D  = round(D, 4),
          Hellinger_I = round(I, 4),
          stringsAsFactors = FALSE
        )
        gc(verbose = FALSE)
      }, error = function(e) NULL)
    }
    rm(r_pres); gc(verbose = FALSE)

    if (length(t17_rows) == 0) stop("niche overlap computation failed for all scenarios")
    t17 <- do.call(rbind, t17_rows)
    t17 <- t17[order(t17$SSP, t17$Period), ]
    write.csv(t17, file.path(fig_dir, "table_17_niche_overlap.csv"), row.names = FALSE)
    cat(sprintf("  + table_17_niche_overlap.csv (%d comparisons)\n", nrow(t17)))
    n <- n + 1L
  }, error = function(e) cat(sprintf("  T17 failed: %s\n", e$message)))

  # ── T18: Spatial CV Block Details ───────────────────────────────────────────
  tryCatch({
    folds_csv <- file.path(proc_dir, "fold_assignments.csv")
    if (!file.exists(folds_csv)) stop("fold_assignments.csv not found")
    folds <- read.csv(folds_csv)

    fold_ids  <- sort(unique(folds$fold))
    n_total_p <- sum(folds$type == "presence", na.rm = TRUE)

    t18_rows <- list()
    for (fd in fold_ids) {
      fp <- folds[folds$fold == fd, ]
      np <- sum(fp$type == "presence",  na.rm = TRUE)
      na <- sum(fp$type != "presence",  na.rm = TRUE)
      t18_rows[[length(t18_rows) + 1L]] <- data.frame(
        Fold              = fd,
        N_presences       = np,
        N_absences        = na,
        Total_records     = np + na,
        Pct_of_presences  = round(100 * np / n_total_p, 1),
        Balance_pres_abs  = round(np / max(na, 1), 3),
        stringsAsFactors  = FALSE
      )
    }
    t18 <- do.call(rbind, t18_rows)
    write.csv(t18, file.path(fig_dir, "table_18_cv_block_details.csv"), row.names = FALSE)
    cat("  + table_18_cv_block_details.csv\n"); n <- n + 1L
  }, error = function(e) cat(sprintf("  T18 failed: %s\n", e$message)))

  # ── T19: Predictor Statistics at Presence vs Background ─────────────────────
  tryCatch({
    dat_csv <- file.path(proc_dir, "modeling_dataset.csv")
    if (!file.exists(dat_csv)) stop("modeling_dataset.csv not found")
    dat <- read.csv(dat_csv)

    excl <- c("id","type","response","longitude","latitude","fold")
    pcols <- setdiff(names(dat), excl)
    pcols <- pcols[vapply(dat[, pcols, drop = FALSE], is.numeric, logical(1))]

    pres_idx <- dat$response == 1
    bg_idx   <- dat$response == 0

    t19_rows <- list()
    for (pc in pcols) {
      pv <- dat[[pc]][pres_idx]; bv <- dat[[pc]][bg_idx]
      t19_rows[[pc]] <- data.frame(
        Predictor       = pc,
        Category        = pred_type(pc),
        Pres_mean       = round(mean(pv, na.rm = TRUE), 3),
        Pres_SD         = round(sd(pv,   na.rm = TRUE), 3),
        Pres_median     = round(median(pv, na.rm = TRUE), 3),
        Bg_mean         = round(mean(bv, na.rm = TRUE), 3),
        Bg_SD           = round(sd(bv,   na.rm = TRUE), 3),
        Bg_median       = round(median(bv, na.rm = TRUE), 3),
        Mean_difference = round(mean(pv, na.rm = TRUE) - mean(bv, na.rm = TRUE), 3),
        stringsAsFactors = FALSE
      )
    }
    t19 <- do.call(rbind, t19_rows)
    t19 <- t19[order(t19$Category, t19$Predictor), ]
    write.csv(t19, file.path(fig_dir, "table_19_predictor_statistics.csv"), row.names = FALSE)
    cat(sprintf("  + table_19_predictor_statistics.csv (%d predictors)\n", nrow(t19)))
    n <- n + 1L
  }, error = function(e) cat(sprintf("  T19 failed: %s\n", e$message)))

  # ── T20: Algorithm Hyperparameters ──────────────────────────────────────────
  tryCatch({
    # Number of selected predictors (for RF mtry calculation)
    manifest <- tryCatch(read.csv(file.path(proc_dir, "predictor_manifest.csv")),
                         error = function(e) NULL)
    n_pred <- if (!is.null(manifest)) sum(manifest$selected, na.rm = TRUE) else 17L
    mtry   <- floor(sqrt(n_pred))

    t20 <- data.frame(
      Algorithm = c("GLM", "Random Forest", "BRT", "MaxEnt"),
      Package   = c("stats", "ranger", "gbm", "maxent.jar"),
      Key_parameters = c(
        "family = binomial, link = logit; forward stepwise selection by AIC",
        sprintf("num.trees = 1000, mtry = %d (floor(sqrt(%d predictors))), min.node.size = 1",
                mtry, n_pred),
        "n.trees = 1000, interaction.depth = 3, shrinkage = 0.01, bag.fraction = 0.5; OOB stopping",
        "betamultiplier = 1.5, features = lqhp (linear, quadratic, hinge, product)"
      ),
      Seed_controlled = c("Yes", "Yes", "Yes", "Yes"),
      Notes = c(
        "Spatial fold cross-validation; OOF predictions used for all evaluation",
        "Permutation importance used for variable importance",
        "Best iteration selected by OOB; relative influence for variable importance",
        "Regularization beta=1.5 chosen for small sample size (n=252 presences)"
      ),
      stringsAsFactors = FALSE
    )
    write.csv(t20, file.path(fig_dir, "table_20_algorithm_hyperparams.csv"), row.names = FALSE)
    cat("  + table_20_algorithm_hyperparams.csv\n"); n <- n + 1L
  }, error = function(e) cat(sprintf("  T20 failed: %s\n", e$message)))

  # ── T21: GCM Trajectories ───────────────────────────────────────────────────
  tryCatch({
    gcm_csv <- file.path(fig_dir, "table_gcm_trajectories.csv")
    if (!file.exists(gcm_csv)) stop("table_gcm_trajectories.csv not found")
    t21 <- read.csv(gcm_csv)
    t21$ssp    <- ssp_lbl[toupper(t21$ssp)]
    t21$period <- period_lbl[t21$period]
    if (any(is.na(t21$ssp)))    t21$ssp[is.na(t21$ssp)]       <- read.csv(gcm_csv)$ssp[is.na(t21$ssp)]
    if (any(is.na(t21$period))) t21$period[is.na(t21$period)] <- read.csv(gcm_csv)$period[is.na(t21$period)]
    names(t21) <- c("GCM","SSP","Period","Pct_suitable")
    t21 <- t21[order(t21$GCM, t21$SSP, t21$Period), ]
    write.csv(t21, file.path(fig_dir, "table_21_gcm_trajectories.csv"), row.names = FALSE)
    cat(sprintf("  + table_21_gcm_trajectories.csv (%d rows)\n", nrow(t21))); n <- n + 1L
  }, error = function(e) cat(sprintf("  T21 failed: %s\n", e$message)))

  # ── T22: Present-Day Conflict Risk Zones ────────────────────────────────────
  tryCatch({
    pres_ens  <- file.path(pres_dir, "suitability_present_ensemble.tif")
    hfp_files <- c(
      file.path(run_dir, "02_data_intermediate", "human_footprint_harmonized.tif"),
      Sys.glob(file.path(repo_root, "01_data_raw", "02_rasters", "present",
                         "*human*footprint*.tif"))
    )
    hfp_file  <- Filter(file.exists, hfp_files)[1]

    if (!file.exists(pres_ens)) stop("ensemble raster not found")
    if (is.na(hfp_file))        stop("human footprint raster not found")

    suit <- rast(pres_ens)
    hfp  <- rast(hfp_file)

    # Resample hfp to suit grid if needed
    if (!isTRUE(compareGeom(suit, hfp, stopOnError = FALSE)))
      hfp <- resample(hfp, suit, method = "bilinear")

    # Normalize both to 0-1
    s_v   <- as.numeric(values(suit, na.rm = FALSE))
    h_v   <- as.numeric(values(hfp,  na.rm = FALSE))
    h_min <- min(h_v, na.rm = TRUE); h_max <- max(h_v, na.rm = TRUE)
    h_n   <- (h_v - h_min) / max(h_max - h_min, 1e-6)

    # Risk = sqrt(suit × human_pressure) — geometric mean
    risk    <- sqrt(s_v * h_n)
    ok      <- !is.na(risk)
    cell_km2 <- prod(res(suit)) / 1e6
    rm(suit, hfp); gc(verbose = FALSE)

    # Classify by quantile-based breaks
    q_breaks <- quantile(risk[ok], probs = c(0, 0.40, 0.65, 0.85, 1.0), na.rm = TRUE)
    classes  <- c("Low", "Moderate", "High", "Very High")
    labels   <- c(
      sprintf("Risk \u2264 %.3f", q_breaks[2]),
      sprintf("%.3f \u2013 %.3f", q_breaks[2], q_breaks[3]),
      sprintf("%.3f \u2013 %.3f", q_breaks[3], q_breaks[4]),
      sprintf("> %.3f",           q_breaks[4])
    )

    t22_rows <- list()
    for (i in seq_along(classes)) {
      lo <- q_breaks[i]; hi <- q_breaks[i + 1]
      in_class <- ok & risk >= lo & (if (i < length(classes)) risk < hi else risk <= hi)
      t22_rows[[i]] <- data.frame(
        Risk_class  = classes[i],
        Risk_range  = labels[i],
        Area_km2    = round(sum(in_class, na.rm = TRUE) * cell_km2, 0),
        Pct_Bhutan  = round(100 * sum(in_class, na.rm = TRUE) / sum(ok, na.rm = TRUE), 1),
        stringsAsFactors = FALSE
      )
    }
    t22 <- do.call(rbind, t22_rows)
    write.csv(t22, file.path(fig_dir, "table_22_conflict_risk_zones.csv"), row.names = FALSE)
    cat("  + table_22_conflict_risk_zones.csv\n"); n <- n + 1L
  }, error = function(e) cat(sprintf("  T22 failed: %s\n", e$message)))

  # ── T23: Refugia Summary (stub — populated after 10_figures_refugia.R built) ─
  tryCatch({
    ref_files <- list.files(pres_dir, "refugia_core\\.tif$", full.names = TRUE)
    stab_files <- list.files(pres_dir, "refugia_stability\\.tif$", full.names = TRUE)

    if (length(ref_files) == 0 && length(stab_files) == 0) {
      # Write stub so pipeline doesn't fail silently
      t23 <- data.frame(
        Refugia_type  = character(0),
        Definition    = character(0),
        Area_km2      = numeric(0),
        Pct_Bhutan    = numeric(0),
        Pct_inside_PA = numeric(0),
        stringsAsFactors = FALSE
      )
      write.csv(t23, file.path(fig_dir, "table_23_refugia_summary.csv"), row.names = FALSE)
      cat("  + table_23_refugia_summary.csv (stub — refugia rasters not yet built)\n")
    } else {
      pres_ens  <- file.path(pres_dir, "suitability_present_ensemble.tif")
      suit      <- rast(pres_ens)
      cell_km2  <- prod(res(suit)) / 1e6
      n_total   <- sum(!is.na(values(suit)))

      pa_mask <- NULL
      if (file.exists(pa_shp)) {
        pa_sf   <- tryCatch(st_transform(st_make_valid(st_read(pa_shp, quiet = TRUE)), crs(suit)),
                            error = function(e) NULL)
        if (!is.null(pa_sf))
          pa_mask <- rasterize(vect(pa_sf), suit, field = 1, background = 0)
      }

      n_scen <- length(persist_files_t23 <- list.files(
        file.path(run_dir, "05_change_metrics"),
        pattern = "^persistence_ensemble_.*\\.tif$", full.names = FALSE))

      t23_rows <- list()

      # Core refugia row (from refugia_core.tif: cells suitable in ALL scenarios)
      for (f in ref_files) {
        r <- tryCatch(rast(f), error = function(e) NULL)
        if (is.null(r)) next
        v <- as.integer(values(r, na.rm = FALSE))
        n_ref <- sum(v == 1L, na.rm = TRUE)
        pa_pct <- if (!is.null(pa_mask)) {
          pa_v <- values(pa_mask, na.rm = FALSE)[, 1]
          round(100 * sum(v == 1L & pa_v == 1L, na.rm = TRUE) / max(n_ref, 1), 1)
        } else NA_real_
        t23_rows[[length(t23_rows) + 1L]] <- data.frame(
          Refugia_type  = "Core refugia",
          Definition    = sprintf("Suitable in ALL %d future scenarios (4 SSPs x 3 periods)", n_scen),
          Area_km2      = round(n_ref * cell_km2, 0),
          Pct_Bhutan    = round(100 * n_ref / n_total, 1),
          Pct_inside_PA = pa_pct,
          stringsAsFactors = FALSE
        )
        rm(r); gc(verbose = FALSE)
      }

      # Stability row (from refugia_stability.tif: count of scenarios suitable)
      for (f in stab_files) {
        r <- tryCatch(rast(f), error = function(e) NULL)
        if (is.null(r)) next
        v <- as.numeric(values(r, na.rm = FALSE))
        thr75 <- ceiling(n_scen * 0.75)
        n_high <- sum(v >= thr75, na.rm = TRUE)
        pa_pct <- if (!is.null(pa_mask)) {
          pa_v <- values(pa_mask, na.rm = FALSE)[, 1]
          round(100 * sum(v >= thr75 & pa_v == 1L, na.rm = TRUE) / max(n_high, 1), 1)
        } else NA_real_
        t23_rows[[length(t23_rows) + 1L]] <- data.frame(
          Refugia_type  = "High-stability refugia",
          Definition    = sprintf("Suitable in >=75%% of %d scenarios (>=%.0f)", n_scen, thr75),
          Area_km2      = round(n_high * cell_km2, 0),
          Pct_Bhutan    = round(100 * n_high / n_total, 1),
          Pct_inside_PA = pa_pct,
          stringsAsFactors = FALSE
        )
        rm(r); gc(verbose = FALSE)
      }

      t23 <- if (length(t23_rows) > 0) do.call(rbind, t23_rows) else
        data.frame(Refugia_type=character(0), Definition=character(0),
                   Area_km2=numeric(0), Pct_Bhutan=numeric(0),
                   Pct_inside_PA=numeric(0), stringsAsFactors=FALSE)
      write.csv(t23, file.path(fig_dir, "table_23_refugia_summary.csv"), row.names = FALSE)
      cat(sprintf("  + table_23_refugia_summary.csv (%d rows)\n", nrow(t23)))
    }
    n <- n + 1L
  }, error = function(e) cat(sprintf("  T23 failed: %s\n", e$message)))

  # ── T24: Per-Dzongkhag habitat suitability summary ───────────────────────────
  tryCatch({
    dzo_shp <- file.path(repo_root, "01_data_raw", "03_vector", "shapefiles",
                         "Dzongkhag Boundary", "Dzongkhag Boundary.shp")
    if (!file.exists(dzo_shp)) stop("Dzongkhag boundary shapefile not found")

    pres_r_file <- file.path(pres_dir, "suitability_present_ensemble.tif")
    if (!file.exists(pres_r_file)) stop("ensemble suitability raster not found")

    dzo  <- st_read(dzo_shp, quiet = TRUE)
    dzo  <- st_make_valid(dzo)
    suit <- rast(pres_r_file)
    if (nlyr(suit) > 1) suit <- suit[[1]]

    # Pixel area in km²
    pix_km2 <- tryCatch({
      res_m <- res(suit)
      prod(res_m) / 1e6
    }, error = function(e) 1e-6)

    # TSS-optimal ensemble threshold (from eval table or default)
    ens_thr <- tryCatch({
      ev <- read.csv(file.path(mod_dir, "evaluation_all.csv"))
      tv <- mean(ev$threshold[!is.na(ev$threshold)], na.rm = TRUE)
      if (is.finite(tv)) tv else 0.5
    }, error = function(e) 0.5)

    # The shapefile has wrong CRS metadata (labelled 32645 but coords are EPSG:3857)
    dzo_v_raw <- tryCatch(vect(dzo), error = function(e) NULL)
    if (is.null(dzo_v_raw)) stop("could not convert Dzongkhag sf to SpatVector")
    # Force correct source CRS then reproject to suit
    crs(dzo_v_raw) <- "EPSG:3857"
    dzo_v_all <- tryCatch(
      terra::project(dzo_v_raw, suit),
      error = function(e) dzo_v_raw
    )

    # Use terra::extract (robust per-polygon pixel extraction)
    ext_df <- tryCatch(
      terra::extract(suit, dzo_v_all, ID = TRUE),
      error = function(e) NULL
    )
    if (is.null(ext_df) || nrow(ext_df) == 0)
      stop("terra::extract returned no data for Dzongkhag polygons")
    names(ext_df)[2] <- "suit_val"

    dzo_names <- as.character(dzo$dzongkhag)
    dzo_codes <- if ("dzo_code" %in% names(dzo)) as.character(dzo$dzo_code) else
                 rep(NA_character_, nrow(dzo))

    rows <- list()
    for (i in seq_along(dzo_names)) {
      vals <- as.numeric(ext_df$suit_val[ext_df$ID == i])
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) next

      # Dzongkhag total area from pixel count (more accurate than SHAPE__Are in 3857)
      area_total_km2 <- length(vals) * pix_km2
      area_suit_km2  <- sum(vals >= ens_thr, na.rm = TRUE) * pix_km2
      pct_suitable   <- 100 * area_suit_km2 / area_total_km2

      rows[[length(rows) + 1]] <- data.frame(
        Dzongkhag          = dzo_names[i],
        Dzo_code           = dzo_codes[i],
        Total_area_km2     = round(area_total_km2, 1),
        Suitable_area_km2  = round(area_suit_km2, 1),
        Pct_suitable       = round(pct_suitable, 1),
        Mean_suitability   = round(mean(vals, na.rm = TRUE), 3),
        Median_suitability = round(median(vals, na.rm = TRUE), 3),
        Max_suitability    = round(max(vals, na.rm = TRUE), 3),
        Threshold_used     = round(ens_thr, 3),
        stringsAsFactors   = FALSE
      )
    }

    if (length(rows) == 0) stop("no Dzongkhag statistics computed")
    t24 <- do.call(rbind, rows)
    t24 <- t24[order(-t24$Pct_suitable), ]
    write.csv(t24, file.path(fig_dir, "table_24_dzongkhag_suitability.csv"),
              row.names = FALSE)
    cat("  + table_24_dzongkhag_suitability.csv\n")
    n <- n + 1L
  }, error = function(e) cat(sprintf("  T24 failed: %s\n", e$message)))

  cat(sprintf("\n=== Publication tables complete: %d generated ===\n", n))
  cat(sprintf("Tables written to: %s\n", fig_dir))
  invisible(list(n_tables = n))
}

# ── entry point ────────────────────────────────────────────────────────────────
if (sys.nframe() == 0) {
  args        <- commandArgs(trailingOnly = TRUE)
  run_dir     <- if (length(args) >= 1) args[[1]] else "."
  config_path <- if (length(args) >= 2) args[[2]] else "00_governance/config.yaml"
  generate_publication_tables(run_dir, config_path)
}
