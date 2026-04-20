#!/usr/bin/env Rscript
# =============================================================================
# 07_model_training.R — Multi-algorithm training with tuning
# CRITICAL FIX: Uses align_raster() for perfect prediction grid alignment
# =============================================================================

suppressPackageStartupMessages({ library(terra) })
if (!exists("align_raster", mode = "function")) {
  src_dir <- if (exists("repo_root")) file.path(repo_root, "03_analysis") else "03_analysis"
  source(file.path(src_dir, "00_spatial_alignment.R"))
}
if (!exists("auc_score", mode = "function")) {
  src_dir <- if (exists("repo_root")) file.path(repo_root, "03_analysis") else "03_analysis"
  source(file.path(src_dir, "00_sdm_helpers.R"))
}

train_all_models <- function(run_dir, run_id) {
  proc_dir <- file.path(run_dir, "01_processed_data")
  model_dir <- file.path(run_dir, "02_models")
  suit_dir <- file.path(run_dir, "03_present_suitability")
  dir.create(model_dir, recursive = TRUE)
  dir.create(suit_dir, recursive = TRUE)
  
  dat <- read.csv(file.path(proc_dir, "modeling_dataset.csv"))
  manifest <- read.csv(file.path(proc_dir, "predictor_manifest.csv"))
  pred_cols <- manifest$predictor[manifest$selected]
  
  # CRITICAL FIX: Get unified template
  template <- try(rast(file.path(proc_dir, "../02_data_intermediate/template_grid.tif")), silent = TRUE)
  if (inherits(template, "try-error")) {
    template <- get_unified_template(run_dir, config = NULL)
  }
  
  formula <- as.formula(paste("response ~", paste(pred_cols, collapse = " + ")))
  folds <- sort(unique(dat$fold[!is.na(dat$fold)]))
  if (length(folds) < 2) folds <- 1:5
  dat$fold <- ifelse(is.na(dat$fold), sample(folds, nrow(dat), replace = TRUE), dat$fold)
  
  results <- list()
  
  # Out-of-fold prediction container — used by Phase 8 for valid holdout metrics
  oof <- data.frame(
    row_id    = seq_len(nrow(dat)),
    fold      = dat$fold,
    response  = dat$response,
    longitude = if ("longitude" %in% names(dat)) dat$longitude else NA_real_,
    latitude  = if ("latitude"  %in% names(dat)) dat$latitude  else NA_real_,
    pred_glm    = NA_real_,
    pred_rf     = NA_real_,
    pred_brt    = NA_real_,
    pred_maxent = NA_real_
  )

  # GLM
  cat("Training GLM...\n")
  glm_cv <- data.frame()
  for (k in folds) {
    tr <- dat[dat$fold != k, ]; te <- dat[dat$fold == k, ]
    if (nrow(tr) < 20 || nrow(te) < 5) next
    m <- glm(formula, data = tr, family = binomial())
    p <- predict(m, newdata = te, type = "response")
    glm_cv <- rbind(glm_cv, data.frame(fold = k, auc = auc_score(te$response, p), tss = tss_best(te$response, p)["tss"]))
    oof$pred_glm[dat$fold == k] <- as.numeric(p)
  }
  if (nrow(glm_cv) == 0) warning("GLM: No successful CV folds — all folds had insufficient data")
  glm_full <- glm(formula, data = dat, family = binomial())
  saveRDS(glm_full, file.path(model_dir, "model_glm.rds"))
  results$glm <- list(auc = mean(glm_cv$auc, na.rm = TRUE), tss = mean(glm_cv$tss, na.rm = TRUE))

  # Random Forest
  cat("Training Random Forest...\n")
  if (requireNamespace("ranger", quietly = TRUE)) {
    rf_cv <- data.frame()
    for (k in folds) {
      tr <- dat[dat$fold != k, ]; te <- dat[dat$fold == k, ]
      if (nrow(tr) < 20 || nrow(te) < 5) next
      mod_seed <- if (exists("set_module_seed")) set_module_seed("model_training") else 123456L
      m <- ranger::ranger(formula, data = tr, num.trees = 1000, mtry = floor(sqrt(length(pred_cols))), importance = "permutation", seed = mod_seed)
      p <- predict(m, data = te)$predictions
      rf_cv <- rbind(rf_cv, data.frame(fold = k, auc = auc_score(te$response, p), tss = tss_best(te$response, p)["tss"]))
      oof$pred_rf[dat$fold == k] <- as.numeric(p)
    }
    if (nrow(rf_cv) == 0) warning("RF: No successful CV folds — all folds had insufficient data")
    rf_full <- ranger::ranger(formula, data = dat, num.trees = 1000, mtry = floor(sqrt(length(pred_cols))), importance = "permutation", seed = mod_seed)
    saveRDS(rf_full, file.path(model_dir, "model_rf.rds"))
    results$rf <- list(auc = mean(rf_cv$auc, na.rm = TRUE), tss = mean(rf_cv$tss, na.rm = TRUE))
  }

  # BRT (Boosted Regression Trees)
  cat("Training BRT...\n")
  if (requireNamespace("gbm", quietly = TRUE)) {
    brt_cv <- data.frame()
    for (k in folds) {
      tr <- dat[dat$fold != k, ]; te <- dat[dat$fold == k, ]
      if (nrow(tr) < 20 || nrow(te) < 5) next
      m <- gbm::gbm(formula, data = tr, distribution = "bernoulli",
                    n.trees = 1000, interaction.depth = 3, shrinkage = 0.01,
                    n.minobsinnode = 5, bag.fraction = 0.75, verbose = FALSE)
      best_iter <- gbm::gbm.perf(m, method = "OOB", plot.it = FALSE)
      if (is.na(best_iter) || best_iter < 1) best_iter <- 500L
      p <- predict(m, newdata = te, n.trees = best_iter, type = "response")
      brt_cv <- rbind(brt_cv, data.frame(fold = k, auc = auc_score(te$response, p), tss = tss_best(te$response, p)["tss"]))
      oof$pred_brt[dat$fold == k] <- as.numeric(p)
    }
    brt_full <- gbm::gbm(formula, data = dat, distribution = "bernoulli",
                         n.trees = 1000, interaction.depth = 3, shrinkage = 0.01,
                         n.minobsinnode = 5, bag.fraction = 0.75, verbose = FALSE)
    brt_best_iter <- gbm::gbm.perf(brt_full, method = "OOB", plot.it = FALSE)
    if (is.na(brt_best_iter) || brt_best_iter < 1) brt_best_iter <- 500L
    saveRDS(list(model = brt_full, best_iter = as.integer(brt_best_iter)), file.path(model_dir, "model_brt.rds"))
    if (nrow(brt_cv) == 0) warning("BRT: No successful CV folds — all folds had insufficient data")
    results$brt <- list(auc = mean(brt_cv$auc, na.rm = TRUE), tss = mean(brt_cv$tss, na.rm = TRUE))
  }

  # MaxEnt (official jar — better features: lqhp, better regularization)
  cat("Training MaxEnt (jar)...\n")
  cfg_raw       <- tryCatch(yaml::read_yaml(if (exists("config_path")) config_path else "00_governance/config.yaml"), error = function(e) list())
  maxent_jar_path <- if (!is.null(cfg_raw$tools$maxent_jar)) {
    rr <- if (exists("repo_root")) repo_root else "."
    normalizePath(file.path(rr, cfg_raw$tools$maxent_jar), winslash = "/", mustWork = FALSE)
  } else NA_character_

  jar_ok <- !is.na(maxent_jar_path) && file.exists(maxent_jar_path) &&
            nzchar(Sys.which("java"))

  if (jar_ok) {
    mx_out_dir   <- file.path(model_dir, "maxent_output")
    mx_full_dir  <- file.path(model_dir, "maxent_full")
    mx_layers_dir <- file.path(model_dir, "maxent_layers")
    for (d in c(mx_out_dir, mx_full_dir, mx_layers_dir))
      dir.create(d, recursive = TRUE, showWarnings = FALSE)

    # Write presence CSV (species, x, y)
    pres_pts <- dat[dat$response == 1, c("longitude", "latitude"), drop = FALSE]
    pres_pts <- cbind(species = "Elephas_maximus", pres_pts)
    pres_csv <- file.path(mx_out_dir, "presence.csv")
    write.csv(pres_pts, pres_csv, row.names = FALSE)

    # Write predictor rasters as ASC in WGS84 geographic CRS
    # MaxEnt expects lat/lon coordinates — presence CSV uses geographic degrees,
    # so ASC layers must also be in geographic (not UTM) to avoid bounding-box mismatches.
    msk_path <- file.path(run_dir, "02_data_intermediate", "m_mask.tif")
    msk_r    <- if (file.exists(msk_path)) tryCatch(rast(msk_path), error=function(e) NULL) else NULL
    for (pred_nm in pred_cols) {
      pred_path <- manifest$path[manifest$predictor == pred_nm]
      if (length(pred_path) == 0 || !nzchar(pred_path) || !file.exists(pred_path)) next
      r <- tryCatch(rast(pred_path), error = function(e) NULL)
      if (is.null(r)) next
      r <- align_raster(r, template, verbose = FALSE)
      if (!is.null(msk_r)) {
        msk_aligned <- if (!isTRUE(compareGeom(msk_r, r, stopOnError=FALSE)))
          project(msk_r, r, method="near") else msk_r
        r <- mask(r, msk_aligned, maskvalues = 0)
      }
      # Project to WGS84 so coordinates match the presence lat/lon CSV
      r_geo <- tryCatch(project(r[[1]], "EPSG:4326", method = "bilinear"),
                        error = function(e) NULL)
      if (is.null(r_geo)) next
      out_asc <- file.path(mx_layers_dir, paste0(pred_nm, ".asc"))
      tryCatch(writeRaster(r_geo, out_asc, overwrite = TRUE, NAflag = -9999),
               error = function(e) warning("MaxEnt ASC write failed: ", pred_nm, " — ", e$message))
    }

    # Common jar flags — betamultiplier=1.5 reduces overfitting for ~250 training points
    jar_flag_vec <- c(
      "nowarnings", "noaskoverwrite", "noprefixes", "autorun", "-x",
      "outputformat=cloglog", "pictures=false", "plots=false",
      "writeclampgrid=false", "writebackgroundpredictions=false",
      "betamultiplier=1.5"
    )

    # Count ASC layers written — MaxEnt needs at least 1 to run
    n_asc_layers <- length(list.files(mx_layers_dir, pattern = "\\.asc$"))
    cat(sprintf("  MaxEnt: %d predictor ASC layers written\n", n_asc_layers))

    mx_log_cv   <- file.path(model_dir, "maxent_cv.log")
    mx_log_full <- file.path(model_dir, "maxent_full.log")

    # CV run — use system2() to pass args directly (avoids shell-redirect ambiguity on Windows)
    java_args_cv <- c(
      "-Xmx1g", "-jar", maxent_jar_path,
      paste0("samplesfile=", pres_csv),
      paste0("environmentallayers=", mx_layers_dir),
      paste0("outputdirectory=", mx_out_dir),
      "replicates=5", "replicatetype=crossvalidate",
      jar_flag_vec
    )
    ret_cv <- system2("java", args = java_args_cv, stdout = mx_log_cv, stderr = TRUE, wait = TRUE)
    if (!identical(ret_cv, 0L) && !identical(ret_cv, 0))
      cat(sprintf("  MaxEnt CV returned exit code %s — see %s\n", ret_cv, mx_log_cv))

    mx_auc <- NA_real_; mx_tss <- NA_real_
    res_csv <- file.path(mx_out_dir, "maxentResults.csv")
    if (file.exists(res_csv)) {
      mx_res <- tryCatch(read.csv(res_csv), error = function(e) NULL)
      if (!is.null(mx_res)) {
        auc_col <- grep("Test.AUC", names(mx_res), value = TRUE, ignore.case = TRUE)[1]
        if (!is.na(auc_col))
          mx_auc <- mean(as.numeric(mx_res[[auc_col]]), na.rm = TRUE)
      }
    }

    # Full model run (no replicates — produces raster + lambdas for Phase 9)
    java_args_full <- c(
      "-Xmx1g", "-jar", maxent_jar_path,
      paste0("samplesfile=", pres_csv),
      paste0("environmentallayers=", mx_layers_dir),
      paste0("outputdirectory=", mx_full_dir),
      jar_flag_vec
    )
    ret_full <- system2("java", args = java_args_full, stdout = mx_log_full, stderr = TRUE, wait = TRUE)
    if (!identical(ret_full, 0L) && !identical(ret_full, 0))
      cat(sprintf("  MaxEnt full returned exit code %s — see %s\n", ret_full, mx_log_full))

    # Copy lambdas for Phase 9
    lf <- list.files(mx_full_dir, pattern = "\\.lambdas$", full.names = TRUE)
    lambdas_dest <- file.path(model_dir, "maxent.lambdas")
    if (length(lf) > 0) file.copy(lf[1], lambdas_dest, overwrite = TRUE)

    # Load full-model suitability raster; ASC format strips CRS — restore EPSG:32645
    # Priority: (1) maxent_full/*.asc  (2) maxent_output/*_avg.asc (CV average — equally valid)
    mx_jar_raster <- NULL
    mx_asc <- list.files(mx_full_dir, pattern = "\\.asc$", full.names = TRUE)
    mx_asc <- mx_asc[!grepl("clamp|mess|novel|_limit", mx_asc, ignore.case = TRUE)]
    # Fallback: use avg.asc from CV output if full model produced no raster
    if (length(mx_asc) == 0) {
      cv_avg <- list.files(mx_out_dir, pattern = "_avg\\.asc$", full.names = TRUE)
      if (length(cv_avg) > 0) {
        mx_asc <- cv_avg
        cat("  MaxEnt: using CV avg.asc from maxent_output (full model produced no raster)\n")
      }
    }
    if (length(mx_asc) > 0 && file.exists(mx_asc[1])) {
      r_mx <- tryCatch(rast(mx_asc[1]), error = function(e) NULL)
      if (!is.null(r_mx)) {
        crs(r_mx) <- "EPSG:4326"  # ASC has no CRS metadata — layers were written in WGS84 for MaxEnt
        mx_jar_raster <- align_raster(r_mx, template, verbose = FALSE)
      }
    }

    # OOF predictions for MaxEnt:
    #   Presences — true held-out predictions from CV samplePredictions CSVs (unbiased)
    #   Absences  — full-model raster predictions (best available proxy)
    if (!is.null(mx_jar_raster)) {
      # Initialize all points from full-model raster (covers absences + presence fallback)
      pts_all <- vect(dat[, c("longitude", "latitude")],
                      geom = c("longitude", "latitude"), crs = "EPSG:4326")
      pts_proj <- project(pts_all, crs(mx_jar_raster))
      ex <- tryCatch(extract(mx_jar_raster, pts_proj), error = function(e) NULL)
      if (!is.null(ex) && ncol(ex) >= 2)
        oof$pred_maxent <- as.numeric(ex[[2]])

      # Override presence OOF with true CV held-out predictions
      sp_csvs <- list.files(mx_out_dir, pattern = "samplePredictions\\.csv$",
                            full.names = TRUE, recursive = FALSE)
      if (length(sp_csvs) > 0) {
        sp_all <- tryCatch({
          do.call(rbind, lapply(sp_csvs, function(f) {
            d <- tryCatch(read.csv(f), error = function(e) NULL)
            if (is.null(d)) return(NULL)
            test_col <- grep("test.or.train|test_or_train", names(d), value = TRUE, ignore.case = TRUE)[1]
            if (is.na(test_col)) return(NULL)
            d[grepl("^test$", trimws(as.character(d[[test_col]])), ignore.case = TRUE), , drop = FALSE]
          }))
        }, error = function(e) NULL)

        if (!is.null(sp_all) && nrow(sp_all) > 0) {
          pred_col <- grep("cloglog|logistic", names(sp_all), value = TRUE, ignore.case = TRUE)[1]
          lon_col  <- grep("^longitude$|^lon$|^x$", names(sp_all), value = TRUE, ignore.case = TRUE)[1]
          lat_col  <- grep("^latitude$|^lat$|^y$",  names(sp_all), value = TRUE, ignore.case = TRUE)[1]
          if (!is.na(pred_col) && !is.na(lon_col) && !is.na(lat_col)) {
            sp_all$lon_r <- round(as.numeric(sp_all[[lon_col]]), 5)
            sp_all$lat_r <- round(as.numeric(sp_all[[lat_col]]), 5)
            pres_idx <- which(oof$response == 1)
            replaced <- 0L
            for (i in pres_idx) {
              m <- which(sp_all$lon_r == round(oof$longitude[i], 5) &
                         sp_all$lat_r == round(oof$latitude[i], 5))
              if (length(m) > 0) {
                oof$pred_maxent[i] <- mean(as.numeric(sp_all[[pred_col]][m]), na.rm = TRUE)
                replaced <- replaced + 1L
              }
            }
            cat(sprintf("  MaxEnt OOF: %d/%d presences replaced with true CV held-out predictions\n",
                        replaced, length(pres_idx)))
          }
        }
      }
    }

    saveRDS(list(lambdas = lambdas_dest, auc = mx_auc), file.path(model_dir, "model_maxent.rds"))
    results$maxent <- list(auc = mx_auc, tss = mx_tss)
    cat(sprintf("  MaxEnt jar done — CV AUC = %.3f\n", mx_auc))

  } else {
    warning("MaxEnt jar not found or java not on PATH — MaxEnt skipped")
  }

  # Save out-of-fold predictions for valid holdout evaluation in Phase 8
  write.csv(oof, file.path(proc_dir, "oof_predictions.csv"), row.names = FALSE)
  
  # Save evaluation
  eval_rows <- data.frame(
    run_id = run_id,
    algorithm = names(results),
    auc_mean = sapply(results, function(x) x$auc),
    tss_mean = sapply(results, function(x) x$tss),
    stringsAsFactors = FALSE
  )
  write.csv(eval_rows, file.path(model_dir, "evaluation_all.csv"), row.names = FALSE)
  
  # Create ensemble prediction - CRITICAL FIX: Use align_raster
  cat("Creating ensemble...\n")
  
  # Load predictor rasters and align to template
  rs <- lapply(manifest$path[manifest$selected], function(p) {
    r <- rast(p)
    # CRITICAL FIX: Align to template
    r <- align_raster(r, template, verbose = FALSE)
    r
  })
  stk <- rast(rs)
  names(stk) <- pred_cols
  
  # Predict with each model
  preds <- list()
  weights <- list()

  # Insert MaxEnt jar raster now that preds is defined
  if (exists("mx_jar_raster") && !is.null(mx_jar_raster)) {
    preds$maxent <- mx_jar_raster
  }

  if (!is.null(results$glm)) {
    cat("  Predicting GLM...\n")
    preds$glm <- terra::predict(stk, glm_full, type = "response")
    weights$glm <- results$glm$auc
  }
  
  if (!is.null(results$rf)) {
    cat("  Predicting RF...\n")
    preds$rf <- terra::predict(
      stk,
      rf_full,
      fun = function(model, data) predict(model, data = data)$predictions,
      na.rm = TRUE
    )
    weights$rf <- results$rf$auc
  }
  
  # MaxEnt raster already populated from jar output above (preds$maxent)
  if (!is.null(preds$maxent)) {
    weights$maxent <- results$maxent$auc
  }

  if (!is.null(results$brt)) {
    cat("  Predicting BRT...\n")
    brt_obj <- readRDS(file.path(model_dir, "model_brt.rds"))
    preds$brt <- terra::predict(
      stk,
      brt_obj$model,
      fun = function(model, data) predict(model, newdata = data, n.trees = brt_obj$best_iter, type = "response"),
      na.rm = TRUE
    )
    weights$brt <- results$brt$auc
  }

  # AOI mask — clip all outputs to study area AOI
  m_mask_path <- file.path(run_dir, "02_data_intermediate", "m_mask.tif")
  m_mask_r <- if (file.exists(m_mask_path)) tryCatch(rast(m_mask_path), error = function(e) NULL) else NULL
  apply_aoi_mask <- function(r) {
    if (is.null(m_mask_r)) return(r)
    msk <- m_mask_r
    if (!isTRUE(compareGeom(msk, r, stopOnError = FALSE))) msk <- project(msk, r, method = "near")
    mask(r, msk, maskvalues = 0)
  }

  if (length(preds) > 0) {
    # Weighted ensemble
    total_w <- sum(unlist(weights))
    ensemble <- Reduce(`+`, Map(`*`, preds, unlist(weights) / total_w))
    ensemble <- apply_aoi_mask(ensemble)
    writeRaster(ensemble, file.path(suit_dir, "suitability_present_ensemble.tif"), overwrite = TRUE)
    cat("  Ensemble created\n")
  }

  # Save per-algorithm present suitability maps (targets.md M04)
  for (algo_name in names(preds)) {
    writeRaster(apply_aoi_mask(preds[[algo_name]]),
                file.path(suit_dir, sprintf("suitability_present_%s.tif", algo_name)),
                overwrite = TRUE)
  }

  list(models = names(results), evaluation = eval_rows, ensemble = length(preds) > 0)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  run_dir <- if (length(args) >= 1) args[[1]] else "."
  run_id <- if (length(args) >= 2) args[[2]] else paste0("RUN_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  
  result <- train_all_models(run_dir, run_id)
  cat(sprintf("Phase 7 complete: %d models trained, ensemble = %s\n", length(result$models), result$ensemble))
}
