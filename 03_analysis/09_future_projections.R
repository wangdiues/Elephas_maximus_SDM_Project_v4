#!/usr/bin/env Rscript
# =============================================================================
# 09_future_projections.R — Full script
# - Uses ALL scenarios (all GCMs x periods x SSPs) by default
# - Aligns every raster to the unified template grid via align_raster()
# - Maps BIO01..BIO19 automatically
# - MESS extrapolation diagnostics (reference = CURRENT env; target = FUTURE env)
# - Correct extrapolation masking (ifel -> NA outside threshold)
# - Writes scenario_status.csv + completeness + extrapolation reports
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  if (requireNamespace("ranger", quietly = TRUE)) library(ranger)
  if (requireNamespace("gbm", quietly = TRUE)) library(gbm)
  if (!exists("predict_from_lambdas", mode="function")) { src_dir <- if (exists("repo_root")) file.path(repo_root,"03_analysis") else "03_analysis"; source(file.path(src_dir,"00_sdm_helpers.R")) }
})

if (!exists("align_raster", mode = "function")) {
  src_dir <- if (exists("repo_root")) file.path(repo_root, "03_analysis") else "03_analysis"
  source(file.path(src_dir, "00_spatial_alignment.R"))
}
if (!exists("calculate_mess", mode = "function")) {
  src_dir <- if (exists("repo_root")) file.path(repo_root, "03_analysis") else "03_analysis"
  source(file.path(src_dir, "00_sdm_helpers.R"))
}

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------
normalize_bio_name <- function(x) gsub("[^A-Z0-9]", "", toupper(x))

# Turn "bio1", "BIO_01", "BIO01", "BiO-1" -> "BIO01" ... and supports BIO1..BIO19
predictor_to_bio <- function(pred_name) {
  n <- normalize_bio_name(pred_name)
  m <- regexec("^BIO0?([1-9]|1[0-9])$", n)
  hit <- regmatches(n, m)[[1]]
  if (length(hit) == 0) return(NULL)
  k <- as.integer(hit[2])
  sprintf("BIO%02d", k)
}

tagify <- function(x) gsub("[^A-Za-z0-9]+", "_", x)

safe_mean_raster <- function(r) {
  # Avoid terra::global(na.rm=TRUE) — it internally calls app(na.rm=TRUE) which
  # triggers a terra 1.8.x bug ("number of values returned by 'fun' is not appropriate").
  # Use values() + base mean() instead.
  tryCatch({
    v <- as.numeric(terra::values(r, na.rm = TRUE))
    if (length(v) == 0) return(NA_real_)
    mean(v, na.rm = TRUE)
  }, error = function(e) NA_real_)
}

# Sample matrix from a SpatRaster stack without pulling all cells into RAM
sample_stack_matrix <- function(stk, size = 5000) {
  df <- terra::spatSample(stk, size = size, method = "random", na.rm = TRUE, as.df = TRUE)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  as.matrix(df)
}

# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------
project_future_with_mess <- function(repo_root, run_dir, max_scenarios = Inf,
                                     gcm_filter = NULL, skip_existing = FALSE) {
  proc_dir <- file.path(run_dir, "01_processed_data")
  model_dir <- file.path(run_dir, "02_models")
  out_dir <- file.path(run_dir, "04_future_projections")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Load models
  models <- list()
  for (algo in c("glm", "rf", "maxent", "brt")) {
    f <- file.path(model_dir, sprintf("model_%s.rds", algo))
    if (file.exists(f)) models[[algo]] <- readRDS(f)
  }

  # AOI mask for clipping outputs
  m_mask_path <- file.path(run_dir, "02_data_intermediate", "m_mask.tif")
  m_mask_r <- if (file.exists(m_mask_path)) tryCatch(rast(m_mask_path), error = function(e) NULL) else NULL
  apply_aoi_mask <- function(r) {
    if (is.null(m_mask_r)) return(r)
    msk <- m_mask_r
    if (!isTRUE(compareGeom(msk, r, stopOnError = FALSE))) msk <- project(msk, r, method = "near")
    mask(r, msk, maskvalues = 0)
  }
  if (length(models) == 0) {
    cat("No models found\n")
    return(list(n_scenarios = 0))
  }
  
  # Load predictor manifest
  manifest_file <- file.path(proc_dir, "predictor_manifest.csv")
  if (!file.exists(manifest_file)) stop("Missing: ", manifest_file)
  manifest <- read.csv(manifest_file, stringsAsFactors = FALSE)

  # Validate manifest structure
  required_cols <- c("predictor", "path", "selected")
  miss_cols <- setdiff(required_cols, names(manifest))
  if (length(miss_cols) > 0) stop("predictor_manifest.csv missing columns: ", paste(miss_cols, collapse = ", "))

  # Ensure selected is logical
  if (!is.logical(manifest$selected)) {
    manifest$selected <- as.logical(manifest$selected)
  }

  pred_cols <- manifest$predictor[manifest$selected]
  if (length(pred_cols) == 0) stop("No predictors marked selected in predictor_manifest.csv")
  
  # Unified template grid (your CRITICAL FIX)
  template_file <- file.path(proc_dir, "../02_data_intermediate/template_grid.tif")
  if (!file.exists(template_file)) stop("Missing template grid: ", template_file)
  template <- rast(template_file)
  
  # Static predictors (usually current rasters / terrain / etc.)
  # name -> path
  static_map <- setNames(manifest$path[manifest$selected], manifest$predictor[manifest$selected])
  
  # Future raster discovery
  future_root <- file.path(repo_root, "01_data_raw/02_rasters/future")
  if (!dir.exists(future_root)) stop("Future root not found: ", future_root)
  
  future_files <- list.files(
    future_root,
    pattern = "\\.tif$",
    full.names = TRUE,
    recursive = TRUE
  )
  # Keep only files that contain a bio variable (BIO01..BIO19 or bio01..bio19)
  future_files <- future_files[grepl("bio[0-9]+", basename(future_files), ignore.case = TRUE)]
  if (length(future_files) == 0) {
    cat("No future BIO*.tif files found under: ", future_root, "\n")
    return(list(n_scenarios = 0))
  }
  
  # Parse scenario info from path: future/<GCM>/<period>/<SSP>/BIOxx.tif
  parse_path <- function(p) {
    parts <- strsplit(gsub("\\\\", "/", p), "/")[[1]]
    i <- which(tolower(parts) == "future")
    if (length(i) == 0 || (i[1] + 3) > length(parts)) return(NULL)
    list(
      gcm = parts[i[1] + 1],
      period = parts[i[1] + 2],
      ssp = parts[i[1] + 3],
      file = basename(p)
    )
  }
  
  info <- lapply(future_files, parse_path)
  info <- info[!sapply(info, is.null)]
  if (length(info) == 0) {
    cat("No parseable future files found\n")
    return(list(n_scenarios = 0))
  }
  
  info_df <- do.call(rbind, lapply(info, function(x) data.frame(
    path = file.path(future_root, x$gcm, x$period, x$ssp, x$file),
    gcm = x$gcm,
    period = x$period,
    ssp = x$ssp,
    bio = {
      # Extract BIO01..BIO19 from filename regardless of naming convention
      m <- regmatches(x$file, regexpr("bio0?([1-9]|1[0-9])", x$file, ignore.case = TRUE))
      if (length(m) > 0) sprintf("BIO%02d", as.integer(gsub("[^0-9]", "", m))) else toupper(tools::file_path_sans_ext(x$file))
    },
    stringsAsFactors = FALSE
  )))
  
  scenarios <- unique(info_df[, c("gcm", "period", "ssp")])
  if (nrow(scenarios) == 0) {
    cat("No scenarios detected\n")
    return(list(n_scenarios = 0))
  }
  # Filter to specific GCMs if requested
  if (!is.null(gcm_filter)) {
    scenarios <- scenarios[tolower(scenarios$gcm) %in% tolower(gcm_filter), , drop = FALSE]
    cat(sprintf("GCM filter applied: %d scenarios remaining\n", nrow(scenarios)))
  }
  if (is.finite(max_scenarios)) scenarios <- head(scenarios, max_scenarios)
  
  # Logging
  scenario_log <- list()
  results <- list()
  n_done <- 0

  # Memory management: limit terra memory usage
  tryCatch(terra::terraOptions(memfrac = 0.5), error = function(e) NULL)

  # MESS settings
  default_mess_cols <- c("BIO01", "BIO12", "BIO15")
  mess_threshold <- -3  # keep cells where MESS > -3 (adjust if needed)

  # Pre-load and cache static predictors + MESS reference (once, not per scenario)
  static_cache <- list()
  for (nm in pred_cols) {
    f_static <- static_map[[nm]]
    if (!is.null(f_static) && !is.na(f_static) && file.exists(f_static)) {
      nm_l <- tolower(nm)
      method <- if (grepl("landcover|class", nm_l)) "near" else "bilinear"
      rr <- rast(f_static)[[1]]
      rr <- align_raster(rr, template, method = method, verbose = FALSE)
      names(rr) <- nm
      static_cache[[nm]] <- rr
    }
  }

  # Pre-compute MESS reference values from current static rasters
  mess_cols <- intersect(default_mess_cols, pred_cols)
  if (length(mess_cols) < 3) mess_cols <- head(pred_cols, min(3, length(pred_cols)))
  mess_ref_vals <- NULL
  if (all(mess_cols %in% names(static_cache))) {
    cur_stk <- rast(unname(static_cache[mess_cols]))
    names(cur_stk) <- mess_cols
    mess_ref_vals <- sample_stack_matrix(cur_stk, size = 5000)
    rm(cur_stk)
  }

  for (i in seq_len(nrow(scenarios))) {
    s <- scenarios[i, ]
    cat(sprintf("Processing %s_%s_%s... (%d/%d)\n", s$gcm, s$ssp, s$period, i, nrow(scenarios)))

    gcm_tag    <- tagify(tolower(s$gcm))
    ssp_tag    <- tagify(tolower(s$ssp))
    period_tag <- tagify(s$period)

    # Skip if all output files already exist
    if (skip_existing) {
      existing_algos <- vapply(names(models), function(algo) {
        f <- file.path(out_dir, sprintf("suitability_future_%s_%s_%s_%s.tif", gcm_tag, ssp_tag, period_tag, algo))
        file.exists(f)
      }, logical(1))
      if (all(existing_algos)) {
        cat(sprintf("  Skipping %s_%s_%s (output files exist)\n", gcm_tag, ssp_tag, period_tag))
        n_done <- n_done + 1
        results[[length(results) + 1]] <- list(gcm = s$gcm, ssp = s$ssp, period = s$period)
        next
      }
    }

    sub <- info_df[info_df$gcm == s$gcm & info_df$period == s$period & info_df$ssp == s$ssp, ]

    # Build scenario predictor stack in strict predictor order
    layers <- list()
    missing_preds <- character()

    for (nm in pred_cols) {
      f <- NULL

      # Try future BIO raster if predictor maps to BIOxx
      bio_code <- predictor_to_bio(nm)
      if (!is.null(bio_code)) {
        idx <- normalize_bio_name(sub$bio) == normalize_bio_name(bio_code)
        f_hit <- sub$path[idx]
        if (length(f_hit) > 0) f <- f_hit[1]
      }

      # If future raster found, load + align it; otherwise use cached static
      if (!is.null(f) && length(f) && file.exists(f)) {
        nm_l <- tolower(nm)
        method <- if (grepl("landcover|class", nm_l)) "near" else "bilinear"
        r <- rast(f)[[1]]
        r <- align_raster(r, template, method = method, verbose = FALSE)
        names(r) <- nm
        layers[[nm]] <- r
      } else if (nm %in% names(static_cache)) {
        layers[[nm]] <- static_cache[[nm]]
      } else {
        missing_preds <- c(missing_preds, nm)
      }
    }

    if (length(missing_preds) > 0) {
      scenario_log[[length(scenario_log) + 1]] <- data.frame(
        gcm = s$gcm, ssp = s$ssp, period = s$period,
        status = "SKIPPED",
        reason = paste("Missing predictors:", paste(missing_preds, collapse = ", ")),
        stringsAsFactors = FALSE
      )
      next
    }

    stk <- rast(unname(layers[pred_cols]))
    names(stk) <- pred_cols

    # ---------------------------
    # MESS: reference = CURRENT; target = FUTURE
    # ---------------------------
    do_mess <- !is.null(mess_ref_vals)

    mess_rast <- stk[[1]]
    if (do_mess) {
      vals <- values(stk[[mess_cols]], mat = TRUE)
      ok <- complete.cases(vals)
      mess_full <- rep(NA_real_, nrow(vals))
      if (any(ok)) {
        mess_full[ok] <- calculate_mess(mess_ref_vals, vals[ok, , drop = FALSE])
      }
      values(mess_rast) <- mess_full
      rm(vals, ok, mess_full)
    } else {
      values(mess_rast) <- NA_real_
    }
    
    # Predict for each model
    for (algo in names(models)) {
      m <- models[[algo]]
      
      pred <- NULL
      if (algo == "glm") {
        pred <- terra::predict(stk, m, type = "response")
      } else if (algo == "maxent") {
        lambdas_path <- if (is.list(m) && !is.null(m$lambdas)) m$lambdas else NULL
        pred <- if (!is.null(lambdas_path) && file.exists(lambdas_path)) {
          # FIX: terra::predict() passes matrix row-by-row to fun
          # predict_from_lambdas expects data.frame with named columns
          terra::predict(stk, list(lambdas = lambdas_path),
            fun = function(model, data) {
              # data is a matrix from terra::predict - convert to data.frame
              if (is.matrix(data) || is.vector(data)) {
                data <- as.data.frame(data)
                names(data) <- pred_cols
              }
              result <- tryCatch({
                predict_from_lambdas(model$lambdas, data)
              }, error = function(e) {
                warning("MaxEnt prediction error: ", e$message)
                rep(NA_real_, nrow(data))
              })
              # Ensure we return the right number of values
              if (length(result) != nrow(data)) {
                warning("MaxEnt prediction returned wrong length: ", length(result), " vs ", nrow(data))
                return(rep(NA_real_, nrow(data)))
              }
              as.numeric(result)
            },
            na.rm = TRUE,
            cores = 1)  # Single-threaded for stability
        } else NULL
      } else if (algo == "rf") {
        pred <- terra::predict(
          stk, m,
          fun = function(model, data) {
            p <- predict(model, data = data)
            if (is.list(p) && !is.null(p$predictions)) return(p$predictions) # ranger
            if (is.numeric(p)) return(p)                                     # numeric vector
            if (is.factor(p)) return(as.numeric(p))                          # fallback
            as.numeric(p)
          },
          na.rm = TRUE
        )
      } else if (algo == "brt") {
        pred <- terra::predict(
          stk, m$model,
          fun = function(model, data) predict(model, newdata = data, n.trees = m$best_iter, type = "response"),
          na.rm = TRUE
        )
      } else {
        next
      }

      # Mask extrapolation areas (only if MESS is valid)
      if (do_mess) {
        pred <- ifel(mess_rast > mess_threshold, pred, NA)
      }

      # Clip to AOI
      pred <- apply_aoi_mask(pred)

      out_file <- file.path(
        out_dir,
        sprintf("suitability_future_%s_%s_%s_%s.tif", gcm_tag, ssp_tag, period_tag, algo)
      )
      writeRaster(pred, out_file, overwrite = TRUE)
    }
    
    # Save MESS + novelty assessment + extrapolation mask
    mess_file <- file.path(out_dir, sprintf("mess_%s_%s_%s.tif", gcm_tag, ssp_tag, period_tag))
    writeRaster(mess_rast, mess_file, overwrite = TRUE)

    # F02: Novelty assessment map (targets.md) — alias of MESS raster
    novelty_file <- file.path(out_dir, sprintf("novelty_%s_%s_%s.tif", gcm_tag, ssp_tag, period_tag))
    writeRaster(mess_rast, novelty_file, overwrite = TRUE)

    extrapolation <- mess_rast < 0
    ex_file <- file.path(out_dir, sprintf("extrapolation_%s_%s_%s.tif", gcm_tag, ssp_tag, period_tag))
    writeRaster(extrapolation, ex_file, overwrite = TRUE)
    
    # Log success
    n_done <- n_done + 1
    results[[length(results) + 1]] <- list(gcm = s$gcm, ssp = s$ssp, period = s$period)

    scenario_log[[length(scenario_log) + 1]] <- data.frame(
      gcm = s$gcm, ssp = s$ssp, period = s$period,
      status = "SUCCESS",
      reason = if (do_mess) "" else "MESS_NOT_COMPUTED (missing current reference rasters for MESS cols)",
      stringsAsFactors = FALSE
    )

    # Memory and connection cleanup between scenarios
    rm(stk, mess_rast, layers)
    gc(verbose = FALSE)
    gc(verbose = FALSE)  # second pass ensures finalizers run
    tryCatch(terra::tmpFiles(remove = TRUE), error = function(e) NULL)
    # Force terra to release file handles by resetting options briefly
    tryCatch(terra::terraOptions(memfrac = 0.5), error = function(e) NULL)
  }
  
  # Write scenario status
  if (length(scenario_log) > 0) {
    scenario_status <- do.call(rbind, scenario_log)
    write.csv(scenario_status, file.path(out_dir, "scenario_status.csv"), row.names = FALSE)
  }
  
  # Save index of successful scenarios
  if (length(results) > 0) {
    idx <- do.call(rbind, lapply(results, function(x) data.frame(
      gcm = x$gcm, ssp = x$ssp, period = x$period, stringsAsFactors = FALSE
    )))
    write.csv(idx, file.path(out_dir, "future_projection_index.csv"), row.names = FALSE)
  }
  
  # -----------------------------------------------------------------------------
  # Build per (SSP, period) GCM ensemble maps from all available algorithm outputs.
  # -----------------------------------------------------------------------------
  unique_ssp <- unique(scenarios$ssp)
  unique_period <- unique(scenarios$period)
  
  for (ssp in unique_ssp) {
    for (period in unique_period) {
      ssp_tag <- tagify(tolower(ssp))
      period_tag <- tagify(period)

      pats <- sprintf("^suitability_future_.*_%s_%s_(glm|maxent|rf|brt)\\.tif$", ssp_tag, period_tag)
      ff <- list.files(out_dir, pattern = pats, full.names = TRUE)
      if (length(ff) == 0) next

      tryCatch({
        st <- rast(ff)
        ens <- if (nlyr(st) == 1) st else app(st, function(v) mean(v, na.rm = TRUE))
        ens_file <- file.path(out_dir, sprintf("future_gcm_ensemble_%s_%s.tif", ssp_tag, period_tag))
        writeRaster(ens, ens_file, overwrite = TRUE)
        rm(st, ens); gc(verbose = FALSE)
      }, error = function(e) {
        cat(sprintf("  Warning: ensemble TIF skipped for %s_%s: %s\n", ssp, period, conditionMessage(e)))
      })
    }
  }
  
  # -----------------------------------------------------------------------------
  # Reports
  # -----------------------------------------------------------------------------
  # Completeness report: count successes by (gcm,ssp,period)
  if (exists("scenario_status") && nrow(scenario_status) > 0) {
    comp <- aggregate(
      status ~ gcm + ssp + period,
      scenario_status,
      FUN = function(x) sum(x == "SUCCESS")
    )
    names(comp)[names(comp) == "status"] <- "n_success"
    write.csv(comp, file.path(out_dir, "gcm_completeness_report.csv"), row.names = FALSE)
  }
  
  # Extrapolation fraction report per (ssp, period)
  extrapolation_report <- data.frame(
    ssp = unique_ssp,
    stringsAsFactors = FALSE
  )
  extrapolation_report <- merge(
    extrapolation_report,
    data.frame(period = unique_period, stringsAsFactors = FALSE),
    all = TRUE
  )
  extrapolation_report$gcm_mean_extrapolation <- NA_real_
  extrapolation_report$ensemble_extrapolation <- NA_real_
  
  for (i in seq_len(nrow(extrapolation_report))) {
    ssp <- extrapolation_report$ssp[i]
    period <- extrapolation_report$period[i]
    ssp_tag <- tagify(tolower(ssp))
    period_tag <- tagify(period)
    
    ex_files <- list.files(
      out_dir,
      pattern = sprintf("^extrapolation_.*_%s_%s\\.tif$", ssp_tag, period_tag),
      full.names = TRUE
    )
    if (length(ex_files) == 0) next
    
    # mean extrapolation fraction across GCMs
    ex_vals <- vapply(ex_files, function(f) {
      tryCatch(safe_mean_raster(rast(f)), error = function(e) NA_real_)
    }, numeric(1))
    extrapolation_report$gcm_mean_extrapolation[i] <- mean(ex_vals, na.rm = TRUE)

    # ensemble extrapolation: any GCM says extrapolation
    ensemble_ex <- tryCatch({
      ex_stk <- rast(ex_files)
      if (nlyr(ex_stk) == 1) ex_stk else app(ex_stk, function(v) max(v, na.rm = TRUE))
    }, error = function(e) {
      cat(sprintf("  Warning: ensemble extrapolation failed for %s_%s: %s\n", ssp, period, conditionMessage(e)))
      NULL
    })
    extrapolation_report$ensemble_extrapolation[i] <-
      if (!is.null(ensemble_ex)) safe_mean_raster(ensemble_ex) else NA_real_
    
    # required per-scenario export
    out_csv <- file.path(out_dir, sprintf("extrapolation_fraction_%s_%s.csv", ssp_tag, period_tag))
    write.csv(
      data.frame(
        ssp = ssp,
        period = period,
        gcm_mean_extrapolation = extrapolation_report$gcm_mean_extrapolation[i],
        ensemble_extrapolation = extrapolation_report$ensemble_extrapolation[i],
        stringsAsFactors = FALSE
      ),
      out_csv,
      row.names = FALSE
    )
  }
  tryCatch(
    write.csv(extrapolation_report, file.path(out_dir, "extrapolation_fraction_report.csv"), row.names = FALSE),
    error = function(e) cat("Warning: extrapolation summary write failed:", conditionMessage(e), "\n")
  )

  cat(sprintf("Scenario status: %s\n", file.path(out_dir, "scenario_status.csv")))
  cat(sprintf("GCM completeness report: %s\n", file.path(out_dir, "gcm_completeness_report.csv")))
  cat(sprintf("Extrapolation report: %s\n", file.path(out_dir, "extrapolation_fraction_report.csv")))

  list(n_scenarios = n_done, results = results)
}

# -----------------------------------------------------------------------------
# CLI
# -----------------------------------------------------------------------------
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  repo_root <- if (length(args) >= 1) args[[1]] else "."
  run_dir   <- if (length(args) >= 2) args[[2]] else "."
  
  res <- project_future_with_mess(repo_root, run_dir, max_scenarios = Inf)
  cat(sprintf("Phase 9 complete: %d scenarios processed\n", res$n_scenarios))
}