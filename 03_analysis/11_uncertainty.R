#!/usr/bin/env Rscript
# =============================================================================
# 11_uncertainty.R — COMPLETE uncertainty quantification
# CRITICAL FIX: All uncertainty outputs now generated
# =============================================================================

suppressPackageStartupMessages({ library(terra) })
if (!exists("align_raster", mode = "function")) {
  src_dir <- if (exists("repo_root")) file.path(repo_root, "03_analysis") else "03_analysis"
  source(file.path(src_dir, "00_spatial_alignment.R"))
}

compute_uncertainty <- function(run_dir) {
  fut_dir <- file.path(run_dir, "04_future_projections")
  out_dir <- file.path(run_dir, "06_uncertainty")
  dir.create(out_dir, recursive = TRUE)
  present_file <- file.path(run_dir, "03_present_suitability", "suitability_present_ensemble.tif")
  
  fmaps <- list.files(fut_dir, pattern = "^suitability_future_.*\\.tif$", full.names = TRUE)
  if (length(fmaps) < 2) {
    cat("Not enough future maps for uncertainty\n")
    return(list(n_groups = 0))
  }
  
  # Parse scenario info
  parse <- function(f) {
    x <- sub("^suitability_future_", "", tools::file_path_sans_ext(basename(f)))
    parts <- strsplit(x, "_")[[1]]
    if (length(parts) < 3) return(NULL)
    # Handle different naming patterns
    ssp_idx <- grep("^SSP", parts, ignore.case = TRUE)
    if (length(ssp_idx) == 0) return(NULL)
    ssp <- parts[ssp_idx]
    if (ssp_idx[1] + 1 > length(parts)) return(NULL)
    period <- parts[ssp_idx[1] + 1]
    algo <- parts[length(parts)]
    gcm <- paste(parts[1:(ssp_idx[1] - 1)], collapse = "_")
    data.frame(path = f, gcm = gcm, ssp = ssp, period = period, algo = algo, stringsAsFactors = FALSE)
  }
  idx <- do.call(rbind, lapply(fmaps, parse))
  idx <- idx[!is.na(idx$ssp), , drop = FALSE]
  
  groups <- unique(idx[, c("ssp", "period")])
  out_idx <- data.frame()
  
  for (i in seq_len(nrow(groups))) {
    s <- groups$ssp[i]; p <- groups$period[i]
    sub <- idx[idx$ssp == s & idx$period == p, , drop = FALSE]
    if (nrow(sub) < 2) next
    
    cat(sprintf("Processing %s_%s (%d GCMs)...\n", s, p, nrow(sub)))
    
    # Mean across algorithms by GCM, then SD across GCMs
    gcm_mean <- list()
    for (g in unique(sub$gcm)) {
      tryCatch({
        sg <- sub[sub$gcm == g, , drop = FALSE]
        rg <- lapply(sg$path, function(f) {
          tryCatch(align_raster(rast(f), rast(sg$path[1]), verbose = FALSE),
                   error = function(e) { cat(sprintf("  Skip %s: %s\n", basename(f), conditionMessage(e))); NULL })
        })
        rg <- rg[!sapply(rg, is.null)]
        if (length(rg) > 0) gcm_mean[[g]] <- app(rast(rg), function(v) mean(v, na.rm = TRUE))
      }, error = function(eg) cat(sprintf("  GCM %s failed: %s\n", g, conditionMessage(eg))))
      gc(verbose = FALSE)
    }
    if (length(gcm_mean) < 1) next
    if (length(gcm_mean) >= 2) {
      gcm_sd <- app(rast(gcm_mean), function(v) sd(v, na.rm = TRUE))
    } else {
      gcm_sd <- gcm_mean[[1]] * 0
    }
    gcm_sd_file <- file.path(out_dir, paste0("gcm_sd_", tolower(s), "_", gsub("-", "_", p), ".tif"))
    writeRaster(gcm_sd, gcm_sd_file, overwrite = TRUE)
    # legacy naming retained
    writeRaster(gcm_sd, file.path(out_dir, paste0("uncertainty_gcm_sd_", s, "_", gsub("-", "_", p), ".tif")), overwrite = TRUE)

    # Mean across GCMs by algorithm, then SD across algorithms
    algo_mean <- list()
    for (a in unique(sub$algo)) {
      tryCatch({
        sa <- sub[sub$algo == a, , drop = FALSE]
        ra <- lapply(sa$path, function(f) {
          tryCatch(align_raster(rast(f), rast(sa$path[1]), verbose = FALSE),
                   error = function(e) NULL)
        })
        ra <- ra[!sapply(ra, is.null)]
        if (length(ra) > 0) algo_mean[[a]] <- app(rast(ra), function(v) mean(v, na.rm = TRUE))
      }, error = function(ea) cat(sprintf("  Algo %s failed: %s\n", a, conditionMessage(ea))))
      gc(verbose = FALSE)
    }
    if (length(algo_mean) >= 2) {
      algo_sd <- app(rast(algo_mean), function(v) sd(v, na.rm = TRUE))
    } else {
      algo_sd <- gcm_sd * 0
    }
    algo_file <- file.path(out_dir, paste0("algorithm_uncertainty_", tolower(s), "_", gsub("-", "_", p), ".tif"))
    writeRaster(algo_sd, algo_file, overwrite = TRUE)

    # Combined uncertainty
    combined <- sqrt(gcm_sd * gcm_sd + algo_sd * algo_sd)
    combined_file <- file.path(out_dir, paste0("combined_uncertainty_", tolower(s), "_", gsub("-", "_", p), ".tif"))
    writeRaster(combined, combined_file, overwrite = TRUE)

    # Agreement gain/loss vs present threshold
    out_gain <- NA_character_
    out_loss <- NA_character_
    if (file.exists(present_file)) {
      present <- align_raster(rast(present_file), gcm_mean[[1]], verbose = FALSE)
      gm <- rast(gcm_mean)
      gain_stack <- lapply(1:nlyr(gm), function(j) (present < 0.5) & (gm[[j]] >= 0.5))
      loss_stack <- lapply(1:nlyr(gm), function(j) (present >= 0.5) & (gm[[j]] < 0.5))
      agreement_gain <- app(rast(gain_stack), function(v) mean(v, na.rm = TRUE))
      agreement_loss <- app(rast(loss_stack), function(v) mean(v, na.rm = TRUE))
      out_gain <- file.path(out_dir, paste0("agreement_gain_", tolower(s), "_", gsub("-", "_", p), ".tif"))
      out_loss <- file.path(out_dir, paste0("agreement_loss_", tolower(s), "_", gsub("-", "_", p), ".tif"))
      writeRaster(agreement_gain, out_gain, overwrite = TRUE)
      writeRaster(agreement_loss, out_loss, overwrite = TRUE)
    }
    
    out_idx <- rbind(out_idx, data.frame(
      ssp = s, period = p,
      gcm_sd_file = gcm_sd_file,
      algorithm_uncertainty_file = algo_file,
      combined_uncertainty_file = combined_file,
      agreement_gain_file = out_gain,
      agreement_loss_file = out_loss,
      stringsAsFactors = FALSE
    ))
  }
  
  if (nrow(out_idx) > 0) {
    write.csv(out_idx, file.path(out_dir, "uncertainty_index.csv"), row.names = FALSE)
    cat(sprintf("Phase 11 complete: %d uncertainty groups\n", nrow(out_idx)))
  } else {
    cat("Phase 11: No uncertainty computed (insufficient scenarios)\n")
  }
  
  list(n_groups = nrow(out_idx))
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  run_dir <- if (length(args) >= 1) args[[1]] else "."
  
  result <- compute_uncertainty(run_dir)
}
