#!/usr/bin/env Rscript
# STD-HARDENING: 2026-02-24
# role: phase_or_helper
# note: normalized script metadata for deterministic maintenance
# Phase 10: Change Metrics — C01 delta maps, C02 gain/loss/persistence,
#            C03 zonal statistics, C04 habitat area summary (analytics.md §4)

module_10_change_metrics <- function(run_dir, config_path = NULL, threshold = 0.5) {

  # Read TSS-optimized threshold from evaluation_all.csv if available
  eval_csv <- file.path(run_dir, "02_models", "evaluation_all.csv")
  if (file.exists(eval_csv)) {
    ev <- tryCatch(read.csv(eval_csv), error = function(e) NULL)
    if (!is.null(ev) && "threshold" %in% names(ev)) {
      thr_val <- mean(ev$threshold, na.rm = TRUE)
      if (is.finite(thr_val)) threshold <- thr_val
    }
  }

  fut_dir  <- file.path(run_dir, "04_future_projections")
  out_dir  <- file.path(run_dir, "05_change_metrics")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  ens_maps <- list.files(fut_dir, pattern = "^future_gcm_ensemble_.*\\.tif$",
                         full.names = TRUE)
  if (length(ens_maps) == 0) {
    # Fallback: any file with 'ensemble' in name (covers legacy naming)
    ens_maps <- list.files(fut_dir, pattern = ".*ensemble.*\\.tif$",
                           full.names = TRUE)
  }
  if (length(ens_maps) == 0) {
    cat("Phase 10: no future suitability maps found — skipping change metrics\n")
    return(list(n_maps = 0))
  }

  present_path <- file.path(run_dir, "03_present_suitability", "suitability_present_ensemble.tif")
  if (!file.exists(present_path)) {
    cat("Phase 10: present suitability not found — skipping change metrics\n")
    return(list(n_maps = 0))
  }
  present <- terra::rast(present_path)

  # Locate PA and conflict vectors for C03 zonal stats
  pa_vec   <- NULL
  conf_vec <- NULL
  if (!is.null(config_path) && !file.exists(config_path)) {
    snap <- file.path(run_dir, "00_manifest", "config_snapshot.yaml")
    if (file.exists(snap)) config_path <- snap
  }
  if (!is.null(config_path) && file.exists(config_path)) {
    cfg <- tryCatch(yaml::read_yaml(config_path), error = function(e) NULL)
    if (!is.null(cfg)) {
      repo_root <- cfg$paths$repo_root
      if (is.null(repo_root) || !nzchar(repo_root)) repo_root <- "."
      repo_root    <- normalizePath(repo_root, winslash = "/", mustWork = FALSE)
      vectors_root <- file.path(repo_root, cfg$vectors$root)
      if (dir.exists(vectors_root)) {
        pa_files <- list.files(vectors_root, pattern = "(PA|protected).*\\.shp$",
                               ignore.case = TRUE, recursive = TRUE, full.names = TRUE)
        if (length(pa_files) > 0) pa_vec <- pa_files[1]
        conf_files <- list.files(vectors_root, pattern = "(conflict|hec).*\\.shp$",
                                 ignore.case = TRUE, recursive = TRUE, full.names = TRUE)
        if (length(conf_files) > 0) conf_vec <- conf_files[1]
      }
    }
  }

  idx          <- data.frame()
  summary_rows <- list()

  for (f in ens_maps) {
    r    <- terra::rast(f)
    base <- sub("^(suitability_future_|future_gcm_)", "", tools::file_path_sans_ext(basename(f)))

    # C01: Delta suitability map
    d          <- r - present
    delta_path <- file.path(out_dir, paste0("delta_suitability_", base, ".tif"))
    terra::writeRaster(d, delta_path, overwrite = TRUE)

    # C02: Gain / loss / persistence binary maps
    p0        <- present >= threshold
    p1        <- r       >= threshold
    gain_rast <- (!p0) & p1
    loss_rast <- p0 & (!p1)
    pers_rast <- p0 & p1
    gain_path <- file.path(out_dir, paste0("gain_",        base, ".tif"))
    loss_path <- file.path(out_dir, paste0("loss_",        base, ".tif"))
    pers_path <- file.path(out_dir, paste0("persistence_", base, ".tif"))
    terra::writeRaster(gain_rast, gain_path, overwrite = TRUE)
    terra::writeRaster(loss_rast, loss_path, overwrite = TRUE)
    terra::writeRaster(pers_rast, pers_path, overwrite = TRUE)

    # C03: Zonal statistics per protected area
    if (!is.null(pa_vec) && file.exists(pa_vec)) {
      tryCatch({
        pa      <- terra::vect(pa_vec)
        pa_mask <- terra::rasterize(pa, d, field = 1)
        zst     <- terra::zonal(d, pa_mask, fun = mean, na.rm = TRUE)
        write.csv(zst, file.path(out_dir, sprintf("zonal_stats_pa_%s.csv", base)),
                  row.names = FALSE)
      }, error = function(e) {
        cat(sprintf("  PA zonal stats failed for %s: %s\n", base, conditionMessage(e)))
      })
    }

    # C03: Zonal statistics per conflict zone
    if (!is.null(conf_vec) && file.exists(conf_vec)) {
      tryCatch({
        conf      <- terra::vect(conf_vec)
        conf_mask <- terra::rasterize(conf, d, field = 1)
        zst_conf  <- terra::zonal(d, conf_mask, fun = mean, na.rm = TRUE)
        write.csv(zst_conf, file.path(out_dir, sprintf("zonal_stats_conflict_%s.csv", base)),
                  row.names = FALSE)
      }, error = function(e) {
        cat(sprintf("  Conflict zonal stats failed for %s: %s\n", base, conditionMessage(e)))
      })
    }

    # C04: Habitat area summary (km²)
    cell_km2    <- prod(terra::res(gain_rast)) / 1e6
    habitat_row <- data.frame(
      scenario = base,
      gain_km2 = terra::global(gain_rast, fun = "sum", na.rm = TRUE)[[1]] * cell_km2,
      loss_km2 = terra::global(loss_rast, fun = "sum", na.rm = TRUE)[[1]] * cell_km2,
      pers_km2 = terra::global(pers_rast, fun = "sum", na.rm = TRUE)[[1]] * cell_km2,
      stringsAsFactors = FALSE
    )
    summary_rows[[length(summary_rows) + 1]] <- habitat_row

    idx <- rbind(idx, data.frame(
      future      = f,
      delta       = delta_path,
      gain        = gain_path,
      loss        = loss_path,
      persistence = pers_path,
      stringsAsFactors = FALSE
    ))
  }

  write.csv(idx, file.path(out_dir, "change_metrics_index.csv"), row.names = FALSE)

  # Write C04 habitat area summary
  if (length(summary_rows) > 0) {
    summary_df <- do.call(rbind, summary_rows)
    write.csv(summary_df, file.path(out_dir, "habitat_area_change_summary.csv"),
              row.names = FALSE)
  }

  cat(sprintf("Phase 10 complete: %d change metric scenarios\n", nrow(idx)))
  list(n_maps = nrow(idx))
}
