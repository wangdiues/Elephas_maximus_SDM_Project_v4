#!/usr/bin/env Rscript
# Refugia products derived only from the validated individual-GCM future
# ensemble projections. Historical and SSP-period mean layers are excluded.

compute_refugia <- function(run_dir, config_path = "00_governance/config.yaml") {
  future_dir <- file.path(run_dir, "04_future_projections")
  present_dir <- file.path(run_dir, "03_present_suitability")
  out_dir <- file.path(run_dir, "05_change_metrics")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  cfg <- yaml::read_yaml(config_path)
  expected_n <- as.integer(cfg$climate_projections$expected_gcm_scenarios)
  status_file <- file.path(future_dir, "scenario_status.csv")
  if (!file.exists(status_file)) stop("Missing future scenario status: ", status_file)
  status <- read.csv(status_file, stringsAsFactors = FALSE)
  status <- status[status$status == "SUCCESS", , drop = FALSE]
  if (nrow(status) != expected_n) stop("Refugia requires exactly ", expected_n, " successful future scenarios")

  tag <- function(x) gsub("[^A-Za-z0-9]+", "_", tolower(x))
  scenario_files <- mapply(function(g, s, p) {
    file.path(future_dir, sprintf("suitability_future_%s_%s_%s_ensemble.tif",
                                  tag(g), tag(s), tag(p)))
  }, status$gcm, status$ssp, status$period, USE.NAMES = FALSE)
  missing <- scenario_files[!file.exists(scenario_files)]
  if (length(missing) > 0) stop("Missing individual-GCM ensemble rasters: ", paste(basename(missing), collapse = ", "))

  evaluation <- read.csv(file.path(run_dir, "02_models", "evaluation_all.csv"), stringsAsFactors = FALSE)
  ens <- evaluation[evaluation$algorithm == "ensemble", , drop = FALSE]
  if (nrow(ens) != 1 || !is.finite(ens$threshold[[1]])) stop("A unique OOF ensemble threshold is required")
  threshold <- ens$threshold[[1]]

  present_file <- file.path(present_dir, "suitability_present_ensemble.tif")
  if (!file.exists(present_file)) stop("Missing present ensemble raster: ", present_file)
  present <- terra::rast(present_file)
  futures <- terra::rast(scenario_files)
  if (!isTRUE(terra::compareGeom(present, futures, stopOnError = FALSE))) {
    stop("Present and future rasters do not share one grid geometry")
  }

  present_suitable <- present >= threshold
  future_suitable <- futures >= threshold
  stability_count <- terra::app(future_suitable, sum, na.rm = FALSE)
  stability_count <- terra::ifel(present_suitable, stability_count, NA)
  stability_fraction <- stability_count / expected_n
  core <- terra::ifel(present_suitable & stability_count == expected_n, 1, 0)
  high_cutoff <- ceiling(0.75 * expected_n)
  high <- terra::ifel(present_suitable & stability_count >= high_cutoff, 1, 0)

  outputs <- c(
    stability_count = file.path(out_dir, "refugia_stability_count.tif"),
    stability_fraction = file.path(out_dir, "refugia_stability_fraction.tif"),
    core = file.path(out_dir, "refugia_core.tif"),
    high_stability = file.path(out_dir, "refugia_high_stability.tif")
  )
  terra::writeRaster(stability_count, outputs[["stability_count"]], overwrite = TRUE)
  terra::writeRaster(stability_fraction, outputs[["stability_fraction"]], overwrite = TRUE)
  terra::writeRaster(core, outputs[["core"]], overwrite = TRUE)
  terra::writeRaster(high, outputs[["high_stability"]], overwrite = TRUE)

  manifest <- data.frame(
    run_id = basename(run_dir),
    gcm = status$gcm,
    ssp = status$ssp,
    period = status$period,
    source_file = basename(scenario_files),
    source_md5 = unname(tools::md5sum(scenario_files)),
    included = TRUE,
    stringsAsFactors = FALSE
  )
  write.csv(manifest, file.path(out_dir, "refugia_scenario_manifest.csv"), row.names = FALSE)
  write.csv(data.frame(
    run_id = basename(run_dir),
    threshold = threshold,
    denominator_type = "individual GCM projections",
    denominator_n = expected_n,
    core_minimum = expected_n,
    high_stability_minimum = high_cutoff,
    present_suitable_mask_required = TRUE,
    stringsAsFactors = FALSE
  ), file.path(out_dir, "refugia_contract.csv"), row.names = FALSE)

  invisible(list(n_scenarios = expected_n, threshold = threshold,
                 high_cutoff = high_cutoff, outputs = outputs))
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  run_dir <- if (length(args) >= 1) args[[1]] else stop("run_dir argument is required")
  config_path <- if (length(args) >= 2) args[[2]] else "00_governance/config.yaml"
  compute_refugia(run_dir, config_path)
}
