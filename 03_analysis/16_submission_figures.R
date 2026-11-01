#!/usr/bin/env Rscript
# Corrected main-figure builders for submission-critical future and refugia plots.

build_spatial_cv_figure <- function(run_dir, config_path, figure_dir = file.path(run_dir, "08_figures_tables")) {
  cfg <- yaml::read_yaml(config_path)
  proc_dir <- file.path(run_dir, "01_processed_data")
  block_file <- file.path(proc_dir, "spatial_blocks.gpkg")
  point_file <- file.path(proc_dir, "fold_assignments.csv")
  if (!file.exists(block_file) || !file.exists(point_file)) stop("Spatial CV artifacts are missing")
  blocks <- sf::st_read(block_file, quiet = TRUE)
  point_data <- read.csv(point_file, stringsAsFactors = FALSE)
  points <- sf::st_as_sf(point_data, coords = c("longitude", "latitude"), crs = 4326,
                         remove = FALSE)
  aoi <- sf::st_read(file.path(cfg$paths$repo_root, cfg$vectors$aoi_bhutan), quiet = TRUE)
  aoi <- sf::st_transform(aoi, sf::st_crs(blocks))
  points <- sf::st_transform(points, sf::st_crs(blocks))
  blocks$fold <- factor(blocks$fold)
  points$fold <- factor(points$fold, levels = levels(blocks$fold))
  fold_pal <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9")
  fold_pal <- setNames(fold_pal[seq_along(levels(blocks$fold))], levels(blocks$fold))
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = blocks, ggplot2::aes(fill = fold), colour = "white",
                     linewidth = 0.12, alpha = 0.32) +
    ggplot2::geom_sf(data = aoi, fill = NA, colour = "#111111", linewidth = 0.45) +
    ggplot2::geom_sf(data = points[points$type != "presence", ], shape = 1,
                     colour = "#666666", size = 0.75, stroke = 0.25, alpha = 0.55) +
    ggplot2::geom_sf(data = points[points$type == "presence", ],
                     ggplot2::aes(fill = fold), shape = 21, colour = "black",
                     size = 1.5, stroke = 0.25) +
    ggplot2::scale_fill_manual(values = fold_pal, name = "Spatial fold") +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(
      x = NULL, y = NULL
    ) + publication_theme(base_size = 9) +
    ggplot2::theme(axis.text = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(),
                   legend.position = "right")
  save_publication_figure(p, file.path(figure_dir, "main_figure_01_spatial_cv"),
                          width_mm = 178, height_mm = 120, line_art_dpi = 600)
}

build_variable_importance_figure <- function(run_dir, figure_dir = file.path(run_dir, "08_figures_tables")) {
  source_file <- file.path(run_dir, "08_figures_tables", "table_E07_variable_importance.csv")
  contract_file <- file.path(run_dir, "02_models", "ensemble_contract.csv")
  if (!file.exists(source_file) || !file.exists(contract_file)) stop("Variable-importance source artifacts are missing")
  x <- read.csv(source_file, stringsAsFactors = FALSE)
  contract <- read.csv(contract_file, stringsAsFactors = FALSE)
  included <- toupper(contract$algorithm[contract$included])
  x <- x[toupper(x$algorithm) %in% included, , drop = FALSE]
  if (nrow(x) == 0) stop("No variable importance for included algorithms")
  x <- do.call(rbind, lapply(split(x, x$algorithm), function(z) {
    z[order(z$importance, decreasing = TRUE)[seq_len(min(8, nrow(z)))], , drop = FALSE]
  }))
  nice <- c(BIO05 = "BIO05 max. temp. warmest month", BIO14 = "BIO14 precip. driest month",
            BIO15 = "BIO15 precip. seasonality", BIO18 = "BIO18 precip. warmest quarter",
            tri = "Terrain ruggedness", slope = "Slope", aspect = "Aspect", evi = "EVI",
            dynamicworld_prob = "Land-cover probability", landcover = "Land-cover class",
            human_footprint = "Human Influence Index")
  x$predictor_label <- ifelse(x$predictor %in% names(nice), nice[x$predictor],
                              sub("^dist to ", "Distance to ", gsub("_", " ", x$predictor)))
  x$predictor_panel <- interaction(x$algorithm, x$predictor_label, lex.order = TRUE)
  x$predictor_panel <- stats::reorder(x$predictor_panel, x$importance)
  category_pal <- c("Climate (BIO)" = "#0072B2", "Anthropogenic" = "#D55E00",
                    "Vegetation" = "#009E73", "Vegetation/LULC" = "#009E73", "Topography" = "#CC79A7",
                    "Distance" = "#E69F00", "Other" = "#777777")
  p <- ggplot2::ggplot(x, ggplot2::aes(x = predictor_panel, y = importance, fill = pred_type)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~algorithm, scales = "free_y", ncol = 2) +
    ggplot2::scale_x_discrete(labels = function(z) sub("^[^.]+\\.", "", z)) +
    ggplot2::scale_fill_manual(values = category_pal, name = "Predictor group", drop = TRUE) +
    ggplot2::labs(
      x = NULL, y = "Within-algorithm relative importance"
    ) + publication_theme(base_size = 8.5) +
    ggplot2::theme(legend.position = "bottom")
  save_publication_figure(p, file.path(figure_dir, "main_figure_02_variable_importance"),
                          width_mm = 178, height_mm = 150, line_art_dpi = 600)
}

build_present_suitability_figure <- function(run_dir, config_path, figure_dir = file.path(run_dir, "08_figures_tables")) {
  cfg <- yaml::read_yaml(config_path)
  r <- terra::rast(file.path(run_dir, "03_present_suitability", "suitability_present_ensemble.tif"))
  xy <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(xy)[3] <- "suitability"
  aoi <- sf::st_transform(sf::st_read(file.path(cfg$paths$repo_root, cfg$vectors$aoi_bhutan), quiet = TRUE), terra::crs(r))
  pa <- sf::st_transform(sf::st_make_valid(sf::st_read(file.path(cfg$paths$repo_root, cfg$vectors$protected_areas), quiet = TRUE)), terra::crs(r))
  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = xy, ggplot2::aes(x = x, y = y, fill = suitability)) +
    ggplot2::geom_sf(data = pa, fill = NA, colour = "#F0E442", linewidth = 0.32) +
    ggplot2::geom_sf(data = aoi, fill = NA, colour = "#111111", linewidth = 0.45) +
    ggplot2::scale_fill_viridis_c(option = "C", limits = c(0, 1), name = "Habitat suitability") +
    ggplot2::coord_sf(crs = sf::st_crs(aoi), expand = FALSE) +
    ggplot2::labs(
      x = NULL, y = NULL
    ) + publication_theme(base_size = 9) +
    ggplot2::theme(axis.text = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(),
                   legend.position = "right")
  save_publication_figure(p, file.path(figure_dir, "main_figure_03_present_suitability"),
                          width_mm = 178, height_mm = 120, line_art_dpi = 600)
}

build_future_habitat_area_figure <- function(run_dir, config_path, figure_dir = file.path(run_dir, "08_figures_tables")) {
  cfg <- yaml::read_yaml(config_path)
  future_dir <- file.path(run_dir, "04_future_projections")
  status <- read.csv(file.path(future_dir, "scenario_status.csv"), stringsAsFactors = FALSE)
  status <- status[status$status == "SUCCESS", , drop = FALSE]
  expected <- as.integer(cfg$climate_projections$expected_gcm_scenarios)
  if (nrow(status) != expected) stop("Future area figure requires all ", expected, " scenarios")

  ev <- read.csv(file.path(run_dir, "02_models", "evaluation_all.csv"), stringsAsFactors = FALSE)
  ens <- ev[ev$algorithm == "ensemble", , drop = FALSE]
  if (nrow(ens) != 1 || !is.finite(ens$threshold[[1]])) stop("Missing OOF ensemble threshold")
  threshold <- ens$threshold[[1]]
  tag <- function(x) gsub("[^A-Za-z0-9]+", "_", tolower(x))
  status$file <- mapply(function(g, s, p) file.path(
    future_dir, sprintf("suitability_future_%s_%s_%s_ensemble.tif", tag(g), tag(s), tag(p))
  ), status$gcm, status$ssp, status$period, USE.NAMES = FALSE)
  if (!all(file.exists(status$file))) stop("A future scenario raster is missing")
  status$area_km2 <- vapply(status$file, function(f) {
    r <- terra::rast(f)
    cell_km2 <- prod(terra::res(r)) / 1e6
    as.numeric(terra::global(r >= threshold, "sum", na.rm = TRUE)[[1]]) * cell_km2
  }, numeric(1))

  summary <- do.call(rbind, lapply(split(status, list(status$ssp, status$period), drop = TRUE), function(z) {
    data.frame(ssp = z$ssp[[1]], period = z$period[[1]], n_gcm = nrow(z),
               mean_km2 = mean(z$area_km2), sd_km2 = stats::sd(z$area_km2),
               min_km2 = min(z$area_km2), max_km2 = max(z$area_km2),
               stringsAsFactors = FALSE)
  }))
  period_order <- cfg$climate_projections$periods_contract
  canonical <- function(x) gsub("[^0-9]", "", x)
  summary$x <- match(canonical(summary$period), canonical(period_order))
  status$x <- match(canonical(status$period), canonical(period_order))
  ssp_map <- setNames(cfg$climate_projections$ssps_contract,
                      canonical(cfg$climate_projections$ssps_contract))
  summary$scenario <- unname(ssp_map[canonical(summary$ssp)])
  status$scenario <- unname(ssp_map[canonical(status$ssp)])

  present <- terra::rast(file.path(run_dir, "03_present_suitability", "suitability_present_ensemble.tif"))
  present_area <- as.numeric(terra::global(present >= threshold, "sum", na.rm = TRUE)[[1]]) *
    prod(terra::res(present)) / 1e6
  gcm_label <- c(acces_cm2 = "ACCESS-CM2", access_cm2 = "ACCESS-CM2", cnrm_cm6_1 = "CNRM-CM6-1",
                 inm_cm5_0 = "INM-CM5-0", miroc6 = "MIROC6", mpi_esm1_2_lr = "MPI-ESM1-2-LR")
  status$gcm_label <- unname(gcm_label[tag(status$gcm)])
  status$gcm_label[is.na(status$gcm_label)] <- status$gcm[is.na(status$gcm_label)]
  med <- do.call(rbind, lapply(split(status, list(status$scenario, status$x), drop = TRUE), function(z)
    data.frame(scenario = z$scenario[1], x = z$x[1], median_km2 = stats::median(z$area_km2))))
  gcm_pal <- c("ACCESS-CM2" = "#E69F00", "CNRM-CM6-1" = "#56B4E9", "INM-CM5-0" = "#009E73",
               "MIROC6" = "#CC79A7", "MPI-ESM1-2-LR" = "#0072B2")
  p <- ggplot2::ggplot(status, ggplot2::aes(x = x, y = area_km2)) +
    ggplot2::geom_hline(yintercept = present_area, colour = "#444444", linetype = "dotted") +
    ggplot2::geom_line(ggplot2::aes(colour = gcm_label, group = gcm_label), linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(colour = gcm_label), size = 1.2) +
    ggplot2::geom_line(data = med, ggplot2::aes(y = median_km2), colour = "black", linewidth = 1.1) +
    ggplot2::geom_point(data = med, ggplot2::aes(y = median_km2), colour = "black", size = 1.8) +
    ggplot2::facet_wrap(~scenario, nrow = 1) +
    ggplot2::scale_colour_manual(values = gcm_pal, name = "GCM") +
    ggplot2::scale_x_continuous(breaks = seq_along(period_order),
                                labels = gsub("-", "\u2013", period_order),
                                expand = ggplot2::expansion(mult = 0.12)) +
    ggplot2::scale_y_continuous(labels = function(v) format(v, big.mark = ",")) +
    ggplot2::labs(x = NULL, y = expression("Suitable habitat (km"^2*")")) +
    publication_theme(base_size = 8) +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  manifest <- save_publication_figure(
    p, file.path(figure_dir, "main_figure_04_future_habitat_area"),
    width_mm = 178, height_mm = 90, line_art_dpi = 600
  )
  write.csv(status[, c("gcm", "scenario", "period", "area_km2")],
            file.path(figure_dir, "main_figure_04_source_data.csv"), row.names = FALSE)
  invisible(manifest)
}

build_refugia_figure <- function(run_dir, config_path, figure_dir = file.path(run_dir, "08_figures_tables")) {
  cfg <- yaml::read_yaml(config_path)
  change_dir <- file.path(run_dir, "05_change_metrics")
  frac_file <- file.path(change_dir, "refugia_stability_fraction.tif")
  contract_file <- file.path(change_dir, "refugia_contract.csv")
  if (!file.exists(frac_file) || !file.exists(contract_file)) stop("Validated refugia products are missing")
  r <- terra::rast(frac_file)
  xy <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(xy)[3] <- "stability"
  aoi_path <- file.path(cfg$paths$repo_root, cfg$vectors$aoi_bhutan)
  aoi <- sf::st_transform(sf::st_read(aoi_path, quiet = TRUE), terra::crs(r))
  pa <- sf::st_transform(sf::st_make_valid(sf::st_read(file.path(cfg$paths$repo_root, cfg$vectors$protected_areas), quiet = TRUE)), terra::crs(r))
  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = xy, ggplot2::aes(x = x, y = y, fill = stability)) +
    ggplot2::geom_sf(data = pa, fill = NA, colour = "#009E73", linewidth = 0.3) +
    ggplot2::geom_sf(data = aoi, fill = NA, colour = "#222222", linewidth = 0.35) +
    ggplot2::scale_fill_viridis_c(option = "C", limits = c(0, 1),
                                  breaks = c(0, 0.25, 0.5, 0.75, 1),
                                  name = "Future projections\nretaining suitability") +
    ggplot2::coord_sf(crs = sf::st_crs(aoi), expand = FALSE) +
    ggplot2::labs(
      x = NULL, y = NULL
    ) + publication_theme(base_size = 9) +
    ggplot2::theme(axis.text = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(),
                   legend.position = "right")
  manifest <- save_publication_figure(
    p, file.path(figure_dir, "main_figure_05_refugia_stability"),
    width_mm = 178, height_mm = 120, line_art_dpi = 600
  )
  invisible(manifest)
}

create_submission_figures <- function(run_dir, config_path, figure_dir = file.path(run_dir, "08_figures_tables")) {
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  rbind(build_spatial_cv_figure(run_dir, config_path, figure_dir),
        build_variable_importance_figure(run_dir, figure_dir),
        build_present_suitability_figure(run_dir, config_path, figure_dir),
        build_future_habitat_area_figure(run_dir, config_path, figure_dir),
        build_refugia_figure(run_dir, config_path, figure_dir))
}

if (sys.nframe() == 0) {
  suppressPackageStartupMessages({
    library(terra); library(sf); library(ggplot2); library(yaml)
  })
  source(file.path("03_analysis", "00_publication_helpers.R"))
  args <- commandArgs(trailingOnly = TRUE)
  run_dir <- if (length(args) >= 1) args[[1]] else stop("Explicit immutable run_dir is required")
  config_path <- if (length(args) >= 2) args[[2]] else "00_governance/config.yaml"
  figure_dir <- if (length(args) >= 3) args[[3]] else file.path(run_dir, "08_figures_tables")
  create_submission_figures(run_dir, config_path, figure_dir)
}
