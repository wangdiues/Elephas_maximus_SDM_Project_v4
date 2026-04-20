#!/usr/bin/env Rscript
# =============================================================================
# 04_predictor_engine.R — Distance derivatives + Collinearity
# =============================================================================

suppressPackageStartupMessages({ library(terra); library(sf); library(yaml) })
if (!exists("auc_score", mode = "function")) {
  src_dir <- if (exists("repo_root")) file.path(repo_root, "03_analysis") else "03_analysis"
  source(file.path(src_dir, "00_sdm_helpers.R"))
}

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x) || (is.character(x) && !nzchar(x[1]))) y else x
}

as_chr_vec <- function(x) {
  if (is.null(x)) return(character())
  out <- unlist(x, recursive = TRUE, use.names = FALSE)
  out <- as.character(out)
  out[nzchar(out)]
}

safe_read_csv <- function(path, required_cols = character()) {
  if (!file.exists(path)) stop("Missing required file: ", path)
  x <- read.csv(path, check.names = FALSE)
  miss <- setdiff(required_cols, names(x))
  if (length(miss) > 0) {
    stop("CSV missing required columns (", basename(path), "): ", paste(miss, collapse = ", "))
  }
  x
}

to_numeric_df <- function(df) {
  out <- df
  for (nm in names(out)) {
    if (!is.numeric(out[[nm]])) {
      out[[nm]] <- suppressWarnings(as.numeric(out[[nm]]))
    }
  }
  out
}

as_repo_path <- function(repo_root, p) {
  if (is.null(p) || !length(p) || !nzchar(p)) return(NA_character_)
  if (grepl("^[A-Za-z]:[/\\\\]", p)) return(p)
  file.path(repo_root, p)
}

as_predictor_name <- function(path, fallback = NULL) {
  nm <- tools::file_path_sans_ext(basename(path))
  bio_match <- regexec("bio[_-]?0?([1-9]|1[0-9])$", nm, ignore.case = TRUE)
  bio_hit <- regmatches(nm, bio_match)[[1]]
  if (length(bio_hit) > 0) {
    return(sprintf("BIO%02d", as.integer(bio_hit[2])))
  }
  if (!is.null(fallback) && nzchar(fallback)) return(fallback)
  nm <- gsub("[^A-Za-z0-9_]+", "_", nm)
  nm <- gsub("_+", "_", nm)
  gsub("^_|_$", "", nm)
}

append_removal_reason <- function(reason_map, predictor, reason) {
  existing <- reason_map[[predictor]]
  if (is.null(existing) || !length(existing)) {
    reason_map[[predictor]] <- reason
  } else {
    reason_map[[predictor]] <- unique(c(existing, reason))
  }
  reason_map
}

build_predictor_engine <- function(repo_root, run_dir, config_path, run_id) {
  if (exists("set_module_seed")) set_module_seed("predictor_engine") else set.seed(123456L)
  proc_dir <- file.path(run_dir, "01_processed_data")
  dist_dir <- file.path(proc_dir, "distance_layers")
  dir.create(dist_dir, recursive = TRUE, showWarnings = FALSE)

  config <- read_yaml(config_path)

  template <- try(rast(file.path(run_dir, "02_data_intermediate", "template_grid.tif")), silent = TRUE)
  if (inherits(template, "try-error")) {
    base_glob <- file.path(repo_root, config$predictors$baseline_climate_glob)
    base_files <- Sys.glob(base_glob)
    if (length(base_files) == 0) stop("No baseline climate files found for template")
    template <- rast(base_files[1])[[1]]
  }

  dist_configs <- list(
    list(name = "dist_to_major_rivers", path = config$vectors$rivers_major, out = "dist_to_major_rivers.tif"),
    list(name = "dist_to_streams", path = config$vectors$streams, out = "dist_to_streams.tif"),
    list(name = "dist_to_water_sources", path = config$vectors$water_sources, out = "dist_to_water_sources.tif"),
    list(name = "dist_to_roads", path = config$vectors$roads, out = "dist_to_roads.tif"),
    list(name = "dist_to_settlements", path = config$vectors$settlements, out = "dist_to_settlements.tif"),
    list(name = "dist_to_protected_areas", path = config$vectors$protected_areas, out = "dist_to_protected_areas.tif"),
    list(name = "dist_to_private_land", path = config$vectors$private_land, out = "dist_to_private_land.tif"),
    list(name = "dist_to_conflict_zones", path = config$vectors$conflict_zones, out = "dist_to_conflict_zones.tif")
  )

  dist_layers <- list()
  for (cfg in dist_configs) {
    source_file <- as_repo_path(repo_root, cfg$path)
    if (is.na(source_file) || !file.exists(source_file)) next
    v <- tryCatch(st_read(source_file, quiet = TRUE), error = function(e) NULL)
    if (is.null(v)) next
    v <- tryCatch(st_zm(v, drop = TRUE, what = "ZM"), error = function(e) v)

    vv <- vect(v)
    if (!isTRUE(crs(vv) == crs(template))) {
      vv <- project(vv, crs(template))
    }

    # Fast distance: rasterize vector to grid, then compute grid-based distance
    # This is orders of magnitude faster than distance(raster, vector) for complex geometries
    presence_r <- tryCatch({
      rasterize(vv, template, field = 1, background = NA)
    }, error = function(e) NULL)
    if (is.null(presence_r)) next
    d <- distance(presence_r)

    out_dist <- file.path(dist_dir, cfg$out)
    if (file.exists(out_dist)) file.remove(out_dist)
    if (file.exists(paste0(out_dist, ".aux.xml"))) file.remove(paste0(out_dist, ".aux.xml"))
    writeRaster(d, out_dist, overwrite = TRUE)
    dist_layers[[cfg$name]] <- out_dist
  }

  bio_glob <- if (!is.null(config$predictors$baseline_climate_glob) &&
                  nzchar(config$predictors$baseline_climate_glob)) {
    config$predictors$baseline_climate_glob
  } else {
    file.path(config$paths$raw_data_root,
              "02_rasters/present/Historical_bioclims/Historical_1986-2015_bio*.tif")
  }
  climate_files <- Sys.glob(file.path(repo_root, bio_glob))
  if (length(climate_files) == 0) climate_files <- Sys.glob(bio_glob)

  landcover_source <- tolower(config$predictors$primary_landcover_source %||% "dynamicworld")
  landcover_path <- if (identical(landcover_source, "local_lulc")) {
    file.path(run_dir, "02_data_intermediate", "local_lulc_harmonized.tif")
  } else {
    file.path(run_dir, "02_data_intermediate", "dynamicworld_label_harmonized.tif")
  }
  dw_prob_path <- file.path(run_dir, "02_data_intermediate", "dynamicworld_prob_harmonized.tif")
  other_layers <- c(
    human_footprint = file.path(run_dir, "02_data_intermediate", "human_footprint_harmonized.tif"),
    ndvi = file.path(run_dir, "02_data_intermediate", "ndvi_harmonized.tif"),
    evi = file.path(run_dir, "02_data_intermediate", "evi_harmonized.tif"),
    landcover = landcover_path
  )
  if (file.exists(dw_prob_path)) {
    other_layers <- c(other_layers, dynamicworld_prob = dw_prob_path)
  }
  terrain_layers <- c(
    elevation = file.path(run_dir, "02_data_intermediate", "elevation_harmonized.tif"),
    slope = file.path(run_dir, "02_data_intermediate", "slope_harmonized.tif"),
    aspect = file.path(run_dir, "02_data_intermediate", "aspect_harmonized.tif"),
    tri = file.path(run_dir, "02_data_intermediate", "tri_harmonized.tif")
  )
  other_layers <- other_layers[file.exists(other_layers)]
  terrain_layers <- terrain_layers[file.exists(terrain_layers)]

  path_map <- setNames(climate_files, vapply(climate_files, as_predictor_name, character(1)))
  static_paths <- c(other_layers, terrain_layers, unlist(dist_layers, use.names = FALSE))
  static_names <- c(names(other_layers), names(terrain_layers), names(dist_layers))
  keep_static <- file.exists(static_paths)
  static_paths <- static_paths[keep_static]
  static_names <- static_names[keep_static]
  if (length(static_paths) > 0) {
    names(static_paths) <- static_names
    path_map <- c(path_map, static_paths)
  }
  path_map <- path_map[!duplicated(names(path_map))]
  if (length(path_map) == 0) stop("No predictor rasters available")

  pres <- safe_read_csv(
    file.path(proc_dir, "presence_points_clean.csv"),
    c("longitude", "latitude")
  )
  bg <- safe_read_csv(
    file.path(proc_dir, "background_points.csv"),
    c("longitude", "latitude")
  )
  folds <- safe_read_csv(
    file.path(proc_dir, "fold_assignments.csv"),
    c("type", "id", "fold")
  )

  pres$response <- 1; bg$response <- 0
  pres$type <- "presence"; bg$type <- "background"
  if (!"id" %in% names(pres)) pres$id <- seq_len(nrow(pres))
  if (!"id" %in% names(bg)) bg$id <- seq_len(nrow(bg)) + max(pres$id, na.rm = TRUE)

  pres <- pres[, c("id", "type", "response", "longitude", "latitude"), drop = FALSE]
  bg <- bg[, c("id", "type", "response", "longitude", "latitude"), drop = FALSE]
  all_pts <- rbind(pres, bg)
  all_pts$row_id <- seq_len(nrow(all_pts))
  all_pts$key <- paste(all_pts$type, all_pts$id, sep = "::")

  vals <- data.frame(row_id = all_pts$row_id)
  pts <- vect(all_pts[, c("longitude", "latitude")], geom = c("longitude", "latitude"), crs = "EPSG:4326")

  layer_map <- character()
  for (pred_name in names(path_map)) {
    f <- path_map[[pred_name]]
    r <- try(rast(f), silent = TRUE)
    if (inherits(r, "try-error")) next

    if (!isTRUE(compareGeom(r, template, stopOnError = FALSE))) {
      method <- if (grepl("landcover|lulc|class|categor", tolower(pred_name))) "near" else "bilinear"
      r <- project(r, template, method = method)
    }

    ex <- try(extract(r, pts), silent = TRUE)
    if (inherits(ex, "try-error") || ncol(ex) < 2) next

    ex_df <- as.data.frame(ex)
    value_cols <- setdiff(names(ex_df), "ID")
    if (length(value_cols) < 1) next

    if (length(value_cols) > 1) {
      ex_vals <- ex_df[value_cols]
      ex_vals <- to_numeric_df(ex_vals)
      row_means <- suppressWarnings(rowMeans(ex_vals, na.rm = TRUE))
      row_means[!is.finite(row_means)] <- NA_real_
      value <- row_means
    } else {
      value <- ex_df[[value_cols[1]]]
    }

    vals[[pred_name]] <- suppressWarnings(as.numeric(value))
    layer_map[pred_name] <- f
  }

  pred_cols <- setdiff(names(vals), "row_id")
  if (length(pred_cols) == 0) stop("No predictor values extracted")

  vals_num <- vals[, pred_cols, drop = FALSE]
  vals_num <- to_numeric_df(vals_num)
  good_numeric <- names(vals_num)[sapply(vals_num, function(z) is.numeric(z) && any(is.finite(z), na.rm = TRUE))]
  if (length(good_numeric) == 0) stop("No numeric predictors extracted")
  pred_cols <- good_numeric

  vals_clean <- vals_num[complete.cases(vals_num[, pred_cols, drop = FALSE]), , drop = FALSE]
  if (nrow(vals_clean) == 0) {
    vals_clean <- vals_num
    for (nm in pred_cols) {
      if (is.numeric(vals_clean[[nm]])) {
        fill <- suppressWarnings(median(vals_clean[[nm]], na.rm = TRUE))
        if (!is.finite(fill)) fill <- 0
        vals_clean[[nm]][is.na(vals_clean[[nm]])] <- fill
      }
    }
  }

  col_cfg <- config$predictors$collinearity %||% list()
  cor_threshold <- suppressWarnings(as.numeric(col_cfg$correlation_threshold %||% 0.7))
  vif_threshold <- suppressWarnings(as.numeric(col_cfg$vif_threshold %||% 5))
  max_predictors <- suppressWarnings(as.integer(col_cfg$max_predictors %||% 25L))
  if (!is.finite(cor_threshold)) cor_threshold <- 0.7
  if (!is.finite(vif_threshold)) vif_threshold <- 5
  if (!is.finite(max_predictors) || max_predictors < 1) max_predictors <- 25L

  sel <- select_predictors_collinearity(
    data = vals_clean,
    pred_cols = pred_cols,
    cor_threshold = cor_threshold,
    vif_threshold = vif_threshold
  )
  final_preds <- sel$selected
  if (length(final_preds) == 0) final_preds <- pred_cols

  if (length(final_preds) > 1) {
    diag_res <- tryCatch(
      collinearity_diagnostics(vals_clean, final_preds, cor_threshold = cor_threshold, vif_threshold = vif_threshold),
      error = function(e) NULL
    )
    if (!is.null(diag_res) && length(diag_res$high_vif) > 0) {
      vif_rank <- sort(diag_res$vif_values[diag_res$high_vif], decreasing = TRUE, na.last = TRUE)
      for (to_remove in names(vif_rank)) {
        if (length(final_preds) <= 1 || !(to_remove %in% final_preds)) next
        final_preds <- setdiff(final_preds, to_remove)
        sel$removed <- unique(c(sel$removed, to_remove))
        sel$reasons <- append_removal_reason(
          sel$reasons,
          to_remove,
          sprintf("VIF %.2f > %.2f", vif_rank[[to_remove]], vif_threshold)
        )
      }
    }
  }

  if (length(final_preds) > max_predictors) {
    ranked <- names(sort(vapply(final_preds, function(nm) {
      vec <- vals_clean[[nm]]
      if (!is.numeric(vec)) return(Inf)
      stats::var(vec, na.rm = TRUE)
    }, numeric(1)), decreasing = TRUE, na.last = NA))
    keep_ranked <- head(ranked, max_predictors)
    dropped_ranked <- setdiff(final_preds, keep_ranked)
    for (to_remove in dropped_ranked) {
      sel$reasons <- append_removal_reason(
        sel$reasons,
        to_remove,
        sprintf("Trimmed to max_predictors=%d after collinearity filtering", max_predictors)
      )
    }
    sel$removed <- unique(c(sel$removed, dropped_ranked))
    final_preds <- keep_ranked
  }

  if (length(final_preds) == 0) stop("No predictors remain after collinearity filtering")

  folds$key <- paste(folds$type, folds$id, sep = "::")
  if (!all(c("key", "fold") %in% names(folds))) stop("fold_assignments.csv missing key/fold")

  dat <- merge(all_pts, folds[, c("key", "fold"), drop = FALSE], by = "key", all.x = TRUE)
  dat <- merge(dat, vals[, c("row_id", pred_cols), drop = FALSE], by = "row_id", all.x = TRUE)

  keep_preds <- intersect(final_preds, names(dat))
  if (length(keep_preds) == 0) {
    keep_preds <- intersect(pred_cols, names(dat))
  }
  if (length(keep_preds) == 0) {
    core <- c("key", "id", "type", "response", "longitude", "latitude", "fold")
    keep_preds <- setdiff(names(dat), core)
    keep_preds <- keep_preds[sapply(dat[, keep_preds, drop = FALSE], is.numeric)]
  }
  keep_preds <- as.character(keep_preds)
  if (length(keep_preds) == 0) stop("No predictors remain after merge")

  dat <- dat[, c("id", "type", "response", "longitude", "latitude", "fold", keep_preds), drop = FALSE]
  dat$fold <- as.integer(dat$fold)
  bad_fold <- which(!is.finite(dat$fold))
  if (length(bad_fold) > 0) {
    dat$fold[bad_fold] <- sample(1:5, length(bad_fold), replace = TRUE)
  }
  for (nm in keep_preds) {
    if (is.numeric(dat[[nm]])) {
      fill <- suppressWarnings(median(dat[[nm]], na.rm = TRUE))
      if (!is.finite(fill)) fill <- 0
      dat[[nm]][is.na(dat[[nm]])] <- fill
    }
  }

  write.csv(dat, file.path(proc_dir, "modeling_dataset.csv"), row.names = FALSE)

  manifest <- data.frame(
    predictor = names(layer_map),
    path = unname(layer_map[names(layer_map)]),
    selected = names(layer_map) %in% keep_preds,
    stringsAsFactors = FALSE
  )
  manifest$path[is.na(manifest$path)] <- ""
  write.csv(manifest, file.path(proc_dir, "predictor_manifest.csv"), row.names = FALSE)

  removed <- as_chr_vec(sel$removed)
  reasons <- sel$reasons %||% list()
  if (!is.list(reasons)) reasons <- as.list(reasons)
  if (length(removed) > 0) {
    reason_vec <- vapply(removed, function(nm) {
      r <- reasons[[nm]]
      if (is.null(r) || !length(r)) "" else paste(unique(as.character(r)), collapse = "; ")
    }, FUN.VALUE = character(1))
  } else {
    reason_vec <- character()
  }
  removal_log <- data.frame(
    predictor = removed,
    reason = reason_vec,
    stringsAsFactors = FALSE
  )
  if (nrow(removal_log) > 0) {
    write.csv(removal_log, file.path(proc_dir, "predictor_removal_log.csv"), row.names = FALSE)
  }

  list(predictors = keep_preds, n_predictors = length(keep_preds), n_rows = nrow(dat))
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  repo_root <- if (length(args) >= 1) args[[1]] else "."
  run_dir <- if (length(args) >= 2) args[[2]] else "."
  config_path <- if (length(args) >= 3) args[[3]] else "00_governance/config.yaml"
  run_id <- if (length(args) >= 4) args[[4]] else paste0("RUN_", format(Sys.time(), "%Y%m%d_%H%M%S"))

  result <- build_predictor_engine(repo_root, run_dir, config_path, run_id)
  cat(sprintf("Phase 4 complete: %d predictors, %d rows\n", result$n_predictors, result$n_rows))
}
