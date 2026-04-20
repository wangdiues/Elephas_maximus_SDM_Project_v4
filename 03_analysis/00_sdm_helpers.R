# STD-HARDENING: 2026-02-24
# role: phase_or_helper
# note: normalized script metadata for deterministic maintenance
# =============================================================================
# 00_sdm_helpers.R — Core Helper Functions for SDM Pipeline
# =============================================================================
# Purpose: Reusable functions for Boyce index, MESS, distance calculations, etc.
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
})

# =============================================================================
# BASIC METRICS
# =============================================================================

auc_score <- function(y_true, y_score) {
  y_true <- as.numeric(y_true)
  y_score <- as.numeric(y_score)
  keep <- is.finite(y_true) & is.finite(y_score)
  y_true <- y_true[keep]
  y_score <- y_score[keep]
  
  pos <- which(y_true == 1)
  neg <- which(y_true == 0)
  if (length(pos) == 0 || length(neg) == 0) return(NA_real_)
  
  r <- rank(y_score, ties.method = "average")
  (sum(r[pos]) - length(pos) * (length(pos) + 1) / 2) / (length(pos) * length(neg))
}

tss_best <- function(y_true, y_score) {
  y_true <- as.numeric(y_true)
  y_score <- as.numeric(y_score)
  keep <- is.finite(y_true) & is.finite(y_score)
  y_true <- y_true[keep]
  y_score <- y_score[keep]
  
  if (length(y_true) == 0) return(c(tss = NA_real_, threshold = NA_real_))
  thr <- sort(unique(y_score))
  best_tss <- -Inf
  best_thr <- NA_real_
  
  for (t in thr) {
    pred <- as.integer(y_score >= t)
    tp <- sum(pred == 1 & y_true == 1)
    fn <- sum(pred == 0 & y_true == 1)
    fp <- sum(pred == 1 & y_true == 0)
    tn <- sum(pred == 0 & y_true == 0)
    sens <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    spec <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
    tss <- sens + spec - 1
    if (is.finite(tss) && tss > best_tss) {
      best_tss <- tss
      best_thr <- t
    }
  }
  
  c(tss = ifelse(is.finite(best_tss), best_tss, NA_real_), threshold = best_thr)
}

# =============================================================================
# BOYCE INDEX (Continuous Boyce Index)
# =============================================================================

#' Calculate Continuous Boyce Index
#'
#' @param predicted Predicted suitability values at presence locations
#' @param background Predicted suitability values across all reference locations
#'   (random background for presence-only models, or all surveyed locations
#'   including true absences for presence-absence models)
#' @param n_bins Number of bins for P/E ratio calculation
#' @return List with boyce_index, p_e_ratios, bin_midpoints
#'
boyce_continuous <- function(predicted, background, n_bins = NULL) {
  predicted <- as.numeric(predicted)
  background <- as.numeric(background)
  predicted <- predicted[!is.na(predicted)]
  background <- background[!is.na(background)]

  if (length(predicted) < 5 || length(background) < 10) {
    return(list(boyce_index = NA_real_, p_e_ratios = NA_real_,
                bin_midpoints = NA_real_, message = "Insufficient data"))
  }

  # Adaptive bin count: fewer bins when presence records are scarce to reduce

  # volatility.  Hirzel et al. (2006) used 10 for large datasets; for small
  # samples (< 100 presences) we default to 5 to avoid many empty bins.
  if (is.null(n_bins)) {
    n_bins <- if (length(predicted) < 50) 5L else if (length(predicted) < 200) 8L else 10L
  }

  # Moving-window Boyce (Hirzel et al. 2006): produces a more stable estimate
  # than fixed bins when samples are small.  Falls back to equal-width bins.
  breaks <- seq(0, 1, length.out = n_bins + 1)

  bg_bins   <- cut(background, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  pred_bins <- cut(predicted,  breaks = breaks, include.lowest = TRUE, labels = FALSE)
  e <- table(factor(bg_bins,   levels = seq_len(n_bins))) / length(background)
  p <- table(factor(pred_bins, levels = seq_len(n_bins))) / length(predicted)

  pe_ratio <- as.numeric(p) / as.numeric(e)
  pe_ratio[is.nan(pe_ratio) | !is.finite(pe_ratio)] <- 0

  # Drop bins where E == 0 (no background) — these carry no information
  occupied <- as.numeric(e) > 0
  pe_used  <- pe_ratio[occupied]
  bin_midpoints <- ((seq_len(n_bins) - 0.5) / n_bins)[occupied]

  if (length(unique(pe_used)) < 2) {
    boyce <- NA_real_
    msg <- "Insufficient P/E variation — all predictions may fall in same bin (poor discrimination)"
  } else {
    boyce <- cor(bin_midpoints, pe_used, method = "spearman", use = "complete.obs")
    msg <- "Success"
  }

  list(
    boyce_index = boyce,
    p_e_ratios = pe_ratio,
    bin_midpoints = bin_midpoints,
    breaks = breaks,
    message = msg
  )
}

# =============================================================================
# MESS (Multivariate Environmental Similarity Surface)
# =============================================================================

#' Calculate MESS scores for novelty detection
#'
#' @param reference Reference data (training environmental space)
#' @param target Target points to evaluate (future climate values)
#' @param template Template raster for output
#' @return Raster with MESS scores (negative = novel)
#'
calculate_mess <- function(reference, target, template = NULL) {
  # reference: matrix or data.frame with training environmental values
  # target: matrix or data.frame with target environmental values
  
  reference <- as.matrix(reference)
  target <- as.matrix(target)
  
  if (ncol(reference) != ncol(target)) {
    stop("Reference and target must have same number of predictors")
  }
  
  n_target <- nrow(target)
  n_ref <- nrow(reference)
  n_pred <- ncol(reference)
  
  # Calculate reference statistics
  ref_min <- apply(reference, 2, min, na.rm = TRUE)
  ref_max <- apply(reference, 2, max, na.rm = TRUE)
  ref_mean <- apply(reference, 2, mean, na.rm = TRUE)
  ref_sd <- apply(reference, 2, sd, na.rm = TRUE)
  
  # For each target point, calculate similarity
  mess_scores <- numeric(n_target)
  
  for (i in 1:n_target) {
    target_vals <- target[i, ]
    
    # Calculate standardized difference for each predictor
    diffs <- numeric(n_pred)
    for (j in 1:n_pred) {
      if (!is.finite(target_vals[j]) || !is.finite(ref_min[j]) || !is.finite(ref_max[j])) {
        diffs[j] <- NA_real_
        next
      }
      sd_j <- ref_sd[j]
      if (!is.finite(sd_j) || sd_j == 0) sd_j <- 1
      if (target_vals[j] < ref_min[j]) {
        # Below reference range
        diffs[j] <- (target_vals[j] - ref_mean[j]) / sd_j
      } else if (target_vals[j] > ref_max[j]) {
        # Above reference range
        diffs[j] <- (target_vals[j] - ref_mean[j]) / sd_j
      } else {
        # Within reference range - most similar
        diffs[j] <- 0
      }
    }
    
    # MESS is the minimum (most negative) standardized difference
    # Positive values = within reference range for all predictors
    # Negative values = novel for at least one predictor
    mess_scores[i] <- if (all(is.na(diffs))) NA_real_ else min(diffs, na.rm = TRUE)
  }
  
  return(mess_scores)
}

#' Calculate MESS for raster stack
#'
#' @param ref_stack Reference raster stack (training climate)
#' @param target_stack Target raster stack (future climate)
#' @return Raster with MESS scores
#'
mess_raster <- function(ref_stack, target_stack, seed = NULL) {
  # Extract reference values (sample if too large)
  ref_vals <- as.matrix(ref_stack)
  if (nrow(ref_vals) > 10000) {
    if (!is.null(seed)) {
      set.seed(seed)
    } else if (exists(".SEED_STATE") && .SEED_STATE$initialized) {
      set_module_seed("mess")
    } else {
      set.seed(123456L)
    }
    ref_vals <- ref_vals[sample(nrow(ref_vals), 10000), , drop = FALSE]
  }
  
  # Calculate MESS for each cell in target
  target_vals <- as.matrix(target_stack)
  mess <- calculate_mess(ref_vals, target_vals)
  
  # Create output raster
  mess_rast <- target_stack[[1]]
  values(mess_rast) <- mess
  
  return(mess_rast)
}

# =============================================================================
# DISTANCE CALCULATIONS
# =============================================================================

#' Calculate distance to features from vector layer
#'
#' @param template Template raster
#' @param vector_sf sf object with features
#' @param output_path Output file path
#' @return Raster with distance values (in meters)
#'
calculate_distance_to_features <- function(template, vector_sf, output_path = NULL) {
  # Ensure vector is in same CRS as template
  if (st_crs(vector_sf) != crs(template, proj = TRUE)) {
    vector_sf <- st_transform(vector_sf, crs(template))
  }
  
  # Convert sf to SpatVector
  vector_v <- vect(vector_sf)
  
  # Calculate distance
  dist_rast <- distance(template, vector_v)
  
  # Save if path provided
  if (!is.null(output_path)) {
    writeRaster(dist_rast, output_path, overwrite = TRUE)
  }
  
  return(dist_rast)
}

#' Calculate multiple distance layers from vector directory
#'
#' @param template Template raster
#' @param vectors_root Root directory with shapefiles
#' @param output_dir Output directory for distance rasters
#' @return List of distance rasters
#'
calculate_all_distances <- function(template, vectors_root, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  distance_layers <- list()
  
  # Define expected vector layers and output names
  vector_configs <- list(
    list(name = "rivers", pattern = "river|stream", file = "dist_to_rivers.tif"),
    list(name = "water", pattern = "water", file = "dist_to_water.tif"),
    list(name = "roads", pattern = "road", file = "dist_to_roads.tif"),
    list(name = "settlements", pattern = "settlement", file = "dist_to_settlements.tif"),
    list(name = "pa", pattern = "pa_|protected", file = "dist_to_pa.tif"),
    list(name = "private_land", pattern = "pvt|private", file = "dist_to_private_land.tif"),
    list(name = "conflict", pattern = "conflict", file = "dist_to_conflict.tif")
  )
  
  for (config in vector_configs) {
    # Find matching shapefile
    shp_files <- list.files(vectors_root, pattern = config$pattern, 
                            ignore.case = TRUE, recursive = TRUE, full.names = TRUE)
    
    if (length(shp_files) > 0) {
      # Load first matching file
      vector_sf <- st_read(shp_files[1], quiet = TRUE)
      
      # Calculate distance
      dist_rast <- calculate_distance_to_features(
        template, 
        vector_sf, 
        file.path(output_dir, config$file)
      )
      
      distance_layers[[config$name]] <- list(
        raster = dist_rast,
        path = file.path(output_dir, config$file),
        source = shp_files[1]
      )
      
      cat(sprintf("✓ Created distance layer: %s\n", config$name))
    } else {
      cat(sprintf("⚠ Vector not found for: %s (pattern: %s)\n", config$name, config$pattern))
    }
  }
  
  return(distance_layers)
}

# =============================================================================
# COLLINEARITY DIAGNOSTICS
# =============================================================================

#' Calculate correlation matrix and VIF for predictors
#'
#' @param data Data frame with predictor columns
#' @param pred_cols Vector of predictor column names
#' @param cor_threshold Correlation threshold for removal
#' @param vif_threshold VIF threshold for removal
#' @return List with correlation matrix, VIF values, and recommended removals
#'
collinearity_diagnostics <- function(data, pred_cols, cor_threshold = 0.7, vif_threshold = 5) {
  # Extract predictor data
  pred_data <- data[, pred_cols, drop = FALSE]
  pred_data <- pred_data[stats::complete.cases(pred_data), , drop = FALSE]
  
  if (nrow(pred_data) < ncol(pred_data) + 10) {
    return(list(
      correlation_matrix = NULL,
      vif_values = NULL,
      high_cor_pairs = NULL,
      high_vif = NULL,
      message = sprintf("Insufficient data (%d rows, %d predictors)", nrow(pred_data), ncol(pred_data))
    ))
  }
  
  # Correlation matrix
  cor_matrix <- cor(pred_data, use = "pairwise.complete.obs")
  
  # Find high correlation pairs
  high_cor_pairs <- list()
  for (i in 1:(ncol(cor_matrix) - 1)) {
    for (j in (i + 1):ncol(cor_matrix)) {
      if (abs(cor_matrix[i, j]) > cor_threshold) {
        high_cor_pairs[[length(high_cor_pairs) + 1]] <- list(
          var1 = colnames(cor_matrix)[i],
          var2 = colnames(cor_matrix)[j],
          correlation = cor_matrix[i, j]
        )
      }
    }
  }
  
  # VIF calculation (R²-based, no external dependency)
  vif_values <- calculate_vif(pred_data, pred_cols)
  
  # Identify high VIF
  high_vif <- names(vif_values)[!is.na(vif_values) & vif_values > vif_threshold]
  
  return(list(
    correlation_matrix = cor_matrix,
    vif_values = vif_values,
    high_cor_pairs = high_cor_pairs,
    high_vif = high_vif,
    message = sprintf("Found %d high-correlation pairs, %d high-VIF predictors", 
                      length(high_cor_pairs), length(high_vif))
  ))
}

#' Select predictors after collinearity filtering
#'
#' @param data Data frame with predictors
#' @param pred_cols Initial predictor list
#' @param cor_threshold Correlation threshold
#' @param vif_threshold VIF threshold
#' @return Vector of selected predictor names
#'
select_predictors_collinearity <- function(data, pred_cols, cor_threshold = 0.7, vif_threshold = 5) {
  selected <- pred_cols
  removed <- character()
  removal_reasons <- list()
  
  # Iterative removal
  changed <- TRUE
  while (changed && length(selected) > 1) {
    changed <- FALSE
    
    # Check correlations
    pred_data <- data[, selected, drop = FALSE]
    pred_data <- pred_data[stats::complete.cases(pred_data), , drop = FALSE]
    
    if (nrow(pred_data) >= 10) {
      cor_matrix <- cor(pred_data, use = "pairwise.complete.obs")
      
      for (i in 1:(ncol(cor_matrix) - 1)) {
        for (j in (i + 1):ncol(cor_matrix)) {
          if (abs(cor_matrix[i, j]) > cor_threshold) {
            # Remove variable with higher mean correlation
            mean_cor_i <- mean(abs(cor_matrix[i, ]), na.rm = TRUE)
            mean_cor_j <- mean(abs(cor_matrix[j, ]), na.rm = TRUE)
            
            if (mean_cor_i > mean_cor_j) {
              to_remove <- colnames(cor_matrix)[i]
            } else {
              to_remove <- colnames(cor_matrix)[j]
            }
            
            if (to_remove %in% selected) {
              selected <- setdiff(selected, to_remove)
              removed <- c(removed, to_remove)
              removal_reasons[[to_remove]] <- sprintf("Correlated (r=%.2f) with %s", 
                                                       cor_matrix[i, j], 
                                                       colnames(cor_matrix)[ifelse(to_remove == colnames(cor_matrix)[i], j, i)])
              changed <- TRUE
              break
            }
          }
        }
        if (changed) break
      }
    }
  }
  
  return(list(
    selected = selected,
    removed = removed,
    reasons = removal_reasons
  ))
}

# =============================================================================
# SPATIAL BLOCKING
# =============================================================================

#' Create spatial blocks for cross-validation
#'
#' @param data Data frame with longitude/latitude
#' @param block_size_km Block size in kilometers
#' @param k_folds Number of folds
#' @param seed Random seed
#' @return Vector of fold assignments
#'
create_spatial_blocks <- function(data, block_size_km = 15, k_folds = 5, seed = NULL) {
  if (is.null(seed)) {
    seed <- if (exists(".SEED_STATE") && .SEED_STATE$initialized) .SEED_STATE$global_seed else 123456L
  }
  set.seed(seed)
  
  # Convert to sf
  pts <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
  
  # Project to UTM for distance calculations
  pts_proj <- st_transform(pts, 32645)  # UTM 45N for Bhutan
  
  # Calculate bounding box
  bbox <- st_bbox(pts_proj)
  width_km <- (bbox$xmax - bbox$xmin) / 1000
  height_km <- (bbox$ymax - bbox$ymin) / 1000
  
  # Calculate number of blocks in each direction
  n_blocks_x <- max(1, round(width_km / block_size_km))
  n_blocks_y <- max(1, round(height_km / block_size_km))
  
  # Create grid
  x_breaks <- seq(bbox$xmin, bbox$xmax, length.out = n_blocks_x + 1)
  y_breaks <- seq(bbox$ymin, bbox$ymax, length.out = n_blocks_y + 1)
  
  # Assign points to blocks
  coords <- st_coordinates(pts_proj)
  block_x <- findInterval(coords[, 1], x_breaks, all.inside = TRUE, rightmost.closed = TRUE)
  block_y <- findInterval(coords[, 2], y_breaks, all.inside = TRUE, rightmost.closed = TRUE)
  block_id <- (block_y - 1) * n_blocks_x + block_x
  
  # Assign blocks to folds
  unique_blocks <- unique(block_id)
  set.seed(seed)
  block_folds <- sample(1:k_folds, length(unique_blocks), replace = TRUE)
  names(block_folds) <- unique_blocks
  
  # Map back to points
  folds <- as.integer(block_folds[as.character(block_id)])
  na_idx <- which(!is.finite(folds))
  if (length(na_idx) > 0) {
    folds[na_idx] <- sample(seq_len(k_folds), length(na_idx), replace = TRUE)
  }
  return(folds)
}

# =============================================================================
# CALIBRATION & SPATIAL DIAGNOSTICS
# =============================================================================

#' Calculate calibration slope
#' @param y Observed (0/1)
#' @param p Predicted probabilities
#' @return List with slope, intercept, and diagnostics
calculate_calibration_slope <- function(y, p) {
  y <- as.numeric(y)
  p <- as.numeric(p)
  keep <- is.finite(y) & is.finite(p)
  y <- y[keep]
  p <- p[keep]
  
  if (length(y) < 10) {
    return(list(slope = NA, intercept = NA, brier = NA))
  }

  brier_val <- mean((p - y)^2)

  # Detect complete separation: presences systematically above background
  pres_p <- p[y == 1]; bg_p <- p[y == 0]
  if (length(pres_p) > 0 && length(bg_p) > 0 &&
      quantile(pres_p, 0.05, na.rm = TRUE) > quantile(bg_p, 0.95, na.rm = TRUE)) {
    return(list(slope = NA_real_, intercept = NA_real_, brier = brier_val,
                note = "complete_separation"))
  }

  p_clipped <- pmin(pmax(p, 0.001), 0.999)
  logit_p <- log(p_clipped / (1 - p_clipped))
  fit <- tryCatch(
    suppressWarnings(glm(y ~ logit_p, family = binomial())),
    error = function(e) NULL
  )
  if (is.null(fit)) return(list(slope = NA_real_, intercept = NA_real_, brier = brier_val))
  slope <- coef(fit)[2]
  intercept <- coef(fit)[1]
  
  # Brier score
  brier <- mean((p - y)^2)
  
  return(list(
    slope = ifelse(is.finite(slope), slope, NA),
    intercept = ifelse(is.finite(intercept), intercept, NA),
    brier = ifelse(is.finite(brier), brier, NA)
  ))
}

#' Calculate Moran's I for spatial autocorrelation
#' @param x Variable values
#' @param lon Longitude coordinates
#' @param lat Latitude coordinates
#' @param k Number of nearest neighbors
#' @return List with I statistic, p-value, and expectation
calculate_moran_i <- function(x, lon, lat, k = 5) {
  x <- as.numeric(x)
  lon <- as.numeric(lon)
  lat <- as.numeric(lat)
  
  keep <- is.finite(x) & is.finite(lon) & is.finite(lat)
  x <- x[keep]
  lon <- lon[keep]
  lat <- lat[keep]
  
  if (length(x) < 10) {
    return(list(I = NA, p_value = NA, expectation = NA))
  }
  
  # Create coordinates matrix
  coords <- cbind(lon, lat)
  n <- length(x)
  
  # Calculate distance matrix
  dist_mat <- as.matrix(dist(coords))
  
  # Create k-nearest neighbor weights
  weights <- matrix(0, n, n)
  for (i in 1:n) {
    order_dist <- order(dist_mat[i, ])
    neighbors <- order_dist[2:(k + 1)]  # Exclude self
    weights[i, neighbors] <- 1
  }
  
  # Symmetrize weights
  weights <- (weights + t(weights)) / 2
  
  # Calculate Moran's I
  x_mean <- mean(x)
  x_centered <- x - x_mean
  
  numerator <- sum(weights * outer(x_centered, x_centered))
  denominator <- sum(x_centered^2)
  sum_weights <- sum(weights)
  
  I <- (n / sum_weights) * (numerator / denominator)
  
  # Expectation under null
  E_I <- -1 / (n - 1)
  
  # Approximate p-value (normal approximation)
  var_I <- (n^2 * sum_weights - 3 * sum_weights^2 + 3 * n * sum(rowSums(weights)^2)) / 
           ((n - 1) * (n + 1) * sum_weights^2) - E_I^2
  
  z <- (I - E_I) / sqrt(var_I)
  p_value <- 2 * pnorm(-abs(z))
  
  return(list(
    I = ifelse(is.finite(I), I, NA),
    p_value = ifelse(is.finite(p_value), p_value, NA),
    expectation = ifelse(is.finite(E_I), E_I, NA)
  ))
}

# =============================================================================
# BRIER SCORE
# =============================================================================

#' Calculate Brier Score
#' @param y Observed binary outcome (0/1)
#' @param p Predicted probabilities
#' @return Brier score (lower is better; 0 = perfect)
brier_score <- function(y, p) {
  mean((as.numeric(p) - as.numeric(y))^2, na.rm = TRUE)
}

# =============================================================================
# VIF CALCULATION (standalone, no car dependency)
# =============================================================================

#' Calculate Variance Inflation Factor without requiring the car package
#' @param data Data frame with predictor columns
#' @param pred_cols Predictor column names
#' @return Named numeric vector of VIF values
calculate_vif <- function(data, pred_cols) {
  pred_data <- data[, pred_cols, drop = FALSE]
  pred_data <- pred_data[stats::complete.cases(pred_data), , drop = FALSE]
  vif_values <- numeric(length(pred_cols))
  names(vif_values) <- pred_cols
  for (i in seq_along(pred_cols)) {
    response <- pred_cols[i]
    predictors <- setdiff(pred_cols, response)
    if (length(predictors) == 0) { vif_values[i] <- 1; next }
    f <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
    fit <- tryCatch(lm(f, data = pred_data), error = function(e) NULL)
    if (is.null(fit)) { vif_values[i] <- NA_real_; next }
    r2 <- summary(fit)$r.squared
    vif_values[i] <- if (is.finite(r2) && r2 < 1) 1 / (1 - r2) else NA_real_
  }
  vif_values
}

# =============================================================================
# FIGURE HELPERS
# =============================================================================

#' Create publication-quality map
#'
#' @param raster Raster to plot
#' @param vector_sf Optional vector overlay
#' @param title Plot title
#' @param output_path Output file path
#' @param dpi Resolution (default 300)
#' @return NULL (saves file)
#'
create_sdm_map <- function(raster, vector_sf = NULL, title = "", output_path, dpi = 300) {
  library(ggplot2)
  library(viridis)
  
  # Convert raster to data.frame
  raster_df <- as.data.frame(raster, xy = TRUE, na.rm = TRUE)
  names(raster_df) <- c("x", "y", "value")
  
  # Create plot
  p <- ggplot() +
    geom_raster(data = raster_df, aes(x = x, y = y, fill = value)) +
    scale_fill_viridis(option = "magma", na.value = "transparent") +
    labs(title = title, fill = "Suitability", x = "Easting", y = "Northing") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      legend.position = "right"
    ) +
    coord_equal()
  
  # Add vector overlay if provided
  if (!is.null(vector_sf)) {
    p <- p + geom_sf(data = vector_sf, fill = NA, color = "black", linewidth = 0.5)
  }
  
  # Save
  ggsave(output_path, p, width = 10, height = 8, dpi = dpi)
  
  cat(sprintf("✓ Saved map: %s\n", output_path))
}

# =============================================================================
# END OF HELPERS
# =============================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x
}

# =============================================================================
# MAXENT LAMBDAS PREDICTOR — cloglog output from official jar model
# =============================================================================

predict_from_lambdas <- function(lambdas_path, new_data) {
  if (is.null(lambdas_path) || !file.exists(lambdas_path)) return(NULL)
  lines <- readLines(lambdas_path, warn = FALSE)

  params   <- list()
  features <- list()
  for (line in lines) {
    line <- trimws(line)
    if (!nzchar(line)) next
    parts <- trimws(strsplit(line, ",")[[1]])
    if (length(parts) == 2) {
      v <- suppressWarnings(as.numeric(parts[2]))
      if (!is.na(v)) params[[parts[1]]] <- v
    } else if (length(parts) == 4) {
      lam  <- suppressWarnings(as.numeric(parts[2]))
      fmin <- suppressWarnings(as.numeric(parts[3]))
      fmax <- suppressWarnings(as.numeric(parts[4]))
      if (!is.na(lam) && !is.na(fmin) && !is.na(fmax) && lam != 0)
        features[[length(features) + 1]] <- list(name=parts[1], lambda=lam, min=fmin, max=fmax)
    }
  }

  linNorm  <- params[["linearPredictorNormalizer"]] %||% 0
  densNorm <- params[["densityNormalizer"]] %||% 1
  H        <- params[["entropy"]] %||% 0

  n <- nrow(new_data)
  S <- rep(0, n)

  for (feat in features) {
    fname <- feat$name; lam <- feat$lambda
    fmin  <- feat$min;  fmax <- feat$max
    rng   <- fmax - fmin
    if (rng == 0) next

    val <- tryCatch({
      if (startsWith(fname, "hinge(")) {
        varname <- sub("hinge\\((.+)\\)", "\\1", fname)
        if (!varname %in% names(new_data)) next
        pmax(0, pmin(1, (as.numeric(new_data[[varname]]) - fmin) / rng))
      } else if (startsWith(fname, "revhinge(")) {
        varname <- sub("revhinge\\((.+)\\)", "\\1", fname)
        if (!varname %in% names(new_data)) next
        pmax(0, pmin(1, (fmax - as.numeric(new_data[[varname]])) / rng))
      } else if (endsWith(fname, "^2")) {
        varname <- sub("\\^2$", "", fname)
        if (!varname %in% names(new_data)) next
        x <- pmax(fmin, pmin(fmax, as.numeric(new_data[[varname]])))
        (x - fmin) / rng
      } else if (startsWith(fname, "product(")) {
        inner <- sub("product\\((.+)\\)", "\\1", fname)
        vars  <- strsplit(inner, "\\*")[[1]]
        if (!all(vars %in% names(new_data))) next
        raw <- as.numeric(new_data[[vars[1]]]) * as.numeric(new_data[[vars[2]]])
        (pmax(fmin, pmin(fmax, raw)) - fmin) / rng
      } else if (startsWith(fname, "(")) {
        m2 <- regexec("^\\((.+?)(>=|<=|>|<)([^)]+)\\)$", fname)
        p2 <- regmatches(fname, m2)[[1]]
        if (length(p2) < 4) next
        varname <- trimws(p2[2]); op <- p2[3]; thresh <- as.numeric(trimws(p2[4]))
        if (!varname %in% names(new_data) || is.na(thresh)) next
        x <- as.numeric(new_data[[varname]])
        if (op == ">=") as.numeric(x >= thresh) else as.numeric(x < thresh)
      } else {
        if (!fname %in% names(new_data)) next
        x <- pmax(fmin, pmin(fmax, as.numeric(new_data[[fname]])))
        (x - fmin) / rng
      }
    }, error = function(e) NULL)

    if (is.null(val) || length(val) != n) next
    S <- S + lam * val
  }

  # MaxEnt cloglog output
  raw <- exp(S - linNorm) / densNorm
  1 - exp(-exp(H) * raw)
}
