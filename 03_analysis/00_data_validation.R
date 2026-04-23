#!/usr/bin/env Rscript
# STD-HARDENING: 2026-02-24
# role: phase_or_helper
# note: normalized script metadata for deterministic maintenance

suppressWarnings({
  options(stringsAsFactors = FALSE)
})

strip_quotes <- function(x) {
  x <- trimws(x)
  x <- gsub("^['\"]|['\"]$", "", x)
  x
}

read_yaml_scalar <- function(lines, key) {
  idx <- grep(paste0("^\\s*", key, ":\\s*"), lines)
  if (length(idx) == 0) return(NA_character_)
  val <- sub(paste0("^\\s*", key, ":\\s*"), "", lines[idx[1]])
  strip_quotes(val)
}

read_required_columns <- function(lines) {
  idx <- grep("^\\s*required_columns:\\s*\\[", lines)
  if (length(idx) == 0) return(c("longitude", "latitude"))
  raw <- lines[idx[1]]
  inside <- sub("^.*\\[(.*)\\].*$", "\\1", raw)
  vals <- trimws(unlist(strsplit(inside, ",")))
  vals <- strip_quotes(vals)
  vals[nzchar(vals)]
}

as_abs_path <- function(repo_root, p) {
  if (is.na(p) || !nzchar(p)) return(NA_character_)
  is_abs <- grepl("^[A-Za-z]:[/\\\\]", p) || startsWith(p, "/")
  full <- if (is_abs) p else file.path(repo_root, p)
  normalizePath(full, winslash = "/", mustWork = FALSE)
}

detect_coord_columns <- function(df, required_cols) {
  nms <- names(df)
  nms_l <- tolower(nms)
  alias_map <- list(
    longitude = c("longitude", "decimallongitude", "lon", "x"),
    latitude = c("latitude", "decimallatitude", "lat", "y")
  )

  out <- list()
  missing <- c()
  for (req in required_cols) {
    req_l <- tolower(req)
    if (req %in% nms) {
      out[[req]] <- req
      next
    }
    choices <- alias_map[[req_l]]
    if (is.null(choices)) choices <- req_l
    hit <- nms[match(choices, nms_l)]
    hit <- hit[!is.na(hit)]
    if (length(hit) > 0) {
      out[[req]] <- hit[1]
    } else {
      missing <- c(missing, req)
    }
  }
  list(mapped = out, missing = missing)
}

build_report_lines <- function(success, cfg, selected_input, required_cols, mapped, n_records, errors, warnings) {
  c(
    "VALIDATION REPORT",
    paste0("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Success: ", success),
    "",
    "CONFIG",
    paste0("- repo_root: ", cfg$repo_root),
    paste0("- raw_data_root: ", cfg$raw_data_root),
    paste0("- outputs_root: ", cfg$outputs_root),
    paste0("- logs_root: ", cfg$logs_root),
    paste0("- registry_root: ", cfg$registry_root),
    "",
    "OCCURRENCE",
    paste0("- selected_input: ", selected_input),
    paste0("- required_columns: ", paste(required_cols, collapse = ", ")),
    paste0("- mapped_columns: ", if (length(mapped) == 0) "none" else paste(names(mapped), "->", unlist(mapped), collapse = ", ")),
    paste0("- n_records: ", ifelse(is.na(n_records), "NA", n_records)),
    "",
    "ERRORS",
    if (length(errors) == 0) "- none" else paste0("- ", errors),
    "",
    "WARNINGS",
    if (length(warnings) == 0) "- none" else paste0("- ", warnings)
  )
}

validate_pipeline <- function(config_path = "00_governance/config.yaml", report_path = NULL) {
  errors <- c()
  warnings <- c()

  if (!file.exists(config_path)) {
    stop("Config file not found: ", config_path)
  }

  # T3-F: Prefer yaml::read_yaml() for reliable parsing; fall back to regex on failure
  yaml_cfg <- NULL
  if (requireNamespace("yaml", quietly = TRUE)) {
    yaml_cfg <- tryCatch(yaml::read_yaml(config_path), error = function(e) NULL)
  }

  lines <- readLines(config_path, warn = FALSE)

  # Extract from yaml_cfg if available, else fall back to regex helpers
  get_val <- function(yaml_path, regex_key) {
    if (!is.null(yaml_cfg)) {
      parts <- strsplit(yaml_path, "\\$")[[1]]
      v <- yaml_cfg
      for (p in parts) { v <- v[[p]]; if (is.null(v)) return(NA_character_) }
      if (is.character(v) && length(v) == 1) return(v)
    }
    read_yaml_scalar(lines, regex_key)
  }

  cfg <- list(
    repo_root     = get_val("paths$repo_root",     "repo_root"),
    raw_data_root = get_val("paths$raw_data_root", "raw_data_root"),
    outputs_root  = get_val("paths$outputs_root",  "outputs_root"),
    logs_root     = get_val("paths$logs_root",      "logs_root"),
    registry_root = get_val("paths$registry_root", "registry_root"),
    primary_input = get_val("occurrence$primary_input",  "primary_input"),
    fallback_input= get_val("occurrence$fallback_input", "fallback_input")
  )
  required_cols <- read_required_columns(lines)

  cfg$repo_root <- as_abs_path(".", cfg$repo_root)
  cfg$raw_data_root <- as_abs_path(cfg$repo_root, cfg$raw_data_root)
  cfg$outputs_root <- as_abs_path(cfg$repo_root, cfg$outputs_root)
  cfg$logs_root <- as_abs_path(cfg$repo_root, cfg$logs_root)
  cfg$registry_root <- as_abs_path(cfg$repo_root, cfg$registry_root)
  primary_input <- as_abs_path(cfg$repo_root, cfg$primary_input)
  fallback_input <- as_abs_path(cfg$repo_root, cfg$fallback_input)

  must_dirs <- c(cfg$repo_root, cfg$raw_data_root, cfg$outputs_root, cfg$logs_root, cfg$registry_root)
  for (d in must_dirs) {
    if (is.na(d) || !dir.exists(d)) {
      errors <- c(errors, paste0("Missing required directory: ", d))
    }
  }

  selected_input <- NA_character_
  if (!is.na(primary_input) && file.exists(primary_input)) {
    selected_input <- primary_input
  } else if (!is.na(fallback_input) && file.exists(fallback_input)) {
    selected_input <- fallback_input
    warnings <- c(warnings, "Primary occurrence file missing; fallback file selected.")
  } else {
    errors <- c(errors, "Neither primary nor fallback occurrence file exists.")
  }

  n_records <- NA_integer_
  mapped <- list()
  if (!is.na(selected_input) && file.exists(selected_input)) {
    occ <- tryCatch(read.csv(selected_input, check.names = FALSE), error = function(e) NULL)
    if (is.null(occ)) {
      errors <- c(errors, paste0("Failed to read occurrence CSV: ", selected_input))
    } else {
      n_records <- nrow(occ)
      map_result <- detect_coord_columns(occ, required_cols)
      mapped <- map_result$mapped
      if (length(map_result$missing) > 0) {
        errors <- c(errors, paste0("Missing required occurrence columns (or aliases): ", paste(map_result$missing, collapse = ", ")))
      }
      if (n_records <= 0) {
        errors <- c(errors, "Occurrence CSV has zero records.")
      }
      # Check for presence/absence column
      pa_col <- names(occ)[tolower(names(occ)) %in% c("presence", "pa", "occ", "occurrence")][1]
      if (!is.na(pa_col)) {
        pa_vals <- suppressWarnings(as.integer(occ[[pa_col]]))
        invalid_pa <- sum(is.na(pa_vals) | !(pa_vals %in% c(0L, 1L)))
        if (invalid_pa > 0) {
          errors <- c(errors, sprintf("Column '%s' contains %d non-0/1 values (must be binary presence/absence)", pa_col, invalid_pa))
        } else {
          n_pres <- sum(pa_vals == 1L, na.rm = TRUE)
          n_abs  <- sum(pa_vals == 0L, na.rm = TRUE)
          warnings <- c(warnings, sprintf("PA column '%s' detected: %d presences, %d absences", pa_col, n_pres, n_abs))
        }
      } else {
        warnings <- c(warnings, "No presence/absence column detected — assuming all records are presences (presence-only mode)")
      }
    }
  }

  predictor_paths <- list(
    dem = if (!is.null(yaml_cfg)) yaml_cfg$predictors$dem else NA_character_,
    human_footprint = if (!is.null(yaml_cfg)) yaml_cfg$predictors$present_human_footprint else NA_character_,
    human_footprint_alt = if (!is.null(yaml_cfg)) yaml_cfg$predictors$alternate_human_footprint else NA_character_,
    landcover = if (!is.null(yaml_cfg)) yaml_cfg$predictors$present_landcover else NA_character_,
    local_lulc_raster = if (!is.null(yaml_cfg)) yaml_cfg$predictors$local_lulc_raster else NA_character_,
    local_lulc_vector = if (!is.null(yaml_cfg)) yaml_cfg$predictors$local_lulc_vector else NA_character_,
    ndvi = if (!is.null(yaml_cfg)) yaml_cfg$predictors$ndvi else NA_character_,
    evi = if (!is.null(yaml_cfg)) yaml_cfg$predictors$evi else NA_character_
  )
  for (nm in names(predictor_paths)) {
    p_rel <- predictor_paths[[nm]]
    if (is.null(p_rel) || is.na(p_rel) || !nzchar(p_rel)) next
    p_abs <- as_abs_path(cfg$repo_root, p_rel)
    if (!file.exists(p_abs)) {
      errors <- c(errors, paste0("Missing predictor file: ", nm, " -> ", p_abs))
    }
  }

  primary_landcover_source <- if (!is.null(yaml_cfg)) yaml_cfg$predictors$primary_landcover_source else NA_character_
  if (identical(primary_landcover_source, "local_lulc")) {
    local_lulc_abs <- as_abs_path(cfg$repo_root, predictor_paths$local_lulc_raster)
    if (is.na(local_lulc_abs) || !file.exists(local_lulc_abs)) {
      errors <- c(errors, "Primary landcover source is local_lulc but local_lulc_raster is missing.")
    }
  } else if (identical(primary_landcover_source, "esa")) {
    landcover_abs <- as_abs_path(cfg$repo_root, predictor_paths$landcover)
    if (is.na(landcover_abs) || !file.exists(landcover_abs)) {
      errors <- c(errors, "Primary landcover source is esa but present_landcover is missing.")
    }
  }

  vector_paths <- list(
    aoi = if (!is.null(yaml_cfg)) yaml_cfg$vectors$aoi_bhutan else NA_character_,
    rivers_major = if (!is.null(yaml_cfg)) yaml_cfg$vectors$rivers_major else NA_character_,
    streams = if (!is.null(yaml_cfg)) yaml_cfg$vectors$streams else NA_character_,
    water_sources = if (!is.null(yaml_cfg)) yaml_cfg$vectors$water_sources else NA_character_,
    roads = if (!is.null(yaml_cfg)) yaml_cfg$vectors$roads else NA_character_,
    settlements = if (!is.null(yaml_cfg)) yaml_cfg$vectors$settlements else NA_character_,
    protected_areas = if (!is.null(yaml_cfg)) yaml_cfg$vectors$protected_areas else NA_character_,
    private_land = if (!is.null(yaml_cfg)) yaml_cfg$vectors$private_land else NA_character_
  )
  for (nm in names(vector_paths)) {
    p_rel <- vector_paths[[nm]]
    if (is.null(p_rel) || is.na(p_rel) || !nzchar(p_rel)) next
    p_abs <- as_abs_path(cfg$repo_root, p_rel)
    if (!file.exists(p_abs)) {
      errors <- c(errors, paste0("Missing vector file: ", nm, " -> ", p_abs))
    }
  }

  conflict_rel <- if (!is.null(yaml_cfg)) yaml_cfg$vectors$conflict_zones else NA_character_
  if (!is.null(conflict_rel) && !is.na(conflict_rel) && nzchar(conflict_rel)) {
    conflict_abs <- as_abs_path(cfg$repo_root, conflict_rel)
    if (!file.exists(conflict_abs)) {
      warnings <- c(warnings, paste0("Configured conflict layer missing: ", conflict_abs))
    }
  } else {
    warnings <- c(warnings, "Conflict geometry not configured; conflict overlays will be skipped.")
  }

  # T3-F: BIO raster count check — fail if fewer than 19 BIO files found
  n_bio_found <- 0L
  if (!is.null(yaml_cfg)) {
    bio_glob <- yaml_cfg$predictors$baseline_climate_glob
    repo_rt  <- cfg$repo_root
    if (!is.null(bio_glob) && nzchar(bio_glob) && !is.null(repo_rt) && !is.na(repo_rt)) {
      bio_pattern <- file.path(repo_rt, bio_glob)
      bio_files   <- Sys.glob(bio_pattern)
      n_bio_found <- length(bio_files)
      if (n_bio_found < 19) {
        errors <- c(errors, sprintf(
          "BIO raster check: only %d of 19 required BIO files found (pattern: %s)",
          n_bio_found, bio_pattern))
      }
    }
  }

  # T3-F: AOI vector geometry validity check
  aoi_valid <- NA
  if (!is.null(yaml_cfg)) {
    aoi_rel  <- yaml_cfg$vectors$aoi_bhutan
    repo_rt  <- cfg$repo_root
    if (!is.null(aoi_rel) && nzchar(aoi_rel) && !is.null(repo_rt) && !is.na(repo_rt)) {
      aoi_path <- if (grepl("^[A-Za-z]:[/\\\\]", aoi_rel)) aoi_rel else file.path(repo_rt, aoi_rel)
      if (file.exists(aoi_path) && requireNamespace("sf", quietly = TRUE)) {
        aoi_sf <- tryCatch(sf::st_read(aoi_path, quiet = TRUE), error = function(e) NULL)
        if (!is.null(aoi_sf)) {
          validity <- sf::st_is_valid(aoi_sf)
          if (!all(validity)) {
            aoi_sf  <- sf::st_make_valid(aoi_sf)
            aoi_valid <- "repaired"
            warnings <- c(warnings, "AOI geometry had invalid features; repaired with st_make_valid().")
          } else {
            aoi_valid <- "valid"
          }
        } else {
          warnings <- c(warnings, paste("AOI could not be read:", aoi_path))
          aoi_valid <- "unreadable"
        }
      }
    }
  }

  success <- length(errors) == 0
  report_lines <- build_report_lines(success, cfg, selected_input, required_cols, mapped, n_records, errors, warnings)
  if (!is.null(report_path)) {
    dir.create(dirname(report_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(report_lines, report_path)

    # T3-F: Write machine-readable validation_summary.csv alongside the text report
    manifest_dir <- dirname(report_path)
    summary_csv  <- file.path(manifest_dir, "validation_summary.csv")
    summary_df   <- data.frame(
      timestamp   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      success     = success,
      n_errors    = length(errors),
      n_warnings  = length(warnings),
      n_records   = ifelse(is.na(n_records), NA_integer_, as.integer(n_records)),
      n_bio_found = n_bio_found,
      aoi_valid   = ifelse(is.na(aoi_valid), "unchecked", aoi_valid),
      stringsAsFactors = FALSE
    )
    write.csv(summary_df, summary_csv, row.names = FALSE)
  }

  list(
    success = success,
    errors = errors,
    warnings = warnings,
    report_lines = report_lines,
    details = list(
      selected_input = selected_input,
      n_records = n_records,
      mapped_columns = mapped,
      required_columns = required_cols,
      cfg = cfg
    )
  )
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  config_path <- if (length(args) >= 1) args[[1]] else "00_governance/config.yaml"
  report_path <- if (length(args) >= 2) args[[2]] else file.path("06_logs", "validation_report_latest.txt")

  result <- validate_pipeline(config_path, report_path)
  cat(paste(result$report_lines, collapse = "\n"), "\n")
  quit(save = "no", status = ifelse(result$success, 0, 1))
}

if (sys.nframe() == 0) {
  main()
}
