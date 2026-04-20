# test-pipeline-integration.R
# Structural integration tests for the SDM pipeline.
# Validates config, source graph, function availability, paths, and phase numbering
# WITHOUT executing the full pipeline (~90 min).
# Target execution time: < 10 seconds.

# --- Test 1: Config loading and required sections --------------------------

test_that("config.yaml loads and contains all required sections", {

  config_path <- repo_path("00_governance", "config.yaml")
  expect_true(file.exists(config_path))

  cfg <- yaml::read_yaml(config_path)
  expect_type(cfg, "list")

  required_sections <- c(
    "project", "paths", "occurrence", "predictors", "execution",
    "vectors", "climate_projections", "reproducibility", "logging"
  )
  for (s in required_sections) {
    expect_true(s %in% names(cfg), info = paste("Missing config section:", s))
  }

  # Verify key sub-fields

  expect_true(!is.null(cfg$project$name))
  expect_true(!is.null(cfg$paths$repo_root))
  expect_true(!is.null(cfg$occurrence$primary_input))
  expect_true(!is.null(cfg$predictors$baseline_climate_glob))
  expect_true(!is.null(cfg$execution$run_future_projections))
  expect_true(!is.null(cfg$reproducibility$global_seed))
  expect_true(!is.null(cfg$logging$pipeline_log))
})

# --- Test 2: Source graph — all R files sourced by run_pipeline.R exist -----

test_that("all R files sourced by run_pipeline.R exist on disk", {

  pipeline_path <- repo_path("03_analysis", "run_pipeline.R")
  expect_true(file.exists(pipeline_path))

  lines <- readLines(pipeline_path, warn = FALSE)
  source_lines <- grep("^\\s*source\\(", lines, value = TRUE)
  expect_true(length(source_lines) >= 15,
              info = paste("Expected >= 15 source() calls, found", length(source_lines)))

  # Extract filenames from source() calls like:
  #   source(file.path(repo_root, "03_analysis", "00_contract_helpers.R"), local = TRUE)
  # Pull the last quoted string before the closing paren that ends in .R
  for (sl in source_lines) {
    # Grab all quoted strings
    matches <- regmatches(sl, gregexpr('"[^"]+\\.R"', sl))[[1]]
    if (length(matches) == 0) next
    # The R filename is the last match (after directory components)
    r_file <- gsub('"', '', tail(matches, 1))
    # Reconstruct the path from repo root
    # The source() calls use file.path(repo_root, "03_analysis", "<file>")
    full_path <- repo_path("03_analysis", r_file)
    expect_true(file.exists(full_path),
                info = paste("Sourced file missing:", full_path))
  }
})

# --- Test 3: Function availability from helper modules ---------------------

test_that("00_sdm_helpers.R defines key SDM functions", {

  helpers_path <- repo_path("03_analysis", "00_sdm_helpers.R")
  expect_true(file.exists(helpers_path))

  env <- new.env(parent = baseenv())
  # Source into an isolated environment; suppress warnings about missing pkgs
  tryCatch(
    source(helpers_path, local = env, chdir = TRUE),
    error = function(e) skip(paste("Cannot source 00_sdm_helpers.R:", e$message))
  )

  expected_fns <- c(
    "auc_score", "tss_best", "boyce_continuous", "calculate_mess",
    "brier_score", "calculate_calibration_slope", "calculate_moran_i",
    "collinearity_diagnostics", "select_predictors_collinearity",
    "calculate_vif"
  )
  for (fn in expected_fns) {
    expect_true(exists(fn, envir = env, mode = "function"),
                info = paste("Function missing from 00_sdm_helpers.R:", fn))
  }
})

test_that("00_contract_helpers.R defines key contract functions", {

  contract_path <- repo_path("03_analysis", "00_contract_helpers.R")
  expect_true(file.exists(contract_path))

  env <- new.env(parent = baseenv())
  tryCatch(
    source(contract_path, local = env, chdir = TRUE),
    error = function(e) skip(paste("Cannot source 00_contract_helpers.R:", e$message))
  )

  expected_fns <- c("resolve_path", "validate_file", "load_contract_config",
                     "create_run_structure", "create_run_manifest")
  for (fn in expected_fns) {
    expect_true(exists(fn, envir = env, mode = "function"),
                info = paste("Function missing from 00_contract_helpers.R:", fn))
  }
})

# --- Test 4: No duplicate function definitions ----------------------------

test_that("00_contract_helpers.R does NOT duplicate SDM-owned functions", {

  contract_path <- repo_path("03_analysis", "00_contract_helpers.R")
  contract_lines <- readLines(contract_path, warn = FALSE)

  sdm_owned <- c("auc_score", "tss_best", "brier_score")
  for (fn in sdm_owned) {
    pattern <- paste0("^\\s*", fn, "\\s*<-\\s*function\\s*\\(")
    dups <- grep(pattern, contract_lines)
    expect_length(dups, 0,
                  info = paste(fn, "is defined in 00_contract_helpers.R but belongs in 00_sdm_helpers.R"))
  }
})

# --- Test 5: Config paths exist on disk -----------------------------------

test_that("all non-empty paths in config.yaml resolve to existing locations", {

  cfg <- yaml::read_yaml(repo_path("00_governance", "config.yaml"))
  repo_root <- normalizePath(REPO_ROOT, winslash = "/", mustWork = FALSE)

  resolve <- function(p) {
    if (grepl("^[A-Za-z]:[/\\\\]", p)) return(p)
    file.path(repo_root, p)
  }

  # paths section — these are directories
  dir_paths <- c(
    cfg$paths$metadata_root, cfg$paths$raw_data_root,
    cfg$paths$analysis_root, cfg$paths$outputs_root,
    cfg$paths$runs_root, cfg$paths$logs_root,
    cfg$paths$registry_root
  )
  for (dp in dir_paths) {
    if (is.null(dp) || !nzchar(dp)) next
    full <- resolve(dp)
    expect_true(dir.exists(full), info = paste("Directory missing:", full))
  }

  # file paths — occurrence, raster inputs, vectors
  file_paths <- c(
    cfg$occurrence$primary_input,
    cfg$predictors$dem,
    cfg$predictors$present_human_footprint,
    cfg$predictors$ndvi,
    cfg$predictors$evi,
    cfg$vectors$aoi_bhutan,
    cfg$vectors$rivers_major,
    cfg$vectors$streams,
    cfg$vectors$water_sources,
    cfg$vectors$roads,
    cfg$vectors$protected_areas
  )
  for (fp in file_paths) {
    if (is.null(fp) || !nzchar(fp)) next
    full <- resolve(fp)
    expect_true(file.exists(full), info = paste("File missing:", full))
  }
})

# --- Test 6: Baseline climate glob matches >= 19 BIO files ----------------

test_that("baseline climate glob resolves to at least 19 BIO rasters", {

  cfg <- yaml::read_yaml(repo_path("00_governance", "config.yaml"))
  repo_root <- normalizePath(REPO_ROOT, winslash = "/", mustWork = FALSE)
  glob_pattern <- file.path(repo_root, cfg$predictors$baseline_climate_glob)
  bio_files <- Sys.glob(glob_pattern)

  expect_gte(length(bio_files), 19,
             info = paste("Found only", length(bio_files), "BIO files"))

  # All should be .tif
  expect_true(all(grepl("\\.tif$", bio_files, ignore.case = TRUE)),
              info = "Not all matched files are .tif")
})

# --- Test 7: All configured shapefile paths exist -------------------------

test_that("all configured vector/shapefile paths exist", {

  cfg <- yaml::read_yaml(repo_path("00_governance", "config.yaml"))
  repo_root <- normalizePath(REPO_ROOT, winslash = "/", mustWork = FALSE)

  resolve <- function(p) {
    if (grepl("^[A-Za-z]:[/\\\\]", p)) return(p)
    file.path(repo_root, p)
  }

  vector_keys <- c(
    "aoi_bhutan", "rivers_major", "streams",
    "water_sources", "roads", "protected_areas"
  )
  for (vk in vector_keys) {
    vp <- cfg$vectors[[vk]]
    if (is.null(vp) || !nzchar(vp)) next
    full <- resolve(vp)
    expect_true(file.exists(full),
                info = paste("Shapefile missing for vectors$", vk, ":", full))
  }
})

# --- Test 8: Phase numbering in run_pipeline.R ----------------------------

test_that("run_pipeline.R references phases 0 through 13", {

  lines <- readLines(repo_path("03_analysis", "run_pipeline.R"), warn = FALSE)
  full_text <- paste(lines, collapse = "\n")

  expected_phases <- c(
    "Phase 0", "Phase 0b",
    "Phase 1", "Phase 2", "Phase 3", "Phase 4", "Phase 5",
    "Phase 6", "Phase 7", "Phase 8", "Phase 9",
    "Phase 10", "Phase 11", "Phase 12", "Phase 13"
  )

  for (ph in expected_phases) {
    expect_true(grepl(ph, full_text, fixed = TRUE),
                info = paste("Missing phase reference:", ph))
  }
})
