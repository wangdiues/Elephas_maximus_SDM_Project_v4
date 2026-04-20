# =============================================================================
# test-core-functions.R — Comprehensive tests for SDM pipeline core functions
# =============================================================================
# Covers: 00_sdm_helpers.R, 00_contract_helpers.R, 00_logging.R
# Style: testthat 3e (test_that blocks, self-contained synthetic data)
# =============================================================================

# Source the files under test
source(repo_path("03_analysis", "00_sdm_helpers.R"))
source(repo_path("03_analysis", "00_contract_helpers.R"))
source(repo_path("03_analysis", "00_logging.R"))

# =============================================================================
# auc_score()
# =============================================================================

test_that("auc_score returns 1.0 for perfect discrimination", {
  y <- c(rep(0, 50), rep(1, 50))
  scores <- c(rep(0, 50), rep(1, 50))
  expect_equal(auc_score(y, scores), 1.0)
})

test_that("auc_score returns ~0.5 for random scores", {
  set.seed(42)
  y <- c(rep(0, 500), rep(1, 500))
  scores <- runif(1000)
  auc <- auc_score(y, scores)
  expect_true(auc > 0.4 && auc < 0.6)
})

test_that("auc_score returns 0.0 for perfectly reversed discrimination", {
  y <- c(rep(0, 50), rep(1, 50))
  scores <- c(rep(1, 50), rep(0, 50))
  expect_equal(auc_score(y, scores), 0.0)
})

test_that("auc_score returns NA when all labels are 0", {
  expect_true(is.na(auc_score(rep(0, 10), runif(10))))
})

test_that("auc_score returns NA when all labels are 1", {
  expect_true(is.na(auc_score(rep(1, 10), runif(10))))
})

test_that("auc_score handles NA values gracefully", {
  y <- c(1, 0, NA, 1, 0)
  scores <- c(0.9, 0.1, 0.5, 0.8, 0.2)
  auc <- auc_score(y, scores)
  expect_true(is.finite(auc))
  expect_equal(auc, 1.0)
})

test_that("auc_score handles Inf in scores by filtering", {
  y <- c(1, 0, 1, 0)
  scores <- c(Inf, 0.1, 0.8, 0.2)
  auc <- auc_score(y, scores)
  # Inf filtered out, leaves 3 valid: y=c(0,1,0), scores=c(0.1,0.8,0.2)
  expect_true(is.finite(auc))
})

test_that("auc_score handles tied scores", {
  y <- c(1, 0, 1, 0)
  scores <- c(0.5, 0.5, 0.5, 0.5)
  auc <- auc_score(y, scores)
  expect_equal(auc, 0.5)
})

# =============================================================================
# tss_best()
# =============================================================================

test_that("tss_best returns TSS = 1 for perfect discrimination", {
  y <- c(rep(0, 50), rep(1, 50))
  scores <- c(rep(0.1, 50), rep(0.9, 50))
  result <- tss_best(y, scores)
  expect_equal(unname(result["tss"]), 1.0)
  expect_true(is.finite(result["threshold"]))
})

test_that("tss_best returns TSS near 0 for random predictions", {
  set.seed(99)
  y <- c(rep(0, 500), rep(1, 500))
  scores <- runif(1000)
  result <- tss_best(y, scores)
  expect_true(result["tss"] < 0.15)
})

test_that("tss_best returns named vector with tss and threshold", {
  y <- c(0, 0, 1, 1)
  scores <- c(0.2, 0.3, 0.7, 0.8)
  result <- tss_best(y, scores)
  expect_true("tss" %in% names(result))
  expect_true("threshold" %in% names(result))
})

test_that("tss_best returns NA for empty input after filtering", {
  result <- tss_best(numeric(0), numeric(0))
  expect_true(is.na(result["tss"]))
  expect_true(is.na(result["threshold"]))
})

test_that("tss_best handles all-NA scores", {
  result <- tss_best(c(0, 1, 0, 1), c(NA, NA, NA, NA))
  expect_true(is.na(result["tss"]))
})

test_that("tss_best handles single-class labels (all 1)", {
  # All y=1 means no negatives -> spec=NA -> TSS=NA for all thresholds
  result <- tss_best(rep(1, 10), runif(10))
  expect_true(is.na(result["tss"]))
})

# =============================================================================
# brier_score()
# =============================================================================

test_that("brier_score returns 0 for perfect predictions", {
  y <- c(0, 0, 1, 1)
  p <- c(0, 0, 1, 1)
  expect_equal(brier_score(y, p), 0)
})

test_that("brier_score returns 1 for worst-case predictions", {
  y <- c(0, 0, 1, 1)
  p <- c(1, 1, 0, 0)
  expect_equal(brier_score(y, p), 1)
})

test_that("brier_score returns ~0.25 for constant 0.5 predictions", {
  y <- c(rep(0, 100), rep(1, 100))
  p <- rep(0.5, 200)
  expect_equal(brier_score(y, p), 0.25)
})

test_that("brier_score handles NAs via na.rm", {
  y <- c(0, 1, NA)
  p <- c(0, 1, 0.5)
  bs <- brier_score(y, p)
  expect_true(is.finite(bs))
})

# =============================================================================
# boyce_continuous()
# =============================================================================

test_that("boyce_continuous returns positive index for good model", {
  set.seed(10)
  # Good model: presences cluster in high suitability
  background <- runif(500, 0, 1)
  predicted <- rbeta(100, 5, 1)  # skewed toward 1
  result <- boyce_continuous(predicted, background)
  expect_true(result$boyce_index > 0)
  expect_equal(result$message, "Success")
})

test_that("boyce_continuous returns negative index for bad model", {
  set.seed(11)
  # Bad model: presences cluster in low suitability
  background <- runif(500, 0, 1)
  predicted <- rbeta(100, 1, 5)  # skewed toward 0
  result <- boyce_continuous(predicted, background)
  expect_true(result$boyce_index < 0)
})

test_that("boyce_continuous returns NA for insufficient presence data", {
  result <- boyce_continuous(c(0.5, 0.6), runif(100))
  expect_true(is.na(result$boyce_index))
  expect_equal(result$message, "Insufficient data")
})

test_that("boyce_continuous returns NA for insufficient background data", {
  result <- boyce_continuous(runif(20), c(0.5, 0.6))
  expect_true(is.na(result$boyce_index))
  expect_equal(result$message, "Insufficient data")
})

test_that("boyce_continuous returns list with expected components", {
  set.seed(12)
  result <- boyce_continuous(runif(50), runif(200))
  expect_true(is.list(result))
  expect_true("boyce_index" %in% names(result))
  expect_true("p_e_ratios" %in% names(result))
  expect_true("bin_midpoints" %in% names(result))
  expect_true("message" %in% names(result))
})

test_that("boyce_continuous respects custom n_bins", {
  set.seed(13)
  result <- boyce_continuous(runif(50), runif(200), n_bins = 3)
  expect_equal(length(result$p_e_ratios), 3)
})

test_that("boyce_continuous adaptive binning uses fewer bins for small samples", {
  set.seed(14)
  # < 50 presences -> 5 bins
  result <- boyce_continuous(runif(30), runif(200))
  expect_equal(length(result$p_e_ratios), 5)
})

# =============================================================================
# calculate_mess()
# =============================================================================

test_that("calculate_mess returns 0 for target within reference range", {
  ref <- matrix(c(1, 5, 2, 6, 3, 7), ncol = 2)
  target <- matrix(c(2, 5), ncol = 2)
  scores <- calculate_mess(ref, target)
  expect_equal(scores, 0)
})

test_that("calculate_mess returns negative for target outside range", {
  ref <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
  # target well outside range
  target <- matrix(c(100, 100), ncol = 2)
  scores <- calculate_mess(ref, target)
  expect_true(all(scores < 0))
})

test_that("calculate_mess errors on mismatched columns", {
  ref <- matrix(1:6, ncol = 2)
  target <- matrix(1:9, ncol = 3)
  expect_error(calculate_mess(ref, target), "same number of predictors")
})

test_that("calculate_mess returns vector of correct length", {
  ref <- matrix(runif(60), ncol = 3)
  target <- matrix(runif(30), ncol = 3)
  scores <- calculate_mess(ref, target)
  expect_length(scores, 10)  # 30/3 = 10 rows
})

test_that("calculate_mess handles NA in target gracefully", {
  ref <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
  target <- matrix(c(NA, 3), ncol = 2)
  scores <- calculate_mess(ref, target)
  # one predictor NA -> diffs has one NA, min should still work with na.rm
  expect_length(scores, 1)
})

# =============================================================================
# calculate_vif()
# =============================================================================

test_that("calculate_vif returns ~1 for uncorrelated predictors", {
  set.seed(20)
  n <- 200
  df <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
  vifs <- calculate_vif(df, c("a", "b", "c"))
  expect_true(all(vifs < 1.5))
  expect_true(all(vifs > 0.8))
})

test_that("calculate_vif returns high values for collinear predictors", {
  set.seed(21)
  n <- 200
  x <- rnorm(n)
  df <- data.frame(a = x, b = x + rnorm(n, sd = 0.01), c = rnorm(n))
  vifs <- calculate_vif(df, c("a", "b", "c"))
  expect_true(vifs["a"] > 10)
  expect_true(vifs["b"] > 10)
})

test_that("calculate_vif returns VIF=1 for single predictor", {
  df <- data.frame(a = rnorm(50))
  vifs <- calculate_vif(df, "a")
  expect_equal(unname(vifs), 1)
})

test_that("calculate_vif returns named vector", {
  df <- data.frame(x1 = rnorm(50), x2 = rnorm(50))
  vifs <- calculate_vif(df, c("x1", "x2"))
  expect_equal(names(vifs), c("x1", "x2"))
})

# =============================================================================
# select_predictors_collinearity()
# =============================================================================

test_that("select_predictors_collinearity removes correlated pairs", {
  set.seed(30)
  n <- 200
  x <- rnorm(n)
  df <- data.frame(
    a = x,
    b = x + rnorm(n, sd = 0.01),  # nearly identical to a
    c = rnorm(n)
  )
  result <- select_predictors_collinearity(df, c("a", "b", "c"), cor_threshold = 0.7)
  # One of a/b should be removed
  expect_true(length(result$selected) < 3)
  expect_true("c" %in% result$selected)
  expect_true(length(result$removed) >= 1)
})

test_that("select_predictors_collinearity keeps all uncorrelated predictors", {
  set.seed(31)
  n <- 200
  df <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
  result <- select_predictors_collinearity(df, c("a", "b", "c"), cor_threshold = 0.7)
  expect_equal(length(result$selected), 3)
  expect_equal(length(result$removed), 0)
})

test_that("select_predictors_collinearity returns list with correct structure", {
  df <- data.frame(x = rnorm(50), y = rnorm(50))
  result <- select_predictors_collinearity(df, c("x", "y"))
  expect_true(is.list(result))
  expect_true("selected" %in% names(result))
  expect_true("removed" %in% names(result))
  expect_true("reasons" %in% names(result))
})

test_that("select_predictors_collinearity handles single predictor", {
  df <- data.frame(a = rnorm(50))
  result <- select_predictors_collinearity(df, "a")
  expect_equal(result$selected, "a")
  expect_equal(length(result$removed), 0)
})

# =============================================================================
# create_spatial_blocks()
# =============================================================================

test_that("create_spatial_blocks returns vector of correct length", {
  set.seed(40)
  df <- data.frame(
    longitude = runif(50, 89.5, 91.5),
    latitude = runif(50, 26.5, 28.0)
  )
  folds <- create_spatial_blocks(df, block_size_km = 15, k_folds = 5, seed = 42)
  expect_length(folds, 50)
})

test_that("create_spatial_blocks returns values in 1:k_folds", {
  df <- data.frame(
    longitude = runif(100, 89.5, 91.5),
    latitude = runif(100, 26.5, 28.0)
  )
  folds <- create_spatial_blocks(df, k_folds = 4, seed = 123)
  expect_true(all(folds %in% 1:4))
})

test_that("create_spatial_blocks is reproducible with same seed", {
  df <- data.frame(
    longitude = runif(80, 89.5, 91.5),
    latitude = runif(80, 26.5, 28.0)
  )
  folds1 <- create_spatial_blocks(df, seed = 999)
  folds2 <- create_spatial_blocks(df, seed = 999)
  expect_identical(folds1, folds2)
})

test_that("create_spatial_blocks produces different folds with different seeds", {
  df <- data.frame(
    longitude = runif(80, 89.5, 91.5),
    latitude = runif(80, 26.5, 28.0)
  )
  folds1 <- create_spatial_blocks(df, seed = 1)
  folds2 <- create_spatial_blocks(df, seed = 2)
  # Very unlikely to be identical with different seeds

  expect_false(all(folds1 == folds2))
})

test_that("create_spatial_blocks handles minimal data (2 points)", {
  df <- data.frame(
    longitude = c(90.0, 90.1),
    latitude = c(27.0, 27.1)
  )
  folds <- create_spatial_blocks(df, k_folds = 2, seed = 42)
  expect_length(folds, 2)
  expect_true(all(folds %in% 1:2))
})

# =============================================================================
# calculate_calibration_slope()
# =============================================================================

test_that("calculate_calibration_slope returns finite slope for calibrated model", {
  set.seed(50)
  n <- 200
  p <- runif(n)
  y <- rbinom(n, 1, p)
  result <- calculate_calibration_slope(y, p)
  expect_true(is.finite(result$slope))
  expect_true(is.finite(result$intercept))
  expect_true(is.finite(result$brier))
})

test_that("calculate_calibration_slope returns brier near 0 for perfect predictions", {
  y <- c(rep(0, 100), rep(1, 100))
  # Near-perfect but not exactly 0/1 to avoid separation
  p <- c(rep(0.05, 100), rep(0.95, 100))
  result <- calculate_calibration_slope(y, p)
  expect_true(result$brier < 0.05)
})

test_that("calculate_calibration_slope returns NA for insufficient data", {
  result <- calculate_calibration_slope(c(0, 1, 0), c(0.1, 0.9, 0.2))
  expect_true(is.na(result$slope))
  expect_true(is.na(result$intercept))
  expect_true(is.na(result$brier))
})

test_that("calculate_calibration_slope detects complete separation", {
  y <- c(rep(0, 50), rep(1, 50))
  p <- c(rep(0.01, 50), rep(0.99, 50))
  result <- calculate_calibration_slope(y, p)
  # Should detect separation since 5th percentile of pres > 95th percentile of bg
  expect_equal(result$note, "complete_separation")
})

test_that("calculate_calibration_slope handles NA values", {
  set.seed(51)
  y <- c(rbinom(50, 1, 0.5), NA, NA)
  p <- c(runif(50), NA, NA)
  result <- calculate_calibration_slope(y, p)
  # Should still work after filtering NAs (50 > 10 threshold)
  expect_true(is.list(result))
})

# =============================================================================
# calculate_moran_i()
# =============================================================================

test_that("calculate_moran_i returns I near 0 for random data", {
  set.seed(60)
  n <- 100
  result <- calculate_moran_i(
    x = rnorm(n),
    lon = runif(n, 0, 10),
    lat = runif(n, 0, 10)
  )
  expect_true(abs(result$I) < 0.3)
})

test_that("calculate_moran_i returns NA for insufficient data", {
  result <- calculate_moran_i(c(1, 2, 3), c(0, 1, 2), c(0, 1, 2))
  expect_true(is.na(result$I))
  expect_true(is.na(result$p_value))
})

test_that("calculate_moran_i returns expected components", {
  set.seed(61)
  result <- calculate_moran_i(rnorm(20), runif(20), runif(20))
  expect_true("I" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("expectation" %in% names(result))
})

test_that("calculate_moran_i expectation is -1/(n-1)", {
  set.seed(62)
  n <- 50
  result <- calculate_moran_i(rnorm(n), runif(n), runif(n))
  expect_equal(result$expectation, -1 / (n - 1), tolerance = 1e-10)
})

test_that("calculate_moran_i detects positive spatial autocorrelation", {
  # Create spatially clustered data: nearby points have similar values
  set.seed(63)
  n <- 100
  lon <- runif(n, 0, 10)
  lat <- runif(n, 0, 10)
  # Values are a function of location (strong spatial structure)
  x <- lon + lat + rnorm(n, sd = 0.1)
  result <- calculate_moran_i(x, lon, lat)
  expect_true(result$I > 0)
})

test_that("calculate_moran_i handles NAs in input", {
  set.seed(64)
  n <- 30
  x <- c(rnorm(n - 2), NA, NA)
  lon <- runif(n)
  lat <- runif(n)
  result <- calculate_moran_i(x, lon, lat)
  # After filtering NAs: 28 > 10 threshold, should work
  expect_true(is.list(result))
})

# =============================================================================
# resolve_path()
# =============================================================================

test_that("resolve_path passes through absolute Windows path", {
  result <- resolve_path("C:/Users/test/data.tif")
  expect_true(grepl("^[A-Z]:", result))
  expect_true(grepl("data.tif$", result))
})

test_that("resolve_path passes through absolute Unix path", {
  result <- resolve_path("/tmp/data.tif")
  expect_true(startsWith(result, "/"))
})

test_that("resolve_path resolves relative to repo_root", {
  result <- resolve_path("subdir/file.csv", repo_root = tempdir())
  expect_true(grepl("subdir", result))
  expect_true(grepl("file.csv$", result))
})

test_that("resolve_path returns NA for empty string", {
  expect_true(is.na(resolve_path("")))
})

test_that("resolve_path returns NA for NA input", {
  expect_true(is.na(resolve_path(NA)))
})

test_that("resolve_path normalizes slashes", {
  result <- resolve_path("a/b/c.txt", repo_root = tempdir())
  # Should use forward slashes (winslash = "/")
  expect_false(grepl("\\\\", result))
})

# =============================================================================
# validate_file()
# =============================================================================

test_that("validate_file returns error for nonexistent file", {
  result <- validate_file("/nonexistent/path/fake.tif")
  expect_false(result$valid)
  expect_equal(result$error, "File does not exist")
})

test_that("validate_file returns error for empty file", {
  tmp <- tempfile(fileext = ".tif")
  file.create(tmp)
  result <- validate_file(tmp)
  expect_false(result$valid)
  expect_equal(result$error, "File is empty")
  unlink(tmp)
})

test_that("validate_file validates a real raster", {
  tmp <- tempfile(fileext = ".tif")
  r <- terra::rast(nrows = 10, ncols = 10, vals = runif(100))
  terra::writeRaster(r, tmp, overwrite = TRUE)
  result <- validate_file(tmp, type = "raster")
  expect_true(result$valid)
  expect_null(result$error)
  unlink(tmp)
})

test_that("validate_file catches all-NA raster", {
  tmp <- tempfile(fileext = ".tif")
  r <- terra::rast(nrows = 10, ncols = 10, vals = NA)
  terra::writeRaster(r, tmp, overwrite = TRUE)
  result <- validate_file(tmp, type = "raster")
  expect_false(result$valid)
  expect_equal(result$error, "All values are NA")
  unlink(tmp)
})

test_that("validate_file catches zero-variance raster", {
  tmp <- tempfile(fileext = ".tif")
  r <- terra::rast(nrows = 10, ncols = 10, vals = rep(5, 100))
  terra::writeRaster(r, tmp, overwrite = TRUE)
  result <- validate_file(tmp, type = "raster")
  expect_false(result$valid)
  expect_equal(result$error, "Zero variance")
  unlink(tmp)
})

test_that("validate_file validates a CSV table", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1:3, b = 4:6), tmp, row.names = FALSE)
  result <- validate_file(tmp, type = "table")
  expect_true(result$valid)
  unlink(tmp)
})

test_that("validate_file catches empty CSV", {
  tmp <- tempfile(fileext = ".csv")
  writeLines("a,b", tmp)  # header only, no data rows
  result <- validate_file(tmp, type = "table")
  expect_false(result$valid)
  expect_equal(result$error, "Table is empty")
  unlink(tmp)
})

# =============================================================================
# initialize_logs() + log_info()
# =============================================================================

test_that("initialize_logs creates log directory and files", {
  log_dir <- file.path(tempdir(), paste0("test_logs_", Sys.getpid()))
  on.exit(unlink(log_dir, recursive = TRUE), add = TRUE)

  # Reset log state to allow re-initialization
  .LOG_STATE$initialized <- FALSE
  .LOG_STATE$run_id <- NULL
  .LOG_STATE$log_dir <- NULL

  initialize_logs("TEST_RUN_001", log_dir = log_dir)

  expect_true(dir.exists(log_dir))
  expect_true(file.exists(file.path(log_dir, "pipeline.log")))
  expect_true(file.exists(file.path(log_dir, "errors.log")))
  expect_true(file.exists(file.path(log_dir, "warnings.log")))
})

test_that("log_info appends entries to pipeline.log", {
  log_dir <- file.path(tempdir(), paste0("test_logs2_", Sys.getpid()))
  on.exit(unlink(log_dir, recursive = TRUE), add = TRUE)

  .LOG_STATE$initialized <- FALSE
  .LOG_STATE$run_id <- NULL
  .LOG_STATE$log_dir <- NULL

  initialize_logs("TEST_RUN_002", log_dir = log_dir)
  log_info("PHASE_1", "Test message alpha")
  log_info("PHASE_2", "Test message beta")

  lines <- readLines(file.path(log_dir, "pipeline.log"), warn = FALSE)
  info_lines <- grep("Test message alpha", lines, value = TRUE)
  expect_true(length(info_lines) >= 1)

  info_lines2 <- grep("Test message beta", lines, value = TRUE)
  expect_true(length(info_lines2) >= 1)
})

test_that("log_error writes to both pipeline.log and errors.log", {
  log_dir <- file.path(tempdir(), paste0("test_logs3_", Sys.getpid()))
  on.exit(unlink(log_dir, recursive = TRUE), add = TRUE)

  .LOG_STATE$initialized <- FALSE
  .LOG_STATE$run_id <- NULL
  .LOG_STATE$log_dir <- NULL

  initialize_logs("TEST_RUN_003", log_dir = log_dir)
  suppressMessages(log_error("PHASE_X", "Something went wrong"))

  pipeline_lines <- readLines(file.path(log_dir, "pipeline.log"), warn = FALSE)
  error_lines <- readLines(file.path(log_dir, "errors.log"), warn = FALSE)

  expect_true(any(grepl("Something went wrong", pipeline_lines)))
  expect_true(any(grepl("Something went wrong", error_lines)))
})

test_that("log_warning writes to warnings.log", {
  log_dir <- file.path(tempdir(), paste0("test_logs4_", Sys.getpid()))
  on.exit(unlink(log_dir, recursive = TRUE), add = TRUE)

  .LOG_STATE$initialized <- FALSE
  .LOG_STATE$run_id <- NULL
  .LOG_STATE$log_dir <- NULL

  initialize_logs("TEST_RUN_004", log_dir = log_dir)
  suppressWarnings(log_warning("PHASE_Y", "Minor issue detected"))

  warn_lines <- readLines(file.path(log_dir, "warnings.log"), warn = FALSE)
  expect_true(any(grepl("Minor issue detected", warn_lines)))
})

test_that("initialize_logs is idempotent for same run_id and log_dir", {
  log_dir <- file.path(tempdir(), paste0("test_logs5_", Sys.getpid()))
  on.exit(unlink(log_dir, recursive = TRUE), add = TRUE)

  .LOG_STATE$initialized <- FALSE
  .LOG_STATE$run_id <- NULL
  .LOG_STATE$log_dir <- NULL

  initialize_logs("TEST_RUN_005", log_dir = log_dir)
  log_info("TEST", "First entry")

  # Call again with same params -- should be a no-op
  initialize_logs("TEST_RUN_005", log_dir = log_dir)
  log_info("TEST", "Second entry")

  lines <- readLines(file.path(log_dir, "pipeline.log"), warn = FALSE)
  # Both entries should be present; only one init header
  expect_true(any(grepl("First entry", lines)))
  expect_true(any(grepl("Second entry", lines)))
  init_headers <- grep("^# Run: TEST_RUN_005", lines)
  expect_equal(length(init_headers), 1)
})

test_that("log_info includes run_id in log entry", {
  log_dir <- file.path(tempdir(), paste0("test_logs6_", Sys.getpid()))
  on.exit(unlink(log_dir, recursive = TRUE), add = TRUE)

  .LOG_STATE$initialized <- FALSE
  .LOG_STATE$run_id <- NULL
  .LOG_STATE$log_dir <- NULL

  initialize_logs("MYRUN_XYZ", log_dir = log_dir)
  log_info("CHECK", "run id check")

  lines <- readLines(file.path(log_dir, "pipeline.log"), warn = FALSE)
  matching <- grep("MYRUN_XYZ.*run id check", lines)
  expect_true(length(matching) >= 1)
})

# =============================================================================
# collinearity_diagnostics() — additional coverage
# =============================================================================

test_that("collinearity_diagnostics finds high-correlation pairs", {
  set.seed(70)
  n <- 200
  x <- rnorm(n)
  df <- data.frame(a = x, b = x + rnorm(n, sd = 0.05), c = rnorm(n))
  result <- collinearity_diagnostics(df, c("a", "b", "c"), cor_threshold = 0.7)
  expect_true(length(result$high_cor_pairs) >= 1)
  # The pair should be a-b
  pair_vars <- c(result$high_cor_pairs[[1]]$var1, result$high_cor_pairs[[1]]$var2)
  expect_true(all(c("a", "b") %in% pair_vars))
})

test_that("collinearity_diagnostics reports insufficient data", {
  df <- data.frame(a = 1:5, b = 6:10, c = 11:15)
  result <- collinearity_diagnostics(df, c("a", "b", "c"))
  expect_true(grepl("Insufficient", result$message))
})

# =============================================================================
# Edge cases and integration-like tests
# =============================================================================

test_that("calculate_mess works with data.frames not just matrices", {
  ref <- data.frame(v1 = c(1, 2, 3), v2 = c(4, 5, 6))
  target <- data.frame(v1 = c(2), v2 = c(5))
  scores <- calculate_mess(ref, target)
  expect_equal(scores, 0)
})

test_that("auc_score and tss_best agree on good vs bad model direction", {
  set.seed(80)
  y <- c(rep(0, 100), rep(1, 100))
  good_scores <- c(rnorm(100, 0.3, 0.1), rnorm(100, 0.7, 0.1))
  bad_scores <- runif(200)

  auc_good <- auc_score(y, good_scores)
  auc_bad <- auc_score(y, bad_scores)
  tss_good <- tss_best(y, good_scores)["tss"]
  tss_bad <- tss_best(y, bad_scores)["tss"]

  expect_true(auc_good > auc_bad)
  expect_true(tss_good > tss_bad)
})

test_that("brier_score is consistent with calibration_slope brier", {
  set.seed(81)
  n <- 200
  p <- runif(n)
  y <- rbinom(n, 1, p)
  bs <- brier_score(y, p)
  cal <- calculate_calibration_slope(y, p)
  expect_equal(bs, cal$brier, tolerance = 1e-10)
})
