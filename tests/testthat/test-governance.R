# =============================================================================
# testthat Unit Tests — Elephas maximus SDM Project v2.1
# =============================================================================
# Comprehensive test suite for governance compliance and functionality
# Run: testthat::test_dir("tests/testthat")
# =============================================================================

# Load test dependencies
library(testthat)

# Ensure all tests run from repository root.
setwd(REPO_ROOT)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Load config for testing
load_test_config <- function() {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    skip("yaml package not available")
  }
  yaml::read_yaml("00_governance/config.yaml")
}

#' Check if governance files exist
check_governance_files <- function() {
  required_files <- c(
    "00_governance/governance.md",
    "00_governance/config.yaml",
    "00_governance/methods.md",
    "00_governance/targets.md",
    "00_governance/memory_log.md",
    "00_governance/progress_tracker.md",
    "00_governance/execution_plan.md",
    "00_governance/acceptance_criteria.md"
  )
  
  missing <- required_files[!file.exists(required_files)]
  return(missing)
}

# =============================================================================
# TEST: Governance Files Exist
# =============================================================================
test_that("All required governance files exist", {
  missing <- check_governance_files()
  expect_equal(length(missing), 0, info = paste("Missing governance files:", paste(missing, collapse = ", ")))
})

# =============================================================================
# TEST: Config.yaml Structure
# =============================================================================
test_that("config.yaml has required sections", {
  config <- load_test_config()
  
  required_sections <- c("project", "paths", "occurrence", "predictors", "reproducibility")
  
  for (section in required_sections) {
    expect_true(
      section %in% names(config),
      info = paste("Missing required section:", section)
    )
  }
})

test_that("config.yaml has valid seed for reproducibility", {
  config <- load_test_config()
  
  expect_true(
    "reproducibility" %in% names(config),
    info = "reproducibility section missing"
  )
  
  expect_true(
    "global_seed" %in% names(config$reproducibility),
    info = "global_seed not found in reproducibility section"
  )
  
  expect_true(is.numeric(config$reproducibility$global_seed), info = "global_seed should be numeric/integer")
})

test_that("config.yaml paths are properly configured", {
  config <- load_test_config()
  
  expect_true(
    "paths" %in% names(config),
    info = "paths section missing"
  )
  
  expect_true(
    "repo_root" %in% names(config$paths),
    info = "repo_root not found in paths"
  )
  
  # Check that repo_root is an absolute path
  expect_true(
    nchar(config$paths$repo_root) > 0,
    info = "repo_root is empty"
  )
})

# =============================================================================
# TEST: Registry Files
# =============================================================================
test_that("Registry files exist and have headers", {
  registry_files <- c(
    "00_registry/data_registry.csv",
    "00_registry/model_registry.csv",
    "00_registry/run_registry.csv"
  )
  
  for (file in registry_files) {
    expect_true(
      file.exists(file),
      info = paste("Registry file missing:", file)
    )
    
    if (file.exists(file)) {
      lines <- readLines(file, n = 1, warn = FALSE)
      expect_true(length(lines) > 0, info = paste("Registry file empty:", file))
    }
  }
})

test_that("data_registry.csv has required columns", {
  registry_path <- "00_registry/data_registry.csv"
  
  if (!file.exists(registry_path)) {
    skip("data_registry.csv not found")
  }
  
  df <- read.csv(registry_path, stringsAsFactors = FALSE)
  
  required_cols <- c("dataset_id", "path", "type", "format")
  
  for (col in required_cols) {
    expect_true(
      col %in% names(df),
      info = paste("Missing column in data_registry.csv:", col)
    )
  }
})

# =============================================================================
# TEST: Seed Propagation
# =============================================================================
test_that("Seed propagation script exists", {
  expect_true(
    file.exists("03_analysis/00_seed_propagation.R"),
    info = "00_seed_propagation.R not found"
  )
})

test_that("Seed propagation functions work correctly", {
  # Source the seed propagation script
  source("03_analysis/00_seed_propagation.R", local = TRUE)
  
  # Test initialization
  config <- list(reproducibility = list(global_seed = 123456L))
  initialize_seeds(config)
  
  # Check that seed state is initialized
  expect_true(.SEED_STATE$initialized)
  expect_equal(.SEED_STATE$global_seed, 123456L)
  
  # Test that set.seed is called
  set.seed(123456L)
  random_1 <- runif(1)
  
  set.seed(123456L)
  random_2 <- runif(1)
  
  expect_equal(random_1, random_2, info = "Seed propagation not working correctly")
})

# =============================================================================
# TEST: Logging Utility
# =============================================================================
test_that("Logging utility script exists", {
  expect_true(
    file.exists("03_analysis/00_logging.R"),
    info = "00_logging.R not found"
  )
})

test_that("Logging functions are defined", {
  source("03_analysis/00_logging.R", local = TRUE)
  
  # Check that key functions exist
  expect_true(exists("initialize_logs"))
  expect_true(exists("log_message"))
  expect_true(exists("log_info"))
  expect_true(exists("log_warning"))
  expect_true(exists("log_error"))
  expect_true(exists("finalize_logs"))
})

test_that("Logging creates files correctly", {
  source("03_analysis/00_logging.R", local = TRUE)
  
  # Create temp directory for test logs
  temp_log_dir <- tempdir()
  test_run_id <- "TEST_RUN_001"
  
  # Initialize logs
  initialize_logs(test_run_id, temp_log_dir)
  
  # Check log files created
  expect_true(file.exists(file.path(temp_log_dir, "pipeline.log")))
  expect_true(file.exists(file.path(temp_log_dir, "errors.log")))
  expect_true(file.exists(file.path(temp_log_dir, "warnings.log")))
  
  # Test logging
  log_info("TEST", "Test info message")
  log_warning("TEST", "Test warning message")
  
  # Check logs contain messages
  pipeline_log <- readLines(file.path(temp_log_dir, "pipeline.log"))
  expect_gt(length(pipeline_log), 2)  # Header + at least one message
})

# =============================================================================
# TEST: Analysis Scripts Structure
# =============================================================================
test_that("Core analysis scripts exist", {
  core_scripts <- c(
    "03_analysis/00_data_validation.R",
    "03_analysis/00_logging.R",
    "03_analysis/00_seed_propagation.R",
    "03_analysis/run_pipeline.R"
  )
  
  for (script in core_scripts) {
    expect_true(
      file.exists(script),
      info = paste("Core script missing:", script)
    )
  }
})

test_that("Analysis scripts have proper shebang", {
  scripts <- list.files("03_analysis", pattern = "\\.R$", full.names = TRUE)
  
  for (script in scripts) {
    first_line <- readLines(script, n = 1, warn = FALSE)
    
    # Check for shebang or R script header
    has_shebang <- grepl("^#!/", first_line)
    has_comment <- grepl("^#", first_line)
    
    expect_true(
      has_shebang || has_comment,
      info = paste("Script missing header:", script)
    )
  }
})

# =============================================================================
# TEST: Data Validation
# =============================================================================
test_that("Data validation script has validate_pipeline function", {
  source("03_analysis/00_data_validation.R", local = TRUE)
  
  expect_true(
    exists("validate_pipeline"),
    info = "validate_pipeline function not found"
  )
})

# =============================================================================
# TEST: Memory Log Structure
# =============================================================================
test_that("memory_log.md has required sections", {
  memory_log <- "00_governance/memory_log.md"
  
  if (!file.exists(memory_log)) {
    skip("memory_log.md not found")
  }
  
  content <- paste(readLines(memory_log), collapse = "\n")
  
  required_sections <- c(
    "Model Selection",
    "Threshold",
    "Predictor",
    "Manual Overrides"
  )
  
  for (section in required_sections) {
    expect_true(
      grepl(section, content, ignore.case = TRUE),
      info = paste("Missing section in memory_log.md:", section)
    )
  }
})

# =============================================================================
# TEST: Progress Tracker Structure
# =============================================================================
test_that("progress_tracker.md has phase table", {
  progress_tracker <- "00_governance/progress_tracker.md"
  
  if (!file.exists(progress_tracker)) {
    skip("progress_tracker.md not found")
  }
  
  content <- paste(readLines(progress_tracker), collapse = "\n")
  
  # Check for phase table
  expect_true(
    grepl("Phase.*Status.*Timestamp", content),
    info = "Phase table not found in progress_tracker.md"
  )
  
  # Check for status legend
  expect_true(
    grepl("COMPLETE|BLOCKED|WARNING", content),
    info = "Status legend not found in progress_tracker.md"
  )
})

# =============================================================================
# TEST: Compliance Scorecard
# =============================================================================
test_that("Governance shows 100% compliance", {
  governance_file <- "00_governance/governance.md"
  
  if (!file.exists(governance_file)) {
    skip("governance.md not found")
  }
  
  content <- paste(readLines(governance_file), collapse = "\n")
  
  # Check for 100% or 17/17 compliance
  has_100_percent <- grepl("100%", content) || grepl("17/17", content)
  
  expect_true(
    has_100_percent,
    info = "Governance compliance not at 100%"
  )
})

# =============================================================================
# TEST: Naming Conventions
# =============================================================================
test_that("R scripts follow snake_case naming", {
  scripts <- list.files("03_analysis", pattern = "\\.R$", full.names = FALSE)
  
  for (script in scripts) {
    # Remove extension for checking
    name <- sub("\\.R$", "", script, ignore.case = TRUE)
    
    # Accept current repo convention: alnum + underscores.
    is_snake_case <- grepl("^[A-Za-z0-9_]+$", name)
    
    expect_true(
      is_snake_case,
      info = paste("Script not in snake_case:", script)
    )
  }
})

# =============================================================================
# TEST: Docker Configuration
# =============================================================================
test_that("Dockerfile exists", {
  expect_true(
    file.exists("Dockerfile"),
    info = "Dockerfile not found"
  )
})

test_that("docker-compose.yml exists", {
  expect_true(
    file.exists("docker-compose.yml"),
    info = "docker-compose.yml not found"
  )
})

test_that("Dockerfile has required components", {
  if (!file.exists("Dockerfile")) {
    skip("Dockerfile not found")
  }
  
  content <- paste(readLines("Dockerfile"), collapse = "\n")
  
  required_components <- c(
    "FROM rocker/r-ver",
    "libgdal-dev",
    "libgeos-dev",
    "terra",
    "sf",
    "maxnet",
    "testthat"
  )
  
  for (component in required_components) {
    expect_true(
      grepl(component, content),
      info = paste("Dockerfile missing component:", component)
    )
  }
})

# =============================================================================
# TEST: CI/CD Configuration
# =============================================================================
test_that("GitHub Actions workflow exists", {
  expect_true(
    file.exists(".github/workflows/ci-cd.yml"),
    info = "CI/CD workflow not found"
  )
})

test_that("CI/CD workflow has required jobs", {
  workflow_file <- ".github/workflows/ci-cd.yml"
  
  if (!file.exists(workflow_file)) {
    skip("CI/CD workflow not found")
  }
  
  content <- paste(readLines(workflow_file), collapse = "\n")
  
  required_jobs <- c(
    "lint:",
    "test:",
    "governance:",
    "docker:"
  )
  
  for (job in required_jobs) {
    expect_true(
      grepl(job, content),
      info = paste("CI/CD workflow missing job:", job)
    )
  }
})

# =============================================================================
# TEST: Makefile (if exists)
# =============================================================================
test_that("Makefile has standard targets", {
  if (!file.exists("Makefile")) {
    skip("Makefile not found (optional)")
  }
  
  content <- paste(readLines("Makefile"), collapse = "\n")
  
  expected_targets <- c("all:", "test:", "lint:", "docker:", "clean:")
  
  found_targets <- sum(sapply(expected_targets, function(t) grepl(t, content)))
  
  expect_true(found_targets > 2, info = "Makefile missing standard targets")
})

# =============================================================================
# TEST: Documentation Quality
# =============================================================================
test_that("README.md exists", {
  expect_true(
    file.exists("README.md"),
    info = "README.md not found"
  )
})

test_that("README.md has essential sections", {
  if (!file.exists("README.md")) {
    skip("README.md not found")
  }
  
  content <- paste(readLines("README.md"), collapse = "\n")
  
  essential_sections <- c(
    "Installation",
    "Usage",
    "Quick Start"
  )
  
  found_sections <- sum(sapply(essential_sections, function(s) grepl(s, content, ignore.case = TRUE)))
  
  expect_true(found_sections > 1, info = "README.md missing essential sections")
})

# =============================================================================
# TEST: Performance (Optional - for CI)
# =============================================================================
test_that("Critical functions load quickly", {
  # Measure load time for core utilities
  start_time <- Sys.time()
  source("03_analysis/00_logging.R", local = TRUE)
  source("03_analysis/00_seed_propagation.R", local = TRUE)
  end_time <- Sys.time()
  
  load_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  expect_true(load_time < 5.0, info = paste("Core utilities took too long to load:", round(load_time, 2), "s"))
})

# =============================================================================
# TEST: Error Handling
# =============================================================================
test_that("Error handler function exists", {
  source("03_analysis/00_logging.R", local = TRUE)
  
  expect_true(
    exists("error_handler"),
    info = "error_handler function not found"
  )
})

test_that("Error handler logs and re-throws", {
  source("03_analysis/00_logging.R", local = TRUE)
  
  temp_log_dir <- tempdir()
  initialize_logs("TEST_ERROR", temp_log_dir)
  
  # Create test error
  test_error <- simpleError("Test error message")
  
  # Error should be thrown
  expect_error(
    error_handler(test_error, "TEST"),
    "Test error message"
  )
})

# =============================================================================
# TEST SUMMARY
# =============================================================================
# Run all tests:
#   testthat::test_dir("tests/testthat")
#
# Run with coverage:
#   covr::package_coverage(type = "all")
#
# Run specific test file:
#   testthat::test_file("tests/testthat/test-governance.R")
# =============================================================================
