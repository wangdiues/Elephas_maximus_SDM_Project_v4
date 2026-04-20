# Helper file for testthat tests
# Loaded automatically before tests run

# Set consistent seed for reproducible tests
set.seed(123456)

# Resolve repository root robustly regardless of testthat working directory.
find_repo_root <- function(start = getwd()) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in 0:8) {
    cand <- if (i == 0) d else normalizePath(file.path(d, paste(rep("..", i), collapse = "/")), winslash = "/", mustWork = FALSE)
    if (file.exists(file.path(cand, "DESCRIPTION")) && dir.exists(file.path(cand, "03_analysis"))) {
      return(cand)
    }
  }
  stop("Could not locate repository root from: ", start)
}

REPO_ROOT <- find_repo_root()

repo_path <- function(...) {
  file.path(REPO_ROOT, ...)
}

# Test configuration
TEST_CONFIG <- list(
  global_seed = 123456L,
  test_mode = TRUE
)
