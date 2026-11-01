# Static and lightweight tests for submission-readiness contracts.

test_that("production configuration locks the analytical contract", {
  cfg <- yaml::read_yaml(repo_path("00_governance", "config.yaml"))
  expect_true(isTRUE(cfg$execution$production_mode))
  expect_false(isTRUE(cfg$execution$skip_existing))
  expect_equal(cfg$execution$max_future_scenarios, 96)
  expect_equal(length(cfg$climate_projections$gcms_contract), 8)
  expect_equal(length(cfg$climate_projections$ssps_contract), 4)
  expect_equal(length(cfg$climate_projections$periods_contract), 3)
  expect_equal(cfg$climate_projections$expected_gcm_scenarios, 96)
  expect_equal(cfg$modelling$ensemble_weighting, "equal")
  expect_true(isTRUE(cfg$modelling$require_complete_oof))
})

test_that("run structure is isolated and refuses reuse", {
  env <- new.env(parent = globalenv())
  source(repo_path("03_analysis", "00_contract_helpers.R"), local = env)
  root <- file.path(tempdir(), paste0("run_contract_", Sys.getpid()))
  run_id <- "RUN_20990101_000000_test"
  paths <- env$create_run_structure(root, run_id)
  expect_equal(basename(paths$run_dir), run_id)
  expect_true(dir.exists(paths$manifest_dir))
  expect_true(dir.exists(paths$intermediate_dir))
  expect_error(env$create_run_structure(root, run_id), "Refusing to reuse")
})

test_that("MaxEnt full-model extraction is not used as OOF prediction", {
  txt <- paste(readLines(repo_path("03_analysis", "07_model_training.R"), warn = FALSE),
               collapse = "\n")
  expect_match(txt, "oof\\$pred_maxent <- NA_real_", fixed = FALSE)
  expect_false(grepl("oof\\$pred_maxent <- as.numeric\\(ex", txt))
})

test_that("ensemble evaluation uses OOF ensemble rather than raster extraction", {
  txt <- paste(readLines(repo_path("03_analysis", "08_model_evaluation.R"), warn = FALSE),
               collapse = "\n")
  expect_match(txt, "ens_all <- as.numeric(oof$pred_ensemble)", fixed = TRUE)
  expect_false(grepl("terra::extract\\(ens_rast", txt))
})

test_that("future summaries use within-GCM contracted ensembles", {
  txt <- paste(readLines(repo_path("03_analysis", "09_future_projections.R"), warn = FALSE),
               collapse = "\n")
  expect_match(txt, "suitability_future_%s_%s_%s_ensemble.tif", fixed = TRUE)
  expect_match(txt, "Expected %d GCM ensemble rasters", fixed = TRUE)
  expect_match(txt, "skip_existing=TRUE is prohibited", fixed = TRUE)
})

test_that("publication helpers provide redundant scenario encodings", {
  env <- new.env(parent = globalenv())
  source(repo_path("03_analysis", "00_publication_helpers.R"), local = env)
  expect_length(env$publication_palette("scenario"), 4)
  expect_length(env$publication_linetypes(), 4)
  expect_length(env$publication_shapes(), 4)
  expect_error(
    env$assert_figure_encoding(data.frame(g = letters[1:3]), "g"),
    "colour alone"
  )
  ev <- data.frame(algorithm = c("glm", "ensemble"), threshold = c(0.2, 0.17))
  expect_equal(env$get_oof_ensemble_threshold(ev), 0.17)
})

test_that("refugia contract excludes historical layers and fixes denominator", {
  txt <- paste(readLines(repo_path("03_analysis", "10_refugia.R"), warn = FALSE),
               collapse = "\n")
  expect_match(txt, "expected_gcm_scenarios", fixed = TRUE)
  expect_match(txt, "source_file", fixed = TRUE)
  expect_match(txt, "Historical and SSP-period mean layers are excluded", fixed = TRUE)
})
