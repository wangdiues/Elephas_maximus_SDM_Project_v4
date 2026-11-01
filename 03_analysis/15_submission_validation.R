#!/usr/bin/env Rscript
# Final scientific and publication-artifact gates. These checks are intentionally
# fail-fast and operate only on artifacts within one immutable run directory.

submission_check <- function(check, passed, detail = "") {
  data.frame(check = check, passed = isTRUE(passed), detail = as.character(detail),
             stringsAsFactors = FALSE)
}

validate_submission_run <- function(run_dir, config_path = "00_governance/config.yaml") {
  run_dir <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)
  cfg <- yaml::read_yaml(config_path)
  checks <- list()
  add <- function(name, ok, detail = "") {
    checks[[length(checks) + 1L]] <<- submission_check(name, ok, detail)
  }

  manifest <- file.path(run_dir, "00_manifest", "run_manifest.json")
  add("run manifest exists", file.exists(manifest), manifest)
  add("run directory is immutable RUN_*", grepl("^RUN_", basename(run_dir)), basename(run_dir))

  oof_file <- file.path(run_dir, "01_processed_data", "oof_predictions.csv")
  contract_file <- file.path(run_dir, "02_models", "ensemble_contract.csv")
  add("OOF predictions exist", file.exists(oof_file), oof_file)
  add("ensemble contract exists", file.exists(contract_file), contract_file)
  if (file.exists(oof_file) && file.exists(contract_file)) {
    oof <- read.csv(oof_file, stringsAsFactors = FALSE)
    contract <- read.csv(contract_file, stringsAsFactors = FALSE)
    included <- contract$algorithm[contract$included]
    valid <- all(vapply(c(included, "ensemble"), function(a) {
      col <- paste0("pred_", a)
      col %in% names(oof) && length(oof[[col]]) == nrow(oof) &&
        all(is.finite(oof[[col]])) && all(oof[[col]] >= 0 & oof[[col]] <= 1)
    }, logical(1)))
    add("complete bounded spatial OOF predictions", valid, paste(included, collapse = ","))
    add("ensemble weights sum to one",
        abs(sum(contract$weight[contract$included]) - 1) < 1e-8,
        as.character(sum(contract$weight[contract$included])))
  }

  if (isTRUE(cfg$execution$run_future_projections)) {
    status_file <- file.path(run_dir, "04_future_projections", "scenario_status.csv")
    add("future scenario status exists", file.exists(status_file), status_file)
    if (file.exists(status_file)) {
      status <- read.csv(status_file, stringsAsFactors = FALSE)
      expected <- as.integer(cfg$climate_projections$expected_gcm_scenarios)
      keys <- paste(status$gcm, status$ssp, status$period, sep = "|")
      add("future scenario count", nrow(status) == expected,
          sprintf("expected=%d observed=%d", expected, nrow(status)))
      add("future scenarios unique", !anyDuplicated(keys), "gcm|ssp|period")
      add("all future scenarios succeeded", all(status$status == "SUCCESS"),
          paste(unique(status$status), collapse = ","))
    }
    refugia_contract_file <- file.path(run_dir, "05_change_metrics", "refugia_contract.csv")
    refugia_manifest_file <- file.path(run_dir, "05_change_metrics", "refugia_scenario_manifest.csv")
    add("refugia contract exists", file.exists(refugia_contract_file), refugia_contract_file)
    add("refugia scenario manifest exists", file.exists(refugia_manifest_file), refugia_manifest_file)
    if (file.exists(refugia_contract_file) && file.exists(refugia_manifest_file)) {
      rc <- read.csv(refugia_contract_file, stringsAsFactors = FALSE)
      rmf <- read.csv(refugia_manifest_file, stringsAsFactors = FALSE)
      expected <- as.integer(cfg$climate_projections$expected_gcm_scenarios)
      add("refugia denominator uses individual GCM projections",
          nrow(rc) == 1 && rc$denominator_type[[1]] == "individual GCM projections" &&
            rc$denominator_n[[1]] == expected && rc$core_minimum[[1]] == expected,
          paste(rc$denominator_type, rc$denominator_n))
      add("refugia scenario count", nrow(rmf) == expected && all(rmf$included),
          sprintf("expected=%d observed=%d", expected, nrow(rmf)))
    }
  }

  table_manifest <- file.path(run_dir, "08_figures_tables", "table_provenance_manifest.csv")
  add("table provenance manifest exists", file.exists(table_manifest), table_manifest)
  if (file.exists(table_manifest)) {
    tables <- read.csv(table_manifest, stringsAsFactors = FALSE)
    table_paths <- file.path(run_dir, "08_figures_tables", tables$table_file)
    add("all manifested tables exist", all(file.exists(table_paths)), "")
    add("table hashes match", all(unname(tools::md5sum(table_paths)) == tables$md5), "")
  }

  figure_manifest <- file.path(run_dir, "08_figures_tables", "figure_provenance_manifest.csv")
  add("figure provenance manifest exists", file.exists(figure_manifest), figure_manifest)
  if (file.exists(figure_manifest)) {
    figures <- read.csv(figure_manifest, stringsAsFactors = FALSE)
    figure_paths <- file.path(run_dir, "08_figures_tables", figures$figure_file)
    add("all manifested figures exist", all(file.exists(figure_paths)), "")
    add("figure hashes match", all(unname(tools::md5sum(figure_paths)) == figures$md5), "")
    png_rows <- figures$format == "png"
    add("PNG dimensions meet minimum", all(figures$short_edge_px[png_rows] >= 1200),
        paste(figures$figure_file[png_rows & figures$short_edge_px < 1200], collapse = ","))
  }
  required_main_figures <- c(
    "main_figure_01_spatial_cv.png",
    "main_figure_02_variable_importance.png",
    "main_figure_03_present_suitability.png",
    "main_figure_04_future_habitat_area.png",
    "main_figure_05_refugia_stability.png"
  )
  missing_main <- required_main_figures[
    !file.exists(file.path(run_dir, "08_figures_tables", required_main_figures))
  ]
  add("required main figures 1-5 exist", length(missing_main) == 0,
      paste(missing_main, collapse = ","))
  if (isTRUE(cfg$interaction_exposure$enabled)) {
    exposure_figure <- file.path(run_dir, "08_figures_tables", "main_figure_06_interaction_exposure_present.png")
    exposure_table <- file.path(run_dir, "08_figures_tables", "table_interaction_exposure_summary.csv")
    add("interaction-exposure figure exists", file.exists(exposure_figure), exposure_figure)
    add("interaction-exposure summary exists", file.exists(exposure_table), exposure_table)
    add("exposure is not labelled as validated risk",
        identical(cfg$interaction_exposure$validated_risk_model, FALSE) &&
          grepl("exposure", cfg$interaction_exposure$claim_level, ignore.case = TRUE),
        cfg$interaction_exposure$claim_level)
  }

  output_csvs <- list.files(run_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  wrong_run <- character()
  for (f in output_csvs) {
    x <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(x) && "run_id" %in% names(x) && any(x$run_id != basename(run_dir), na.rm = TRUE)) {
      wrong_run <- c(wrong_run, f)
    }
  }
  add("CSV run IDs match active run", length(wrong_run) == 0, paste(basename(wrong_run), collapse = ","))

  report <- do.call(rbind, checks)
  report_path <- file.path(run_dir, "00_manifest", "submission_validation.csv")
  write.csv(report, report_path, row.names = FALSE)
  if (any(!report$passed)) {
    failed <- report$check[!report$passed]
    stop("Submission validation failed: ", paste(failed, collapse = "; "))
  }
  invisible(report)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  run_dir <- if (length(args) >= 1) args[[1]] else stop("run_dir argument is required")
  config_path <- if (length(args) >= 2) args[[2]] else "00_governance/config.yaml"
  validate_submission_run(run_dir, config_path)
}
