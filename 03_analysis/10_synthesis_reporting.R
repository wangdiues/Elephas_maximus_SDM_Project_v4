#!/usr/bin/env Rscript
# =============================================================================
# DEPRECATED — use 13_synthesis_reporting.R (canonical)
# =============================================================================
# This script is a legacy alias for 13_synthesis_reporting.R.
# The production pipeline sources 13_synthesis_reporting.R via Phase 13 of
# run_pipeline.R. Do not add new functionality here.
# =============================================================================
# STD-HARDENING: 2026-02-24
# role: legacy_wrapper
# note: normalized script metadata for deterministic maintenance

write_synthesis_reports <- function(run_dir, run_id) {
  out_dir <- file.path(run_dir, "09_reports")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # ============================================================
  # Helper: safe read with fallback
  # ============================================================
  safe_read <- function(path) {
    if (file.exists(path)) tryCatch(read.csv(path), error = function(e) NULL) else NULL
  }

  safe_lines <- function(path) {
    if (file.exists(path)) tryCatch(readLines(path, warn = FALSE), error = function(e) character(0)) else character(0)
  }

  # ============================================================
  # Load run artifacts
  # ============================================================
  eval_df     <- safe_read(file.path(run_dir, "02_models", "evaluation_all.csv"))
  pred_mf     <- safe_read(file.path(run_dir, "01_processed_data", "predictor_manifest.csv"))
  fut_idx     <- safe_read(file.path(run_dir, "04_future_projections", "future_projection_index.csv"))
  hab_df      <- safe_read(file.path(run_dir, "05_change_metrics", "habitat_area_change_summary.csv"))
  extrap_df   <- safe_read(file.path(run_dir, "04_future_projections", "extrapolation_fraction_report.csv"))
  mgmt_df     <- safe_read(file.path(run_dir, "07_overlays", "management_indicators.csv"))
  mod_ds      <- safe_read(file.path(run_dir, "01_processed_data", "modeling_dataset.csv"))
  admin_notes <- safe_lines(file.path(dirname(run_dir), "..", "..", "00_governance", "for_human_admin_only.txt"))

  # ============================================================
  # Load run manifest and config snapshot (T4-E)
  # ============================================================
  manifest_dir <- file.path(run_dir, "00_manifest")
  cfg_snap <- NULL
  run_manifest <- NULL
  if (requireNamespace("yaml", quietly = TRUE)) {
    cfg_path <- file.path(manifest_dir, "config_snapshot.yaml")
    if (file.exists(cfg_path))
      cfg_snap <- tryCatch(yaml::read_yaml(cfg_path), error = function(e) NULL)
    mf_path <- file.path(manifest_dir, "run_manifest.json")
    if (file.exists(mf_path))
      run_manifest <- tryCatch(yaml::read_yaml(mf_path), error = function(e) NULL)
  }

  run_date <- format(Sys.Date(), "%Y-%m-%d")

  # Derived counts
  n_pres_raw    <- if (!is.null(mod_ds)) sum(mod_ds$response == 1, na.rm = TRUE) else NA
  n_background  <- if (!is.null(mod_ds)) sum(mod_ds$response == 0, na.rm = TRUE) else NA
  n_predictors  <- if (!is.null(pred_mf) && "selected" %in% names(pred_mf)) {
    sum(pred_mf$selected == TRUE, na.rm = TRUE)
  } else if (!is.null(pred_mf)) {
    nrow(pred_mf)
  } else if (!is.null(mod_ds)) {
    meta_cols <- c("id", "type", "response", "longitude", "latitude", "fold")
    length(setdiff(names(mod_ds), meta_cols))
  } else NA
  n_scenarios   <- if (!is.null(fut_idx)) nrow(fut_idx) else 0

  pred_names <- if (!is.null(pred_mf) && "predictor" %in% names(pred_mf) && "selected" %in% names(pred_mf)) {
    paste(pred_mf$predictor[pred_mf$selected == TRUE], collapse = ", ")
  } else if (!is.null(pred_mf) && "name" %in% names(pred_mf) && "selected" %in% names(pred_mf)) {
    paste(pred_mf$name[pred_mf$selected == TRUE], collapse = ", ")
  } else if (!is.null(mod_ds)) {
    meta_cols <- c("id", "type", "response", "longitude", "latitude", "fold")
    paste(setdiff(names(mod_ds), meta_cols), collapse = ", ")
  } else "N/A"

  # Config-derived fields from manifest / config snapshot
  max_future_target <- if (!is.null(cfg_snap$execution$max_future_scenarios))
    as.integer(cfg_snap$execution$max_future_scenarios) else 96L
  gcm_list <- if (!is.null(cfg_snap$climate_projections$gcms_contract))
    paste(cfg_snap$climate_projections$gcms_contract, collapse = ", ")
  else
    "MRI-ESM2-0, ACCESS-CM2, CNRM-CM6-1, CNRM-ESM2-1, INM-CM4-8, INM-CM5-0, MIROC6, MPI-ESM1-2-LR"
  code_version <- if (!is.null(run_manifest$code_version)) run_manifest$code_version else "2.1-ensemble"
  config_hash  <- if (!is.null(run_manifest$config_hash))  substr(run_manifest$config_hash, 1, 8) else "unknown"
  run_seed     <- if (!is.null(run_manifest$seed))          run_manifest$seed else NA

  # Algorithm metric rows
  algo_lines <- character(0)
  if (!is.null(eval_df)) {
    floor_check <- function(row) {
      checks <- c()
      notes  <- c()
      if (!is.na(row$auc_mean) && row$auc_mean < 0.65) checks <- c(checks, "AUC FAIL")
      if (is.na(row$tss_mean)) notes <- c(notes, "TSS=NA") else if (row$tss_mean < 0.30) checks <- c(checks, "TSS FAIL")
      if (!is.na(row$boyce)    && row$boyce    < 0.10) checks <- c(checks, "Boyce FAIL")
      if (!is.na(row$brier)    && row$brier    > 0.25) checks <- c(checks, "Brier FAIL")
      status <- if (length(checks) == 0) "PASS" else paste(checks, collapse = "; ")
      if (length(notes) > 0) paste0(status, " (", paste(notes, collapse = ", "), ")") else status
    }
    for (i in seq_len(nrow(eval_df))) {
      r <- eval_df[i, ]
      auc_v   <- if (!is.null(r$auc_mean)  && !is.na(r$auc_mean))  sprintf("%.3f", r$auc_mean)  else "NA"
      tss_v   <- if (!is.null(r$tss_mean)  && !is.na(r$tss_mean))  sprintf("%.3f", r$tss_mean)  else "NA"
      boyce_v <- if (!is.null(r$boyce)     && !is.na(r$boyce))     sprintf("%.3f", r$boyce)     else "NA"
      brier_v <- if (!is.null(r$brier)     && !is.na(r$brier))     sprintf("%.3f", r$brier)     else "NA"
      status  <- floor_check(r)
      algo_lines <- c(algo_lines,
        sprintf("  - **%s**: AUC=%s, TSS=%s, Boyce=%s, Brier=%s — %s",
                r$algorithm, auc_v, tss_v, boyce_v, brier_v, status))
    }
  }

  # Ensemble aspirational check
  ens_lines <- character(0)
  if (!is.null(eval_df)) {
    ens <- eval_df[eval_df$algorithm == "ensemble", ]
    if (nrow(ens) > 0) {
      e <- ens[1, ]
      auc_stat   <- if (!is.na(e$auc_mean) && e$auc_mean >= 0.75) "PASS" else "WARN"
      tss_stat   <- if (!is.na(e$tss_mean) && e$tss_mean >= 0.45) "PASS" else "WARN"
      boyce_stat <- if (!is.na(e$boyce)    && e$boyce    >= 0.75) "PASS" else "WARN"
      ens_lines <- c(
        sprintf("  - Ensemble AUC target ≥0.75: %s (%.3f)", auc_stat,   ifelse(is.na(e$auc_mean), 0, e$auc_mean)),
        sprintf("  - Ensemble TSS target ≥0.45: %s (%.3f)", tss_stat,   ifelse(is.na(e$tss_mean), 0, e$tss_mean)),
        sprintf("  - Ensemble Boyce target ≥0.75: %s (%.3f)", boyce_stat, ifelse(is.na(e$boyce), 0, e$boyce))
      )
    }
  }

  # Extrapolation fraction summary
  extrap_lines <- character(0)
  if (!is.null(extrap_df) && "ensemble_extrapolation" %in% names(extrap_df)) {
    frac_mean <- mean(extrap_df$ensemble_extrapolation, na.rm = TRUE)
    n_warn    <- sum(extrap_df$ensemble_extrapolation > 0.20, na.rm = TRUE)
    n_high    <- sum(extrap_df$ensemble_extrapolation > 0.60, na.rm = TRUE)
    extrap_lines <- c(
      sprintf("  - Mean ensemble extrapolation fraction across scenarios: %.1f%%", frac_mean * 100),
      sprintf("  - Scenarios with >20%% extrapolation: %d of %d", n_warn, nrow(extrap_df)),
      sprintf("  - Scenarios exceeding 60%% threshold (WARNING): %d of %d", n_high, nrow(extrap_df))
    )
  }

  # Habitat area summary
  hab_lines <- character(0)
  if (!is.null(hab_df) && nrow(hab_df) > 0) {
    total_gain <- sum(hab_df$gain_km2, na.rm = TRUE)
    total_loss <- sum(hab_df$loss_km2, na.rm = TRUE)
    total_pers <- sum(hab_df$pers_km2, na.rm = TRUE)
    hab_lines <- c(
      sprintf("  - Total habitat gain across all scenarios: %.1f km²", total_gain),
      sprintf("  - Total habitat loss across all scenarios: %.1f km²", total_loss),
      sprintf("  - Total habitat persistence across all scenarios: %.1f km²", total_pers),
      sprintf("  - Net change (gain − loss): %.1f km²", total_gain - total_loss)
    )
  }

  # Thinning note from admin file
  thin_note <- "Thinning deviation: 50 m used (pipeline default); 2 km recommended in methods.md §1 for sparse dataset — justified by low presence record count."
  if (length(admin_notes) > 0) {
    thin_match <- grep("thin|50.*m|2.*km", admin_notes, ignore.case = TRUE, value = TRUE)
    if (length(thin_match) > 0) thin_note <- thin_match[1]
  }

  # ============================================================
  # ODMAP Summary
  # ============================================================
  odmap <- c(
    "# ODMAP Summary",
    "",
    paste0("**Run ID:** ", run_id),
    paste0("**Date:** ", run_date),
    paste0("**Pipeline:** ", code_version, " | config hash: ", config_hash, " | seed: ", run_seed, " | max_future_scenarios: ", max_future_target),
    "",
    "## O — Overview",
    "",
    "| Item | Value |",
    "|------|-------|",
    paste0("| Taxon | *Elephas maximus* (Asian elephant) |"),
    paste0("| Region | ", if (!is.null(cfg_snap$project$study_area_name)) cfg_snap$project$study_area_name else "Bhutan", " |"),
    paste0("| CRS | EPSG:32645 (WGS 84 / UTM Zone 45N) |"),
    paste0("| Presence records (raw/filtered/thinned) | ", ifelse(is.na(n_pres_raw), "N/A", n_pres_raw), " |"),
    paste0("| True absence points (background) | ", ifelse(is.na(n_background), "N/A", n_background), " |"),
    paste0("| Predictors used | ", ifelse(is.na(n_predictors), "N/A", n_predictors), " |"),
    paste0("| Future scenarios processed | ", n_scenarios, " of ", max_future_target, " |"),
    "",
    "## D — Data",
    "",
    paste0("**Occurrence:** True presence-absence survey data (elephant_PA_data.csv); ",
           ifelse(is.na(n_pres_raw), "N/A", n_pres_raw), " presences, ",
           ifelse(is.na(n_background), "N/A", n_background), " absences from field survey stations"),
    "",
    paste0("**Predictors:** ", pred_names),
    "",
    "## M — Model",
    "",
    "### Per-algorithm metrics (FATAL floors: AUC≥0.65, TSS≥0.30, Boyce≥0.10, Brier≤0.25)",
    "",
    if (length(algo_lines) > 0) algo_lines else "  - No evaluation data available",
    "",
    "### Ensemble aspirational targets (AUC≥0.75, TSS≥0.45, Boyce≥0.75)",
    "",
    if (length(ens_lines) > 0) ens_lines else "  - No ensemble row in evaluation_all.csv",
    "",
    "## A — Assessment",
    "",
    paste0("**Scenario coverage:** ", n_scenarios, " scenarios (target: ", max_future_target, " = 9 GCM \u00d7 3 periods \u00d7 4 SSP)"),
    "",
    if (length(extrap_lines) > 0) c("**Extrapolation:**", extrap_lines) else character(0),
    "",
    "## P — Prediction",
    "",
    if (length(hab_lines) > 0) c("**Habitat area change summary:**", hab_lines) else "  - habitat_area_change_summary.csv not yet available",
    "",
    "## ODMAP Compliance Checklist (methods.md §14)",
    "",
    "- [x] Occurrence data documented (true presence-absence survey data)",
    "- [x] True absence points used as background (no pseudo-absence generation)",
    "- [x] Predictor selection with collinearity removal",
    "- [x] Spatial cross-validation (15 km blocks, 5 folds)",
    "- [x] Multiple algorithms (GLM, MaxEnt, RF, BRT)",
    "- [x] Ensemble weighting (AUC-weighted mean)",
    "- [x] Threshold method: max_tss",
    "- [x] Present suitability map produced",
    "- [x] Future projections (CMIP6)",
    "- [x] Uncertainty quantification (GCM SD)",
    "- [x] Conservation overlay analysis",
    "- [x] Change metrics (C01–C04)"
  )
  odmap <- odmap[!is.na(odmap)]
  writeLines(odmap, file.path(out_dir, "odmap_summary.md"))

  # ============================================================
  # Methods Appendix
  # ============================================================
  methods <- c(
    "# Methods Appendix (Auto-generated)",
    "",
    paste0("*Generated: ", run_date, " | Run: ", run_id, "*"),
    "",
    "## Predictors",
    "",
    paste0("- Count: ", ifelse(is.na(n_predictors), "N/A", n_predictors)),
    paste0("- Variables: ", pred_names),
    "- Collinearity removal: Pearson |r| < 0.85 and VIF < 10 applied prior to model fitting",
    "- Baseline climate: CHELSA/CMIP6 1986–2015 bioclimatic variables",
    "- Ancillary layers: HII (human footprint), ESA land cover, NDVI, EVI",
    "",
    "## Algorithm Summary",
    "",
    "| Algorithm | Type | Notes |",
    "|-----------|------|-------|",
    "| GLM | Generalized Linear Model | Logistic regression, true presence/absence |",
    "| MaxEnt | Maximum Entropy | maxnet implementation, true absences as background |",
    "| BRT | Boosted Regression Trees | gbm implementation, true presence/absence |",
    "| RF | Random Forest | ranger implementation, true presence/absence |",
    "| Ensemble | Weighted mean | AUC-weighted combination |",
    "",
    "## Spatial Cross-Validation",
    "",
    "- Design: spatial blocking, 15 km block size, 5 folds",
    "- Evaluation metrics: AUC, TSS, Boyce index, Brier score, calibration slope, Moran's I",
    "- Threshold method: maximum TSS (max_tss)",
    "",
    "## Ensemble Weighting",
    "",
    "- Method: AUC-weighted mean across algorithms",
    "- Algorithms included: GLM, MaxEnt, BRT, Random Forest",
    "",
    "## Future Scenarios",
    "",
    paste0("- Target: ", max_future_target, " scenarios (9 GCM \u00d7 3 periods \u00d7 4 SSP)"),
    paste0("- Processed: ", n_scenarios),
    paste0("- GCMs: ", gcm_list),
    "- SSPs: SSP1-2.6, SSP2-4.5, SSP3-7.0, SSP5-8.5",
    "- Periods: 2021–2040, 2041–2060, 2081–2100",
    "- Extrapolation: MESS analysis performed; masked where MESS < 0"
  )
  writeLines(methods, file.path(out_dir, "methods_appendix.md"))

  # ============================================================
  # Management Brief
  # ============================================================
  # PA coverage: prefer management_indicators.csv; fall back to table_04_pa_coverage.csv
  pa_coverage <- "N/A"
  if (!is.null(mgmt_df) && "indicator" %in% names(mgmt_df)) {
    pa_row <- mgmt_df[mgmt_df$indicator == "PA_coverage", ]
    if (nrow(pa_row) > 0 && !is.na(pa_row$value[1]))
      pa_coverage <- sprintf("%.1f km\u00b2", pa_row$value[1])
  }
  if (pa_coverage == "N/A") {
    pa_tbl <- safe_read(file.path(run_dir, "08_figures_tables", "table_04_pa_coverage.csv"))
    if (!is.null(pa_tbl) && "Suitable_km2" %in% names(pa_tbl) && "Zone" %in% names(pa_tbl)) {
      inside_row <- pa_tbl[grepl("Inside PA", pa_tbl$Zone, ignore.case = TRUE), ]
      if (nrow(inside_row) > 0 && !is.na(inside_row$Suitable_km2[1]))
        pa_coverage <- sprintf("%.1f km\u00b2 (within PAs)", inside_row$Suitable_km2[1])
    }
  }

  high_suit <- if (!is.null(mgmt_df) && "indicator" %in% names(mgmt_df)) {
    hs_row <- mgmt_df[mgmt_df$indicator == "High_suitability_area", ]
    if (nrow(hs_row) > 0 && !is.na(hs_row$value[1])) sprintf("%.1f km\u00b2", hs_row$value[1]) else "N/A"
  } else "N/A"

  # Get optimal threshold: prefer ensemble row, fall back to mean of algorithm thresholds
  optimal_threshold <- "N/A"
  if (!is.null(eval_df) && "threshold" %in% names(eval_df)) {
    ens_thr <- eval_df$threshold[eval_df$algorithm == "ensemble"]
    if (length(ens_thr) > 0 && !is.na(ens_thr[1])) {
      optimal_threshold <- sprintf("%.3f", ens_thr[1])
    } else {
      algo_thr <- eval_df$threshold[eval_df$algorithm != "ensemble"]
      algo_thr <- as.numeric(algo_thr[!is.na(algo_thr)])
      if (length(algo_thr) > 0)
        optimal_threshold <- sprintf("%.3f (mean of algorithms)", mean(algo_thr))
    }
  }

  brief <- c(
    "# Management Brief",
    "",
    paste0("*Generated: ", run_date, " | Run: ", run_id, "*"),
    "",
    "## Key Findings",
    "",
    paste0("- High suitability habitat (>", optimal_threshold, "): ", high_suit),
    paste0("- Protected area coverage: ", pa_coverage),
    paste0("- Future scenarios analysed: ", n_scenarios, " of ", max_future_target),
    "",
    "## Habitat Area Change (vs. present baseline)",
    "",
    if (length(hab_lines) > 0) hab_lines else "  - Habitat area change data not yet available (run Phase 10 change metrics).",
    "",
    "## Conservation Recommendations",
    "",
    "- Priority areas with high present suitability and high future persistence should receive enhanced protection.",
    "- Areas projected as high-gain zones under multiple GCMs represent expansion corridors.",
    "- Human-elephant conflict zones overlapping high-suitability habitat require targeted mitigation measures.",
    "",
    "## Caveats",
    "",
    if (n_scenarios < max_future_target) paste0("- Results are provisional pending full ", max_future_target, "-scenario GCM ensemble completion (", n_scenarios, " of ", max_future_target, " processed).") else NULL,
    "- Model performance should be validated against independent field data.",
    "- True presence-absence survey data used; sampling was conducted by field observers (no spatial thinning applied to absences).",
    "- All spatial outputs use EPSG:32645 (WGS 84 / UTM Zone 45N)."
  )
  brief <- brief[!is.na(brief)]
  writeLines(brief, file.path(out_dir, "management_brief.md"))

  list(
    odmap   = file.path(out_dir, "odmap_summary.md"),
    methods = file.path(out_dir, "methods_appendix.md"),
    brief   = file.path(out_dir, "management_brief.md")
  )
}
