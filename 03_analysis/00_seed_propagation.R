# STD-HARDENING: 2026-02-24
# role: phase_or_helper
# note: normalized script metadata for deterministic maintenance
# =============================================================================
# 00_seed_propagation.R — Seed Management for Elephas maximus SDM Project
# =============================================================================
# Purpose: Ensure deterministic, reproducible execution across all stochastic modules
# Governance Compliance: Section 2.2 (Determinism & Reproducibility)
# =============================================================================

# Global seed state (initialized per run)
.SEED_STATE <- new.env(parent = emptyenv())
.SEED_STATE$initialized <- FALSE
.SEED_STATE$global_seed <- NULL
.SEED_STATE$module_seeds <- list()

# =============================================================================
# Initialize Seed State from Config
# =============================================================================
#' Initialize seed state from config.yaml
#'
#' @param config Loaded config list (from yaml::read_yaml)
#' @return Invisible NULL
#'
initialize_seeds <- function(config) {
  
  # Extract global seed
  global_seed <- 123456  # default
  if (!is.null(config$reproducibility)) {
    if (!is.null(config$reproducibility$global_seed)) {
      global_seed <- config$reproducibility$global_seed
    }
  }
  
  # Extract per-module seeds
  module_seeds <- list()
  if (!is.null(config$reproducibility$per_module_seeds)) {
    module_seeds <- config$reproducibility$per_module_seeds
  }
  
  # Store state
  .SEED_STATE$initialized <- TRUE
  .SEED_STATE$global_seed <- global_seed
  .SEED_STATE$module_seeds <- module_seeds
  
  # Set global seed immediately
  set.seed(global_seed)
  
  invisible(NULL)
}

# =============================================================================
# Set Seed for Specific Module
# =============================================================================
#' Set seed for a specific module
#'
#' @param module_name Name of module (e.g., "background_sampling", "spatial_cv")
#' @return Invisible NULL
#'
set_module_seed <- function(module_name) {
  
  if (!.SEED_STATE$initialized) {
    # Auto-initialize with default if not initialized
    initialize_seeds(list(reproducibility = list(global_seed = 123456)))
  }
  
  # Check for module-specific seed
  module_seeds <- .SEED_STATE$module_seeds
  if (!is.null(module_seeds[[module_name]])) {
    set.seed(module_seeds[[module_name]])
    return(invisible(module_seeds[[module_name]]))
  }
  
  # Fall back to global seed
  set.seed(.SEED_STATE$global_seed)
  return(invisible(.SEED_STATE$global_seed))
}

# =============================================================================
# Get Current Seed State (for logging)
# =============================================================================
#' Get current seed state for manifest logging
#'
#' @return List with global_seed, module_seeds, and timestamp
#'
get_seed_state <- function() {
  return(list(
    global_seed = .SEED_STATE$global_seed,
    module_seeds = .SEED_STATE$module_seeds,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ))
}

# =============================================================================
# Convenience Functions for Common Modules
# =============================================================================

set_background_seed <- function() set_module_seed("background_sampling")
set_cv_seed <- function() set_module_seed("spatial_cv")
set_training_seed <- function() set_module_seed("model_training")
set_projection_seed <- function() set_module_seed("future_projections")
set_uncertainty_seed <- function() set_module_seed("uncertainty")

# =============================================================================
# Usage Example
# =============================================================================
# # At start of pipeline (after loading config):
# config <- yaml::read_yaml("00_governance/config.yaml")
# initialize_seeds(config)
#
# # In background sampling module:
# set_background_seed()  # or set_module_seed("background_sampling")
# # ... stochastic operations (kernel density, random sampling)
#
# # In spatial CV module:
# set_cv_seed()
# # ... stochastic operations (block generation, fold assignment)
#
# # In model training:
# set_training_seed()
# # ... stochastic operations (model fitting, cross-validation)
#
# # For manifest logging:
# seed_info <- get_seed_state()
# # Include in run_manifest.json

