#!/usr/bin/env Rscript
# =============================================================================
# 00_logging.R — Structured Logging Utility
# =============================================================================
# Purpose: Standardized logging across all pipeline modules
# Version: 2.1-ensemble
# =============================================================================

# Global logging state
.LOG_STATE <- new.env(parent = emptyenv())
.LOG_STATE$initialized <- FALSE
.LOG_STATE$run_id <- NULL
.LOG_STATE$log_dir <- NULL
.LOG_STATE$start_time <- NULL

# =============================================================================
# INITIALIZATION
# =============================================================================

#' Initialize logging for a run
#' @param run_id Run identifier
#' @param log_dir Log directory (default: 06_logs)
#' @param config Optional config list
initialize_logs <- function(run_id, log_dir = "06_logs", config = NULL) {
  # Prevent accidental re-initialization from phase modules.
  if (isTRUE(.LOG_STATE$initialized) &&
      identical(.LOG_STATE$run_id, run_id) &&
      identical(normalizePath(.LOG_STATE$log_dir, winslash = "/", mustWork = FALSE),
                normalizePath(log_dir, winslash = "/", mustWork = FALSE))) {
    return(invisible(NULL))
  }

  # Create log directory
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Initialize log files without truncating prior runs.
  pipeline_log <- file.path(log_dir, "pipeline.log")
  errors_log <- file.path(log_dir, "errors.log")
  warnings_log <- file.path(log_dir, "warnings.log")

  init_log_file <- function(path, title) {
    if (!file.exists(path) || isTRUE(file.info(path)$size == 0)) {
      writeLines(c(
        paste0("# ", title),
        paste0("# Run: ", run_id),
        paste0("# Started: ", timestamp),
        paste0("# ", strrep("=", 70)),
        ""
      ), path)
    } else {
      cat(
        paste0(
          "\n# ", strrep("=", 70), "\n",
          "# Run: ", run_id, "\n",
          "# Started: ", timestamp, "\n"
        ),
        file = path,
        append = TRUE,
        sep = ""
      )
    }
  }

  init_log_file(pipeline_log, "PIPELINE LOG")
  init_log_file(errors_log, "ERRORS LOG")
  init_log_file(warnings_log, "WARNINGS LOG")
  
  # Store state
  .LOG_STATE$initialized <<- TRUE
  .LOG_STATE$run_id <<- run_id
  .LOG_STATE$log_dir <<- log_dir
  .LOG_STATE$start_time <<- Sys.time()
  
  log_info("BOOTSTRAP", "Logging initialized")
  
  invisible(NULL)
}

# =============================================================================
# CORE LOGGING
# =============================================================================

#' Log a message
#' @param phase Phase/module name
#' @param message Message to log
#' @param level Log level (INFO, WARNING, ERROR, DEBUG)
#' @param script Optional script name
log_message <- function(phase, message, level = "INFO", script = NULL) {
  if (!.LOG_STATE$initialized) {
    initialize_logs(paste0("RUN_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  }
  
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  run_id <- .LOG_STATE$run_id
  log_dir <- .LOG_STATE$log_dir
  
  # Format log entry
  script_part <- if (!is.null(script)) paste0(script, " | ") else ""
  log_entry <- paste0(timestamp, " | ", run_id, " | ", phase, " | ", 
                      level, " | ", script_part, message)
  
  # Write to pipeline log
  pipeline_log <- file.path(log_dir, "pipeline.log")
  cat(log_entry, "\n", file = pipeline_log, append = TRUE, sep = "")
  
  # Write to level-specific logs
  if (level == "ERROR") {
    errors_log <- file.path(log_dir, "errors.log")
    cat(log_entry, "\n", file = errors_log, append = TRUE, sep = "")
    message(paste0("[ERROR] ", phase, ": ", message))
    
  } else if (level == "WARNING") {
    warnings_log <- file.path(log_dir, "warnings.log")
    cat(log_entry, "\n", file = warnings_log, append = TRUE, sep = "")
    warning(paste0("[WARNING] ", phase, ": ", message), call. = FALSE)
  }
  
  invisible(NULL)
}

# Convenience functions
log_info <- function(phase, message, script = NULL) {
  log_message(phase, message, level = "INFO", script = script)
}

log_warning <- function(phase, message, script = NULL) {
  log_message(phase, message, level = "WARNING", script = script)
}

log_error <- function(phase, message, script = NULL) {
  log_message(phase, message, level = "ERROR", script = script)
}

log_debug <- function(phase, message, script = NULL) {
  log_message(phase, message, level = "DEBUG", script = script)
}

# =============================================================================
# PHASE TRACKING
# =============================================================================

log_phase_start <- function(phase, description = NULL) {
  msg <- paste0("Phase START: ", phase)
  if (!is.null(description)) msg <- paste0(msg, " (", description, ")")
  log_message(phase, msg, level = "INFO")
}

log_phase_end <- function(phase, status = "COMPLETE", duration_min = NULL) {
  msg <- paste0("Phase END: ", phase, " [", status, "]")
  if (!is.null(duration_min)) msg <- paste0(msg, sprintf(" (%.1f min)", duration_min))
  
  level <- if (status == "BLOCKED") "ERROR" else if (status == "WARNING") "WARNING" else "INFO"
  log_message(phase, msg, level = level)
}

# =============================================================================
# SPECIALIZED LOGGING
# =============================================================================

log_validation <- function(check_name, passed, details = NULL) {
  status <- if (passed) "PASS" else "FAIL"
  msg <- paste0("Validation [", check_name, "]: ", status)
  if (!is.null(details)) msg <- paste0(msg, " - ", details)
  level <- if (passed) "INFO" else "ERROR"
  log_message("VALIDATION", msg, level = level)
}

log_performance <- function(algorithm, metrics) {
  msg_parts <- sapply(names(metrics), function(nm) {
    paste0(nm, "=", sprintf("%.3f", metrics[[nm]]))
  })
  msg <- paste0("Performance [", algorithm, "]: ", paste(msg_parts, collapse = ", "))
  log_message("EVALUATION", msg, level = "INFO")
}

log_dropped_predictor <- function(predictor, reason, reference = NULL) {
  if (!is.null(reference)) {
    msg <- paste0("Dropped [", predictor, "]: ", reason, " (correlated with ", reference, ")")
  } else {
    msg <- paste0("Dropped [", predictor, "]: ", reason)
  }
  log_message("PREDICTOR_ENGINE", msg, level = "WARNING")
}

log_gcm_status <- function(gcm, ssp, period, status, details = NULL) {
  msg <- paste0("GCM [", gcm, "] ", ssp, " ", period, ": ", status)
  if (!is.null(details)) msg <- paste0(msg, " - ", details)
  level <- if (status == "SUCCESS") "INFO" else "ERROR"
  log_message("FUTURE_PROJECTIONS", msg, level = level)
}

# =============================================================================
# FINALIZATION
# =============================================================================

finalize_logs <- function() {
  if (!.LOG_STATE$initialized) return(invisible(NULL))
  
  end_time <- Sys.time()
  duration <- difftime(end_time, .LOG_STATE$start_time, units = "secs")
  
  log_dir <- .LOG_STATE$log_dir
  pipeline_log <- file.path(log_dir, "pipeline.log")
  
  # Count entries
  n_info <- if (file.exists(pipeline_log)) {
    sum(grepl(" \\| INFO \\|", readLines(pipeline_log, warn = FALSE)))
  } else 0
  
  n_warnings <- if (file.exists(file.path(log_dir, "warnings.log"))) {
    sum(grepl(" \\| WARNING \\|", readLines(file.path(log_dir, "warnings.log"), warn = FALSE)))
  } else 0
  
  n_errors <- if (file.exists(file.path(log_dir, "errors.log"))) {
    sum(grepl(" \\| ERROR \\|", readLines(file.path(log_dir, "errors.log"), warn = FALSE)))
  } else 0
  
  # Write summary
  summary_msg <- paste0(
    "PIPELINE SUMMARY | Duration: ", round(as.numeric(duration), 1), "s | ",
    "Info: ", n_info, " | Warnings: ", n_warnings, " | Errors: ", n_errors
  )
  
  cat(summary_msg, "\n", file = pipeline_log, append = TRUE, sep = "")
  log_info("BOOTSTRAP", "Logging finalized")
  
  invisible(NULL)
}

# Error handler for tryCatch
error_handler <- function(e, phase = "UNKNOWN") {
  log_error(phase, conditionMessage(e))
  stop(e)
}
