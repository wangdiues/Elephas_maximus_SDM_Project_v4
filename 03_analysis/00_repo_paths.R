current_script_path <- function() {
  cmd_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(cmd_file) > 0) {
    return(normalizePath(sub("^--file=", "", cmd_file[1]), winslash = "/", mustWork = FALSE))
  }

  frame_files <- vapply(
    sys.frames(),
    function(env) {
      ofile <- env$ofile
      if (is.null(ofile)) "" else as.character(ofile)
    },
    character(1)
  )
  frame_files <- frame_files[nzchar(frame_files)]
  if (length(frame_files) > 0) {
    return(normalizePath(frame_files[[length(frame_files)]], winslash = "/", mustWork = FALSE))
  }

  NULL
}

find_repo_root <- function(start = NULL) {
  markers <- c(
    file.path("00_governance", "config.yaml"),
    file.path("03_analysis", "run_pipeline.R")
  )

  candidates <- unique(Filter(Negate(is.null), c(start, dirname(current_script_path()), getwd())))
  for (candidate in candidates) {
    cur <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
    repeat {
      if (all(vapply(markers, function(marker) file.exists(file.path(cur, marker)), logical(1)))) {
        return(cur)
      }
      parent <- dirname(cur)
      if (identical(parent, cur)) break
      cur <- parent
    }
  }

  stop("Could not locate repository root from the current script or working directory.")
}

repo_path <- function(..., repo_root = find_repo_root()) {
  file.path(repo_root, ...)
}

latest_run_dir <- function(repo_root = find_repo_root()) {
  runs_root <- repo_path("04_outputs", "runs", repo_root = repo_root)
  dirs <- list.dirs(runs_root, recursive = FALSE, full.names = TRUE)
  if (length(dirs) == 0) {
    stop(sprintf("No run directories found under %s", runs_root))
  }
  normalizePath(dirs[[which.max(file.info(dirs)$mtime)]], winslash = "/", mustWork = TRUE)
}

resolve_run_dir <- function(run = NULL, repo_root = find_repo_root()) {
  if (is.null(run) || !nzchar(run)) {
    return(latest_run_dir(repo_root = repo_root))
  }

  if (dir.exists(run)) {
    return(normalizePath(run, winslash = "/", mustWork = TRUE))
  }

  candidate <- repo_path("04_outputs", "runs", run, repo_root = repo_root)
  if (dir.exists(candidate)) {
    return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  }

  stop(sprintf("Run directory not found: %s", run))
}
