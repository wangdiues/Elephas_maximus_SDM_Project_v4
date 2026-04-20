library(terra)
library(sf)

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_file) == 0) {
  frame_files <- vapply(sys.frames(), function(env) if (is.null(env$ofile)) "" else as.character(env$ofile), character(1))
  frame_files <- frame_files[nzchar(frame_files)]
  if (length(frame_files) > 0) script_file <- frame_files[[length(frame_files)]]
}
script_dir <- if (length(script_file) > 0) dirname(normalizePath(sub("^--file=", "", script_file[1]), winslash = "/", mustWork = FALSE)) else normalizePath("03_analysis", winslash = "/", mustWork = FALSE)
source(file.path(script_dir, "00_repo_paths.R"))
repo_root <- find_repo_root()
args <- commandArgs(trailingOnly = TRUE)
run_dir <- resolve_run_dir(if (length(args) >= 1) args[[1]] else NULL, repo_root = repo_root)

cat("=== Present suitability rasters ===\n")
suit_files <- list.files(file.path(run_dir, "03_present_suitability"),
                         pattern = "suitability_present.*\\.tif$", full.names = TRUE)
for (f in suit_files) {
  r <- rast(f)
  v <- values(r, na.rm = TRUE)
  cat(sprintf("  %s: n_valid=%d range=[%.4f,%.4f] mean=%.4f\n",
    basename(f), length(v),
    ifelse(length(v) > 0, min(v), NA),
    ifelse(length(v) > 0, max(v), NA),
    ifelse(length(v) > 0, mean(v), NA)))
}

cat("\n=== Template grid ===\n")
tpl <- rast(file.path(run_dir, "02_data_intermediate/template_grid.tif"))
tv <- values(tpl, na.rm = TRUE)
cat(sprintf("  dims: %d x %d, n_valid=%d, range=[%.1f,%.1f]\n",
    ncol(tpl), nrow(tpl), length(tv), min(tv), max(tv)))
cat(sprintf("  extent: xmin=%.1f xmax=%.1f ymin=%.1f ymax=%.1f\n",
    xmin(tpl), xmax(tpl), ymin(tpl), ymax(tpl)))
cat(sprintf("  CRS: %s\n", crs(tpl, describe = TRUE)$code))

cat("\n=== AOI vector ===\n")
aoi <- st_read(file.path(run_dir, "02_data_intermediate/m_area_vector.gpkg"), quiet = TRUE)
bb <- st_bbox(aoi)
cat(sprintf("  CRS EPSG: %s\n", st_crs(aoi)$epsg))
cat(sprintf("  bbox: xmin=%.1f xmax=%.1f ymin=%.1f ymax=%.1f\n", bb[1], bb[3], bb[2], bb[4]))

cat("\n=== M mask ===\n")
mm <- rast(file.path(run_dir, "02_data_intermediate/m_mask.tif"))
mv <- values(mm, na.rm = TRUE)
cat(sprintf("  n_valid=%d, unique values: %s\n", length(mv), paste(unique(mv), collapse = ",")))

cat("\n=== Future projection sample ===\n")
fut_files <- list.files(file.path(run_dir, "04_future_projections"),
                        pattern = "\\.tif$", full.names = TRUE)
cat(sprintf("  Total future rasters: %d\n", length(fut_files)))
if (length(fut_files) > 0) {
  r <- rast(fut_files[1])
  v <- values(r, na.rm = TRUE)
  cat(sprintf("  Sample %s: n_valid=%d range=[%.4f,%.4f]\n",
    basename(fut_files[1]), length(v),
    ifelse(length(v) > 0, min(v), NA),
    ifelse(length(v) > 0, max(v), NA)))
}
