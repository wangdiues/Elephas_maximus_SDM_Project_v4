library(terra)

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
RUN <- resolve_run_dir(if (length(args) >= 1) args[[1]] else NULL, repo_root = repo_root)

r_pres <- rast(file.path(RUN,"03_present_suitability/suitability_present_ensemble.tif"))
v_pres <- values(r_pres, na.rm=TRUE)
cat("PRESENT min=",round(min(v_pres),4)," max=",round(max(v_pres),4),
    " mean=",round(mean(v_pres),4)," p50=",round(quantile(v_pres,0.5),4),
    " p90=",round(quantile(v_pres,0.9),4),"\n")
cat("PRESENT non-NA:", length(v_pres), "\n")
cat("PRESENT extent:", paste(round(as.vector(ext(r_pres)),2), collapse=" "), "\n")
cat("PRESENT CRS:", crs(r_pres, describe=TRUE)[["name"]], "\n")
cat("PRESENT res:", paste(round(res(r_pres),1), collapse="x"), "\n\n")

r_fut <- rast(file.path(RUN,"04_future_projections/suitability_future_acces_cm2_ssp245_2071_2100_glm.tif"))
v_fut <- values(r_fut, na.rm=TRUE)
cat("FUTURE GLM min=",round(min(v_fut),4)," max=",round(max(v_fut),4),
    " mean=",round(mean(v_fut),4)," p90=",round(quantile(v_fut,0.9),4),"\n")
cat("FUTURE extent:", paste(round(as.vector(ext(r_fut)),2), collapse=" "), "\n")
cat("FUTURE CRS:", crs(r_fut, describe=TRUE)[["name"]], "\n\n")

# Bhutan bbox in WGS84
r_wgs <- project(r_pres, "EPSG:4326")
cat("WGS84 extent:", paste(round(as.vector(ext(r_wgs)),4), collapse=" "), "\n")
cat("  (xmin xmax ymin ymax)\n")
