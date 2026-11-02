#!/usr/bin/env Rscript
# 20_sensitivity_comparison.R — present-day map agreement between the accepted
# run and a sensitivity run. Writes <out_dir>/map_agreement.csv (key,value).
#
# Usage (PowerShell, repo root):
#   Rscript 03_analysis/20_sensitivity_comparison.R <primary_run> <sensitivity_run> <out_dir>

suppressPackageStartupMessages(library(terra))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) stop("primary_run, sensitivity_run and out_dir are required")
runs <- normalizePath(args[1:2], winslash = "/", mustWork = TRUE)
out_dir <- args[[3]]
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

thr <- vapply(runs, function(r) {
  ev <- read.csv(file.path(r, "02_models", "evaluation_all.csv"))
  ev$threshold[ev$algorithm == "ensemble"]
}, numeric(1))
v <- lapply(runs, function(r) values(rast(file.path(r, "03_present_suitability", "suitability_present_ensemble.tif")))[, 1])
ok <- is.finite(v[[1]]) & is.finite(v[[2]])
a <- v[[1]][ok]; b <- v[[2]][ok]
A <- a >= thr[[1]]; B <- b >= thr[[2]]
cell_km2 <- prod(res(rast(file.path(runs[[1]], "03_present_suitability", "suitability_present_ensemble.tif")))) / 1e6

out <- data.frame(
  key = c("map_pearson_r", "map_spearman_rho", "map_schoener_d", "map_jaccard",
          "map_shared_km2", "map_primary_only_km2", "map_sensitivity_only_km2"),
  value = c(round(cor(a, b), 3), round(cor(a, b, method = "spearman"), 3),
            round(1 - 0.5 * sum(abs(a / sum(a) - b / sum(b))), 3),
            round(sum(A & B) / sum(A | B), 3),
            round(sum(A & B) * cell_km2), round(sum(A & !B) * cell_km2), round(sum(B & !A) * cell_km2)),
  primary_run = basename(runs[[1]]), sensitivity_run = basename(runs[[2]])
)
write.csv(out, file.path(out_dir, "map_agreement.csv"), row.names = FALSE)
print(out[, 1:2])
