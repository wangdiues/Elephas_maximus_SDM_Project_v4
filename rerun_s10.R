setwd("E:/Elephas_maximus_SDM_Project_v4")
source("03_analysis/00_contract_helpers.R")
source("03_analysis/10_figures_supplementary.R")
latest <- find_latest_run_dir(normalizePath(".", winslash = "/"))
run_id <- read_run_id_from_manifest(latest)
cat("Run dir:", latest, "\n")

# Run only S10 by temporarily wrapping — just call the full function which is fast
# (S10 is cheap: just MESS rasters averaging)
n <- create_supplementary_figures(latest, run_id)
cat("Done:", n, "figures\n")
