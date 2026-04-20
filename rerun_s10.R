setwd("E:/Elephas_maximus_SDM_Project_v4")
source("03_analysis/10_figures_supplementary.R")
latest <- tail(sort(list.files("04_outputs/runs", full.names = TRUE)), 1)
cat("Run dir:", latest, "\n")

# Run only S10 by temporarily wrapping — just call the full function which is fast
# (S10 is cheap: just MESS rasters averaging)
n <- create_supplementary_figures(latest)
cat("Done:", n, "figures\n")
