setwd("E:/Elephas_maximus_SDM_Project_v4")
run_dir <- tail(sort(list.files("04_outputs/runs", full.names = TRUE)), 1)
cat("Run dir:", run_dir, "\n")

# Regenerate conflict map (E enhanced figures)
source("03_analysis/10_figures_enhanced.R")
create_enhanced_figures(run_dir)
cat("Enhanced figures done\n")

# Regenerate supplementary (S11, S16)
source("03_analysis/10_figures_supplementary.R")
create_supplementary_figures(run_dir)
cat("Supplementary figures done\n")
