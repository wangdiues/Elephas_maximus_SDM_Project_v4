# renv is available for cross-machine reproducibility.
# On this machine the system R library is used directly (renv isolation is OFF).
# To restore the exact package environment on a NEW machine:
#   install.packages("renv")
#   renv::restore()
#
# Uncomment the block below to activate renv isolation on this machine:
# local({
#   renv_script <- file.path(getwd(), "renv", "activate.R")
#   if (file.exists(renv_script)) source(renv_script)
# })

# Redirect R and terra temp dirs to E: (large drive) to avoid filling C: system drive.
local({
  e_tmp <- "E:/tmp/Rtmp_SDM"
  if (!dir.exists(e_tmp)) dir.create(e_tmp, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(TMPDIR = e_tmp, TMP = e_tmp, TEMP = e_tmp)
  options(terra.tmpdir = e_tmp)
})
