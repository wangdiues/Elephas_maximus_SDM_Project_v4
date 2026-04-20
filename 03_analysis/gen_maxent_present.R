library(terra)
source("E:/Elephas_maximus_SDM_Project_v4/03_analysis/00_spatial_alignment.R")

RUN <- "E:/Elephas_maximus_SDM_Project_v4/04_outputs/runs/RUN_20260317_203608_b990"

# MaxEnt produced the avg prediction ASC directly — use it
asc_file <- file.path(RUN, "02_models/maxent_output/Elephas_maximus_avg.asc")
template  <- rast(file.path(RUN, "02_data_intermediate/template_grid.tif"))

r_mx <- rast(asc_file)
crs(r_mx) <- "EPSG:4326"   # MaxEnt ASC layers are in WGS84
cat("MaxEnt ASC loaded. Values: min=", round(min(values(r_mx,na.rm=TRUE)),4),
    " max=", round(max(values(r_mx,na.rm=TRUE)),4), "\n")
cat("Extent:", paste(round(as.vector(ext(r_mx)),4), collapse=" "), "\n")

# Reproject to UTM 32645 and align to template grid
r_utm <- project(r_mx, "EPSG:32645", method="bilinear")
r_aligned <- align_raster(r_utm, template, verbose=FALSE)

# Apply AOI mask
m_mask <- tryCatch(rast(file.path(RUN,"02_data_intermediate/m_mask.tif")), error=function(e) NULL)
if (!is.null(m_mask)) {
  if (!isTRUE(compareGeom(m_mask, r_aligned, stopOnError=FALSE)))
    m_mask <- project(m_mask, r_aligned, method="near")
  r_aligned <- mask(r_aligned, m_mask, maskvalues=0)
}

out_f <- file.path(RUN, "03_present_suitability/suitability_present_maxent.tif")
writeRaster(r_aligned, out_f, overwrite=TRUE)
v <- values(r_aligned, na.rm=TRUE)
cat("Saved MaxEnt present suitability.\n")
cat("Final values: min=",round(min(v),4)," max=",round(max(v),4),
    " mean=",round(mean(v),4)," p90=",round(quantile(v,0.9),4),"\n")
