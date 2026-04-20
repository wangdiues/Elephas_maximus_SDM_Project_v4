suppressPackageStartupMessages({ library(terra); library(sf) })

cat("========================================\n")
cat("OCCURRENCE DATA DIAGNOSIS\n")
cat("========================================\n")
occ <- read.csv("E:/Elephas_maximus_SDM_Project_v4/01_data_raw/01_occurrences/elephant_PA_data.csv")
cat("Total records:", nrow(occ), "\n")
cat("Presences:", sum(occ$presence == 1, na.rm = TRUE), "\n")
cat("Absences: ", sum(occ$presence == 0, na.rm = TRUE), "\n")
cat("Longitude:", round(min(occ$longitude),4), "to", round(max(occ$longitude),4), "\n")
cat("Latitude: ", round(min(occ$latitude),4), "to", round(max(occ$latitude),4), "\n")
cat("\nLatitude bands (0.25 deg bins):\n")
breaks_lat <- seq(26.5, 28.5, by=0.25)
print(table(cut(occ$latitude, breaks=breaks_lat)))
cat("\nLongitude bands (0.5 deg bins):\n")
breaks_lon <- seq(88, 93, by=0.5)
print(table(cut(occ$longitude, breaks=breaks_lon)))

cat("\n========================================\n")
cat("MODELING DATASET DIAGNOSIS\n")
cat("========================================\n")
RUN <- "RUN_20260315_124209_b990"
BASE <- paste0("E:/Elephas_maximus_SDM_Project_v4/04_outputs/runs/", RUN)
d <- read.csv(file.path(BASE, "01_processed_data/modeling_dataset.csv"))
cat("n presences:", sum(d$response==1), "\n")
cat("n absences (background):", sum(d$response==0), "\n")
pred_cols <- setdiff(names(d), c("id","type","response","longitude","latitude","fold"))
cat("\nSelected predictors:", paste(pred_cols, collapse=", "), "\n")

cat("\n--- Presence vs Background predictor comparison ---\n")
pres <- d[d$response==1, pred_cols]
bg   <- d[d$response==0, pred_cols]
for (col in pred_cols) {
  pv <- pres[[col]]; bv <- bg[[col]]
  cat(sprintf("%-30s  pres[med=%.1f, sd=%.1f]  bg[med=%.1f, sd=%.1f]\n",
    col, median(pv,na.rm=T), sd(pv,na.rm=T), median(bv,na.rm=T), sd(bv,na.rm=T)))
}

cat("\n--- Presence latitude vs background latitude ---\n")
cat("Presence lat range:", round(min(d$latitude[d$response==1]),3), "to", round(max(d$latitude[d$response==1]),3), "\n")
cat("Background lat range:", round(min(d$latitude[d$response==0]),3), "to", round(max(d$latitude[d$response==0]),3), "\n")
cat("Presence lon range:", round(min(d$longitude[d$response==1]),3), "to", round(max(d$longitude[d$response==1]),3), "\n")
cat("Background lon range:", round(min(d$longitude[d$response==0]),3), "to", round(max(d$longitude[d$response==0]),3), "\n")

cat("\n========================================\n")
cat("ENSEMBLE SUITABILITY RASTER CHECK\n")
cat("========================================\n")
ens_path <- file.path(BASE, "03_suitability_maps/ensemble_suitability_present.tif")
if (file.exists(ens_path)) {
  r <- rast(ens_path)
  v <- values(r, na.rm=TRUE)
  cat("Non-NA cells:", length(v), "\n")
  cat("Range:", round(min(v),4), "to", round(max(v),4), "\n")
  cat("Mean:", round(mean(v),4), "\n")
  cat("% cells > 0.5:", round(100*mean(v > 0.5),1), "%\n")
} else {
  cat("Ensemble raster not found at expected path\n")
  cat("Checking 03_suitability_maps/:\n")
  fls <- list.files(file.path(BASE, "03_suitability_maps"), full.names=FALSE)
  cat(paste(fls, collapse="\n"), "\n")
}

cat("\n========================================\n")
cat("MODEL PREDICTIONS SPREAD CHECK\n")
cat("========================================\n")
eval_df <- read.csv(file.path(BASE, "02_models/evaluation_all.csv"))
print(eval_df[, c("algorithm","auc_mean","tss_mean","boyce","calibration_slope","moran_i")])

cat("\n========================================\n")
cat("ELEVATION CHECK AT PRESENCE POINTS\n")
cat("========================================\n")
dem_path <- file.path(BASE, "02_data_intermediate/elevation_harmonized.tif")
if (file.exists(dem_path)) {
  elev <- rast(dem_path)
  pres_sf <- st_as_sf(d[d$response==1,], coords=c("longitude","latitude"), crs=4326)
  pres_utm <- st_transform(pres_sf, 32645)
  elev_vals <- extract(elev, vect(pres_utm))[[2]]
  cat("Elevation at presence points (m):\n")
  cat("  min:", round(min(elev_vals, na.rm=T)), "\n")
  cat("  median:", round(median(elev_vals, na.rm=T)), "\n")
  cat("  max:", round(max(elev_vals, na.rm=T)), "\n")
  cat("  > 1000m:", sum(elev_vals > 1000, na.rm=T), "of", sum(!is.na(elev_vals)), "\n")

  # Background elevation
  bg_sf <- st_as_sf(d[d$response==0,], coords=c("longitude","latitude"), crs=4326)
  bg_utm <- st_transform(bg_sf, 32645)
  bg_elev <- extract(elev, vect(bg_utm))[[2]]
  cat("Elevation at absence/background points (m):\n")
  cat("  min:", round(min(bg_elev, na.rm=T)), "\n")
  cat("  median:", round(median(bg_elev, na.rm=T)), "\n")
  cat("  max:", round(max(bg_elev, na.rm=T)), "\n")
  cat("  > 2000m:", sum(bg_elev > 2000, na.rm=T), "of", sum(!is.na(bg_elev)), "\n")
}

cat("\n========================================\n")
cat("NEW VECTOR DATA DISCOVERED\n")
cat("========================================\n")
cat("building_footprints.shp (Settlements/):\n")
bf <- tryCatch(st_read("E:/Elephas_maximus_SDM_Project_v4/01_data_raw/03_vector/shapefiles/Settlements/building_footprints.shp", quiet=TRUE), error=function(e) NULL)
if (!is.null(bf)) {
  cat("  Features:", nrow(bf), "| CRS:", st_crs(bf)$input, "\n")
  bb <- st_bbox(bf)
  cat("  BBox: lon", round(bb[1],2), "-", round(bb[3],2), "| lat", round(bb[2],2), "-", round(bb[4],2), "\n")
}

cat("2022_07_01_cadastral_fixed.shp (Pvt land/):\n")
cad <- tryCatch(st_read("E:/Elephas_maximus_SDM_Project_v4/01_data_raw/03_vector/shapefiles/Pvt land/2022_07_01_cadastral_fixed.shp", quiet=TRUE), error=function(e) NULL)
if (!is.null(cad)) {
  cat("  Features:", nrow(cad), "| CRS:", st_crs(cad)$input, "\n")
  bb <- st_bbox(cad)
  cat("  BBox: lon", round(bb[1],2), "-", round(bb[3],2), "| lat", round(bb[2],2), "-", round(bb[4],2), "\n")
}

cat("\nDone.\n")
