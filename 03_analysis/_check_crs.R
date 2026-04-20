library(terra)
library(sf)

# Check bioclim raster CRS (the source CRS)
bio1 <- rast("01_data_raw/02_rasters/present/Historical_bioclims/data/Historical/1986-2015/Historical_1986-2015_bio1.tif")
cat("=== Bioclim raster CRS ===\n")
cat(crs(bio1), "\n\n")
cat(sprintf("Bioclim extent: %.1f %.1f %.1f %.1f\n", xmin(bio1), xmax(bio1), ymin(bio1), ymax(bio1)))
cat(sprintf("Bioclim EPSG code: %s\n\n", crs(bio1, describe = TRUE)$code))

# Check AOI shapefile CRS (read raw, without fix_vector_crs interference)
cat("=== AOI .prj contents ===\n")
prj <- readLines("01_data_raw/03_vector/shapefiles/Bhutan/bhutan.prj", warn = FALSE)
cat(prj, "\n\n")

# Read AOI
aoi <- st_read("01_data_raw/03_vector/shapefiles/Bhutan/bhutan.shp", quiet = TRUE)
cat(sprintf("AOI CRS: %s\n", st_crs(aoi)$input))
bb <- st_bbox(aoi)
cat(sprintf("AOI bbox: %.1f %.1f %.1f %.1f\n\n", bb[1], bb[3], bb[2], bb[4]))

# What does the AOI look like projected to real EPSG:32645?
# First set the correct source CRS (same as bioclim), then project
aoi_custom <- aoi
st_crs(aoi_custom) <- crs(bio1)
aoi_utm <- st_transform(aoi_custom, "EPSG:32645")
bb2 <- st_bbox(aoi_utm)
cat("=== AOI with correct CRS assignment + reprojection ===\n")
cat(sprintf("AOI in true EPSG:32645: %.1f %.1f %.1f %.1f\n", bb2[1], bb2[3], bb2[2], bb2[4]))
