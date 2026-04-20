# ============================================================
# SIMPLE CONFLICT HOTSPOT MAP FOR BHUTAN
# Stable version with minimal dependencies
# ============================================================

# 1. Packages
required_pkgs <- c("sf", "terra", "ggplot2")
new_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)

library(sf)
library(terra)
library(ggplot2)

# 2. File paths
pts_path <- "D:/FRPMD/2026-2027/CRISES AND CHALLENGES IN BHUTAN’S DURING CORDYCEPS COLLECTION/Conflict Points.shp"
bhutan_path <- "D:/FRPMD/2026-2027/CRISES AND CHALLENGES IN BHUTAN’S DURING CORDYCEPS COLLECTION/Bhutan.shp"
gewog_path <- "D:/FRPMD/2026-2027/CRISES AND CHALLENGES IN BHUTAN’S DURING CORDYCEPS COLLECTION/Conflict Gewogs.shp"

out_dir <- "D:/FRPMD/2026-2027/CRISES AND CHALLENGES IN BHUTAN’S DURING CORDYCEPS COLLECTION/outputs"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 3. Read data
pts <- st_read(pts_path, quiet = TRUE)
bhutan <- st_read(bhutan_path, quiet = TRUE)
gewog <- st_read(gewog_path, quiet = TRUE)

# 4. Basic checks
if (nrow(pts) == 0) stop("Conflict Points.shp has no features.")
if (nrow(bhutan) == 0) stop("Bhutan.shp has no features.")
if (nrow(gewog) == 0) stop("Conflict Gewogs.shp has no features.")

# 5. Fix geometry
pts <- st_make_valid(pts)
bhutan <- st_make_valid(bhutan)
gewog <- st_make_valid(gewog)

# 6. Project to same CRS as your SDM-style map
crs_use <- 32645

pts <- st_transform(pts, crs_use)
bhutan <- st_transform(bhutan, crs_use)
gewog <- st_transform(gewog, crs_use)

# 7. Clip to Bhutan
bhutan_union <- st_union(bhutan)
pts <- suppressWarnings(st_intersection(pts, bhutan_union))
gewog <- suppressWarnings(st_intersection(gewog, bhutan_union))

if (nrow(pts) == 0) stop("No points remain after clipping to Bhutan.")

# 8. Use only points inside conflict gewogs if available
pts_in_gewog <- suppressWarnings(st_intersection(pts, st_union(gewog)))
if (nrow(pts_in_gewog) > 0) {
  pts_use <- pts_in_gewog
} else {
  pts_use <- pts
}

# 9. Create raster template
bhutan_vect <- vect(bhutan)
r_template <- rast(ext(bhutan_vect), resolution = 500, crs = paste0("EPSG:", crs_use))

# 10. Rasterize points
pts_vect <- vect(pts_use)
r_pts <- rasterize(pts_vect, r_template, field = 1, fun = "sum", background = 0)

# 11. Smooth to create hotspot surface
w <- focalMat(r_pts, d = 5000, type = "Gauss")
r_hotspot <- focal(r_pts, w = w, fun = sum, na.rm = TRUE)

# 12. Clip hotspot raster to Bhutan
r_hotspot <- crop(r_hotspot, bhutan_vect)
r_hotspot <- mask(r_hotspot, bhutan_vect)

# 13. Normalize to 0-1
gmin <- global(r_hotspot, "min", na.rm = TRUE)[1, 1]
gmax <- global(r_hotspot, "max", na.rm = TRUE)[1, 1]

if (is.na(gmin) || is.na(gmax) || gmax <= gmin) {
  stop("Hotspot raster could not be normalized. Check points and CRS.")
}

r_hotspot <- (r_hotspot - gmin) / (gmax - gmin)

# 14. Convert raster to dataframe
hotspot_df <- as.data.frame(r_hotspot, xy = TRUE, na.rm = TRUE)
colnames(hotspot_df)[3] <- "hotspot"

if (nrow(hotspot_df) == 0) stop("Hotspot raster is empty.")

# 15. Make simple map
p <- ggplot() +
  geom_raster(data = hotspot_df, aes(x = x, y = y, fill = hotspot)) +
  geom_sf(data = gewog, fill = NA, color = "grey50", linewidth = 0.25, linetype = "dashed") +
  geom_sf(data = bhutan, fill = NA, color = "black", linewidth = 0.7) +
  scale_fill_gradientn(
    colours = c("#ffffcc", "#fd8d3c", "#e31a1c"),
    limits = c(0, 1),
    name = "Conflict\nhotspot"
  ) +
  labs(
    title = "Conflict hotspot map of Bhutan",
    subtitle = "Conflict points smoothed into hotspot surface",
    x = "Easting (m)",
    y = "Northing (m)"
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "#d9e6ef", color = NA),
    plot.background = element_rect(fill = "grey95", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    legend.title = element_text(face = "bold")
  )

# 16. Save outputs
writeRaster(r_hotspot, file.path(out_dir, "conflict_hotspot_simple.tif"), overwrite = TRUE)

ggsave(
  filename = file.path(out_dir, "conflict_hotspot_simple.png"),
  plot = p,
  width = 11,
  height = 7,
  dpi = 300
)

print(p)

cat("Done.\n")
cat("Saved to:\n", out_dir, "\n")



# create centroids (avoids messy overlapping)
gewog_cent <- st_centroid(gewog)

p <- ggplot() +
  geom_raster(data = hotspot_df, aes(x = x, y = y, fill = hotspot)) +
  
  geom_sf(data = gewog, fill = NA, color = "grey50",
          linewidth = 0.25, linetype = "dashed") +
  
  geom_sf(data = bhutan, fill = NA, color = "black", linewidth = 0.7) +
  
  # ✔ labels
  geom_sf_text(
    data = gewog_cent,
    aes(label = name_eng),
    size = 2.5,
    color = "black",
    check_overlap = TRUE
  ) +
  
  scale_fill_gradientn(
    colours = c("#ffffcc", "#fd8d3c", "#e31a1c"),
    limits = c(0, 1),
    name = "Conflict\nhotspot"
  ) +
  
  labs(
    title = "Conflict hotspot map of Bhutan",
    subtitle = "Conflict points smoothed into hotspot surface",
    x = "Easting (m)",
    y = "Northing (m)"
  ) +
  
  coord_sf(expand = FALSE) +
  theme_minimal(base_size = 12)



names(gewog)
head(gewog$name_eng)
nrow(gewog)






# label points inside polygons
gewog_lab <- st_point_on_surface(gewog)

p <- ggplot() +
  geom_raster(data = hotspot_df, aes(x = x, y = y, fill = hotspot)) +
  geom_sf(data = gewog, fill = NA, color = "grey50",
          linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = bhutan, fill = NA, color = "black", linewidth = 0.8) +
  
  # use label instead of text
  geom_sf_label(
    data = gewog_lab,
    aes(label = name_eng),
    size = 3,
    fill = "white",
    color = "black",
    label.size = 0.2,
    alpha = 0.9
  ) +
  
  scale_fill_gradientn(
    colours = c("#ffffcc", "#fd8d3c", "#e31a1c"),
    limits = c(0, 1),
    name = "Conflict\nhotspot"
  ) +
  labs(
    title = "Conflict hotspot map of Bhutan",
    subtitle = "Conflict points smoothed into hotspot surface",
    x = "Easting (m)",
    y = "Northing (m)"
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "#d9e6ef", color = NA),
    plot.background = element_rect(fill = "grey95", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    legend.title = element_text(face = "bold")
  )

print(p)

