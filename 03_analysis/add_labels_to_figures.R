# add_labels_to_figures.R
# Re-renders 5 figures with Dzongkhag / PA / BC name labels,
# matching the original pipeline figure style exactly.
# Run: Rscript 03_analysis/add_labels_to_figures.R <run_dir>

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(ggplot2)
  library(scales)
})

has_repel    <- requireNamespace("ggrepel",   quietly = TRUE)
has_ggspatial <- requireNamespace("ggspatial", quietly = TRUE)

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_file) == 0) {
  frame_files <- vapply(sys.frames(), function(env) if (is.null(env$ofile)) "" else as.character(env$ofile), character(1))
  frame_files <- frame_files[nzchar(frame_files)]
  if (length(frame_files) > 0) script_file <- frame_files[[length(frame_files)]]
}
script_dir <- if (length(script_file) > 0) dirname(normalizePath(sub("^--file=", "", script_file[1]), winslash = "/", mustWork = FALSE)) else normalizePath("03_analysis", winslash = "/", mustWork = FALSE)
source(file.path(script_dir, "00_repo_paths.R"))
repo_root <- find_repo_root()

# ── args ──────────────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
run_dir <- if (length(args) >= 1) resolve_run_dir(args[1], repo_root = repo_root) else latest_run_dir(repo_root = repo_root)

cat(sprintf("Run dir: %s\n", run_dir))

fig_dir   <- file.path(run_dir, "08_figures_tables")
pres_dir  <- file.path(run_dir, "03_present_suitability")
fut_dir   <- file.path(run_dir, "04_future_projections")
over_dir  <- file.path(run_dir, "07_overlays")
proc_dir  <- file.path(run_dir, "01_processed_data")
interm    <- file.path(run_dir, "02_data_intermediate")

# ── shapefiles ────────────────────────────────────────────────────────────────
dzong_shp <- file.path(repo_root, "01_data_raw", "03_vector", "shapefiles",
                       "Dzongkhag Boundary", "Dzongkhag Boundary.shp")
pa_shp    <- file.path(repo_root, "01_data_raw", "03_vector", "shapefiles",
                       "PA_Bhutan", "PA_Bnd_Final_20230316.shp")
aoi_shp   <- file.path(repo_root, "01_data_raw", "03_vector", "shapefiles",
                       "Bhutan", "bhutan.shp")

load_dzong <- function() {
  d <- st_read(dzong_shp, quiet = TRUE)
  suppressWarnings(st_crs(d) <- 3857L)
  st_transform(d, 32645L)
}

load_pa <- function(target_crs = 32645L) {
  pa <- st_read(pa_shp, quiet = TRUE)
  st_transform(st_make_valid(pa), target_crs)
}

load_aoi <- function(target_crs = 32645L) {
  a <- st_read(aoi_shp, quiet = TRUE)
  st_transform(st_make_valid(a), target_crs)
}

# ── threshold ─────────────────────────────────────────────────────────────────
thr_file  <- file.path(run_dir, "06_evaluation", "thresholds.csv")
threshold <- 0.5
if (file.exists(thr_file)) {
  tdf <- read.csv(thr_file, stringsAsFactors = FALSE)
  ens <- tdf[tdf$algorithm == "ensemble", ]
  if (nrow(ens) > 0 && "tss_optimal" %in% names(ens))
    threshold <- ens$tss_optimal[1]
}
cat(sprintf("Threshold: %.3f\n", threshold))

# ── original pipeline style ───────────────────────────────────────────────────
map_theme <- function() {
  theme_bw() +
  theme(
    panel.background  = element_rect(fill = "#d6e8f5"),
    panel.grid.major  = element_line(colour = "white", linewidth = 0.3),
    plot.title        = element_text(size = 13, face = "bold"),
    plot.subtitle     = element_text(size = 9,  colour = "grey40"),
    axis.title        = element_blank(),
    legend.title      = element_text(size = 9,  face = "bold"),
    legend.text       = element_text(size = 8),
    plot.caption      = element_text(size = 7,  colour = "grey50")
  )
}

add_cartographic <- function(p) {
  if (has_ggspatial)
    p <- p +
      ggspatial::annotation_scale(location = "bl", width_hint = 0.25,
                                  height = unit(0.15, "cm")) +
      ggspatial::annotation_north_arrow(
        location = "tl", which_north = "true",
        style    = ggspatial::north_arrow_fancy_orienteering(text_size = 7),
        height   = unit(1.0, "cm"), width = unit(1.0, "cm"))
  p + labs(caption = "CRS: EPSG:32645 (WGS 84 / UTM Zone 45N)")
}

suit_scale <- function(name = "Suitability") {
  scale_fill_gradientn(
    colours  = c("#f7f7f7","#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),
    values   = c(0, 0.05, 0.2, 0.4, 0.65, 1),
    limits   = c(0, 1), na.value = "grey88",
    name     = name,
    guide    = guide_colorbar(barwidth = unit(0.45, "cm"), barheight = unit(5, "cm")))
}

# ── helpers ───────────────────────────────────────────────────────────────────
rast_df <- function(r, aoi_v = NULL) {
  if (!is.null(aoi_v)) {
    av <- if (!isTRUE(crs(aoi_v) == crs(r))) project(aoi_v, crs(r)) else aoi_v
    r  <- tryCatch(mask(crop(r, av), av), error = function(e) r)
  }
  if (nlyr(r) > 1) r <- r[[1]]
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "value"
  df
}

data_bbox <- function(r, pad = 0.04) {
  r2 <- tryCatch(trim(r), error = function(e) r)
  e  <- as.vector(ext(r2))
  dx <- (e[2] - e[1]) * pad
  dy <- (e[4] - e[3]) * pad
  list(xlim = c(e[1]-dx, e[2]+dx), ylim = c(e[3]-dy, e[4]+dy))
}

# Simple min-max (used for display only)
norm_rast_simple <- function(r) {
  mn <- global(r, "min", na.rm = TRUE)[1,1]
  mx <- global(r, "max", na.rm = TRUE)[1,1]
  if (is.na(mx) || mx == mn) return(r)
  (r - mn) / (mx - mn)
}

# Percentile-clipped normalisation — matches original pipeline exactly
norm_rast <- function(r) {
  v  <- values(r, na.rm = TRUE)
  lo <- quantile(v, 0.02); hi <- quantile(v, 0.98)
  if (hi <= lo) return(r * 0 + 0.5)
  clamp((r - lo) / (hi - lo), 0, 1, values = TRUE)
}

save_fig <- function(p, path, w, h, dpi = 350) {
  ggsave(path, p, width = w, height = h, dpi = dpi, limitsize = FALSE)
  cat(sprintf("  Saved: %s\n", basename(path)))
}

# Dzongkhag label layer — ggrepel if available, else geom_sf_text (no overlap check)
dzong_label_layer <- function(dz_sf, size = 2.8) {
  ctr    <- suppressWarnings(st_centroid(dz_sf))
  coords <- st_coordinates(ctr)
  df_lbl <- data.frame(x = coords[,1], y = coords[,2],
                        label = dz_sf$dzongkhag)
  if (has_repel) {
    ggrepel::geom_label_repel(
      data            = df_lbl, aes(x = x, y = y, label = label),
      inherit.aes     = FALSE,
      size            = size, colour = "grey10", fill = alpha("white", 0.72),
      label.size      = 0.18, label.padding = unit(0.1, "lines"),
      min.segment.length = 0.25, segment.colour = "grey50", segment.size = 0.3,
      max.overlaps    = 50, seed = 42, fontface = "bold")
  } else {
    geom_sf_text(data = ctr, aes(label = dzongkhag), inherit.aes = FALSE,
                 size = size, colour = "grey10", fontface = "bold",
                 check_overlap = FALSE)
  }
}

# PA / BC label layer
pa_label_layer <- function(pa_sf, col = "PA_name", size = 2.5,
                            colour = "#1b7837", fontface = "bold") {
  ctr    <- suppressWarnings(st_centroid(pa_sf))
  coords <- st_coordinates(ctr)
  df_lbl <- data.frame(x = coords[,1], y = coords[,2], label = pa_sf[[col]])
  if (has_repel) {
    ggrepel::geom_label_repel(
      data            = df_lbl, aes(x = x, y = y, label = label),
      inherit.aes     = FALSE,
      size            = size, colour = colour, fill = alpha("white", 0.78),
      label.size      = 0.18, label.padding = unit(0.1, "lines"), fontface = fontface,
      min.segment.length = 0.25, segment.colour = colour, segment.size = 0.3,
      max.overlaps    = 50, seed = 42)
  } else {
    geom_text(data = df_lbl, aes(x = x, y = y, label = label),
              inherit.aes = FALSE, size = size, colour = colour, fontface = fontface)
  }
}

# ── load shared data ──────────────────────────────────────────────────────────
dzong_sf <- load_dzong()
aoi_sf   <- load_aoi()
aoi_v    <- vect(aoi_sf)

dz_utm <- st_transform(dzong_sf, 32645L)

# ──────────────────────────────────────────────────────────────────────────────
# Figure 1: conflict_present — original style + Dzongkhag names
# ──────────────────────────────────────────────────────────────────────────────
cat("\nFig 1: conflict_present\n")
tryCatch({
  suit_pres <- rast(file.path(pres_dir, "suitability_present_ensemble.tif"))

  # ── Human pressure: footprint + building proximity (matches original pipeline)
  fp_rast <- rast(file.path(interm, "human_footprint_harmonized.tif"))
  fp_rast <- tryCatch(resample(fp_rast, suit_pres, method = "bilinear"),
                      error = function(e) fp_rast)
  fp_norm <- norm_rast(fp_rast); rm(fp_rast)

  bldg_gpkg <- file.path(interm, "building_footprints_harmonized.gpkg")
  human_pressure <- if (file.exists(bldg_gpkg)) {
    bldg_v    <- tryCatch(vect(bldg_gpkg), error = function(e) NULL)
    bldg_pres <- if (!is.null(bldg_v))
      tryCatch(rasterize(bldg_v, suit_pres, field = 1, background = NA),
               error = function(e) NULL) else NULL
    dist_r <- if (!is.null(bldg_pres))
      tryCatch(distance(bldg_pres), error = function(e) NULL) else NULL
    if (!is.null(dist_r)) {
      prox <- 1 / (1 + dist_r / 1000)
      norm_rast(fp_norm + norm_rast(prox))
    } else fp_norm
  } else fp_norm

  conflict_r <- norm_rast(norm_rast(suit_pres) * human_pressure)

  df   <- rast_df(conflict_r, aoi_v)
  bbox <- data_bbox(conflict_r)

  # HWC data
  hwc_path <- file.path(repo_root, "01_data_raw", "04_conflicts",
                         "HWC_Species_Elephant_clean.csv")
  hwc_sf <- if (file.exists(hwc_path)) {
    hw <- read.csv(hwc_path, stringsAsFactors = FALSE)
    hw <- hw[!is.na(hw$longitude) & !is.na(hw$latitude), ]
    hw$conflict_type <- trimws(ifelse(
      grepl("crop",     hw$damage_type, ignore.case=TRUE), "Crop Damage",
      ifelse(grepl("property", hw$damage_type, ignore.case=TRUE), "Property Damage",
             ifelse(grepl("casualty|human", hw$damage_type, ignore.case=TRUE),
                    "Human Casualty", "Other"))))
    sf_obj <- st_as_sf(hw, coords = c("longitude","latitude"), crs = 4326L)
    hwc_utm <- st_transform(sf_obj, 32645L)
    # Keep only points inside Bhutan
    hwc_utm[st_within(hwc_utm, st_union(aoi_sf), sparse = FALSE)[, 1], ]
  } else NULL

  hwc_col <- c("Crop Damage"="#f0e442","Property Damage"="#e69f00",
               "Human Casualty"="#cc0000","Other"="#999999")
  hwc_shp <- c("Crop Damage"=21L,"Property Damage"=24L,
               "Human Casualty"=23L,"Other"=22L)

  p <- ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) +
    scale_fill_gradientn(
      colours = c("#ffffcc","#fecc5c","#fd8d3c","#e31a1c","#800026"),
      limits  = c(0,1), breaks = c(0,0.25,0.5,0.75,1),
      labels  = c("Low","","Moderate","","Very High"),
      na.value= "grey88", name = "Conflict\nRisk",
      guide   = guide_colorbar(barwidth=unit(0.45,"cm"), barheight=unit(5,"cm"))) +
    geom_sf(data = dz_utm, inherit.aes = FALSE,
            fill = NA, colour = "grey40", linewidth = 0.22, linetype = "dashed") +
    dzong_label_layer(dz_utm, size = 2.8) +
    geom_sf(data = aoi_sf, inherit.aes = FALSE,
            fill = NA, colour = "black", linewidth = 0.55) +
    { if (!is.null(hwc_sf))
        list(
          geom_sf(data = hwc_sf, aes(colour = conflict_type, shape = conflict_type),
                  inherit.aes = FALSE, size = 2.2, stroke = 0.7, fill = NA, alpha = 0.85),
          scale_colour_manual(values = hwc_col, name = "HWC incident",
                              guide = guide_legend(override.aes = list(size=3, fill=NA))),
          scale_shape_manual(values = hwc_shp, name = "HWC incident")
        ) } +
    labs(title = "Present-day human-elephant conflict risk \u2014 Elephas maximus (Bhutan)",
         x = NULL, y = NULL) +
    map_theme() +
    coord_sf(xlim = bbox$xlim, ylim = bbox$ylim, expand = FALSE, crs = st_crs(32645L))

  p <- add_cartographic(p)
  save_fig(p, file.path(fig_dir, "figure_conflict_present_labelled.png"), 12, 9)
}, error = function(e) cat(sprintf("  FAILED: %s\n", e$message)))

# ──────────────────────────────────────────────────────────────────────────────
# Figure 2: s11_background_points — original style + Dzongkhag names
# ──────────────────────────────────────────────────────────────────────────────
cat("\nFig 2: s11_background_points\n")
tryCatch({
  dat <- read.csv(file.path(proc_dir, "modeling_dataset.csv"), stringsAsFactors = FALSE)
  suit_r  <- rast(file.path(pres_dir, "suitability_present_ensemble.tif"))
  df_suit <- rast_df(suit_r, aoi_v)
  bbox    <- data_bbox(suit_r)

  prs <- dat[dat$response==1 & !is.na(dat$longitude) & !is.na(dat$latitude), ]
  bg  <- dat[dat$response==0 & !is.na(dat$longitude) & !is.na(dat$latitude), ]
  prs_sf <- st_transform(st_as_sf(prs, coords=c("longitude","latitude"), crs=4326L), 32645L)
  bg_sf  <- st_transform(st_as_sf(bg,  coords=c("longitude","latitude"), crs=4326L), 32645L)

  aoi_u  <- st_union(aoi_sf)
  prs_sf <- tryCatch(prs_sf[st_within(prs_sf, aoi_u, sparse=FALSE)[,1],], error=function(e) prs_sf)
  bg_sf  <- tryCatch(bg_sf[st_within(bg_sf,  aoi_u, sparse=FALSE)[,1],], error=function(e) bg_sf)

  target_crs <- st_crs(crs(suit_r))
  dz  <- st_transform(dzong_sf, target_crs)

  p <- ggplot() +
    geom_raster(data = df_suit, aes(x = x, y = y, fill = value)) +
    scale_fill_gradientn(
      colours  = c("#f7f7f7","#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),
      values   = c(0,0.05,0.2,0.4,0.65,1), limits=c(0,1), na.value="grey90",
      name     = "Ensemble\nsuitability",
      guide    = guide_colorbar(barwidth=unit(0.45,"cm"), barheight=unit(5,"cm"))) +
    geom_sf(data = dz, inherit.aes = FALSE,
            fill = NA, colour = "grey40", linewidth = 0.22, linetype = "dashed") +
    dzong_label_layer(dz, size = 2.8) +
    geom_sf(data = aoi_sf, inherit.aes = FALSE,
            fill = NA, colour = "black", linewidth = 0.55) +
    geom_sf(data = bg_sf, inherit.aes = FALSE,
            colour = "grey30", shape = 3, size = 0.8, alpha = 0.55) +
    geom_sf(data = prs_sf, inherit.aes = FALSE,
            fill = "#FFD700", colour = "#8B0000", shape = 21,
            size = 2.0, alpha = 0.9, stroke = 0.6) +
    labs(title    = "Occurrence and background sampling \u2014 Elephas maximus (Bhutan)",
         subtitle = sprintf("Presences (gold, n=%d) | Background (grey+, n=%d) | Backdrop = ensemble suitability",
                            nrow(prs_sf), nrow(bg_sf)),
         x = NULL, y = NULL) +
    map_theme() +
    coord_sf(xlim = bbox$xlim, ylim = bbox$ylim, expand = FALSE, crs = st_crs(32645L))

  p <- add_cartographic(p)
  save_fig(p, file.path(fig_dir, "figure_s11_background_points_labelled.png"), 12, 9)
}, error = function(e) cat(sprintf("  FAILED: %s\n", e$message)))

# ──────────────────────────────────────────────────────────────────────────────
# Figure 3: s13_gcm_agreement — original style + Dzongkhag names
# ──────────────────────────────────────────────────────────────────────────────
cat("\nFig 3: s13_gcm_agreement\n")
tryCatch({
  ssps    <- c("ssp126","ssp245","ssp370","ssp585")
  periods <- c("2021_2050","2051_2080","2071_2100")

  rast_list <- list()
  for (ssp in ssps) for (prd in periods) {
    f <- file.path(fut_dir, sprintf("future_gcm_ensemble_%s_%s.tif", ssp, prd))
    if (file.exists(f)) {
      r <- tryCatch({ rr <- rast(f); if (nlyr(rr)>1) rr[[1]] else rr }, error=function(e) NULL)
      if (!is.null(r)) rast_list[[length(rast_list)+1]] <- r
    }
  }
  if (length(rast_list) == 0) stop("no future ensemble rasters found")

  ref_r  <- rast_list[[1]]
  binary <- lapply(rast_list, function(r) {
    if (!isTRUE(crs(r) == crs(ref_r))) r <- project(r, ref_r, method="bilinear")
    b   <- r >= threshold; b[] <- as.integer(b[]); b
  })
  agree_r <- Reduce("+", binary)

  if (!is.null(aoi_v)) {
    av2 <- if (!isTRUE(crs(aoi_v)==crs(agree_r))) project(aoi_v, crs(agree_r)) else aoi_v
    agree_r <- tryCatch(mask(crop(agree_r, av2), av2), error=function(e) agree_r)
  }

  agree_df <- as.data.frame(agree_r, xy=TRUE, na.rm=TRUE)
  names(agree_df)[3] <- "n_scenarios"
  agree_df$frac <- agree_df$n_scenarios / length(rast_list)
  bbox <- data_bbox(agree_r)

  target_crs <- st_crs(crs(agree_r))
  dz       <- st_transform(dzong_sf, target_crs)
  aoi_plot <- st_transform(aoi_sf,   target_crs)

  p <- ggplot() +
    geom_raster(data = agree_df, aes(x=x, y=y, fill=frac)) +
    scale_fill_gradientn(
      colours = c("#f7f7f7","#fee08b","#fdae61","#f46d43","#d73027","#a50026"),
      values  = c(0,0.25,0.5,0.67,0.83,1), limits=c(0,1), na.value="grey90",
      name    = sprintf("Fraction of\n%d scenarios\nsuitable", length(rast_list)),
      labels  = percent_format(),
      guide   = guide_colorbar(barwidth=unit(0.45,"cm"), barheight=unit(5,"cm"))) +
    geom_sf(data = dz, inherit.aes = FALSE,
            fill = NA, colour = "grey40", linewidth = 0.22, linetype = "dashed") +
    dzong_label_layer(dz, size = 2.8) +
    geom_sf(data = aoi_plot, inherit.aes = FALSE,
            fill = NA, colour = "black", linewidth = 0.55) +
    labs(title    = "GCM agreement across future scenarios \u2014 Elephas maximus SDM (Bhutan)",
         subtitle = sprintf("%d future scenarios (4 SSPs \u00d7 3 periods) | Threshold = %.3f (TSS-optimal) | Red = consistent high suitability",
                            length(rast_list), threshold),
         x = NULL, y = NULL) +
    map_theme() +
    coord_sf(xlim = bbox$xlim, ylim = bbox$ylim, expand = FALSE)

  p <- add_cartographic(p)
  save_fig(p, file.path(fig_dir, "figure_s13_gcm_agreement_labelled.png"), 12, 9)
}, error = function(e) cat(sprintf("  FAILED: %s\n", e$message)))

# ──────────────────────────────────────────────────────────────────────────────
# Figure 4: E05_pa_overlay — original style + PA/BC names
# ──────────────────────────────────────────────────────────────────────────────
cat("\nFig 4: E05_pa_overlay\n")
tryCatch({
  suit_r  <- rast(file.path(pres_dir, "suitability_present_ensemble.tif"))
  df_suit <- rast_df(suit_r, aoi_v)
  bbox    <- data_bbox(suit_r)

  pa_sf_utm <- load_pa(32645L)
  pa_only   <- pa_sf_utm[!grepl("^Biological Corridor", pa_sf_utm$park), ]
  bc_only   <- pa_sf_utm[ grepl("^Biological Corridor", pa_sf_utm$park), ]

  pa_v    <- vect(pa_sf_utm)
  pa_mask <- rasterize(pa_v, suit_r, field=1, background=0)
  inside  <- mask(suit_r, pa_mask, maskvalues=0)
  outside <- mask(suit_r, pa_mask, maskvalues=1)
  pct_in  <- round(100*sum(values(inside, na.rm=TRUE)>=threshold)/sum(!is.na(values(suit_r))),1)
  pct_out <- round(100*sum(values(outside,na.rm=TRUE)>=threshold)/sum(!is.na(values(suit_r))),1)

  p <- ggplot() +
    geom_raster(data = df_suit, aes(x=x, y=y, fill=value)) +
    suit_scale("Suitability") +
    geom_sf(data = aoi_sf, inherit.aes = FALSE,
            fill = NA, colour = "black", linewidth = 0.55) +
    geom_sf(data = bc_only, inherit.aes = FALSE,
            fill = NA, colour = "#4dac26", linewidth = 0.5, linetype = "dashed") +
    geom_sf(data = pa_only, inherit.aes = FALSE,
            fill = NA, colour = "#1b7837", linewidth = 0.7) +
    pa_label_layer(pa_only, col="PA_name", size=2.5, colour="#1b7837", fontface="bold") +
    pa_label_layer(bc_only, col="PA_name", size=2.2, colour="#4dac26", fontface="italic") +
    annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.5,
             label=sprintf("Inside PAs:  %.1f%% suitable\nOutside PAs: %.1f%% suitable",
                           pct_in, pct_out),
             size=3, fontface="bold", colour="#1b7837") +
    labs(title    = "Present Habitat Suitability with Protected Area Boundaries",
         subtitle = "Green outlines = protected areas",
         x = NULL, y = NULL) +
    map_theme() +
    coord_sf(xlim=bbox$xlim, ylim=bbox$ylim, expand=FALSE, crs=st_crs(32645L))

  p <- add_cartographic(p)
  save_fig(p, file.path(fig_dir, "figure_E05_pa_overlay_labelled.png"), 14, 9)
}, error = function(e) cat(sprintf("  FAILED: %s\n", e$message)))

# ──────────────────────────────────────────────────────────────────────────────
# Figure 5: r07_conservation_overlays — original style + PA/BC names
# ──────────────────────────────────────────────────────────────────────────────
cat("\nFig 5: r07_conservation_overlays\n")
tryCatch({
  pa_rast <- rast(file.path(over_dir, "suitability_within_PAs.tif"))
  if (nlyr(pa_rast) > 1) pa_rast <- pa_rast[[1]]

  df   <- rast_df(pa_rast)
  bbox <- data_bbox(pa_rast)

  pa_sf_utm <- load_pa(32645L)
  pa_only   <- pa_sf_utm[!grepl("^Biological Corridor", pa_sf_utm$park), ]
  bc_only   <- pa_sf_utm[ grepl("^Biological Corridor", pa_sf_utm$park), ]

  p <- ggplot() +
    geom_raster(data = df, aes(x=x, y=y, fill=value)) +
    suit_scale("Suitability\n(within PAs)") +
    geom_sf(data = aoi_sf, inherit.aes = FALSE,
            fill = NA, colour = "black", linewidth = 0.55) +
    geom_sf(data = bc_only, inherit.aes = FALSE,
            fill = NA, colour = "#4dac26", linewidth = 0.5, linetype = "dashed") +
    geom_sf(data = pa_only, inherit.aes = FALSE,
            fill = NA, colour = "#1b7837", linewidth = 0.8) +
    pa_label_layer(pa_only, col="PA_name", size=2.5, colour="#1b7837", fontface="bold") +
    pa_label_layer(bc_only, col="PA_name", size=2.2, colour="#4dac26", fontface="italic") +
    annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.5,
             label=sprintf("Mean suitability inside PAs = %.3f",
                           mean(df$value, na.rm=TRUE)),
             size=3, fontface="bold", colour="#1b7837") +
    labs(title    = "Habitat suitability within protected areas \u2014 Elephas maximus (Bhutan)",
         subtitle = sprintf("Green outlines = PAs | Mean suitability inside PAs = %.3f",
                            mean(df$value, na.rm=TRUE)),
         x = NULL, y = NULL) +
    map_theme() +
    coord_sf(xlim=bbox$xlim, ylim=bbox$ylim, expand=FALSE, crs=st_crs(32645L))

  p <- add_cartographic(p)
  save_fig(p, file.path(fig_dir, "figure_r07_conservation_overlays_labelled.png"), 14, 9)
}, error = function(e) cat(sprintf("  FAILED: %s\n", e$message)))

cat("\nAll done.\n")
