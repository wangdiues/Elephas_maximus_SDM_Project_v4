#!/usr/bin/env Rscript
# Publication-safe plotting, export, and tabular formatting helpers.

publication_palette <- function(kind = c("scenario", "algorithm", "habitat", "change")) {
  kind <- match.arg(kind)
  switch(
    kind,
    scenario = c("SSP1-2.6" = "#0072B2", "SSP2-4.5" = "#009E73",
                 "SSP3-7.0" = "#E69F00", "SSP5-8.5" = "#CC79A7"),
    algorithm = c("GLM" = "#0072B2", "RF" = "#009E73", "BRT" = "#E69F00",
                  "MaxEnt" = "#CC79A7", "Ensemble" = "#000000"),
    habitat = c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"),
    change = c("loss" = "#D55E00", "stable_unsuitable" = "#D9D9D9",
               "persistence" = "#0072B2", "gain" = "#009E73")
  )
}

publication_linetypes <- function() {
  c("SSP1-2.6" = "solid", "SSP2-4.5" = "dashed",
    "SSP3-7.0" = "dotdash", "SSP5-8.5" = "twodash")
}

publication_shapes <- function() {
  c("SSP1-2.6" = 16, "SSP2-4.5" = 17, "SSP3-7.0" = 15, "SSP5-8.5" = 18)
}

robust_unit_scale_raster <- function(r, lower = 0.02, upper = 0.98) {
  if (!inherits(r, "SpatRaster")) stop("r must be a terra SpatRaster")
  q <- terra::global(r, fun = function(x) stats::quantile(x, c(lower, upper), na.rm = TRUE))
  lo <- as.numeric(q[[1]]); hi <- as.numeric(q[[2]])
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) stop("Raster cannot be robustly scaled")
  terra::clamp((r - lo) / (hi - lo), 0, 1, values = TRUE)
}

interaction_exposure_index <- function(suitability, human_pressure) {
  if (!isTRUE(terra::compareGeom(suitability, human_pressure, stopOnError = FALSE))) {
    human_pressure <- terra::resample(human_pressure, suitability, method = "bilinear")
  }
  s <- robust_unit_scale_raster(suitability)
  h <- robust_unit_scale_raster(human_pressure)
  sqrt(s * h)
}

get_oof_ensemble_threshold <- function(evaluation) {
  if (is.character(evaluation) && length(evaluation) == 1L) {
    if (!file.exists(evaluation)) stop("Evaluation file not found: ", evaluation)
    evaluation <- read.csv(evaluation, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(evaluation) || !all(c("algorithm", "threshold") %in% names(evaluation))) {
    stop("Evaluation must contain algorithm and threshold columns")
  }
  ens <- evaluation[evaluation$algorithm == "ensemble", , drop = FALSE]
  if (nrow(ens) != 1L || !is.finite(ens$threshold[[1]])) {
    stop("Exactly one finite spatial-OOF ensemble threshold is required")
  }
  as.numeric(ens$threshold[[1]])
}

publication_theme <- function(base_size = 9, base_family = "Arial") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required")
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 1),
      plot.subtitle = ggplot2::element_text(size = base_size),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(colour = "#222222"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(face = "bold"),
      legend.key.height = grid::unit(4, "mm"),
      plot.caption = ggplot2::element_text(hjust = 0, size = base_size - 1),
      plot.margin = ggplot2::margin(5.5, 7, 5.5, 5.5)
    )
}

save_publication_figure <- function(plot, stem, width_mm, height_mm,
                                    raster_dpi = 300, line_art_dpi = 600,
                                    create_pdf = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required")
  if (!is.finite(width_mm) || !is.finite(height_mm) || width_mm <= 0 || height_mm <= 0) {
    stop("Figure dimensions must be positive millimetre values")
  }
  dir.create(dirname(stem), recursive = TRUE, showWarnings = FALSE)
  png_path <- paste0(stem, ".png")
  ggplot2::ggsave(png_path, plot, width = width_mm, height = height_mm,
                  units = "mm", dpi = line_art_dpi, bg = "white", limitsize = FALSE)
  outputs <- png_path
  if (isTRUE(create_pdf)) {
    pdf_path <- paste0(stem, ".pdf")
    device <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
    ggplot2::ggsave(pdf_path, plot, width = width_mm, height = height_mm,
                    units = "mm", device = device, bg = "white", limitsize = FALSE)
    outputs <- c(outputs, pdf_path)
  }
  data.frame(
    file = normalizePath(outputs, winslash = "/", mustWork = TRUE),
    format = tools::file_ext(outputs),
    width_mm = width_mm,
    height_mm = height_mm,
    dpi = ifelse(tools::file_ext(outputs) == "png", line_art_dpi, NA_integer_),
    md5 = unname(tools::md5sum(outputs)),
    stringsAsFactors = FALSE
  )
}

assert_figure_encoding <- function(data, group_column, colour_column = group_column,
                                   linetype_column = NULL, shape_column = NULL) {
  groups <- unique(data[[group_column]])
  if (length(groups) >= 3 && is.null(linetype_column) && is.null(shape_column)) {
    stop("Three or more groups may not be distinguished by colour alone")
  }
  invisible(TRUE)
}

format_publication_number <- function(x, digits = 2, na = "—") {
  out <- ifelse(is.finite(x), formatC(x, format = "f", digits = digits, big.mark = ","), na)
  unname(out)
}

png_dimensions <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  bytes <- readBin(con, what = "integer", n = 24, size = 1, signed = FALSE)
  if (length(bytes) < 24 || !identical(bytes[1:8], c(137L, 80L, 78L, 71L, 13L, 10L, 26L, 10L))) {
    stop("Invalid PNG file: ", path)
  }
  decode_u32 <- function(z) sum(z * 256^(3:0))
  c(width_px = decode_u32(bytes[17:20]), height_px = decode_u32(bytes[21:24]))
}

write_figure_manifest <- function(fig_dir, run_id, minimum_short_edge_px = 1200) {
  files <- sort(list.files(fig_dir, pattern = "\\.(png|pdf)$", full.names = TRUE,
                           ignore.case = TRUE))
  if (length(files) == 0) stop("No publication figures found in: ", fig_dir)
  rows <- lapply(files, function(f) {
    ext <- tolower(tools::file_ext(f))
    dims <- if (ext == "png") png_dimensions(f) else c(width_px = NA, height_px = NA)
    data.frame(
      run_id = run_id,
      figure_file = basename(f),
      format = ext,
      width_px = unname(dims[["width_px"]]),
      height_px = unname(dims[["height_px"]]),
      short_edge_px = if (ext == "png") min(dims) else NA_real_,
      size_bytes = file.info(f)$size,
      md5 = unname(tools::md5sum(f)),
      stringsAsFactors = FALSE
    )
  })
  manifest <- do.call(rbind, rows)
  low_res <- manifest$format == "png" & manifest$short_edge_px < minimum_short_edge_px
  if (any(low_res, na.rm = TRUE)) {
    stop("PNG figures below the minimum pixel dimension: ",
         paste(manifest$figure_file[low_res], collapse = ", "))
  }
  path <- file.path(fig_dir, "figure_provenance_manifest.csv")
  write.csv(manifest, path, row.names = FALSE)
  invisible(path)
}
