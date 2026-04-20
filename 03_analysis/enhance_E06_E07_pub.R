# enhance_E06_E07_pub.R
# Publication-quality re-render of E06 (response curves) and E07 (variable importance)
# Run: Rscript 03_analysis/enhance_E06_E07_pub.R <run_dir>
# Or:  Rscript 03_analysis/enhance_E06_E07_pub.R
#      (auto-detects latest run)

library(ggplot2)
library(grid)
library(scales)

`%||%` <- function(a, b) if (!is.null(a)) a else b

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_file) == 0) {
  frame_files <- vapply(sys.frames(), function(env) if (is.null(env$ofile)) "" else as.character(env$ofile), character(1))
  frame_files <- frame_files[nzchar(frame_files)]
  if (length(frame_files) > 0) script_file <- frame_files[[length(frame_files)]]
}
script_dir <- if (length(script_file) > 0) dirname(normalizePath(sub("^--file=", "", script_file[1]), winslash = "/", mustWork = FALSE)) else normalizePath("03_analysis", winslash = "/", mustWork = FALSE)
source(file.path(script_dir, "00_repo_paths.R"))
repo_root <- find_repo_root()

# ── helpers ──────────────────────────────────────────────────────────────────

# Human-readable predictor labels
pred_label <- function(p) {
  # Strip .contribution suffix (MaxEnt artifact)
  p <- sub("\\.contribution$", "", p)
  p <- sub(" contribution$", "", p)
  lut <- c(
    BIO01 = "BIO01 — Annual Mean Temp",
    BIO02 = "BIO02 — Mean Diurnal Range",
    BIO03 = "BIO03 — Isothermality",
    BIO04 = "BIO04 — Temp Seasonality",
    BIO05 = "BIO05 — Max Temp Warmest Mo.",
    BIO06 = "BIO06 — Min Temp Coldest Mo.",
    BIO07 = "BIO07 — Temp Annual Range",
    BIO08 = "BIO08 — Mean Temp Wettest Q.",
    BIO09 = "BIO09 — Mean Temp Driest Q.",
    BIO10 = "BIO10 — Mean Temp Warmest Q.",
    BIO11 = "BIO11 — Mean Temp Coldest Q.",
    BIO12 = "BIO12 — Annual Precipitation",
    BIO13 = "BIO13 — Precip Wettest Mo.",
    BIO14 = "BIO14 — Precip Driest Mo.",
    BIO15 = "BIO15 — Precip Seasonality",
    BIO16 = "BIO16 — Precip Wettest Q.",
    BIO17 = "BIO17 — Precip Driest Q.",
    BIO18 = "BIO18 — Precip Warmest Q.",
    BIO19 = "BIO19 — Precip Coldest Q.",
    dist_to_major_rivers    = "Dist. to Major Rivers",
    dist_to_streams         = "Dist. to Streams",
    dist_to_water_sources   = "Dist. to Water Sources",
    dist_to_roads           = "Dist. to Roads",
    dist_to_settlements     = "Dist. to Settlements",
    dist_to_protected_areas = "Dist. to Protected Areas",
    dist_to_private_land    = "Dist. to Private Land",
    human_footprint         = "Human Footprint Index",
    evi                     = "EVI (Vegetation Index)",
    landcover               = "Land Cover Class",
    dynamicworld_prob       = "DynamicWorld Veg. Prob.",
    slope                   = "Slope (degrees)",
    aspect                  = "Aspect (degrees)",
    tri                     = "Terrain Ruggedness Index",
    elevation               = "Elevation (m)"
  )
  ifelse(p %in% names(lut), lut[p], gsub("_", " ", tools::toTitleCase(p)))
}

pred_type <- function(p) {
  p <- sub("\\.contribution$", "", p)
  p <- sub(" contribution$", "", p)
  dplyr_case <- function(x) {
    if (grepl("^BIO", x, ignore.case = TRUE))                             "Climate (BIO)"
    else if (grepl("footprint", x, ignore.case = TRUE))                   "Anthropogenic"
    else if (grepl("evi|ndvi|landcover|dynamicworld", x, ignore.case = TRUE)) "Vegetation"
    else if (grepl("slope|aspect|tri|elevation", x, ignore.case = TRUE))  "Topography"
    else if (grepl("dist_", x, ignore.case = TRUE))                       "Distance"
    else "Other"
  }
  sapply(p, dplyr_case, USE.NAMES = FALSE)
}

type_colors <- c(
  "Climate (BIO)"  = "#d7191c",
  "Anthropogenic"  = "#7b3f00",
  "Vegetation"     = "#1a9641",
  "Topography"     = "#a6611a",
  "Distance"       = "#2c7bb6",
  "Other"          = "grey50"
)

algo_colors <- c(
  GLM    = "#e41a1c",
  RF     = "#377eb8",
  BRT    = "#4daf4a",
  MaxEnt = "#984ea3"
)

pub_theme <- function(base_size = 11) {
  theme_classic(base_size = base_size) +
    theme(
      text             = element_text(family = "sans"),
      plot.title       = element_text(face = "bold", size = base_size + 2, hjust = 0),
      plot.subtitle    = element_text(size = base_size - 1, colour = "grey35", hjust = 0),
      plot.caption     = element_text(size = base_size - 2, colour = "grey45", hjust = 1),
      axis.title       = element_text(size = base_size),
      axis.text        = element_text(size = base_size - 1, colour = "grey20"),
      strip.text       = element_text(size = base_size - 1, face = "bold"),
      strip.background = element_rect(fill = "#e8ecef", colour = NA),
      panel.border     = element_rect(colour = "grey70", fill = NA, linewidth = 0.4),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      legend.title     = element_text(face = "bold", size = base_size - 1),
      legend.text      = element_text(size = base_size - 1),
      legend.key.size  = unit(0.45, "cm"),
      legend.position  = "bottom",
      legend.box       = "horizontal",
      plot.margin      = margin(8, 10, 6, 8)
    )
}

save_pub <- function(p, path, w, h, dpi = 400) {
  ggsave(path, p, width = w, height = h, dpi = dpi, limitsize = FALSE)
  cat(sprintf("  Saved: %s\n", basename(path)))
}

# ── resolve run directory ─────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  run_dir <- resolve_run_dir(args[1], repo_root = repo_root)
} else {
  run_dir <- latest_run_dir(repo_root = repo_root)
}
cat(sprintf("Run directory: %s\n", run_dir))

fig_dir <- file.path(run_dir, "08_figures_tables")
mod_dir <- file.path(run_dir, "02_models")
dat_file <- file.path(run_dir, "01_processed_data", "modeling_dataset.csv")

dat <- if (file.exists(dat_file)) read.csv(dat_file, stringsAsFactors = FALSE) else NULL

# ── E07: Variable Importance ──────────────────────────────────────────────────
cat("\n=== E07: Variable Importance (publication) ===\n")

# Try reading pre-computed CSV first
imp_csv <- file.path(fig_dir, "table_E07_variable_importance.csv")
if (file.exists(imp_csv)) {
  imp_df_raw <- read.csv(imp_csv, stringsAsFactors = FALSE)
} else if (!is.null(dat)) {
  # Rebuild from models
  meta_cols <- c("id","type","response","longitude","latitude","fold",
                 "station_id","source","presence","key","row_id","X")
  pred_cols <- setdiff(names(dat), meta_cols)
  pred_cols <- pred_cols[sapply(dat[, pred_cols, drop = FALSE], is.numeric)]

  imp_list <- list()

  glm_file <- file.path(mod_dir, "model_glm.rds")
  if (file.exists(glm_file)) {
    tryCatch({
      mdl <- readRDS(glm_file)
      co  <- coef(mdl); co <- co[names(co) != "(Intercept)"]
      sds <- sapply(names(co), function(v) if (v %in% names(dat)) sd(dat[[v]], na.rm=TRUE) else 1)
      std_co <- abs(co * sds)
      imp_list[["GLM"]] <- data.frame(predictor = names(std_co),
                                       importance = as.numeric(std_co / max(std_co,na.rm=TRUE)*100),
                                       algorithm = "GLM", stringsAsFactors=FALSE)
    }, error = function(e) message("GLM: ", e$message))
  }

  rf_file <- file.path(mod_dir, "model_rf.rds")
  if (file.exists(rf_file)) {
    tryCatch({
      mdl <- readRDS(rf_file); vi <- mdl$variable.importance
      if (!is.null(vi))
        imp_list[["RF"]] <- data.frame(predictor = names(vi),
                                        importance = as.numeric(vi/max(vi,na.rm=TRUE)*100),
                                        algorithm = "RF", stringsAsFactors=FALSE)
    }, error = function(e) message("RF: ", e$message))
  }

  brt_file <- file.path(mod_dir, "model_brt.rds")
  if (file.exists(brt_file)) {
    tryCatch({
      if (!requireNamespace("gbm", quietly=TRUE)) library(gbm)
      brt_saved <- readRDS(brt_file)
      mdl <- if (is.list(brt_saved) && !is.null(brt_saved$model)) brt_saved$model else brt_saved
      best <- if (is.list(brt_saved) && !is.null(brt_saved$best_iter)) brt_saved$best_iter else mdl$n.trees
      ri_df <- tryCatch(suppressMessages(summary(mdl, plotit=FALSE)), error=function(e) NULL)
      if (!is.null(ri_df) && all(c("var","rel.inf") %in% names(ri_df)))
        imp_list[["BRT"]] <- data.frame(predictor = ri_df$var,
                                         importance = as.numeric(ri_df$rel.inf/max(ri_df$rel.inf,na.rm=TRUE)*100),
                                         algorithm = "BRT", stringsAsFactors=FALSE)
    }, error = function(e) message("BRT: ", e$message))
  }

  mx_results <- file.path(mod_dir, "maxent_output", "maxentResults.csv")
  if (file.exists(mx_results)) {
    tryCatch({
      mx_df  <- read.csv(mx_results, stringsAsFactors=FALSE)
      mx_row <- mx_df[nrow(mx_df), , drop=FALSE]
      contrib_cols <- grep("contribution$", names(mx_row), value=TRUE)
      if (length(contrib_cols) > 0) {
        mx_preds <- sub("[ .]contribution$", "", contrib_cols)
        mx_vals  <- as.numeric(mx_row[contrib_cols])
        mx_max   <- max(mx_vals, na.rm=TRUE)
        if (!is.na(mx_max) && mx_max > 0)
          imp_list[["MaxEnt"]] <- data.frame(predictor = mx_preds,
                                              importance = mx_vals/mx_max*100,
                                              algorithm = "MaxEnt", stringsAsFactors=FALSE)
      }
    }, error = function(e) message("MaxEnt: ", e$message))
  }

  imp_df_raw <- if (length(imp_list) > 0) do.call(rbind, imp_list) else NULL
} else {
  imp_df_raw <- NULL
}

if (!is.null(imp_df_raw) && nrow(imp_df_raw) > 0) {
  imp_df <- imp_df_raw

  # Clean predictor names — strip .contribution suffix (MaxEnt artifact)
  imp_df$predictor <- sub("\\.contribution$", "", imp_df$predictor)
  imp_df$predictor <- sub(" contribution$",  "", imp_df$predictor)

  # Deduplicate: keep max importance per predictor × algorithm
  imp_df <- aggregate(importance ~ predictor + algorithm, data = imp_df,
                      FUN = function(x) max(x, na.rm = TRUE))

  # Add readable label and type
  imp_df$label    <- pred_label(imp_df$predictor)
  imp_df$pred_type <- pred_type(imp_df$predictor)

  # Consistent algorithm order
  algo_order <- intersect(c("BRT","GLM","MaxEnt","RF"), unique(imp_df$algorithm))
  imp_df$algorithm <- factor(imp_df$algorithm, levels = algo_order)

  # Compute mean importance for ordering; only keep predictors present in >=1 algorithm
  mean_imp <- tapply(imp_df$importance, imp_df$predictor, mean, na.rm = TRUE)
  pred_order <- names(sort(mean_imp, decreasing = FALSE))
  imp_df$label <- factor(imp_df$label, levels = pred_label(pred_order))

  # Canonical top-N by mean importance per algorithm panel (keep all, sorted)
  n_alg <- length(algo_order)

  p_e07 <- ggplot(imp_df, aes(x = label, y = importance, fill = pred_type)) +
    geom_col(width = 0.65, colour = NA) +
    geom_text(aes(label = sprintf("%.0f", importance)),
              hjust = -0.15, size = 2.8, colour = "grey20") +
    scale_fill_manual(values = type_colors,
                      name = "Predictor category",
                      guide = guide_legend(nrow = 1, override.aes = list(size = 4))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                       breaks = c(0, 25, 50, 75, 100),
                       labels = c("0","25","50","75","100")) +
    coord_flip() +
    facet_wrap(~ algorithm, nrow = 1, scales = "free_x") +
    labs(
      title    = "Variable importance by algorithm",
      subtitle = "Relative importance normalised to 100 | Metric: permutation (RF), relative influence (BRT),\nstandardised coefficient (GLM), percent contribution (MaxEnt)",
      x = NULL,
      y = "Relative importance (%)",
      caption = "Elephas maximus SDM — Bhutan | Ensemble: RF + BRT + GLM + MaxEnt"
    ) +
    pub_theme(base_size = 10) +
    theme(
      axis.text.y   = element_text(size = 8.5, colour = "grey15"),
      axis.text.x   = element_text(size = 8),
      panel.spacing = unit(0.7, "lines"),
      legend.margin = margin(t = 2)
    )

  save_pub(p_e07,
           file.path(fig_dir, "figure_E07_variable_importance_pub.png"),
           w = 4.5 * n_alg + 1.5, h = max(7, 0.28 * length(pred_order) + 3),
           dpi = 400)
} else {
  cat("  No importance data found — skipping E07.\n")
}

# ── E06: Response Curves ──────────────────────────────────────────────────────
cat("\n=== E06: Response Curves (publication) ===\n")

if (!is.null(dat)) {
  meta_cols <- c("id","type","response","longitude","latitude","fold",
                 "station_id","source","presence","key","row_id","X")
  pred_cols <- setdiff(names(dat), meta_cols)
  pred_cols <- pred_cols[sapply(dat[, pred_cols, drop = FALSE], is.numeric)]

  # Load models
  algo_models    <- list()
  algo_best_iter <- list()

  glm_f <- file.path(mod_dir, "model_glm.rds")
  if (file.exists(glm_f))
    algo_models[["GLM"]] <- tryCatch(readRDS(glm_f), error = function(e) NULL)

  rf_f <- file.path(mod_dir, "model_rf.rds")
  if (file.exists(rf_f))
    algo_models[["RF"]] <- tryCatch(readRDS(rf_f), error = function(e) NULL)

  brt_f <- file.path(mod_dir, "model_brt.rds")
  if (file.exists(brt_f)) {
    tryCatch({
      brt_raw <- readRDS(brt_f)
      if (is.list(brt_raw) && !is.null(brt_raw$model)) {
        algo_models[["BRT"]]    <- brt_raw$model
        algo_best_iter[["BRT"]] <- if (!is.null(brt_raw$best_iter)) brt_raw$best_iter else brt_raw$model$n.trees
      } else {
        algo_models[["BRT"]]    <- brt_raw
        algo_best_iter[["BRT"]] <- brt_raw$n.trees
      }
    }, error = function(e) NULL)
  }

  # MaxEnt via maxnet if available
  mx_rds <- file.path(mod_dir, "model_maxent.rds")
  if (!file.exists(mx_rds)) mx_rds <- file.path(mod_dir, "maxent_output", "model_maxent.rds")
  if (file.exists(mx_rds)) {
    algo_models[["MaxEnt"]] <- tryCatch(readRDS(mx_rds), error = function(e) NULL)
  }

  dat_num <- dat[, pred_cols, drop = FALSE]
  ref_row  <- as.data.frame(lapply(dat_num, median, na.rm = TRUE))

  pd_rows <- list()
  for (pc in pred_cols) {
    pc_range <- seq(quantile(dat[[pc]], 0.02, na.rm = TRUE),
                    quantile(dat[[pc]], 0.98, na.rm = TRUE),
                    length.out = 80)
    for (algo in names(algo_models)) {
      mdl <- algo_models[[algo]]
      if (is.null(mdl)) next
      preds <- tryCatch({
        newdat <- ref_row[rep(1, length(pc_range)), , drop = FALSE]
        newdat[[pc]] <- pc_range
        if (algo == "GLM") {
          predict(mdl, newdat, type = "response")
        } else if (algo == "RF") {
          p_rf <- predict(mdl, data = newdat)
          if (!is.null(p_rf$predictions) && !is.null(ncol(p_rf$predictions)) && ncol(p_rf$predictions) >= 2)
            p_rf$predictions[, 2]
          else as.numeric(p_rf$predictions)
        } else if (algo == "BRT") {
          if (!requireNamespace("gbm", quietly = TRUE)) library(gbm)
          n_tr <- algo_best_iter[["BRT"]]
          if (is.null(n_tr)) n_tr <- mdl$n.trees
          predict(mdl, newdat, n.trees = n_tr, type = "response")
        } else if (algo == "MaxEnt") {
          if (inherits(mdl, "maxnet")) {
            if (!requireNamespace("maxnet", quietly = TRUE)) library(maxnet)
            predict(mdl, newdat, type = "cloglog")
          }
        }
      }, error = function(e) NULL)
      if (!is.null(preds) && length(preds) == length(pc_range)) {
        pd_rows[[length(pd_rows) + 1]] <- data.frame(
          predictor = pc, value = pc_range, suitability = as.numeric(preds),
          algorithm = algo, stringsAsFactors = FALSE)
      }
    }
  }

  if (length(pd_rows) > 0) {
    pd_df <- do.call(rbind, pd_rows)
    pd_df$algorithm <- factor(pd_df$algorithm, levels = intersect(c("GLM","RF","BRT","MaxEnt"),
                                                                    unique(pd_df$algorithm)))
    pd_df$pred_type  <- pred_type(pd_df$predictor)
    pd_df$pred_label <- factor(pred_label(pd_df$predictor),
                                levels = unique(pred_label(pred_cols)))

    # Rug: presence locations
    rug_df   <- dat[dat$response == 1, pred_cols, drop = FALSE]
    rug_long <- do.call(rbind, lapply(pred_cols, function(pc)
      data.frame(predictor  = pc,
                 pred_label = pred_label(pc),
                 value      = rug_df[[pc]],
                 stringsAsFactors = FALSE)))
    rug_long$pred_label <- factor(rug_long$pred_label, levels = levels(pd_df$pred_label))

    # strip fill by predictor type
    strip_colors <- setNames(
      alpha(type_colors[pred_type(pred_cols)], 0.35),
      pred_label(pred_cols)
    )

    ncol_f <- 4
    nrow_f <- ceiling(length(pred_cols) / ncol_f)

    p_e06 <- ggplot(pd_df, aes(x = value, y = suitability, colour = algorithm)) +
      geom_line(linewidth = 0.9, alpha = 0.92) +
      geom_rug(data  = rug_long, aes(x = value), sides = "b",
               inherit.aes = FALSE, alpha = 0.25, linewidth = 0.35,
               length = unit(0.025, "npc")) +
      scale_colour_manual(values = algo_colors,
                          name   = "Algorithm",
                          guide  = guide_legend(nrow = 1,
                                                override.aes = list(linewidth = 1.5))) +
      scale_x_continuous(n.breaks = 4, labels = label_number(big.mark = ",")) +
      scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1),
                         labels = c("0","0.5","1")) +
      facet_wrap(~ pred_label, scales = "free_x", ncol = ncol_f) +
      labs(
        title    = "Partial-dependence response curves",
        subtitle = "Other predictors held at median | Rug marks = presence locations",
        x = "Predictor value",
        y = "Predicted suitability",
        caption = "Elephas maximus SDM — Bhutan | GLM + RF + BRT (+ MaxEnt where available)"
      ) +
      pub_theme(base_size = 9.5) +
      theme(
        strip.text   = element_text(size = 7.5, face = "bold"),
        axis.text    = element_text(size = 7),
        axis.text.x  = element_text(angle = 30, hjust = 1),
        panel.spacing = unit(0.5, "lines")
      )

    save_pub(p_e06,
             file.path(fig_dir, "figure_E06_response_curves_pub.png"),
             w = 5.5 * ncol_f, h = 3.8 * nrow_f + 1.2,
             dpi = 400)
  } else {
    cat("  No partial-dependence data — skipping E06.\n")
  }
} else {
  cat("  modeling_dataset.csv not found — skipping E06.\n")
}

cat("\nDone.\n")
