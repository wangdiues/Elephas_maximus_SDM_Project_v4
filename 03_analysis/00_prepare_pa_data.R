# =============================================================================
# 00_prepare_pa_data.R
# Purpose : Combine two camera-trap datasets and produce a binary
#           presence-absence (PA) CSV for Elephas maximus SDM.
#
# Inputs  :
#   01_data_raw/01_occurrences/elephant_presence_absence_by_station.csv
#     - One row per camera station; 'elephant_presence' column is already 0/1.
#
#   01_data_raw/01_occurrences/Book1.csv
#     - Wide occasion matrix (columns 1-90 = trap nights); values are elephant
#       detection counts (0, positive integer, or NA = camera inactive).
#     - NOTE: 'x' column holds latitude (~26-28°N) and 'y' holds longitude
#       (~88-92°E) — non-standard naming, corrected here.
#
# Output  :
#   01_data_raw/01_occurrences/elephant_PA_combined.csv
#     - Columns: station_id, longitude, latitude, presence, source
#     - Spatial duplicates (same lon/lat) are resolved by keeping the record
#       with the higher presence value (presence wins over absence).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

repo_root <- "."

occ_dir  <- file.path(repo_root, "01_data_raw", "01_occurrences")
file1    <- file.path(occ_dir, "elephant_presence_absence_by_station.csv")
file2    <- file.path(occ_dir, "Book1.csv")
out_file <- file.path(occ_dir, "elephant_PA_combined.csv")

# Rough bounding box for Bhutan + buffer (filters clearly erroneous coords)
LAT_MIN <- 20; LAT_MAX <- 35
LON_MIN <- 80; LON_MAX <- 98

# =============================================================================
# SOURCE 1 — elephant_presence_absence_by_station.csv
# =============================================================================
cat("Reading File 1:", file1, "\n")
sta_raw <- read_csv(file1, show_col_types = FALSE)

sta_pa <- sta_raw |>
  filter(
    !is.na(latitude),
    !is.na(longitude),
    latitude  > LAT_MIN, latitude  < LAT_MAX,
    longitude > LON_MIN, longitude < LON_MAX,
    !is.na(elephant_presence)
  ) |>
  transmute(
    station_id = station_id,
    longitude  = longitude,
    latitude   = latitude,
    presence   = as.integer(elephant_presence),
    source     = "station_survey"
  )

cat(sprintf("  Source 1: %d stations retained (presences: %d, absences: %d)\n",
            nrow(sta_pa), sum(sta_pa$presence == 1), sum(sta_pa$presence == 0)))

# =============================================================================
# SOURCE 2 — Book1.csv (occasion matrix)
# =============================================================================
cat("Reading File 2:", file2, "\n")
book_raw <- read_csv(file2, show_col_types = FALSE)

# Identify occasion columns (purely numeric names: "1", "2", ..., "90")
occasion_cols <- names(book_raw)[grepl("^[0-9]+$", names(book_raw))]
cat(sprintf("  Book1: %d stations, %d occasion columns\n",
            nrow(book_raw), length(occasion_cols)))

# Derive binary PA per station:
#   - n_valid = number of non-NA occasion cells (active trap nights)
#   - total   = sum of all detection counts
#   - presence = 1 if total > 0, 0 if total == 0 and n_valid > 0, NA if all NA
book_pa <- book_raw |>
  rowwise() |>
  mutate(
    n_valid  = sum(!is.na(c_across(all_of(occasion_cols)))),
    total    = sum(c_across(all_of(occasion_cols)), na.rm = TRUE),
    presence = case_when(
      n_valid == 0   ~ NA_integer_,       # all NA → camera never active, exclude
      total   >  0   ~ 1L,                # at least one detection
      TRUE           ~ 0L                 # active but no detections
    )
  ) |>
  ungroup() |>
  # In Book1.csv: x = latitude (~26-28), y = longitude (~88-92)
  rename(latitude = x, longitude = y) |>
  filter(
    !is.na(presence),
    latitude  > LAT_MIN, latitude  < LAT_MAX,
    longitude > LON_MIN, longitude < LON_MAX
  ) |>
  transmute(
    station_id = station,
    longitude  = longitude,
    latitude   = latitude,
    presence   = presence,
    source     = "book1_survey"
  )

cat(sprintf("  Source 2: %d stations retained (presences: %d, absences: %d)\n",
            nrow(book_pa), sum(book_pa$presence == 1), sum(book_pa$presence == 0)))

# =============================================================================
# COMBINE AND DEDUPLICATE
# =============================================================================
pa_combined <- bind_rows(sta_pa, book_pa)

# Spatial deduplication: if two records share the same lon/lat, keep the one
# with higher presence value (so a detected presence is never overridden by an
# absence from the other dataset).
pa_final <- pa_combined |>
  group_by(longitude, latitude) |>
  slice_max(order_by = presence, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(desc(presence), longitude, latitude)

n_removed <- nrow(pa_combined) - nrow(pa_final)

cat("\n=== Final PA Dataset ===\n")
cat(sprintf("  Combined (pre-dedup) : %d records\n", nrow(pa_combined)))
cat(sprintf("  Spatial duplicates   : %d removed\n", n_removed))
cat(sprintf("  Final records        : %d\n", nrow(pa_final)))
cat(sprintf("  Presences (1)        : %d\n", sum(pa_final$presence == 1)))
cat(sprintf("  Absences  (0)        : %d\n", sum(pa_final$presence == 0)))
cat("\n  Records by source:\n")
print(as.data.frame(table(pa_final$source, pa_final$presence,
                           dnn = c("source", "presence"))))

# =============================================================================
# SAVE
# =============================================================================
write_csv(pa_final, out_file)
cat(sprintf("\nSaved: %s\n", out_file))
