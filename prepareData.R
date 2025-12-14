library(sf)
library(tidyverse)
library(giscoR)
library(here)
library(janitor)
library(lwgeom) # For some geometry operations if needed
library(glue)

# 1. Setup & Configuration ----------------------------------------------------
# Directory setup
dir_output <- here("static/geodata")
if (!dir.exists(dir_output)) {
  dir.create(dir_output, recursive = TRUE)
}

# Target aspect ratio for mobile (9:16 portrait)
val_target_ar <- 9 / 16

# Function to calculate metrics
fn_calculate_metrics <- function(sf_poly) {
  # Ensure single polygon (take largest if multipolygon)
  if (st_geometry_type(sf_poly) %in% c("MULTIPOLYGON", "GEOMETRYCOLLECTION")) {
    sf_poly <- sf_poly %>%
      st_cast("POLYGON") %>%
      mutate(area_tmp = st_area(.)) %>%
      filter(area_tmp == max(area_tmp)) %>%
      select(-area_tmp)
  }

  # 1. Roundness (Area / Min Bounding Circle Area)
  sf_circle <- st_minimum_bounding_circle(sf_poly)
  val_area_poly <- as.numeric(st_area(sf_poly))
  val_area_circle <- as.numeric(st_area(sf_circle))
  val_roundness <- (val_area_poly / val_area_circle) * 100

  # 2. Rectangularity (Area / Min Rotated Rectangle Area)
  # st_minimum_rotated_rectangle is available in sf >= 1.0.0
  sf_mrr <- st_minimum_rotated_rectangle(sf_poly)
  val_area_mrr <- as.numeric(st_area(sf_mrr))
  val_rectangularity <- (val_area_poly / val_area_mrr) * 100

  # 3. Mobile Fit (Aspect Ratio match - original rotation)
  # Use standard bounding box for original rotation fit
  bbox <- st_bbox(sf_poly)
  val_width <- as.numeric(bbox["xmax"] - bbox["xmin"])
  val_height <- as.numeric(bbox["ymax"] - bbox["ymin"])

  val_ar <- val_width / val_height

  # Fit score: How close is AR to 9:16?
  # 100% = perfect match, decreasing as it deviates
  val_mobile_fit <- (1 - abs(val_ar - val_target_ar)) * 100

  # Geometries for visual verification (optional, keeping minimal for JSON size)
  # We return the metrics attached to the original polygon
  sf_poly %>%
    mutate(
      metric_roundness = round(val_roundness, 2),
      metric_rectangularity = round(val_rectangularity, 2),
      metric_mobile_fit = round(val_mobile_fit, 2),
      metric_aspect_ratio = round(val_ar, 3),
      metric_area_km2 = round(val_area_poly / 1e6, 2)
    )
}

# Function to save individual feature and update index
fn_process_and_save <- function(
  sf_data,
  txt_category,
  txt_name_col,
  txt_id_col = NULL
) {
  cli::cli_h1(glue("Processing: {txt_category}"))

  # Create category directory
  dir_cat <- file.path(dir_output, txt_category)
  if (!dir.exists(dir_cat)) {
    dir.create(dir_cat)
  }

  # Ensure data is in 3857 for metric calculation (meters)
  sf_data_proj <- st_transform(sf_data, 3857)

  # Iterate through features
  # Using a loop instead of purrr to easily manage file writes and skipping

  vec_names <- sf_data_proj[[txt_name_col]]
  n_total <- length(vec_names)

  # Prepare index list to write later
  lst_index_entries <- list()

  for (i in seq_len(nrow(sf_data_proj))) {
    if (i %% 10 == 0) {
      cat(glue("\rProcessing {i}/{n_total}"))
    }

    sf_feat <- sf_data_proj[i, ]
    txt_name <- vec_names[i]

    # Skip if name is NA
    if (is.na(txt_name)) {
      next
    }

    # Calculate metrics
    # Try-catch for geometry errors
    sf_result <- tryCatch(
      {
        fn_calculate_metrics(sf_feat)
      },
      error = function(e) {
        warning(glue("Failed to calc metrics for {txt_name}: {e$message}"))
        return(NULL)
      }
    )

    if (is.null(sf_result)) {
      next
    }

    # Prepare metadata for export
    # Transform back to 4326 for web storage (geojson standard)
    sf_export <- sf_result %>%
      st_transform(4326) %>%
      select(
        name = all_of(txt_name_col),
        starts_with("metric_")
      )

    # Determine output path based on first 2 letters
    txt_name_clean <- janitor::make_clean_names(txt_name)
    txt_prefix <- tolower(substr(txt_name_clean, 1, 2))

    # Handle short names or empty
    if (nchar(txt_prefix) < 2) {
      txt_prefix <- "xx"
    }

    dir_prefix <- file.path(dir_cat, txt_prefix)
    if (!dir.exists(dir_prefix)) {
      dir.create(dir_prefix, recursive = TRUE)
    }

    path_file <- file.path(dir_prefix, glue("{txt_name_clean}.geojson"))

    # Save GeoJSON
    if (!file.exists(path_file)) {
      write_sf(sf_export, path_file, delete_dsn = TRUE, quiet = TRUE)
    }

    # Add to index list
    lst_index_entries[[length(lst_index_entries) + 1]] <- data.frame(
      category = txt_category,
      name = txt_name,
      slug = txt_name_clean,
      prefix = txt_prefix,
      roundness = sf_export$metric_roundness,
      rectangularity = sf_export$metric_rectangularity,
      mobile_fit = sf_export$metric_mobile_fit
    )
  }

  cat("\nWriting indices...\n")

  # Combine index
  if (length(lst_index_entries) > 0) {
    df_index <- bind_rows(lst_index_entries)

    # Split by prefix and append/write to index files
    # We want one index file per prefix folder for frontend search optimization

    df_index_split <- split(df_index, df_index$prefix)

    walk2(df_index_split, names(df_index_split), function(df_sub, prefix) {
      path_index <- file.path(dir_cat, prefix, "index.csv")

      # If index exists, read it, bind, unique, write back (to support incremental runs if needed)
      # For a clean run, we could just write.
      # Since we process linearly, overwriting per run is cleaner if we assume full re-run.
      # But to be safe with partial updates, let's append if file exists?
      # Actually, simple is better: Overwrite for now, or append if distinct.

      # Let's just write the CSV for this batch.
      # Ideally we would merge with existing on disk if we run this script incrementally.
      # Given the prompt, let's assume we might re-run.

      if (file.exists(path_index)) {
        df_existing <- read_csv(path_index, show_col_types = FALSE)
        df_combined <- bind_rows(df_existing, df_sub) %>%
          distinct(slug, .keep_all = TRUE)
        write_csv(df_combined, path_index)
      } else {
        write_csv(df_sub, path_index)
      }
    })
  }
}

# 2. Data Processing Sections -------------------------------------------------

# --- A. World Countries ---
cli::cli_h2("Fetching World Countries")
sf_countries <- gisco_get_countries(resolution = "01") %>%
  filter(!is.na(NAME_ENGL))

# add all params with names
fn_process_and_save(
  sf_data = sf_countries,
  txt_category = "world_countries",
  txt_name_col = "NAME_ENGL"
)


# --- B1. Europe Cities (Urban Audit) ---
# cli::cli_h2("Fetching Europe Cities (Urban Audit)")
# sf_urban <- gisco_get_urban_audit(year = "2021", level = "CITIES") %>%
#   filter(!is.na(URAU_NAME))
#
# fn_process_and_save(
#   sf_data = sf_urban,
#   txt_category = "europe_cities",
#   txt_name_col = "URAU_NAME"
# )

# --- B2. Europe LAU (Local Administrative Units) ---
cli::cli_h2("Fetching Europe LAU (Local Administrative Units)")
# LAU are the building blocks of the NUTS regions, comparable to municipalities
# NOTE: This is a very large dataset if downloading for all of EU!
# Downloading for year 2021 to match other data
sf_cities <- gisco_get_lau(year = "2021") %>%
  filter(!is.na(LAU_NAME))

fn_process_and_save(
  sf_data = sf_cities,
  txt_category = "europe_lau",
  txt_name_col = "LAU_NAME"
)


# --- C. Austrian Gemeinden (Municipalities) ---
cli::cli_h2("Fetching Austrian Municipalities")
# Using gisco_get_communes specifically for Austria
sf_at_communes <- davR::at_get_gemeinden(2025)

fn_process_and_save(sf_at_communes, "austria_municipalities", "g_name")


cli::cli_alert_success("Processing Complete!")
