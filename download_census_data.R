# download_census_data.R
# Downloads census tract and city child poverty data, performs spatial analysis,
# and exports to Databricks-compatible Parquet format.

# =============================================================================
# SETUP
# =============================================================================

# Install packages if needed (uncomment if first run)
# install.packages(c("tidycensus", "tidyverse", "sf", "arrow", "logger"))

library(tidycensus)
library(tidyverse)
library(sf)
library(arrow)

# Source logging utilities and initialize
source("R/logging.R")
init_logging("download_census_data")
script_start_time <- Sys.time()

# Census API key should be set in ~/.Renviron
# Run once: census_api_key("YOUR_KEY", install = TRUE)

#' Clear the download cache to force fresh downloads
#' Run this if you need to re-download all data
clear_cache <- function() {
  if (dir.exists("cache")) {
    unlink("cache", recursive = TRUE)
    log_info("Cache cleared. Next run will download all data fresh.")
  } else {
    log_info("No cache directory found.")
  }
}

# =============================================================================
# CONFIGURATION
# =============================================================================

# Cache directory for intermediate results (enables resume on failure)
cache_dir <- "cache"
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir)
}

# ACS variables for child poverty analysis
acs_variables <- c(
  total_children = "B09001_001",      # Total population under 18
  poverty_male = "B17001_004",        # Male under 18 below poverty
  poverty_female = "B17001_018",      # Female under 18 below poverty
  total_pop = "B01003_001"            # Total population (for city filtering)
)

# All state + territory FIPS codes
# Note: Only including territories with ACS 5-year data available
state_fips <- c(
  # 50 states + DC
  "01", "02", "04", "05", "06", "08", "09", "10", "11", "12",
  "13", "15", "16", "17", "18", "19", "20", "21", "22", "23",
  "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
  "34", "35", "36", "37", "38", "39", "40", "41", "42", "44",
  "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56",
  # Territories with ACS 5-year data
  "72"   # Puerto Rico
  # Excluded: 60 (American Samoa), 66 (Guam), 69 (N. Mariana Islands), 78 (Virgin Islands)
  # - these don't have ACS 5-year tract-level estimates
)

# Minimum city population threshold
min_city_pop <- 100000

# =============================================================================
# DATA DOWNLOAD FUNCTIONS
# =============================================================================

#' Download census tract data for a single state
download_tracts_state <- function(state_fips, variables, year = 2023) {
  tryCatch({
    get_acs(
      geography = "tract",
      variables = variables,
      state = state_fips,
      year = year,
      survey = "acs5",
      geometry = TRUE,
      output = "wide"
    )
  }, error = function(e) {
    log_warn("Failed to download tracts for state {state_fips}: {e$message}")
    return(NULL)
  })
}

#' Download place (city) data for a single state
download_places_state <- function(state_fips, variables, year = 2023) {
  tryCatch({
    get_acs(
      geography = "place",
      variables = variables,
      state = state_fips,
      year = year,
      survey = "acs5",
      geometry = TRUE,
      output = "wide"
    )
  }, error = function(e) {
    log_warn("Failed to download places for state {state_fips}: {e$message}")
    return(NULL)
  })
}

#' Download data for all states/territories with progress tracking and caching
#' @param data_type Either "tracts" or "places" - used for cache file naming
download_all_states <- function(download_fn, variables, data_type, year = 2023) {
  total_states <- length(state_fips)
  results <- vector("list", total_states)

  # Count cached vs to-download
  cached_count <- sum(sapply(state_fips, function(fips) {
    file.exists(file.path(cache_dir, paste0(data_type, "_", fips, ".rds")))
  }))

  if (cached_count > 0) {
    log_info("Found {cached_count} cached states, {total_states - cached_count} remaining to download")
  }

  for (i in seq_along(state_fips)) {
    fips <- state_fips[i]
    cache_file <- file.path(cache_dir, paste0(data_type, "_", fips, ".rds"))

    if (file.exists(cache_file)) {
      # Load from cache
      log_info("[{i}/{total_states}] ({round((i / total_states) * 100)}%) Loading cached: {fips}")
      results[[i]] <- readRDS(cache_file)
    } else {
      # Download and cache
      log_info("[{i}/{total_states}] ({round((i / total_states) * 100)}%) Downloading: {fips}")
      results[[i]] <- download_fn(fips, variables, year)

      if (!is.null(results[[i]])) {
        saveRDS(results[[i]], cache_file)
      }
    }
  }

  # Remove NULLs and bind rows
  results <- results[!sapply(results, is.null)]

  if (length(results) == 0) {
    stop("No data downloaded successfully")
  }

  bind_rows(results)
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

# Check for Census API key
if (Sys.getenv("CENSUS_API_KEY") == "") {
  log_error("Census API key not found. Please run: census_api_key('YOUR_KEY', install = TRUE)")
  stop("Census API key not found.")
}

log_info("Starting census data download...")

# Download tract data
log_info("=== Downloading Census Tract Data ===")
tracts_raw <- download_all_states(download_tracts_state, acs_variables, "tracts")

# Download place data
log_info("=== Downloading Place (City) Data ===")
places_raw <- download_all_states(download_places_state, acs_variables, "places")

# =============================================================================
# DATA PROCESSING
# =============================================================================

log_info("=== Processing Data ===")

log_info("[1/3] Processing tract data...")
# Process tract data: calculate total children in poverty
tracts <- tracts_raw %>%
  mutate(
    children_poverty = poverty_maleE + poverty_femaleE,
    children_poverty_moe = sqrt(poverty_maleM^2 + poverty_femaleM^2)
  ) %>%
  select(
    GEOID,
    NAME,
    total_children = total_childrenE,
    total_children_moe = total_childrenM,
    children_poverty,
    children_poverty_moe,
    geometry
  )

log_info("[2/3] Filtering cities to 100k+ population...")
# Process place data: filter to 100k+ population cities
cities <- places_raw %>%
  filter(total_popE >= min_city_pop) %>%
  mutate(
    children_poverty = poverty_maleE + poverty_femaleE,
    children_poverty_moe = sqrt(poverty_maleM^2 + poverty_femaleM^2)
  ) %>%
  select(
    GEOID,
    NAME,
    total_pop = total_popE,
    total_pop_moe = total_popM,
    total_children = total_childrenE,
    total_children_moe = total_childrenM,
    children_poverty,
    children_poverty_moe,
    geometry
  )

log_info("[3/3] Data processing complete")
log_info("Cities with 100k+ population: {nrow(cities)}")
log_info("Total tracts to process: {nrow(tracts)}")

# =============================================================================
# SPATIAL ANALYSIS: ASSIGN TRACTS TO CITIES
# =============================================================================

log_info("=== Performing Spatial Analysis ===")

# Ensure both datasets use the same CRS
tracts <- st_transform(tracts, 4326)
cities <- st_transform(cities, 4326)

# Validate geometries
tracts <- st_make_valid(tracts)
cities <- st_make_valid(cities)

# Find intersections between tracts and cities
# Process by city with progress bar for better feedback
log_info("Finding tract-city intersections...")
total_cities <- nrow(cities)
all_intersections <- vector("list", total_cities)

pb <- txtProgressBar(min = 0, max = total_cities, style = 3, width = 50)
tracts_subset <- tracts %>% select(tract_geoid = GEOID, tract_name = NAME)

for (i in seq_len(total_cities)) {
  city <- cities[i, ] %>% select(city_geoid = GEOID, city_name = NAME)
  city_intersections <- suppressWarnings(
    st_intersection(tracts_subset, city)
  )
  if (nrow(city_intersections) > 0) {
    all_intersections[[i]] <- city_intersections
  }
  setTxtProgressBar(pb, i)
}
close(pb)

# Combine all intersections
intersections <- bind_rows(all_intersections)
log_info("Found {nrow(intersections)} tract-city intersections")

log_info("Calculating intersection areas...")
# Calculate intersection areas
intersections <- intersections %>%
  mutate(intersection_area = st_area(geometry))

log_info("Calculating tract areas for coverage percentage...")
# Calculate tract areas for coverage percentage
tract_areas <- tracts %>%
  mutate(tract_area = st_area(geometry)) %>%
  st_drop_geometry() %>%
  select(tract_geoid = GEOID, tract_area)

log_info("Assigning primary city to each tract...")
# Find primary city for each tract (largest intersection area)
tract_city_assignments <- intersections %>%
  st_drop_geometry() %>%
  left_join(tract_areas, by = "tract_geoid") %>%
  mutate(coverage_pct = as.numeric(intersection_area / tract_area) * 100) %>%
  group_by(tract_geoid) %>%
  slice_max(intersection_area, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(
    tract_geoid,
    primary_city_geoid = city_geoid,
    primary_city_name = city_name,
    city_coverage_pct = coverage_pct
  )

log_info("Joining city assignments and filtering tracts...")
# Join city assignments back to tracts
tracts_with_city <- tracts %>%
  left_join(tract_city_assignments, by = c("GEOID" = "tract_geoid"))

# Filter to only tracts that intersect a 100k+ city
tracts_final <- tracts_with_city %>%
  filter(!is.na(primary_city_geoid))

log_info("Tracts intersecting 100k+ cities: {nrow(tracts_final)}")

# =============================================================================
# EXPORT TO PARQUET
# =============================================================================

log_info("=== Exporting to Parquet ===")

# Convert geometry to WKT for Databricks compatibility
tracts_export <- tracts_final %>%
  mutate(geometry_wkt = st_as_text(geometry)) %>%
  st_drop_geometry()

cities_export <- cities %>%
  mutate(geometry_wkt = st_as_text(geometry)) %>%
  st_drop_geometry()

# Write Parquet files
write_parquet(tracts_export, "census_tracts_children_poverty.parquet")
write_parquet(cities_export, "cities_100k_children_poverty.parquet")

log_info("=== Export Complete ===")
log_info("Created: census_tracts_children_poverty.parquet")
log_info("Created: cities_100k_children_poverty.parquet")

# =============================================================================
# VERIFICATION
# =============================================================================

log_info("=== Verification ===")
log_info("Total tracts exported: {nrow(tracts_export)}")
log_info("Total cities exported: {nrow(cities_export)}")

# Check geometry validity
invalid_tracts <- sum(!st_is_valid(tracts_final))
invalid_cities <- sum(!st_is_valid(cities))

if (invalid_tracts > 0) {
  log_warn("Found {invalid_tracts} invalid tract geometries")
} else {
  log_info("All tract geometries valid")
}

if (invalid_cities > 0) {
  log_warn("Found {invalid_cities} invalid city geometries")
} else {
  log_info("All city geometries valid")
}

# Test reading back the Parquet files
tracts_test <- read_parquet("census_tracts_children_poverty.parquet")
cities_test <- read_parquet("cities_100k_children_poverty.parquet")
log_info("Parquet read test - Tracts: {nrow(tracts_test)} rows")
log_info("Parquet read test - Cities: {nrow(cities_test)} rows")

# Log session completion
log_session_end(script_start_time, success = TRUE)
