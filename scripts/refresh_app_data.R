# refresh_app_data.R
# Bundles enriched census data for Shiny app deployment to shinyapps.io
#
# Run this script after running R/prepare_data_for_databricks.R
# The bundled data will be included with the Shiny app for offline access

library(arrow)
library(fs)

source("R/logging.R")
init_logging("refresh_app_data")

log_info("=== Bundling Data for Shiny App ===")

# =============================================================================
# CONFIGURATION
# =============================================================================

# Source files (enriched data)
TRACTS_SOURCE <- "census_tracts_children_poverty_enriched.parquet"
CITIES_SOURCE <- "cities_100k_children_poverty_enriched.parquet"

# Destination (app data folder)
APP_DATA_DIR <- "app/data"

# =============================================================================
# CHECK SOURCE FILES
# =============================================================================

if (!file.exists(TRACTS_SOURCE)) {
  log_error("Source file not found: {TRACTS_SOURCE}")
  log_info("Run R/prepare_data_for_databricks.R first to create enriched data")
  stop("Missing source file")
}

if (!file.exists(CITIES_SOURCE)) {
  log_error("Source file not found: {CITIES_SOURCE}")
  log_info("Run R/prepare_data_for_databricks.R first to create enriched data")
  stop("Missing source file")
}

# =============================================================================
# CREATE APP DATA DIRECTORY
# =============================================================================

if (!dir.exists(APP_DATA_DIR)) {
  dir_create(APP_DATA_DIR)
  log_info("Created directory: {APP_DATA_DIR}")
}

# =============================================================================
# COPY DATA TO APP
# =============================================================================

log_info("Copying data to app/data/...")

# Copy tracts
tracts_dest <- file.path(APP_DATA_DIR, "tracts.parquet")
file_copy(TRACTS_SOURCE, tracts_dest, overwrite = TRUE)
log_info("Copied: {TRACTS_SOURCE} -> {tracts_dest}")

# Copy cities
cities_dest <- file.path(APP_DATA_DIR, "cities.parquet")
file_copy(CITIES_SOURCE, cities_dest, overwrite = TRUE)
log_info("Copied: {CITIES_SOURCE} -> {cities_dest}")

# =============================================================================
# VERIFY
# =============================================================================

log_info("=== Verification ===")

tracts <- read_parquet(tracts_dest)
cities <- read_parquet(cities_dest)

log_info("Bundled tracts: {nrow(tracts)} rows ({round(file.size(tracts_dest) / 1024 / 1024, 1)} MB)")
log_info("Bundled cities: {nrow(cities)} rows ({round(file.size(cities_dest) / 1024 / 1024, 1)} MB)")

log_info("=== App Data Bundle Complete ===")
log_info("Ready for deployment to shinyapps.io")
