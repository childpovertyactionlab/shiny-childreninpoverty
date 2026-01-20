# upload_to_databricks.R
# Script to upload enriched census data to Databricks
#
# NOTE: Data is currently stored in sandbox.michael for development.
# TODO: Move to a production schema (e.g., prod.census or prod.child_poverty)
#       before deploying to production.
#
# This script uploads parquet files to a Databricks Unity Catalog volume,
# then creates tables from those files using the REST API.

library(httr2)
library(arrow)
library(glue)

source("R/logging.R")
init_logging("upload_databricks")

# =============================================================================
# CONFIGURATION
# =============================================================================

# Databricks catalog and schema
# NOTE: Currently using sandbox.michael for development
# TODO: Update to production schema before deployment
DATABRICKS_CATALOG <- "sandbox"
DATABRICKS_SCHEMA  <- "michael"

# Table names for the data
TRACTS_TABLE <- "census_tracts_children_poverty"
CITIES_TABLE <- "cities_100k_children_poverty"

# Volume for staging files (will be created if it doesn't exist)
VOLUME_NAME <- "child_poverty_staging"

# =============================================================================
# LOAD ENVIRONMENT AND DATA
# =============================================================================

log_info("=== Uploading Data to Databricks ===")
log_info("Target: {DATABRICKS_CATALOG}.{DATABRICKS_SCHEMA}")

# Load .Renviron
readRenviron(".Renviron")

# Get Databricks credentials
DATABRICKS_HOST <- Sys.getenv("DATABRICKS_HOST")
DATABRICKS_TOKEN <- Sys.getenv("DATABRICKS_TOKEN")
DATABRICKS_WAREHOUSE_ID <- Sys.getenv("DATABRICKS_WAREHOUSE_ID")

if (DATABRICKS_HOST == "" || DATABRICKS_TOKEN == "") {
  stop("DATABRICKS_HOST and DATABRICKS_TOKEN must be set in .Renviron")
}

# Ensure host has https:// prefix
if (!grepl("^https?://", DATABRICKS_HOST)) {
  DATABRICKS_HOST <- paste0("https://", DATABRICKS_HOST)
}

# Load the enriched parquet files
log_info("Loading enriched parquet files...")
tracts <- read_parquet("census_tracts_children_poverty_enriched.parquet")
cities <- read_parquet("cities_100k_children_poverty_enriched.parquet")

log_info("Loaded {nrow(tracts)} tracts and {nrow(cities)} cities")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Execute SQL via Databricks SQL Statement API
execute_sql <- function(sql, wait = TRUE) {
  log_info("Executing SQL: {substr(sql, 1, 100)}...")

  resp <- request(glue("{DATABRICKS_HOST}/api/2.0/sql/statements/")) |>
    req_headers(
      Authorization = paste("Bearer", DATABRICKS_TOKEN),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(list(
      warehouse_id = DATABRICKS_WAREHOUSE_ID,
      statement = sql,
      wait_timeout = if (wait) "50s" else "0s"
    )) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  result <- resp_body_json(resp)

  # Check for errors
  if (!is.null(result$status$state) && result$status$state == "FAILED") {
    error_msg <- result$status$error$message %||% "Unknown error"
    log_error("SQL failed: {error_msg}")
    stop(glue("SQL execution failed: {error_msg}"))
  }

  # Poll for completion if still running
  if (!is.null(result$status$state) && result$status$state %in% c("PENDING", "RUNNING")) {
    statement_id <- result$statement_id
    log_info("Statement {statement_id} is running, waiting for completion...")

    for (i in 1:60) {  # Wait up to 5 minutes
      Sys.sleep(5)

      status_resp <- request(glue("{DATABRICKS_HOST}/api/2.0/sql/statements/{statement_id}")) |>
        req_headers(Authorization = paste("Bearer", DATABRICKS_TOKEN)) |>
        req_error(is_error = function(resp) FALSE) |>
        req_perform()

      result <- resp_body_json(status_resp)

      if (result$status$state == "SUCCEEDED") {
        log_info("Statement completed successfully")
        break
      } else if (result$status$state == "FAILED") {
        error_msg <- result$status$error$message %||% "Unknown error"
        log_error("SQL failed: {error_msg}")
        stop(glue("SQL execution failed: {error_msg}"))
      }
    }
  }

  return(result)
}

# Upload file to volume using Files API
upload_to_volume <- function(local_path, volume_file_name) {
  file_size <- file.info(local_path)$size
  log_info("Uploading {volume_file_name} ({round(file_size/1024/1024, 1)} MB)...")

  # Read file content
  file_content <- readBin(local_path, "raw", file_size)

  # Use the Files API for Unity Catalog volumes
  api_path <- glue("/api/2.0/fs/files/Volumes/{DATABRICKS_CATALOG}/{DATABRICKS_SCHEMA}/{VOLUME_NAME}/{volume_file_name}")

  resp <- request(glue("{DATABRICKS_HOST}{api_path}")) |>
    req_headers(
      Authorization = paste("Bearer", DATABRICKS_TOKEN),
      `Content-Type` = "application/octet-stream"
    ) |>
    req_body_raw(file_content) |>
    req_method("PUT") |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  if (resp_status(resp) >= 400) {
    error_body <- tryCatch(resp_body_json(resp), error = function(e) list(message = "Unknown error"))
    stop(glue("Upload failed: {error_body$message %||% resp_status(resp)}"))
  }

  log_info("Upload complete: {volume_file_name}")
  return(resp)
}

# =============================================================================
# CREATE VOLUME (if needed)
# =============================================================================

log_info("Creating volume if not exists: {DATABRICKS_CATALOG}.{DATABRICKS_SCHEMA}.{VOLUME_NAME}")

tryCatch({
  execute_sql(glue("CREATE VOLUME IF NOT EXISTS {DATABRICKS_CATALOG}.{DATABRICKS_SCHEMA}.{VOLUME_NAME}"))
  log_info("Volume ready")
}, error = function(e) {
  log_warn("Could not create volume: {e$message}")
  log_info("Continuing - volume may already exist")
})

# =============================================================================
# UPLOAD PARQUET FILES TO VOLUME
# =============================================================================

# Create temp directory for staging
temp_dir <- tempdir()

# Write and upload tracts
log_info("Writing tracts to temporary parquet file...")
tracts_temp <- file.path(temp_dir, "tracts.parquet")
write_parquet(tracts, tracts_temp)
upload_to_volume(tracts_temp, "tracts.parquet")

# Write and upload cities
log_info("Writing cities to temporary parquet file...")
cities_temp <- file.path(temp_dir, "cities.parquet")
write_parquet(cities, cities_temp)
upload_to_volume(cities_temp, "cities.parquet")

# =============================================================================
# CREATE TABLES FROM PARQUET FILES
# =============================================================================

volume_path <- glue("/Volumes/{DATABRICKS_CATALOG}/{DATABRICKS_SCHEMA}/{VOLUME_NAME}")

# Create tracts table
log_info("Creating tracts table from parquet...")
tracts_full_name <- glue("{DATABRICKS_CATALOG}.{DATABRICKS_SCHEMA}.{TRACTS_TABLE}")

execute_sql(glue("DROP TABLE IF EXISTS {tracts_full_name}"))
execute_sql(glue("
  CREATE TABLE {tracts_full_name}
  AS SELECT * FROM parquet.`{volume_path}/tracts.parquet`
"))
log_info("Tracts table created: {tracts_full_name}")

# Create cities table
log_info("Creating cities table from parquet...")
cities_full_name <- glue("{DATABRICKS_CATALOG}.{DATABRICKS_SCHEMA}.{CITIES_TABLE}")

execute_sql(glue("DROP TABLE IF EXISTS {cities_full_name}"))
execute_sql(glue("
  CREATE TABLE {cities_full_name}
  AS SELECT * FROM parquet.`{volume_path}/cities.parquet`
"))
log_info("Cities table created: {cities_full_name}")

# =============================================================================
# VERIFY UPLOAD
# =============================================================================

log_info("Verifying upload...")

tracts_result <- execute_sql(glue("SELECT COUNT(*) as n FROM {tracts_full_name}"))
cities_result <- execute_sql(glue("SELECT COUNT(*) as n FROM {cities_full_name}"))

# Extract counts from result
tracts_count <- tracts_result$result$data_array[[1]][[1]]
cities_count <- cities_result$result$data_array[[1]][[1]]

log_info("Verification - Tracts in Databricks: {tracts_count}")
log_info("Verification - Cities in Databricks: {cities_count}")

# =============================================================================
# CLEANUP
# =============================================================================

log_info("=== Upload Complete ===")
log_info("Tables created:")
log_info("  - {tracts_full_name}")
log_info("  - {cities_full_name}")
log_info("Staging files in volume: {volume_path}")
