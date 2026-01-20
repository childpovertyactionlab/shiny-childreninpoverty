# upload_to_databricks.R
# Template script to upload enriched census data to Databricks
#
# IMPORTANT: Review and configure the settings below before running!
# This script will NOT run automatically - you must execute it manually.

library(DBI)
library(odbc)
library(arrow)

source("R/logging.R")
init_logging("upload_databricks")

# =============================================================================
# CONFIGURATION - EDIT THESE VALUES BEFORE RUNNING
# =============================================================================

# Databricks catalog and schema - CHANGE THESE to your preferred location
DATABRICKS_CATALOG <- "sandbox"      # e.g., "sandbox", "prod", "dev"
DATABRICKS_SCHEMA  <- "michael"       # e.g., "michael", "child_poverty", "census"

# Table names for the data
TRACTS_TABLE <- "census_tracts_children_poverty"
CITIES_TABLE <- "cities_100k_children_poverty"

# Set to TRUE when you're ready to run
READY_TO_UPLOAD <- FALSE

# =============================================================================
# SAFETY CHECK
# =============================================================================

if (!READY_TO_UPLOAD) {
  message("
============================================================
  UPLOAD SCRIPT NOT CONFIGURED
============================================================

Before running this script, please:

1. Edit the CONFIGURATION section above:
   - DATABRICKS_CATALOG: Your target catalog
   - DATABRICKS_SCHEMA: Your target schema
   - Table names if you want different names

2. Ensure your .Renviron has valid Databricks credentials:
   - DATABRICKS_HOST
   - DATABRICKS_TOKEN
   - DATABRICKS_HTTP_PATH (or DATABRICKS_WAREHOUSE_ID)

3. Set READY_TO_UPLOAD <- TRUE

4. Run this script again

Target location: {DATABRICKS_CATALOG}.{DATABRICKS_SCHEMA}
Tables to create:
  - {TRACTS_TABLE}
  - {CITIES_TABLE}
============================================================
")
  stop("Please configure the script before running.")
}

# =============================================================================
# LOAD ENRICHED DATA
# =============================================================================

log_info("=== Uploading Data to Databricks ===")
log_info("Target: {DATABRICKS_CATALOG}.{DATABRICKS_SCHEMA}")

# Load the enriched parquet files
log_info("Loading enriched parquet files...")
tracts <- read_parquet("census_tracts_children_poverty_enriched.parquet")
cities <- read_parquet("cities_100k_children_poverty_enriched.parquet")

log_info("Loaded {nrow(tracts)} tracts and {nrow(cities)} cities")

# =============================================================================
# CONNECT TO DATABRICKS
# =============================================================================

log_info("Connecting to Databricks...")

# Load .Renviron if running interactively
readRenviron(".Renviron")

# Connect using odbc::databricks() - requires DATABRICKS_HOST and DATABRICKS_TOKEN
con <- tryCatch({
  DBI::dbConnect(
    odbc::databricks(),
    httpPath = Sys.getenv("DATABRICKS_HTTP_PATH"),
    catalog = DATABRICKS_CATALOG,
    schema = DATABRICKS_SCHEMA
  )
}, error = function(e) {
  log_error("Failed to connect to Databricks: {e$message}")
  log_info("Make sure your .Renviron has: DATABRICKS_HOST, DATABRICKS_TOKEN, DATABRICKS_HTTP_PATH")
  stop(e)
})

log_info("Connected successfully!")

# =============================================================================
# UPLOAD DATA
# =============================================================================

# Full table names
tracts_full_name <- paste(DATABRICKS_CATALOG, DATABRICKS_SCHEMA, TRACTS_TABLE, sep = ".")
cities_full_name <- paste(DATABRICKS_CATALOG, DATABRICKS_SCHEMA, CITIES_TABLE, sep = ".")

# Upload tracts
log_info("Uploading tracts to {tracts_full_name}...")
DBI::dbWriteTable(
  con,
  name = DBI::Id(catalog = DATABRICKS_CATALOG, schema = DATABRICKS_SCHEMA, table = TRACTS_TABLE),
  value = tracts,
  overwrite = TRUE
)
log_info("Tracts uploaded successfully!")

# Upload cities
log_info("Uploading cities to {cities_full_name}...")
DBI::dbWriteTable(
  con,
  name = DBI::Id(catalog = DATABRICKS_CATALOG, schema = DATABRICKS_SCHEMA, table = CITIES_TABLE),
  value = cities,
  overwrite = TRUE
)
log_info("Cities uploaded successfully!")

# =============================================================================
# VERIFY UPLOAD
# =============================================================================

log_info("Verifying upload...")

tracts_count <- DBI::dbGetQuery(con, sprintf(
  "SELECT COUNT(*) as n FROM %s.%s.%s",
  DATABRICKS_CATALOG, DATABRICKS_SCHEMA, TRACTS_TABLE
))
cities_count <- DBI::dbGetQuery(con, sprintf(
  "SELECT COUNT(*) as n FROM %s.%s.%s",
  DATABRICKS_CATALOG, DATABRICKS_SCHEMA, CITIES_TABLE
))

log_info("Verification - Tracts in Databricks: {tracts_count$n}")
log_info("Verification - Cities in Databricks: {cities_count$n}")

# =============================================================================
# CLEANUP
# =============================================================================

DBI::dbDisconnect(con)
log_info("=== Upload Complete ===")
log_info("Tables created:")
log_info("  - {tracts_full_name}")
log_info("  - {cities_full_name}")
