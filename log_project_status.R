# log_project_status.R
# Logs current project status and milestones for historical tracking

library(arrow)

source("R/logging.R")
init_logging("project_status")

log_info("=== PROJECT STATUS UPDATE ===")
log_info("Project: shiny-childreninpoverty")
log_info("Phase: Data Download - COMPLETE")

# Log data file stats if they exist
if (file.exists("census_tracts_children_poverty.parquet")) {
  tracts <- read_parquet("census_tracts_children_poverty.parquet")
  log_info("Tracts data: {format(nrow(tracts), big.mark = ',')} rows")
  log_info("Tracts file size: {round(file.size('census_tracts_children_poverty.parquet') / 1024 / 1024, 1)} MB")

  # Log some summary stats
  cities_covered <- length(unique(tracts$primary_city_geoid))
  log_info("Unique cities covered: {cities_covered}")
}

if (file.exists("cities_100k_children_poverty.parquet")) {
  cities <- read_parquet("cities_100k_children_poverty.parquet")
  log_info("Cities data: {format(nrow(cities), big.mark = ',')} rows")
  log_info("Cities file size: {round(file.size('cities_100k_children_poverty.parquet') / 1024 / 1024, 1)} MB")
}

log_info("=== COMPLETED MILESTONES ===
- Census API integration configured
- Tract-level data downloaded for all 50 states + DC + Puerto Rico
- Place-level data downloaded and filtered to 100k+ population cities
- Spatial analysis complete: tracts assigned to primary cities
- Data exported to Parquet format with WKT geometries
- Logging infrastructure established")

log_info("=== NEXT PHASES ===
- Databricks Integration: Upload Parquet files to Databricks
- Shiny App: Build interactive visualization")

log_info("=== END STATUS UPDATE ===")
