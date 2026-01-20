# log_project_status.R
# Logs current project status and milestones for historical tracking

library(arrow)

source("R/logging.R")
init_logging("project_status")

log_info("=== PROJECT STATUS UPDATE ===")
log_info("Project: shiny-childreninpoverty")
log_info("Phase: Data Download - COMPLETE")
log_info("Phase: Data Exploration - COMPLETE")
log_info("Phase: Shiny App Development - IN PROGRESS")

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
- Logging infrastructure established (logger package)
- Data exploration script created (explore_data.R)
- mapgl visualization working (fixed jsonlite named vector issue)
- Data preparation script created (R/prepare_data_for_databricks.R)
- Databricks upload template created (scripts/upload_to_databricks.R)
- App data bundling script created (scripts/refresh_app_data.R)
- Shiny app structure created (app/ directory)")

log_info("=== IN PROGRESS ===
- Shiny dashboard development (3-tab layout with cpaltemplates)
- Tab 1: Where do children live? (midnight palette)
- Tab 2: Where do children in poverty live? (coral palette, top-N filter)
- Tab 3: Where are poverty rates highest? (sage palette, top-N filter)
- Testing app locally before deployment")

log_info("=== NEXT STEPS ===
- Run data preparation: source('R/prepare_data_for_databricks.R')
- Bundle app data: source('scripts/refresh_app_data.R')
- Test Shiny app locally: shiny::runApp('app')
- Implement bidirectional map-table sync (click highlighting)
- Upload data to Databricks (configure scripts/upload_to_databricks.R)
- Deploy to shinyapps.io")

log_info("=== FILES CREATED ===
- R/logging.R - Logging utility
- R/prepare_data_for_databricks.R - Data enrichment script
- explore_data.R - Data exploration with mapgl
- test_mapgl.R - Debug script for mapgl rendering
- scripts/upload_to_databricks.R - Databricks upload template
- scripts/refresh_app_data.R - App data bundling script
- app/app.R - Main Shiny dashboard (page_navbar, 3 tabs)
- app/R/utils.R - Helper functions
- app/data/ - Directory for bundled parquet files
- app/views/ - Directory for UI components
- app/www/ - Directory for static assets")

log_info("=== END STATUS UPDATE ===")
