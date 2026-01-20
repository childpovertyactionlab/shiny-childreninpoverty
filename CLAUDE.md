# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A Shiny application for visualizing U.S. Census data on child poverty at the census tract and city level.

## Project Phases

1. **Data Download** (COMPLETE): Download census tract and place data via tidycensus, perform spatial analysis to link tracts to cities, export as Parquet
2. **Data Exploration** (COMPLETE): Interactive exploration with mapgl, verified data quality
3. **Shiny App Development** (COMPLETE): Build dashboard with cpaltemplates standards
4. **Databricks Integration** (COMPLETE): Upload processed data to Databricks, app queries from Databricks
5. **Deployment** (READY): Deploy to shinyapps.io - see deployment section below

## Technology Stack

- **Language**: R
- **Key Packages**: tidycensus, tidyverse, sf (spatial), arrow (parquet), shiny, bslib, mapgl, reactable, cpaltemplates, httr2
- **Data Source**: American Community Survey (ACS) 5-year estimates via Census API
- **Data Storage**: Databricks (source of truth)
- **Deployment**: shinyapps.io (requires Databricks connectivity)

## Project Structure

```
shiny-childreninpoverty/
├── R/
│   ├── logging.R                    # Logging utility
│   └── prepare_data_for_databricks.R # Data enrichment script
├── scripts/
│   ├── download_census_data.R       # Census data download (optimized spatial analysis)
│   ├── explore_data.R               # Interactive data exploration with mapgl
│   ├── log_project_status.R         # Project status logging
│   ├── refresh_app_data.R           # Bundle data for app deployment
│   └── upload_to_databricks.R       # Databricks upload script
├── app/                             # Shiny app directory (deploy this folder)
│   ├── app.R                        # Main Shiny dashboard
│   ├── R/
│   │   ├── databricks.R             # Databricks REST API query functions
│   │   └── utils.R                  # Helper functions
│   └── www/custom.css               # Custom styles (responsive layout)
├── logs/                            # Log files (gitignored)
└── *.parquet                        # Data files (gitignored)
```

## Shiny Dashboard

Three-tab layout using `bslib::page_navbar()` with cpaltemplates theming:

1. **"Where Do Children Live?"** - midnight blue palette, city-level stats (official Census figures)
2. **"Where Do Children in Poverty Live?"** - coral palette, top-N filter, tract aggregation
3. **"Where Are Poverty Rates Highest?"** - sage green palette, top-N filter, tract aggregation

Each tab has:
- City selector dropdown (synced across tabs)
- Value box with headline stat
- Interactive map (mapgl) with Jenks natural breaks coloring
- Reactable table with inline bar visualization

Map features:
- Hover highlighting (opacity change + black outline)
- Click popup with tract details
- Table-to-map selection (click row → fly to tract with animation)

## Logging

All scripts use the `logger` package for structured logging with dated log files.

- **Log location**: `logs/` directory (gitignored)
- **Log format**: `YYYY-MM-DD_scriptname.log`
- **Logging utility**: `R/logging.R`

```r
source("R/logging.R")
init_logging("script_name")

log_info("Informational message")
log_warn("Warning message")
log_error("Error message")
```

## Key ACS Variables

| Metric | Variable ID | Table |
|--------|-------------|-------|
| Population under 18 (poverty universe) | S1701_C01_002 | S1701 - Poverty Status |
| Population under 18 below poverty | S1701_C02_002 | S1701 - Poverty Status |
| Total population (for city filtering) | B01003_001 | B01003 - Total Population |

## Data Files

**Raw data:**
- `census_tracts_children_poverty.parquet` - Tracts intersecting 100k+ cities
- `cities_100k_children_poverty.parquet` - Cities with 100k+ population

**Enriched data (after running prepare_data_for_databricks.R):**
- `census_tracts_children_poverty_enriched.parquet` - With poverty_rate, tract_label
- `cities_100k_children_poverty_enriched.parquet` - With city_poverty_rate, tract_count

**Databricks tables:**
- `sandbox.michael.census_tracts_children_poverty` - Enriched tract data
- `sandbox.michael.cities_100k_children_poverty` - Enriched city data

## Running the App

The app requires Databricks connectivity. Ensure your `.Renviron` has the required credentials.

```r
# Run the app (data loaded from Databricks)
shiny::runApp("app")
```

### Initial Data Setup (one-time)

If the Databricks tables don't exist yet:

```r
# Step 1: Download census data (~15 min)
source("scripts/download_census_data.R")

# Step 2: Create enriched data
source("R/prepare_data_for_databricks.R")

# Step 3: Upload to Databricks
source("scripts/upload_to_databricks.R")

# Step 4: Run the app
shiny::runApp("app")
```

## Census API Setup

Configure your Census API key (stored in `~/.Renviron`):
```r
census_api_key("YOUR_KEY", install = TRUE)
```

## Databricks Configuration

The app queries data from Databricks using the SQL Statement Execution REST API (not ODBC). This approach is necessary and provides several benefits:

**Why REST API instead of ODBC:**
- ODBC drivers truncate strings to 1024 characters by default
- Geometry WKT strings in this dataset can exceed 190,000 characters
- Truncated WKT causes "OGR: Corrupt data" errors when converting to sf objects
- REST API returns full string values without truncation
- No driver installation needed (works on shinyapps.io)

**Pattern also used in:** `neighborhood-safety-dashboard`

### Required Environment Variables

Add these to your project `.Renviron` file:

```
DATABRICKS_HOST=<workspace-url>
DATABRICKS_TOKEN=<personal-access-token>
DATABRICKS_WAREHOUSE_ID=<sql-warehouse-id>
MAPBOX_PUBLIC_TOKEN=<mapbox-public-token>
```

For the Mapbox token, use the CPAL Analytics token or create one at https://mapbox.com

### Current Schema Location

**Development**: `sandbox.michael`
- Tables:
  - `sandbox.michael.census_tracts_children_poverty`
  - `sandbox.michael.cities_100k_children_poverty`
- Staging volume (for data refresh):
  - `/Volumes/sandbox/michael/child_poverty_staging/`

### TODO: Production Migration

Before production deployment, migrate tables to a production schema:
1. Create production schema (e.g., `prod.child_poverty` or `prod.census`)
2. Copy/move tables from `sandbox.michael` to production schema
3. Update `DATABRICKS_CATALOG` and `DATABRICKS_SCHEMA` in:
   - `app/app.R`
   - `scripts/upload_to_databricks.R`

---

## Databricks REST API Reference

This section documents the REST API patterns used for uploading data and querying from Shiny apps. These patterns are reusable for other projects.

### API Endpoints Used

| Purpose | Endpoint | Method |
|---------|----------|--------|
| Execute SQL | `/api/2.0/sql/statements` | POST |
| Check statement status | `/api/2.0/sql/statements/{id}` | GET |
| Fetch result chunks | `/api/2.0/sql/statements/{id}/result/chunks/{index}` | GET |
| Upload file to volume | `/api/2.0/fs/files/Volumes/{catalog}/{schema}/{volume}/{file}` | PUT |

### Uploading Data to Databricks

The upload process uses two APIs: **Files API** (to upload parquet to a volume) and **SQL Statement API** (to create tables from those files).

**Script:** `scripts/upload_to_databricks.R`

**Process:**
1. Create a Unity Catalog volume for staging files
2. Write data frames to temporary parquet files locally
3. Upload parquet files to the volume using the Files API
4. Create tables from parquet using `CREATE TABLE ... AS SELECT * FROM parquet.\`path\``

**Key code patterns:**

```r
# Upload file to Unity Catalog volume
upload_to_volume <- function(local_path, volume_file_name) {
  file_content <- readBin(local_path, "raw", file.info(local_path)$size)

  api_path <- glue("/api/2.0/fs/files/Volumes/{catalog}/{schema}/{volume}/{volume_file_name}")

  request(glue("{host}{api_path}")) |>
    req_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/octet-stream"
    ) |>
    req_body_raw(file_content) |>
    req_method("PUT") |>
    req_perform()
}

# Create table from parquet in volume
execute_sql(glue("
  CREATE TABLE {catalog}.{schema}.{table_name}
  AS SELECT * FROM parquet.`/Volumes/{catalog}/{schema}/{volume}/file.parquet`
"))
```

**To refresh data:**
```r
source("scripts/download_census_data.R")
source("R/prepare_data_for_databricks.R")
source("scripts/upload_to_databricks.R")
```

### Querying Data from Shiny Apps

The query process uses the **SQL Statement Execution API** with support for chunked/paginated responses.

**Module:** `app/R/databricks.R`

**Key functions:**
- `databricks_query(sql, host, token, warehouse_id)` - Execute SQL and return data frame
- `parse_databricks_response(result, ...)` - Parse JSON response, fetch additional chunks
- `convert_column_type(col, type_name)` - Convert Databricks types to R types

**Critical parameters for large datasets:**

```r
body <- list(
  warehouse_id = warehouse_id,
  statement = sql,
  wait_timeout = "50s",
  disposition = "INLINE",
  format = "JSON_ARRAY",
  row_limit = 100000  # IMPORTANT: Default is ~8k rows
)
```

**Handling chunked responses:**

The API returns results in chunks. The initial response includes `next_chunk_index` if more data exists:

```r
# Fetch additional chunks
while (!is.null(chunk_index)) {
  chunk_url <- paste0(base_url, "/", statement_id, "/result/chunks/", chunk_index)

  chunk_resp <- request(chunk_url) |>
    req_headers(Authorization = paste("Bearer", token)) |>
    req_perform()

  chunk_result <- resp_body_json(chunk_resp)
  all_data <- c(all_data, chunk_result$data_array)
  chunk_index <- chunk_result$next_chunk_index  # NULL when done
}
```

**Polling for long-running queries:**

```r
while (result$status$state %in% c("PENDING", "RUNNING")) {
  Sys.sleep(2)
  result <- request(paste0(base_url, "/", statement_id)) |>
    req_headers(Authorization = paste("Bearer", token)) |>
    req_perform() |>
    resp_body_json()
}
```

**Usage in Shiny:**

```r
source("R/databricks.R")

tracts <- databricks_query(
  sql = "SELECT * FROM sandbox.michael.census_tracts_children_poverty",
  host = Sys.getenv("DATABRICKS_HOST"),
  token = Sys.getenv("DATABRICKS_TOKEN"),
  warehouse_id = Sys.getenv("DATABRICKS_WAREHOUSE_ID")
)
```

### Common Issues and Solutions

| Issue | Cause | Solution |
|-------|-------|----------|
| Only ~8k rows returned | Default API row limit | Set `row_limit = 100000` in request body |
| Truncated strings (1024 chars) | ODBC driver limitation | Use REST API instead of ODBC |
| "OGR: Corrupt data" on sf conversion | Geometry WKT truncated | Use REST API (returns full strings) |
| Query timeout | Long-running query | Implement polling loop for PENDING/RUNNING states |
| Missing data chunks | Didn't fetch `next_chunk_index` | Implement chunk fetching loop |

## mapgl Note

When using `st_bbox()` for map centering, always use `unname()` on coordinates to avoid jsonlite serialization issues:
```r
bbox <- st_bbox(data_sf)
center_lng <- unname((bbox["xmin"] + bbox["xmax"]) / 2)
center_lat <- unname((bbox["ymin"] + bbox["ymax"]) / 2)
```

---

## Shinyapps.io Deployment

### Pre-Deployment Checklist

1. **Verify Databricks tables exist** - Data should already be uploaded to `sandbox.michael`
2. **Test app locally** - Run `shiny::runApp("app")` and verify all tabs work
3. **Confirm environment variables** - All 4 required variables must be set

### Deployment Steps

1. **Configure shinyapps.io account** (one-time):
   ```r
   rsconnect::setAccountInfo(
     name = "<account-name>",
     token = "<token>",
     secret = "<secret>"
   )
   ```

2. **Set environment variables on shinyapps.io**:

   In the shinyapps.io dashboard, go to your app's Settings > Environment Variables and add:
   - `DATABRICKS_HOST`
   - `DATABRICKS_TOKEN`
   - `DATABRICKS_WAREHOUSE_ID`
   - `MAPBOX_PUBLIC_TOKEN`

3. **Deploy the app**:
   ```r
   rsconnect::deployApp(
     appDir = "app",
     appName = "child-poverty-explorer",
     appTitle = "Child Poverty Explorer"
   )
   ```

### App Files Deployed

Only the `app/` directory is deployed:
- `app.R` - Main application
- `R/databricks.R` - Databricks REST API functions
- `R/utils.R` - Helper functions
- `www/custom.css` - Custom responsive styles

### Responsive Layout Notes

The app is optimized for all screen sizes:
- **Desktop (1200px+)**: Side-by-side layout (controls | value box, map | table)
- **Tablet (768-1199px)**: Stacked layout with full-width components
- **Phone (<768px)**: Compact layout with simplified slider labels

Key CSS fixes applied:
- `fill = FALSE` on controls layout_columns prevents bslib fillable overlap issues
- Slider grid labels progressively hidden at narrow widths (0/50/100 → 0/100)
- Value box icons hidden on screens < 992px for better text visibility
