# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A Shiny application for visualizing U.S. Census data on child poverty at the census tract and city level.

## Project Phases

1. **Data Download** (COMPLETE): Download census tract and place data via tidycensus, perform spatial analysis to link tracts to cities, export as Parquet
2. **Data Exploration** (COMPLETE): Interactive exploration with mapgl, verified data quality
3. **Shiny App Development** (IN PROGRESS): Build dashboard with cpaltemplates standards
4. **Databricks Integration**: Upload processed data to Databricks
5. **Deployment**: Deploy to shinyapps.io

## Technology Stack

- **Language**: R
- **Key Packages**: tidycensus, tidyverse, sf (spatial), arrow (parquet), shiny, bslib, mapgl, reactable, cpaltemplates
- **Data Source**: American Community Survey (ACS) 5-year estimates via Census API
- **Data Storage**: Databricks (source of truth), bundled parquet (for deployment)
- **Deployment**: shinyapps.io

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
│   └── upload_to_databricks.R       # Databricks upload template
├── app/
│   ├── app.R                        # Main Shiny dashboard
│   ├── R/utils.R                    # Helper functions
│   ├── data/                        # Bundled parquet files (gitignored)
│   ├── views/                       # UI components
│   └── www/custom.css               # Custom styles
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

**App data (after running refresh_app_data.R):**
- `app/data/tracts.parquet`
- `app/data/cities.parquet`

## Running the App

```r
# Step 1: Download census data (only needed once, ~15 min)
source("scripts/download_census_data.R")

# Step 2: Create enriched data
source("R/prepare_data_for_databricks.R")

# Step 3: Bundle data for app
source("scripts/refresh_app_data.R")

# Step 4: Run the app
shiny::runApp("app")
```

## Census API Setup

Configure your Census API key (stored in `~/.Renviron`):
```r
census_api_key("YOUR_KEY", install = TRUE)
```

## mapgl Note

When using `st_bbox()` for map centering, always use `unname()` on coordinates to avoid jsonlite serialization issues:
```r
bbox <- st_bbox(data_sf)
center_lng <- unname((bbox["xmin"] + bbox["xmax"]) / 2)
center_lat <- unname((bbox["ymin"] + bbox["ymax"]) / 2)
```
