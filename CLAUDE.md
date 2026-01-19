# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A Shiny application for visualizing U.S. Census data on child poverty at the census tract and city level.

## Project Phases

1. **Data Download** (current): Download census tract and place data via tidycensus, perform spatial analysis to link tracts to cities, export as Parquet
2. **Databricks Integration**: Upload processed data to Databricks
3. **Shiny App**: Build interactive visualization app pulling data from Databricks

## Technology Stack

- **Language**: R
- **Key Packages**: tidycensus, tidyverse, sf (spatial), arrow (parquet export), shiny, logger
- **Data Source**: American Community Survey (ACS) 5-year estimates via Census API
- **Data Storage**: Databricks

## Logging

All scripts use the `logger` package for structured logging with dated log files.

- **Log location**: `logs/` directory (gitignored)
- **Log format**: `YYYY-MM-DD_scriptname.log`
- **Logging utility**: `R/logging.R`

To use logging in a script:
```r
source("R/logging.R")
init_logging("script_name")

log_info("Informational message")
log_warn("Warning message")
log_error("Error message")
```

## Key ACS Variables

| Metric | Variable ID |
|--------|-------------|
| Total population under 18 | B09001_001 |
| Children under 18 in poverty | B17001_004 + B17001_018 (sum male + female) |
| Total population (for filtering) | B01003_001 |

## Data Pipeline

1. Download tract-level data via `get_acs(geography = "tract")` for all states/territories
2. Download place-level data via `get_acs(geography = "place")`
3. Filter places to population >= 100,000
4. Spatial join: assign each tract to its primary city based on maximum intersection area
5. Export as Parquet files with WKT geometry strings

## Output Files

- `census_tracts_children_poverty.parquet` - Tracts intersecting 100k+ cities
- `cities_100k_children_poverty.parquet` - Cities with 100k+ population

## Census API Setup

Configure your Census API key (stored in `~/.Renviron`):
```r
census_api_key("YOUR_KEY", install = TRUE)
```
