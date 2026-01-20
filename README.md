# Child Poverty Explorer

A Shiny dashboard for exploring U.S. Census data on child poverty at the census tract and city level. Built with R, mapgl, and cpaltemplates.

## Features

- **Interactive Maps**: Explore child poverty data across 346 U.S. cities with 100k+ population
- **Three Analysis Views**:
  - Where do children live? (total population)
  - Where do children in poverty live? (concentration)
  - Where are poverty rates highest? (rates)
- **Table-to-Map Interaction**: Click any table row to fly to that tract on the map
- **Top-N Filtering**: Focus on the tracts with highest poverty counts or rates
- **City Comparison**: Switch between cities with synced dropdowns across tabs
- **Dark Mode**: Toggle between light and dark themes

## Data Source

- **American Community Survey (ACS)** 5-year estimates via Census API
- **Variables**: S1701 (Poverty Status) and B01003 (Total Population)
- **Geography**: Census tracts intersecting cities with 100k+ population
- **Coverage**: All 50 states, DC, and Puerto Rico

## Installation

### Prerequisites

- R 4.4+
- Census API key ([get one here](https://api.census.gov/data/key_signup.html))

### Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/childpovertyactionlab/shiny-childreninpoverty.git
   cd shiny-childreninpoverty
   ```

2. Install required packages:
   ```r
   install.packages(c(
     "shiny", "bslib", "bsicons", "tidyverse", "sf", "arrow",
     "reactable", "mapgl", "cpaltemplates", "htmltools",
     "classInt", "shinyWidgets", "tidycensus", "logger"
   ))
   ```

3. Configure your Census API key:
   ```r
   tidycensus::census_api_key("YOUR_KEY", install = TRUE)
   ```

## Usage

### Quick Start (with existing data)

If data files already exist:
```r
shiny::runApp("app")
```

### Full Pipeline

To download fresh data and run the app:
```r
# Step 1: Download census data (~15 min, cached)
source("scripts/download_census_data.R")

# Step 2: Enrich data with derived columns
source("R/prepare_data_for_databricks.R")

# Step 3: Bundle data for app
source("scripts/refresh_app_data.R")

# Step 4: Run the app
shiny::runApp("app")
```

## Project Structure

```
shiny-childreninpoverty/
├── R/
│   ├── logging.R                    # Logging utility
│   └── prepare_data_for_databricks.R # Data enrichment
├── scripts/
│   ├── download_census_data.R       # Census data download
│   ├── explore_data.R               # Interactive exploration
│   ├── log_project_status.R         # Status logging
│   ├── refresh_app_data.R           # Bundle data for app
│   └── upload_to_databricks.R       # Databricks upload
├── app/
│   ├── app.R                        # Main Shiny dashboard
│   ├── R/utils.R                    # Helper functions
│   ├── data/                        # Bundled parquet files
│   └── www/custom.css               # Custom styles
└── *.parquet                        # Data files (gitignored)
```

## Technical Notes

### Spatial Analysis

Census tracts are assigned to their "primary city" based on the largest intersection area. The spatial analysis uses:
- EPSG 5070 (Albers Equal Area) for accurate area calculations
- Vectorized `st_intersection()` for performance
- Pre-filtering with `st_filter()` to reduce computation

### Data Methodology

- **Tab 1** uses official city-level Census figures for headline stats
- **Tabs 2 & 3** use tract-level aggregation for analyzing spatial concentration
- Tract totals may exceed city totals due to tracts crossing city boundaries

## Built With

- [Shiny](https://shiny.rstudio.com/) - Web application framework
- [bslib](https://rstudio.github.io/bslib/) - Bootstrap theming
- [mapgl](https://github.com/walkerke/mapgl) - Mapbox GL JS for R
- [cpaltemplates](https://github.com/childpovertyactionlab/cpaltemplates) - CPAL design system
- [reactable](https://glin.github.io/reactable/) - Interactive tables
- [tidycensus](https://walker-data.com/tidycensus/) - Census data access

## License

This project is maintained by the [Child Poverty Action Lab](https://childpovertyactionlab.org/).

## Acknowledgments

- U.S. Census Bureau for ACS data
- Kyle Walker for tidycensus and mapgl packages
