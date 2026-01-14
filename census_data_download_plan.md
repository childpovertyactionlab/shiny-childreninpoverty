# Census Tract & City Child Poverty Data Download Plan

## Objective
Download census tract and city (place) level data for children and child poverty using tidycensus, perform spatial analysis to link tracts to their primary city, and export in Databricks-compatible format.

## Data Specifications
- **Source**: ACS 5-year estimates (most recent available, likely 2022 or 2023)
- **Age Definition**: Children under 18
- **Geography**: All 50 states + DC + territories
- **Output Format**: Parquet files with WKT geometry strings (Databricks compatible)

## Key ACS Variables
| Metric | Variable ID | Table |
|--------|-------------|-------|
| Total population under 18 | B09001_001 | Age of Children Under 18 |
| Children under 18 in poverty | B17001_004 + B17001_018 (sum male + female) | Poverty Status by Age |

*Alternative: Use B17020_003 for total children under 18 below poverty level*

---

## Implementation Steps

### Step 1: Environment Setup
```r
# Install/load required packages
install.packages(c("tidycensus", "tidyverse", "sf", "arrow"))
library(tidycensus)
library(tidyverse)
library(sf)
library(arrow)

# Set Census API key (user must obtain from https://api.census.gov/data/key_signup.html)
census_api_key("YOUR_KEY", install = TRUE)
```

### Step 2: Define State/Territory FIPS Codes
Create vector of all state + territory FIPS codes to iterate over:
- 50 states + DC (01-56, excluding gaps)
- Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60), Northern Mariana Islands (69)

### Step 3: Download Census Tract Data
```r
# Loop through all states/territories to get tract-level data
# Variables: total children, children in poverty
# Include geometry = TRUE for spatial data
```
- Use `get_acs()` with `geography = "tract"`
- Download in batches by state to avoid API limits
- Combine into single sf object

### Step 4: Download Place (City) Data
```r
# Loop through all states/territories to get place-level data
# Same variables plus total population for filtering
# Include geometry = TRUE
```
- Use `get_acs()` with `geography = "place"`
- Download in batches by state
- Combine into single sf object

### Step 5: Filter Cities by Population
```r
# Filter places to those with total population >= 100,000
# Will need to also download B01003_001 (total population) for filtering
```

### Step 6: Spatial Analysis - Tract to City Assignment
```r
# For each tract:
# 1. Find all intersecting cities (from filtered set)
# 2. Calculate intersection area for each
# 3. Determine which city covers the largest percentage of the tract
# 4. Assign that city's GEOID and NAME to the tract
```

Key operations:
- `st_intersection()` to find overlaps
- `st_area()` to calculate areas
- Group by tract, find max coverage city

### Step 7: Export to Databricks-Compatible Format
```r
# Convert sf geometry to WKT string
tracts_export <- tracts %>%
  mutate(geometry_wkt = st_as_text(geometry)) %>%
  st_drop_geometry()

cities_export <- cities %>%
  mutate(geometry_wkt = st_as_text(geometry)) %>%
  st_drop_geometry()

# Write as Parquet files
write_parquet(tracts_export, "census_tracts_children_poverty.parquet")
write_parquet(cities_export, "cities_100k_children_poverty.parquet")
```

---

### Step 8: Filter Tracts to City-Intersecting Only
```r
# Remove tracts that don't intersect any 100k+ city
tracts_final <- tracts_with_city %>%
  filter(!is.na(primary_city_geoid))
```

---

## Output Files

| File | Contents |
|------|----------|
| `census_tracts_children_poverty.parquet` | Census tracts intersecting 100k+ cities with: GEOID, NAME, total_children, children_poverty, geometry_wkt, primary_city_geoid, primary_city_name, city_coverage_pct |
| `cities_100k_children_poverty.parquet` | Cities with 100k+ pop: GEOID, NAME, total_pop, total_children, children_poverty, geometry_wkt |

---

## Critical Files to Create
- `download_census_data.R` - Main R script with all logic

---

## Verification Steps
1. Check row counts match expected tract/place counts
2. Verify geometry validity with `st_is_valid()`
3. Spot-check a few tracts to confirm city assignment logic
4. Test Parquet import in Databricks or with arrow::read_parquet()
5. Validate WKT geometries can be parsed in Databricks using `ST_GeomFromWKT()`

---

## Notes & Considerations
- API rate limits: tidycensus handles this but large downloads may take time
- Tracts not intersecting any 100k+ city will be excluded from final output
- Some territories may have limited place data
- Consider caching intermediate results to avoid re-downloading
