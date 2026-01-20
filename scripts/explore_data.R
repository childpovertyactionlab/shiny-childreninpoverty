# explore_data.R
# Interactive exploration script for spot-checking census data and maps
# Uses mapgl for interactive mapping

# =============================================================================
# SETUP
# =============================================================================

# Install mapgl if needed (uncomment if first run)
# install.packages("mapgl")

# Load environment variables (contains MAPBOX_ACCESS_TOKEN)
readRenviron(".Renviron")

library(tidyverse)
library(sf)
library(arrow)
library(mapgl)

# Load the data
message("Loading data...")
tracts <- read_parquet("census_tracts_children_poverty.parquet")
cities <- read_parquet("cities_100k_children_poverty.parquet")

# Convert WKT geometry back to sf objects
tracts_sf <- tracts %>%
  st_as_sf(wkt = "geometry_wkt", crs = 4326)

cities_sf <- cities %>%
 st_as_sf(wkt = "geometry_wkt", crs = 4326)

message("Data loaded successfully!")

# =============================================================================
# DATA SUMMARY
# =============================================================================

#' Print overall data summary
data_summary <- function() {
  cat("\n=== DATA SUMMARY ===\n\n")

  cat("TRACTS:\n")
  cat(sprintf("  Total tracts: %s\n", format(nrow(tracts), big.mark = ",")))
  cat(sprintf("  Total children: %s\n", format(sum(tracts$total_children, na.rm = TRUE), big.mark = ",")))
  cat(sprintf("  Children in poverty: %s\n", format(sum(tracts$children_poverty, na.rm = TRUE), big.mark = ",")))
  cat(sprintf("  Overall poverty rate: %.1f%%\n",
              sum(tracts$children_poverty, na.rm = TRUE) / sum(tracts$total_children, na.rm = TRUE) * 100))

  cat("\nCITIES:\n")
  cat(sprintf("  Total cities (100k+): %d\n", nrow(cities)))
  cat(sprintf("  Total population: %s\n", format(sum(cities$total_pop, na.rm = TRUE), big.mark = ",")))
  cat(sprintf("  Total children: %s\n", format(sum(cities$total_children, na.rm = TRUE), big.mark = ",")))
  cat(sprintf("  Children in poverty: %s\n", format(sum(cities$children_poverty, na.rm = TRUE), big.mark = ",")))

  cat("\n")
}

# =============================================================================
# CITY LISTING & SELECTION
# =============================================================================

#' List all available cities sorted by population
list_cities <- function(n = 50) {
  cities %>%
    arrange(desc(total_pop)) %>%
    select(GEOID, NAME, total_pop, total_children, children_poverty) %>%
    mutate(
      poverty_rate = round(children_poverty / total_children * 100, 1)
    ) %>%
    head(n) %>%
    print(n = n)
}

#' Search for cities by name
search_cities <- function(pattern) {
  cities %>%
    filter(str_detect(NAME, regex(pattern, ignore_case = TRUE))) %>%
    arrange(desc(total_pop)) %>%
    select(GEOID, NAME, total_pop, total_children, children_poverty) %>%
    mutate(
      poverty_rate = round(children_poverty / total_children * 100, 1)
    ) %>%
    print(n = 50)
}

# =============================================================================
# CITY DATA INSPECTION
# =============================================================================

#' Get detailed stats for a specific city
city_stats <- function(city_name_pattern) {
  # Find matching city
  city <- cities_sf %>%
    filter(str_detect(NAME, regex(city_name_pattern, ignore_case = TRUE))) %>%
    slice(1)

  if (nrow(city) == 0) {
    message("No city found matching: ", city_name_pattern)
    return(invisible(NULL))
  }

  city_geoid <- city$GEOID
  city_tracts <- tracts_sf %>% filter(primary_city_geoid == city_geoid)

  cat("\n=== CITY STATS:", city$NAME, "===\n\n")
  cat(sprintf("GEOID: %s\n", city$GEOID))
  cat(sprintf("Total Population: %s\n", format(city$total_pop, big.mark = ",")))
  cat(sprintf("Total Children: %s\n", format(city$total_children, big.mark = ",")))
  cat(sprintf("Children in Poverty: %s\n", format(city$children_poverty, big.mark = ",")))
  cat(sprintf("City-level Poverty Rate: %.1f%%\n", city$children_poverty / city$total_children * 100))

  cat(sprintf("\nTracts assigned to city: %d\n", nrow(city_tracts)))
  cat(sprintf("Tract-level total children: %s\n", format(sum(city_tracts$total_children, na.rm = TRUE), big.mark = ",")))
  cat(sprintf("Tract-level children in poverty: %s\n", format(sum(city_tracts$children_poverty, na.rm = TRUE), big.mark = ",")))

  # Coverage stats
  cat(sprintf("\nAverage city coverage of tracts: %.1f%%\n", mean(city_tracts$city_coverage_pct, na.rm = TRUE)))
  cat(sprintf("Min coverage: %.1f%%\n", min(city_tracts$city_coverage_pct, na.rm = TRUE)))
  cat(sprintf("Max coverage: %.1f%%\n", max(city_tracts$city_coverage_pct, na.rm = TRUE)))

  # Poverty rate distribution across tracts
  tract_rates <- city_tracts %>%
    st_drop_geometry() %>%
    mutate(poverty_rate = children_poverty / total_children * 100) %>%
    filter(!is.na(poverty_rate) & is.finite(poverty_rate))

  cat(sprintf("\nTract poverty rates:\n"))
  cat(sprintf("  Min: %.1f%%\n", min(tract_rates$poverty_rate)))
  cat(sprintf("  Median: %.1f%%\n", median(tract_rates$poverty_rate)))
  cat(sprintf("  Mean: %.1f%%\n", mean(tract_rates$poverty_rate)))
  cat(sprintf("  Max: %.1f%%\n", max(tract_rates$poverty_rate)))

  cat("\n")
  invisible(city)
}

# =============================================================================
# MAPPING FUNCTIONS
# =============================================================================

#' Map a city with its tracts colored by child poverty rate
#' @param city_name_pattern Partial name to match (e.g., "Seattle", "Los Angeles")
#' @param show_city_boundary Show the city boundary outline (default TRUE)
map_city <- function(city_name_pattern, show_city_boundary = TRUE) {
  # Find matching city
  city <- cities_sf %>%
    filter(str_detect(NAME, regex(city_name_pattern, ignore_case = TRUE))) %>%
    slice(1)

  if (nrow(city) == 0) {
    message("No city found matching: ", city_name_pattern)
    return(invisible(NULL))
  }

  city_geoid <- city$GEOID
  message("Mapping: ", city$NAME)

  # Get tracts for this city - filter raw data first, then convert to sf
  city_tracts <- tracts %>%
    filter(primary_city_geoid == city_geoid) %>%
    mutate(
      poverty_rate = round(children_poverty / total_children * 100, 1),
      poverty_rate = if_else(is.na(poverty_rate) | is.infinite(poverty_rate), 0, poverty_rate)
    ) %>%
    st_as_sf(wkt = "geometry_wkt", crs = 4326)

  # Calculate bounding box for the view (unname to avoid jsonlite issues)
  bbox <- st_bbox(city_tracts)
  center_lng <- unname((bbox["xmin"] + bbox["xmax"]) / 2)
  center_lat <- unname((bbox["ymin"] + bbox["ymax"]) / 2)

  # Create the map
  m <- mapboxgl(
    style = mapbox_style("light"),
    center = c(center_lng, center_lat),
    zoom = 10
  ) |>
    add_fill_layer(
      id = "tracts",
      source = city_tracts,
      fill_color = interpolate(
        column = "poverty_rate",
        values = c(0, 15, 30, 50),
        stops = c("#f7fbff", "#6baed6", "#2171b5", "#08306b"),
        na_color = "#cccccc"
      ),
      fill_opacity = 0.7,
      tooltip = "Name: {{NAME}}<br>Children: {{total_children}}<br>In Poverty: {{children_poverty}}<br>Rate: {{poverty_rate}}%"
    ) |>
    add_line_layer(
      id = "tract_borders",
      source = city_tracts,
      line_color = "#ffffff",
      line_width = 0.5
    )

  # Add city boundary if requested
  if (show_city_boundary) {
    # Convert city to sf
    city_sf <- city %>%
      st_as_sf(wkt = "geometry_wkt", crs = 4326)

    m <- m |>
      add_line_layer(
        id = "city_boundary",
        source = city_sf,
        line_color = "#e31a1c",
        line_width = 2
      )
  }

  print(m)
}

#' Map comparing two cities side by side (open in browser)
map_compare_cities <- function(city1_pattern, city2_pattern) {
  message("Creating maps for comparison...")
  message("Note: Open each map in a separate browser tab to compare")

  map_city(city1_pattern)
  map_city(city2_pattern)
}

# =============================================================================
# QUICK START
# =============================================================================

cat("
================================================================================
  CENSUS DATA EXPLORATION SCRIPT
================================================================================

Data loaded:
- tracts_sf: Census tracts with poverty data (sf object)
- cities_sf: Cities with 100k+ population (sf object)

QUICK START FUNCTIONS:

  data_summary()              # Overall data summary
  list_cities(n = 50)         # List top 50 cities by population
  search_cities('seattle')    # Search for cities by name
  city_stats('seattle')       # Detailed stats for a city
  map_city('seattle')         # Interactive map of a city's tracts

EXAMPLES:

  # View data summary
  data_summary()


  # Find cities in Texas
  search_cities('texas')

  # Map Seattle with poverty rates
  map_city('Seattle')

  # Map Los Angeles
  map_city('Los Angeles')

  # Get detailed stats for Chicago
  city_stats('Chicago')

================================================================================
")

# Run data summary on load
data_summary()
