# prepare_data_for_databricks.R
# Enriches census data with derived columns for Databricks and Shiny dashboard

library(tidyverse)
library(arrow)

source("R/logging.R")
init_logging("prepare_data")

log_info("=== Preparing Data for Databricks ===")

# =============================================================================
# LOAD RAW DATA
# =============================================================================

log_info("Loading parquet files...")
tracts <- read_parquet("census_tracts_children_poverty.parquet")
cities <- read_parquet("cities_100k_children_poverty.parquet")

log_info("Loaded {nrow(tracts)} tracts and {nrow(cities)} cities")

# =============================================================================
# ENRICH TRACTS DATA
# =============================================================================

log_info("Enriching tract data...")

tracts_enriched <- tracts %>%
  mutate(
    # Poverty rate (handle division by zero)
    poverty_rate = if_else(
      total_children > 0,
      children_poverty / total_children * 100,
      NA_real_
    ),
    # Rounded for display
    poverty_rate_display = round(poverty_rate, 1),
    # Full tract label (e.g., "Census Tract 1082.02, Los Angeles County, California")
    tract_label = str_extract(NAME, "^[^,]+"),
    # Short tract number only (e.g., "1082.02")
    tract_number = str_extract(NAME, "(?<=Census Tract )\\S+"),
    # State FIPS for potential filtering
    state_fips = substr(GEOID, 1, 2)
  )

# Check for problematic values
na_rates <- sum(is.na(tracts_enriched$poverty_rate))
log_info("Tracts with NA poverty rate (0 children): {na_rates}")

# =============================================================================
# ENRICH CITIES DATA
# =============================================================================

log_info("Enriching city data...")

# Calculate tract counts per city
tract_counts <- tracts_enriched %>%
  count(primary_city_geoid, name = "tract_count")

cities_enriched <- cities %>%
  mutate(
    # City poverty rate
    city_poverty_rate = if_else(
      total_children > 0,
      children_poverty / total_children * 100,
      NA_real_
    ),
    city_poverty_rate_display = round(city_poverty_rate, 1),
    # State extracted from full name
    state_name = str_extract(NAME, "(?<=, )[^,]+$"),
    # Clean city name (remove suffixes like "city", "town", "CDP", "metropolitan government (balance)", etc.)
    city_name_clean = str_extract(NAME, "^[^,]+") %>%
      str_remove_all(regex(" (city|town|municipality|village|borough|CDP)$", ignore_case = TRUE)) %>%
      str_remove_all(regex(" (metropolitan|metro|unified|consolidated) government \\(balance\\)$", ignore_case = TRUE)),
    # Full label with state: "Dallas, Texas"
    city_label = paste0(city_name_clean, ", ", state_name)
  ) %>%
  select(-city_name_clean) %>%
  left_join(tract_counts, by = c("GEOID" = "primary_city_geoid"))

# =============================================================================
# SAVE ENRICHED DATA
# =============================================================================

log_info("Saving enriched data...")

write_parquet(tracts_enriched, "census_tracts_children_poverty_enriched.parquet")
write_parquet(cities_enriched, "cities_100k_children_poverty_enriched.parquet")

log_info("=== Data Preparation Complete ===")
log_info("Created: census_tracts_children_poverty_enriched.parquet")
log_info("Created: cities_100k_children_poverty_enriched.parquet")

# =============================================================================
# VERIFICATION
# =============================================================================

log_info("=== Verification ===")

# Tract summary
log_info("Tract columns: {paste(names(tracts_enriched), collapse = ', ')}")
log_info("Poverty rate range: {round(min(tracts_enriched$poverty_rate, na.rm = TRUE), 1)}% - {round(max(tracts_enriched$poverty_rate, na.rm = TRUE), 1)}%")

# City summary
log_info("City columns: {paste(names(cities_enriched), collapse = ', ')}")
log_info("Tracts per city range: {min(cities_enriched$tract_count, na.rm = TRUE)} - {max(cities_enriched$tract_count, na.rm = TRUE)}")

# Quick sample
log_info("Sample tract labels: {paste(head(tracts_enriched$tract_label, 3), collapse = ', ')}")
log_info("Sample city labels: {paste(head(cities_enriched$city_label, 3), collapse = ', ')}")
