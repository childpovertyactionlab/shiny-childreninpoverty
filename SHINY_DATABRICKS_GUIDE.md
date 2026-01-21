# Building Shiny Apps with cpaltemplates and Databricks

A comprehensive guide for creating responsive Shiny dashboards that connect to Databricks, based on lessons learned from the Child Poverty Flipbook project.

---

## Table of Contents

1. [Project Structure](#project-structure)
2. [Environment Variables & Secrets](#environment-variables--secrets)
3. [Databricks Integration](#databricks-integration)
4. [cpaltemplates Theming](#cpaltemplates-theming)
5. [Responsive Design with bslib](#responsive-design-with-bslib)
6. [CSS Customization](#css-customization)
7. [mapgl Maps](#mapgl-maps)
8. [Deployment to shinyapps.io](#deployment-to-shinyappsio)
9. [Loading Overlays for Deferred Data Loading](#loading-overlays-for-deferred-data-loading)
10. [Common Issues & Solutions](#common-issues--solutions)

---

## Project Structure

Recommended folder structure for a Shiny app with Databricks:

```
project-root/
├── app/                          # Deploy this folder to shinyapps.io
│   ├── app.R                     # Main Shiny application
│   ├── R/
│   │   ├── databricks.R          # Databricks REST API functions
│   │   └── utils.R               # Helper functions
│   ├── www/
│   │   └── custom.css            # Custom styles
│   └── .Renviron                  # Secrets (gitignored, created for deployment)
├── scripts/
│   ├── download_data.R           # Data download scripts
│   └── upload_to_databricks.R    # Databricks upload script
├── R/
│   └── prepare_data.R            # Data preparation scripts
├── logs/                          # Log files (gitignored)
├── .Renviron                      # Local secrets (gitignored)
├── .gitignore
├── CLAUDE.md                      # Project instructions for Claude Code
└── README.md
```

**Key principle**: The `app/` folder should be self-contained and deployable on its own.

---

## Environment Variables & Secrets

### Required Variables

```
DATABRICKS_HOST=https://your-workspace.cloud.databricks.com
DATABRICKS_TOKEN=your-personal-access-token
DATABRICKS_WAREHOUSE_ID=your-sql-warehouse-id
MAPBOX_PUBLIC_TOKEN=your-mapbox-token
```

### Local Development

Create a `.Renviron` file in your project root:

```
DATABRICKS_HOST=https://your-workspace.cloud.databricks.com
DATABRICKS_TOKEN=dapi1234567890abcdef
DATABRICKS_WAREHOUSE_ID=abc123def456
MAPBOX_PUBLIC_TOKEN=pk.eyJ1...
```

Restart R after creating/modifying `.Renviron` for changes to take effect.

### Accessing in Code

```r
# Check if variables are set
if (Sys.getenv("DATABRICKS_HOST") == "") {
  stop("DATABRICKS_HOST must be set in .Renviron")
}

# Use the variables
host <- Sys.getenv("DATABRICKS_HOST")
token <- Sys.getenv("DATABRICKS_TOKEN")
```

### Security Best Practices

1. **Always gitignore `.Renviron`** - Add to `.gitignore`:
   ```
   .Renviron
   ```

2. **Never hardcode secrets** in R files

3. **Use Databricks personal access tokens** with minimal required permissions

4. **Rotate tokens periodically** and update in all environments

---

## Databricks Integration

### Why REST API Instead of ODBC?

The Databricks ODBC driver has critical limitations:

| Issue | ODBC Behavior | REST API Behavior |
|-------|---------------|-------------------|
| String truncation | Truncates to 1024 chars | Returns full strings |
| Geometry WKT | Corrupted (causes sf errors) | Works perfectly |
| Driver installation | Required on server | Not needed |
| shinyapps.io | Won't work | Works |

**Bottom line**: Always use REST API for Shiny apps that will be deployed.

### REST API Module (databricks.R)

Create `app/R/databricks.R`:

```r
#' Query Databricks using REST API
#' @param sql SQL query string
#' @param host Databricks workspace URL
#' @param token Personal access token
#' @param warehouse_id SQL warehouse ID
#' @return Data frame with query results
databricks_query <- function(sql, host, token, warehouse_id) {
  library(httr2)

  base_url <- paste0(host, "/api/2.0/sql/statements")

  # Build request
  body <- list(
    warehouse_id = warehouse_id,
    statement = sql,
    wait_timeout = "50s",
    disposition = "INLINE",
    format = "JSON_ARRAY",
    row_limit = 100000  # IMPORTANT: Default is only ~8k rows!
  )

  # Execute query
  resp <- request(base_url) |>
    req_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(body) |>
    req_perform()

  result <- resp_body_json(resp)

  # Check for errors
  if (result$status$state == "FAILED") {
    stop(paste("Query failed:", result$status$error$message))
  }

  # Poll if still running
  statement_id <- result$statement_id
  while (result$status$state %in% c("PENDING", "RUNNING")) {
    Sys.sleep(2)
    result <- request(paste0(base_url, "/", statement_id)) |>
      req_headers(Authorization = paste("Bearer", token)) |>
      req_perform() |>
      resp_body_json()
  }

  # Parse results
  parse_databricks_response(result, base_url, statement_id, token)
}

#' Parse Databricks response with chunked data support
parse_databricks_response <- function(result, base_url, statement_id, token) {
  # Get column info
  columns <- result$manifest$schema$columns
  col_names <- sapply(columns, function(x) x$name)
  col_types <- sapply(columns, function(x) x$type_name)

  # Get data (handle chunked responses)
  all_data <- result$result$data_array
  chunk_index <- result$result$next_chunk_index

  # Fetch additional chunks if they exist
  while (!is.null(chunk_index)) {
    chunk_url <- paste0(base_url, "/", statement_id, "/result/chunks/", chunk_index)
    chunk_resp <- request(chunk_url) |>
      req_headers(Authorization = paste("Bearer", token)) |>
      req_perform()
    chunk_result <- resp_body_json(chunk_resp)
    all_data <- c(all_data, chunk_result$data_array)
    chunk_index <- chunk_result$next_chunk_index
  }

  # Convert to data frame
  if (length(all_data) == 0) {
    return(data.frame())
  }

  df <- do.call(rbind, lapply(all_data, function(row) {
    as.data.frame(t(sapply(row, function(x) ifelse(is.null(x), NA, x))),
                  stringsAsFactors = FALSE)
  }))

  colnames(df) <- col_names

  # Convert column types
  for (i in seq_along(col_names)) {
    df[[col_names[i]]] <- convert_column_type(df[[col_names[i]]], col_types[i])
  }

  df
}

#' Convert Databricks types to R types
convert_column_type <- function(col, type_name) {
  switch(type_name,
    "INT" = as.integer(col),
    "LONG" = as.numeric(col),
    "DOUBLE" = as.numeric(col),
    "FLOAT" = as.numeric(col),
    "DECIMAL" = as.numeric(col),
    "BOOLEAN" = as.logical(col),
    col  # Default: keep as character
  )
}
```

### Usage in app.R

```r
source("R/databricks.R")

# Query data at app startup
tracts <- databricks_query(
  sql = "SELECT * FROM catalog.schema.tracts_table",
  host = Sys.getenv("DATABRICKS_HOST"),
  token = Sys.getenv("DATABRICKS_TOKEN"),
  warehouse_id = Sys.getenv("DATABRICKS_WAREHOUSE_ID")
)
```

### Critical Parameters

| Parameter | Default | Recommended | Why |
|-----------|---------|-------------|-----|
| `row_limit` | ~8,000 | 100,000+ | Default truncates large datasets |
| `wait_timeout` | 10s | 50s | Complex queries need more time |
| `format` | JSON | JSON_ARRAY | Easier to parse in R |

---

## cpaltemplates Theming

### Setup

```r
library(cpaltemplates)

# Setup Google Fonts (call once at app startup)
setup_cpal_google_fonts()

# Apply CPAL theme to dashboard
ui <- page_navbar(
  theme = cpal_dashboard_theme(),
  ...
)
```

### Color Palettes

```r
# Single colors
cpal_color("midnight")  # #1E3C5A
cpal_color("coral")     # #E86E4C
cpal_color("sage")      # #7A9B7A

# Sequential palettes (for maps)
cpal_palette("midnight_seq_5")  # 5-color blue gradient
cpal_palette("coral_seq_5")     # 5-color coral gradient
cpal_palette("sage_seq_5")      # 5-color green gradient
```

### Maps with cpal_mapgl

```r
library(mapgl)

# Create themed map
cpal_mapgl(theme = "light") |>  # or "dark"
  fit_bounds(sf_data, padding = 20) |>
  add_fill_layer(
    id = "layer_id",
    source = sf_data,
    fill_color = interpolate(
      column = "value_column",
      values = breaks,
      stops = cpal_palette("midnight_seq_5")
    ),
    fill_opacity = 0.7,
    popup = "popup_column",
    hover_options = list(fill_opacity = 1, fill_outline_color = "#000000")
  ) |>
  add_cpal_popup_style(theme = "light") |>
  add_continuous_legend(
    legend_title = "Legend Title",
    values = c("min", "max"),
    colors = cpal_palette("midnight_seq_5"),
    position = "bottom-left",
    style = cpal_legend_style(theme = "light")
  )
```

### Popup HTML

```r
# Create styled popup content
popup_html <- cpal_popup_html_metrics(
  title = "Tract Name",
  subtitle = "GEOID",
  metrics = c(
    "Metric 1" = "Value 1",
    "Metric 2" = "Value 2"
  ),
  theme = "light"  # or "dark"
)
```

---

## Responsive Design with bslib

### The fillable Layout Problem

bslib's `page_navbar()` with `fillable = TRUE` tries to make content fill available vertical space. This can cause **overlap issues at tablet sizes** (768px - 1199px).

**Symptom**: Elements overlap instead of stacking properly on tablets.

**Solution**: Add `fill = FALSE` to layout_columns that should NOT participate in fill behavior:

```r
# Controls row - should NOT fill
layout_columns(
  col_widths = breakpoints(sm = c(12, 12), xl = c(6, 6)),
  fill = FALSE,  # KEY: Prevents overlap at tablet sizes
  class = "mb-3",

  card(...),  # Controls
  div(...)    # Value box
)

# Map/table row - SHOULD fill remaining space
layout_columns(
  col_widths = breakpoints(sm = c(12, 12), xl = c(6, 6)),
  fill = TRUE,

  card(...),  # Map
  card(...)   # Table
)
```

### Breakpoints

| Breakpoint | Width | Typical Devices |
|------------|-------|-----------------|
| xs | <576px | Small phones |
| sm | ≥576px | Large phones |
| md | ≥768px | Tablets |
| lg | ≥992px | Small laptops |
| xl | ≥1200px | Desktops |
| xxl | ≥1400px | Large desktops |

### Value Boxes

```r
value_box(
  title = "Title",
  value = "123",
  showcase = bsicons::bs_icon("icon-name"),
  theme = "primary",  # or "secondary", "danger", "success", etc.
  p("Additional text"),
  p(class = "small", "Source: Data Source")
)
```

---

## CSS Customization

### File Location

Create `app/www/custom.css` and include in app:

```r
ui <- page_navbar(
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  ...
)
```

### Responsive Value Boxes

```css
/* Disable bslib's container queries that trigger "compact mode" */
.bslib-value-box {
  container-type: normal !important;
}

/* Responsive font sizes */
.bslib-value-box .value-box-title {
  font-size: clamp(0.75rem, 2vw, 1rem) !important;
}

.bslib-value-box .value-box-value {
  font-size: clamp(1.25rem, 4vw, 2.25rem) !important;
}

/* Dynamic icon sizing */
.bslib-value-box .value-box-showcase {
  font-size: clamp(1.25rem, 4vw, 2.5rem) !important;
}

/* Hide icon only on very small screens */
@media (max-width: 480px) {
  .bslib-value-box .value-box-showcase {
    display: none !important;
  }
}
```

### Responsive Sliders (Shiny sliderInput)

Shiny sliders use the ionRangeSlider library with these CSS classes:

- `.irs` - Main container
- `.irs-grid` - Grid container (ticks and labels)
- `.irs-grid-pol` - Tick marks
- `.irs-grid-text` - Labels (0, 10, 20, etc.)
- `.irs-grid-text-0`, `.irs-grid-text-5`, `.irs-grid-text-10` - Individual labels

```css
/* Hide intermediate labels on small screens */
@media (max-width: 400px) {
  .controls-compact .irs-grid-text {
    display: none;
  }

  /* Show only 0, 50, 100 */
  .controls-compact .irs-grid-text-0,
  .controls-compact .irs-grid-text-5,
  .controls-compact .irs-grid-text-10 {
    display: block;
  }
}

/* Even narrower: show only 0 and 100 */
@media (max-width: 370px) {
  .controls-compact .irs-grid-text-5 {
    display: none;
  }
}
```

### Navbar Title

```css
/* Responsive title */
.navbar-title-text {
  font-size: clamp(0.85rem, 2.5vw, 1.1rem);
  white-space: nowrap;
}

/* Hide title on very small screens, keep logo */
@media (max-width: 420px) {
  .navbar-title-text {
    display: none;
  }
}
```

---

## mapgl Maps

### st_bbox() Named Vector Issue

When using `st_bbox()` for calculations, always use `unname()` to avoid jsonlite serialization issues:

```r
bbox <- st_bbox(sf_data)

# WRONG - causes issues
center_lng <- (bbox["xmin"] + bbox["xmax"]) / 2

# CORRECT
center_lng <- unname((bbox["xmin"] + bbox["xmax"]) / 2)
center_lat <- unname((bbox["ymin"] + bbox["ymax"]) / 2)
```

### Jenks Natural Breaks

```r
library(classInt)

# Calculate breaks (n=4 gives 5 classes for 5 colors)
jenks <- classIntervals(data$value_column, n = 4, style = "jenks")
breaks <- jenks$brks
colors <- cpal_palette("midnight_seq_5")

# Use in map
add_fill_layer(
  ...,
  fill_color = interpolate(
    column = "value_column",
    values = breaks,
    stops = colors
  )
)
```

### Dark Mode Support

```r
# Get current theme
current_theme <- if (!is.null(input$dark_mode) && input$dark_mode == "dark") {
  "dark"
} else {
  "light"
}

# Apply to map
cpal_mapgl(theme = current_theme) |>
  ...
  add_cpal_popup_style(theme = current_theme) |>
  add_continuous_legend(..., style = cpal_legend_style(theme = current_theme))
```

---

## Deployment to shinyapps.io

### Pre-Deployment Checklist

1. ✅ App works locally with `shiny::runApp("app")`
2. ✅ All environment variables set in local `.Renviron`
3. ✅ `app/.Renviron` created with secrets (gitignored)
4. ✅ No hardcoded secrets in code
5. ✅ `.gitignore` includes `.Renviron`

### Create app/.Renviron for Deployment

**IMPORTANT**: shinyapps.io does NOT support the `envVars` parameter. You must include a `.Renviron` file in the app folder.

```bash
# Create app/.Renviron with your secrets
DATABRICKS_HOST=https://your-workspace.cloud.databricks.com
DATABRICKS_TOKEN=your-token
DATABRICKS_WAREHOUSE_ID=your-warehouse-id
MAPBOX_PUBLIC_TOKEN=your-mapbox-token
```

Make sure `app/.Renviron` is gitignored!

### Deploy

```r
# First time setup (one-time)
rsconnect::setAccountInfo(
  name = 'your-account',
  token = 'your-token',
  secret = 'your-secret'
)

# Deploy
rsconnect::deployApp(
  appDir = "app",
  appName = "your-app-name",
  appTitle = "Your App Title"
)
```

### Redeploy After Changes

```r
rsconnect::deployApp(appDir = "app")
```

### Common Deployment Errors

| Error | Cause | Solution |
|-------|-------|----------|
| "envVars only supported for Posit Connect" | Using envVars parameter | Use app/.Renviron instead |
| App crashes on load | Missing environment variables | Check app/.Renviron exists |
| "object not found" | Databricks query failed | Verify credentials |
| Package install fails | Version conflicts | Check deployment logs |

---

## Loading Overlays for Deferred Data Loading

### The Problem

When a Shiny app queries Databricks at startup (in the global scope), the entire app blocks until the query completes. If the Databricks warehouse is cold or slow, this can exceed shinyapps.io's startup timeout and crash the app before the UI even renders.

### The Solution

Defer data loading to inside the server function and show a loading overlay while data loads. Use `conditionalPanel` with a reactive output to control overlay visibility, and `shinyjs::html()` for dynamic status messages.

### Implementation

**UI: Add loading overlay with conditionalPanel and Bootstrap spinner**

```r
library(shiny)
library(bslib)
library(shinyjs)

ui <- page_navbar(
  header = tagList(
    useShinyjs(),  # Required for dynamic message updates
    tags$head(
      tags$style(HTML("
        .loading-overlay {
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background: rgba(255, 255, 255, 0.95);
          z-index: 9999;
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
        }
      "))
    )
  ),

  # Loading overlay - hidden when data_ready becomes TRUE
  conditionalPanel(
    condition = "!output.data_ready",
    div(
      class = "loading-overlay",
      # Bootstrap spinner with CPAL theming (text-primary = teal)
      div(
        class = "spinner-border text-primary",
        role = "status",
        style = "width: 3rem; height: 3rem;"
      ),
      # Static HTML with id for dynamic updates via shinyjs
      # NOTE: uiOutput/textOutput inside conditionalPanel does NOT work!
      p(id = "loading-message-text",
        "Connecting to data warehouse...",
        class = "mt-3",
        style = "font-size: 1.1rem; color: #666666;")
    )
  ),

  # Rest of UI...
  nav_panel(...)
)
```

**Server: Load data with stage-based message updates**

```r
server <- function(input, output, session) {

  # Reactive values for data
  data_loaded <- reactiveVal(FALSE)
  my_data <- reactiveVal(NULL)

  # Helper function to update loading message via shinyjs
  update_loading_message <- function(msg) {
    shinyjs::html("loading-message-text", msg)
  }

  # Load data on session start
  observe({
    if (data_loaded()) return()  # Only run once

    tryCatch({
      # Stage 1: First query
      update_loading_message("Loading census tract data...")
      tracts <- databricks_query(...)

      # Stage 2: Second query
      update_loading_message("Loading city data...")
      cities <- databricks_query(...)

      # Stage 3: Finalize
      update_loading_message("Preparing dashboard...")
      # ... prepare data ...

      data_loaded(TRUE)

    }, error = function(e) {
      update_loading_message("Error loading data. Please refresh.")
      data_loaded(TRUE)  # Hide overlay even on error
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # CRITICAL: Output for conditionalPanel to check
  output$data_ready <- reactive({
    data_loaded()
  })
  # CRITICAL: Must set suspendWhenHidden = FALSE or conditionalPanel won't update!
  outputOptions(output, "data_ready", suspendWhenHidden = FALSE)

  # Convenience accessor that waits for data
  data <- reactive({
    req(data_loaded())
    my_data()
  })

  # Rest of server logic...
}
```

### Why This Works

1. **UI renders immediately** - The browser shows the loading spinner right away
2. **Data loads in background** - The `observe()` runs after UI is rendered
3. **conditionalPanel watches output.data_ready** - JavaScript evaluates `!output.data_ready`
4. **`suspendWhenHidden = FALSE` is critical** - Without this, Shiny won't evaluate the output when conditionalPanel hides it, so it never updates
5. **`shinyjs::html()` updates static text** - Works reliably unlike uiOutput inside conditionalPanel

### What Doesn't Work

| Approach | Problem |
|----------|---------|
| `uiOutput()` / `textOutput()` inside `conditionalPanel` | **Does not render** - outputs inside conditionalPanel have timing/rendering issues |
| `shinyjs::hide()` | May not initialize properly in `page_navbar()` |
| `removeUI()` | Doesn't work reliably with elements in `page_navbar()` |
| Global data loading | Blocks startup, causes timeout on shinyapps.io |

### Key Gotcha: Dynamic Text in conditionalPanel

**Problem**: You cannot use `uiOutput()` or `textOutput()` inside a `conditionalPanel` - the content simply won't render, even with `suspendWhenHidden = FALSE`.

**Solution**: Use static HTML with an `id` attribute and update it dynamically with `shinyjs::html()`:

```r
# In UI - static HTML with id
p(id = "my-message", "Initial message...")

# In server - update via shinyjs
shinyjs::html("my-message", "New message!")
```

---

## Common Issues & Solutions

### 1. Only ~8,000 Rows Returned from Databricks

**Cause**: Default API row limit

**Solution**: Set `row_limit = 100000` in request body

### 2. "OGR: Corrupt data" When Converting to sf

**Cause**: Geometry WKT strings truncated by ODBC driver

**Solution**: Use REST API instead of ODBC

### 3. Elements Overlap at Tablet Sizes

**Cause**: bslib fillable layout distributing space incorrectly

**Solution**: Add `fill = FALSE` to layout_columns for non-filling rows

### 4. Slider Gets Clipped on Small Screens

**Cause**: Container overflow or insufficient padding

**Solution**: Add responsive CSS to hide labels at narrow widths

### 5. Value Box Content Clipped

**Cause**: bslib container queries triggering compact mode

**Solution**: Add `container-type: normal !important` to `.bslib-value-box`

### 6. Map Doesn't Render

**Cause**: Missing Mapbox token or invalid sf geometry

**Solution**:
- Verify `MAPBOX_PUBLIC_TOKEN` is set
- Check geometry with `st_is_valid()`
- Use `st_make_valid()` to fix invalid geometries

### 7. Secrets Exposed in Git

**Cause**: `.Renviron` not gitignored

**Solution**: Add `.Renviron` to `.gitignore` immediately, rotate exposed tokens

### 8. App Crashes on Startup / Timeout on shinyapps.io

**Cause**: Data loading in global scope blocks app startup; cold Databricks warehouse takes too long

**Solution**: Defer data loading to server function with loading overlay. See [Loading Overlays for Deferred Data Loading](#loading-overlays-for-deferred-data-loading).

### 9. Loading Spinner Doesn't Disappear

**Cause**: Using `shinyjs::hide()` or `removeUI()` which don't work reliably in `page_navbar()`

**Solution**: Use `conditionalPanel` with a reactive output and `outputOptions(..., suspendWhenHidden = FALSE)`. See [Loading Overlays for Deferred Data Loading](#loading-overlays-for-deferred-data-loading).

### 10. Dynamic Text Not Showing in Loading Overlay

**Cause**: Using `uiOutput()` or `textOutput()` inside `conditionalPanel` - these outputs don't render properly due to timing/rendering issues

**Solution**: Use static HTML with an `id` attribute and update dynamically with `shinyjs::html()`:

```r
# UI - static HTML
p(id = "loading-message", "Initial message...")

# Server - update via shinyjs
shinyjs::html("loading-message", "New message!")
```

See [Loading Overlays for Deferred Data Loading](#loading-overlays-for-deferred-data-loading).

---

## Quick Reference

### Essential Packages

```r
library(shiny)
library(bslib)
library(bsicons)
library(shinyjs)       # For dynamic loading message updates
library(tidyverse)
library(sf)
library(httr2)
library(reactable)
library(mapgl)
library(cpaltemplates)
library(shinyWidgets)
library(classInt)
```

### Minimal app.R Template

```r
library(shiny)
library(bslib)
library(cpaltemplates)

source("R/databricks.R")

# Check environment
stopifnot(Sys.getenv("DATABRICKS_HOST") != "")

# Load data
data <- databricks_query(
 sql = "SELECT * FROM catalog.schema.table",
 host = Sys.getenv("DATABRICKS_HOST"),
 token = Sys.getenv("DATABRICKS_TOKEN"),
 warehouse_id = Sys.getenv("DATABRICKS_WAREHOUSE_ID")
)

setup_cpal_google_fonts()

ui <- page_navbar(
 title = "App Title",
 theme = cpal_dashboard_theme(),
 nav_panel("Tab 1", ...)
)

server <- function(input, output, session) {
 ...
}

shinyApp(ui, server)
```

---

## Resources

- [bslib documentation](https://rstudio.github.io/bslib/)
- [mapgl documentation](https://walkerke.github.io/mapgl/)
- [Databricks SQL Statement API](https://docs.databricks.com/api/workspace/statementexecution)
- [shinyapps.io User Guide](https://docs.posit.co/shinyapps.io/)
- [cpaltemplates GitHub](https://github.com/childpovertyactionlab/cpaltemplates)

---

*Last updated: 2026-01-21*
*Based on: Child Poverty Flipbook project*
*Loading overlay pattern refined after debugging uiOutput/conditionalPanel issues*
