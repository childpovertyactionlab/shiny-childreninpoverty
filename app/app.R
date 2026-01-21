# app.R
# Child Poverty Census Data Dashboard
# Visualizes child poverty data across US cities with 100k+ population
#
# NOTE: Data is loaded from Databricks (sandbox.michael)
# TODO: Update to production schema before deployment

# =============================================================================
# SETUP
# =============================================================================

library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(sf)
library(httr2)
library(reactable)
library(mapgl)
library(cpaltemplates)
library(sysfonts)    # Required for cpaltemplates Google Fonts
library(showtext)    # Required for cpaltemplates Google Fonts
library(htmltools)
library(classInt)
library(shinyWidgets)  # For better dropdown inputs
library(shinyjs)       # For updating loading message text

# Load helper functions
source("R/utils.R")
source("R/databricks.R")

# Set Mapbox token for cpal_mapgl() basemaps (read from .Renviron)
# For shinyapps.io, set MAPBOX_PUBLIC_TOKEN in the app settings
if (Sys.getenv("MAPBOX_PUBLIC_TOKEN") == "") {
  stop("MAPBOX_PUBLIC_TOKEN must be set in .Renviron or shinyapps.io environment variables")
}

# Setup CPAL fonts
setup_cpal_google_fonts()

# =============================================================================
# LOAD DATA FROM DATABRICKS
# =============================================================================

# Databricks configuration
# NOTE: Currently using sandbox.michael for development
# TODO: Update to production schema before deployment
DATABRICKS_CATALOG <- "sandbox"
DATABRICKS_SCHEMA <- "michael"
TRACTS_TABLE <- "census_tracts_children_poverty"
CITIES_TABLE <- "cities_100k_children_poverty"

# Get Databricks credentials
DATABRICKS_HOST <- Sys.getenv("DATABRICKS_HOST")
DATABRICKS_TOKEN <- Sys.getenv("DATABRICKS_TOKEN")
DATABRICKS_WAREHOUSE_ID <- Sys.getenv("DATABRICKS_WAREHOUSE_ID")

if (DATABRICKS_HOST == "" || DATABRICKS_TOKEN == "" || DATABRICKS_WAREHOUSE_ID == "") {
  stop("DATABRICKS_HOST, DATABRICKS_TOKEN, and DATABRICKS_WAREHOUSE_ID must be set in .Renviron")
}

# Default city: Dallas, Texas
default_city <- "4819000"

# NOTE: Data is now loaded inside the server function to prevent startup timeout.
# The UI renders immediately with a loading spinner while data loads in the background.

# =============================================================================
# UI
# =============================================================================

ui <- page_navbar(
  # Include custom CSS and loading overlay
  header = tagList(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
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

  title = tags$span(
    tags$img(src = "https://raw.githubusercontent.com/childpovertyactionlab/cpaltemplates/main/inst/assets/cpal_logo.png", height = "30px", style = "margin-right: 10px;"),
    tags$span("Child Poverty Flipbook", class = "navbar-title-text")
  ),
  theme = cpal_dashboard_theme(),
  fillable = TRUE,

  # Loading overlay (hidden after data loads via conditionalPanel)
  conditionalPanel(
    condition = "!output.data_ready",
    div(
      class = "loading-overlay",
      div(
        class = "spinner-border text-primary",
        role = "status",
        style = "width: 3rem; height: 3rem;"
      ),
      p(id = "loading-message-text",
        "Connecting to data warehouse...",
        class = "mt-3",
        style = "font-size: 1.1rem; color: #004855;")
      # Note: #004855 is CPAL midnight color
    )
  ),

  # --------------------------------------------------------------------------
  # Tab 1: Where do children live?
  # --------------------------------------------------------------------------
  nav_panel(
    title = "Where Do Children Live?",
    value = "tab_children",

    # Controls and stat row
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), xl = c(6, 6)),
      fill = FALSE,
      class = "mb-3",

      # City selector on the left with data description
      card(
        class = "controls-compact",
        card_body(
          class = "p-2",
          uiOutput("city_picker_tab1"),
          # Data source description
          tags$p(
            class = "text-muted small mt-2 mb-0",
            "This dashboard shows data for all U.S. cities with a population over 100,000 residents. ",
            "Data is from the 2023 American Community Survey 5-Year Estimates."
          )
        )
      ),

      # Value box for headline stat on the right
      div(class = "valuebox-wrapper", uiOutput("valuebox_tab1"))
    ),

    # Map and table layout
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), xl = c(6, 6)),
      fill = TRUE,

      # Map
      card(
        card_header("Census Tracts by Total Children"),
        full_screen = TRUE,
        mapboxglOutput("map_tab1", height = "500px")
      ),

      # Table
      card(
        card_header("Tracts Ranked by Total Children"),
        full_screen = TRUE,
        reactableOutput("table_tab1")
      )
    )
  ),

  # --------------------------------------------------------------------------
  # Tab 2: Where do children in poverty live?
  # --------------------------------------------------------------------------
  nav_panel(
    title = "Where Do Children in Poverty Live?",
    value = "tab_poverty",

    # Controls and stat row
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), xl = c(6, 6)),
      fill = FALSE,
      class = "mb-3",

      # Stacked controls on the left
      card(
        class = "controls-compact",
        card_body(
          class = "p-2",
          # City selector (no label, uses placeholder)
          uiOutput("city_picker_tab2"),
          # Slider with inline label
          div(
            class = "d-flex align-items-center gap-2 mt-2",
            tags$small(class = "text-muted text-nowrap", "Top N tracts:"),
            div(
              class = "flex-grow-1",
              sliderInput(
                "topn_tab2",
                label = NULL,
                min = 0,
                max = 100,
                value = 0,
                step = 5,
                width = "100%"
              )
            )
          )
        )
      ),

      # Value box for headline stat on the right
      div(class = "valuebox-wrapper", uiOutput("valuebox_tab2"))
    ),

    # Map and table layout
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), xl = c(6, 6)),
      fill = TRUE,

      # Map
      card(
        card_header("Census Tracts by Children in Poverty"),
        full_screen = TRUE,
        mapboxglOutput("map_tab2", height = "500px")
      ),

      # Table
      card(
        card_header("Tracts Ranked by Children in Poverty"),
        full_screen = TRUE,
        reactableOutput("table_tab2")
      )
    )
  ),

  # --------------------------------------------------------------------------
  # Tab 3: Where are child poverty rates highest?
  # --------------------------------------------------------------------------
  nav_panel(
    title = "Where Are Poverty Rates Highest?",
    value = "tab_rates",

    # Controls and stat row
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), xl = c(6, 6)),
      fill = FALSE,
      class = "mb-3",

      # Stacked controls on the left
      card(
        class = "controls-compact",
        card_body(
          class = "p-2",
          # City selector (no label, uses placeholder)
          uiOutput("city_picker_tab3"),
          # Slider with inline label
          div(
            class = "d-flex align-items-center gap-2 mt-2",
            tags$small(class = "text-muted text-nowrap", "Top N tracts:"),
            div(
              class = "flex-grow-1",
              sliderInput(
                "topn_tab3",
                label = NULL,
                min = 0,
                max = 100,
                value = 0,
                step = 5,
                width = "100%"
              )
            )
          )
        )
      ),

      # Value box for headline stat on the right
      div(class = "valuebox-wrapper", uiOutput("valuebox_tab3"))
    ),

    # Map and table layout
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), xl = c(6, 6)),
      fill = TRUE,

      # Map
      card(
        card_header("Census Tracts by Child Poverty Rate"),
        full_screen = TRUE,
        mapboxglOutput("map_tab3", height = "500px")
      ),

      # Table
      card(
        card_header("Tracts Ranked by Poverty Rate"),
        full_screen = TRUE,
        reactableOutput("table_tab3")
      )
    )
  ),

  # Dark mode toggle on right side of navbar (after tabs)
  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light"))
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # --------------------------------------------------------------------------
  # Load data from Databricks (deferred to prevent startup timeout)
  # --------------------------------------------------------------------------

  # Reactive values to hold loaded data
  data_loaded <- reactiveVal(FALSE)
  tracts_data <- reactiveVal(NULL)
  cities_data <- reactiveVal(NULL)
  city_choices <- reactiveVal(NULL)

  # Helper function to update loading message via shinyjs
  update_loading_message <- function(msg) {
    shinyjs::html("loading-message-text", msg)
  }

  # Load data once on session start
  observe({
    # Only run once
    if (data_loaded()) return()

    message(">>> Starting data load...")

    tryCatch({
      # Stage 1: Query tracts
      update_loading_message("Loading census tract data...")
      message(">>> Querying tracts table...")
      tracts <- databricks_query(
        sql = sprintf("SELECT * FROM %s.%s.%s", DATABRICKS_CATALOG, DATABRICKS_SCHEMA, TRACTS_TABLE),
        host = DATABRICKS_HOST,
        token = DATABRICKS_TOKEN,
        warehouse_id = DATABRICKS_WAREHOUSE_ID
      )
      tracts_data(tracts)
      message(paste(">>> Tracts loaded:", nrow(tracts), "rows"))

      # Stage 2: Query cities
      update_loading_message("Loading city data...")
      message(">>> Querying cities table...")
      cities <- databricks_query(
        sql = sprintf("SELECT * FROM %s.%s.%s", DATABRICKS_CATALOG, DATABRICKS_SCHEMA, CITIES_TABLE),
        host = DATABRICKS_HOST,
        token = DATABRICKS_TOKEN,
        warehouse_id = DATABRICKS_WAREHOUSE_ID
      )
      cities_data(cities)
      message(paste(">>> Cities loaded:", nrow(cities), "rows"))

      # Stage 3: Prepare data
      update_loading_message("Preparing dashboard...")
      choices <- cities %>%
        arrange(city_label) %>%
        { setNames(.$GEOID, .$city_label) }
      city_choices(choices)

      # Mark data as loaded (this triggers conditionalPanel to hide overlay)
      data_loaded(TRUE)
      message(">>> Data loading complete! Overlay should now hide.")

    }, error = function(e) {
      message(paste(">>> ERROR loading data:", e$message))
      update_loading_message("Error loading data. Please refresh the page.")
      # Mark as loaded even on error so overlay hides and user sees error
      data_loaded(TRUE)
      showNotification(
        paste("Error loading data:", e$message),
        type = "error",
        duration = NULL
      )
    })
  })

  # Convenience accessors that wait for data
  tracts <- reactive({
    req(data_loaded())
    tracts_data()
  })

  cities <- reactive({
    req(data_loaded())
    cities_data()
  })

  # --------------------------------------------------------------------------
  # Output for conditionalPanel to check (controls loading overlay visibility)
  # --------------------------------------------------------------------------
  output$data_ready <- reactive({
    data_loaded()
  })
  # Critical: must evaluate even when not visible, otherwise conditionalPanel won't update
  outputOptions(output, "data_ready", suspendWhenHidden = FALSE)

  # --------------------------------------------------------------------------
  # Render city pickers (after data loads)
  # --------------------------------------------------------------------------

  output$city_picker_tab1 <- renderUI({
    req(city_choices())
    pickerInput(
      "city_tab1",
      label = NULL,
      choices = city_choices(),
      selected = default_city,
      options = pickerOptions(
        liveSearch = TRUE,
        liveSearchPlaceholder = "Search cities...",
        title = "Select a City",
        size = 10,
        dropupAuto = FALSE,
        container = "body"
      ),
      width = "100%"
    )
  })

  output$city_picker_tab2 <- renderUI({
    req(city_choices())
    pickerInput(
      "city_tab2",
      label = NULL,
      choices = city_choices(),
      selected = default_city,
      options = pickerOptions(
        liveSearch = TRUE,
        liveSearchPlaceholder = "Search cities...",
        title = "Select a City",
        size = 10,
        dropupAuto = FALSE,
        container = "body"
      ),
      width = "100%"
    )
  })

  output$city_picker_tab3 <- renderUI({
    req(city_choices())
    pickerInput(
      "city_tab3",
      label = NULL,
      choices = city_choices(),
      selected = default_city,
      options = pickerOptions(
        liveSearch = TRUE,
        liveSearchPlaceholder = "Search cities...",
        title = "Select a City",
        size = 10,
        dropupAuto = FALSE,
        container = "body"
      ),
      width = "100%"
    )
  })

  # --------------------------------------------------------------------------
  # Sync city selection across all tabs using shared reactive
  # --------------------------------------------------------------------------
  selected_city <- reactiveVal(default_city)

  # Update shared value when any tab's city changes
  observeEvent(input$city_tab1, {
    if (!is.null(input$city_tab1) && input$city_tab1 != selected_city()) {
      selected_city(input$city_tab1)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$city_tab2, {
    if (!is.null(input$city_tab2) && input$city_tab2 != selected_city()) {
      selected_city(input$city_tab2)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$city_tab3, {
    if (!is.null(input$city_tab3) && input$city_tab3 != selected_city()) {
      selected_city(input$city_tab3)
    }
  }, ignoreInit = TRUE)

  # Sync all pickers when shared value changes
  observeEvent(selected_city(), {
    city <- selected_city()
    if (!is.null(input$city_tab1) && input$city_tab1 != city) {
      updatePickerInput(session, "city_tab1", selected = city)
    }
    if (!is.null(input$city_tab2) && input$city_tab2 != city) {
      updatePickerInput(session, "city_tab2", selected = city)
    }
    if (!is.null(input$city_tab3) && input$city_tab3 != city) {
      updatePickerInput(session, "city_tab3", selected = city)
    }
  }, ignoreInit = TRUE)

  # --------------------------------------------------------------------------
  # Tab 1: Where do children live?
  # --------------------------------------------------------------------------

  # Filtered tracts for Tab 1
  city_tracts_tab1 <- reactive({
    req(input$city_tab1)
    tracts() %>%
      filter(primary_city_geoid == input$city_tab1) %>%
      arrange(desc(total_children))
  })

  # Value box stat for Tab 1
  # Uses city-level Census data (official figures) rather than tract aggregation
  output$valuebox_tab1 <- renderUI({
    req(input$city_tab1)
    city <- cities() %>% filter(GEOID == input$city_tab1)
    req(nrow(city) > 0)

    value_box(
      title = city$city_label,
      value = format(city$total_children, big.mark = ","),
      showcase = bsicons::bs_icon("people-fill"),
      theme = "secondary",
      p(paste0("children with ", city$city_poverty_rate_display, "% poverty rate")),
      p(class = "small", "Source: 2023 ACS 5-Year")
    )
  })

  # Map for Tab 1
  output$map_tab1 <- renderMapboxgl({
    req(nrow(city_tracts_tab1()) > 0)

    # Get current theme from dark mode toggle
    current_theme <- if (!is.null(input$dark_mode) && input$dark_mode == "dark") "dark" else "light"

    city_tracts_sf <- city_tracts_tab1() %>%
      rowwise() %>%
      mutate(
        popup_content = cpal_popup_html_metrics(
          title = tract_label,
          subtitle = GEOID,
          metrics = c(
            "Total Children" = format(total_children, big.mark = ","),
            "Children in Poverty" = format(children_poverty, big.mark = ","),
            "Poverty Rate" = paste0(poverty_rate_display, "%")
          ),
          theme = current_theme
        )
      ) %>%
      ungroup() %>%
      st_as_sf(wkt = "geometry_wkt", crs = 4326)

    # Jenks natural breaks for total children (continuous scale)
    # n=4 gives 5 breaks to match 5 colors for interpolate()
    jenks <- classIntervals(city_tracts_sf$total_children, n = 4, style = "jenks")
    breaks <- jenks$brks
    colors <- cpal_palette("midnight_seq_5")

    cpal_mapgl(theme = current_theme) |>
      fit_bounds(city_tracts_sf, padding = 20) |>
      add_fill_layer(
        id = "tracts",
        source = city_tracts_sf,
        fill_color = interpolate(
          column = "total_children",
          values = breaks,
          stops = colors
        ),
        fill_opacity = 0.7,
        fill_emissive_strength = 1,
        popup = "popup_content",
        hover_options = list(fill_opacity = 1, fill_outline_color = "#000000")
      ) |>
      add_line_layer(
        id = "tract_borders",
        source = city_tracts_sf,
        line_color = "#ffffff",
        line_width = 0.5
      ) |>
      add_cpal_popup_style(theme = current_theme) |>
      add_continuous_legend(
        legend_title = "Total Children",
        values = c(
          format(round(min(breaks)), big.mark = ","),
          format(round(max(breaks)), big.mark = ",")
        ),
        colors = colors,
        position = "bottom-left",
        style = cpal_legend_style(theme = current_theme)
      )
  })

  # Table for Tab 1
  output$table_tab1 <- renderReactable({
    req(nrow(city_tracts_tab1()) > 0)

    data <- city_tracts_tab1() %>%
      select(GEOID, tract_label, total_children, children_poverty, poverty_rate_display)

    max_val <- max(data$total_children, na.rm = TRUE)

    reactable(
      data,
      columns = list(
        GEOID = colDef(show = FALSE),
        tract_label = colDef(name = "Tract", sticky = "left", minWidth = 140),
        total_children = colDef(
          name = "Total Children",
          cell = function(value) {
            width <- paste0(value / max_val * 100, "%")
            bar_color <- cpal_color("midnight")
            div(
              style = list(display = "flex", alignItems = "center"),
              div(style = list(
                background = bar_color,
                width = width,
                height = "14px",
                marginRight = "8px",
                borderRadius = "2px"
              )),
              format(value, big.mark = ",")
            )
          }
        ),
        children_poverty = colDef(name = "In Poverty", format = colFormat(separators = TRUE)),
        poverty_rate_display = colDef(name = "Poverty Rate", format = colFormat(suffix = "%"))
      ),
      highlight = TRUE,
      compact = TRUE,
      defaultPageSize = 20,
      selection = "single",
      onClick = JS("function(rowInfo, column) {
        Shiny.setInputValue('selected_tract_tab1', rowInfo.row.GEOID, {priority: 'event'});
      }")
    )
  })

  # Observer to fly to selected tract on Tab 1

  observeEvent(input$selected_tract_tab1, {
    req(input$selected_tract_tab1)

    selected_tract <- city_tracts_tab1() %>%
      filter(GEOID == input$selected_tract_tab1) %>%
      st_as_sf(wkt = "geometry_wkt", crs = 4326)

    req(nrow(selected_tract) > 0)

    mapboxgl_proxy("map_tab1") |>
      fit_bounds(selected_tract, padding = 100, animate = TRUE)
  })

  # --------------------------------------------------------------------------
  # Tab 2: Where do children in poverty live?
  # --------------------------------------------------------------------------

  # Filtered tracts for Tab 2
  city_tracts_tab2 <- reactive({
    req(input$city_tab2)
    tracts() %>%
      filter(primary_city_geoid == input$city_tab2) %>%
      arrange(desc(children_poverty))
  })

  # Display tracts for Tab 2 (all if slider=0, top N otherwise)
  # NOTE: No req() inside - matches Tab 3 pattern to avoid render blocking
  display_tracts_tab2 <- reactive({
    all_tracts <- city_tracts_tab2()
    n <- input$topn_tab2
    if (!is.null(n) && n > 0) {
      all_tracts %>% head(n)
    } else {
      all_tracts
    }
  })

  # Value box stat for Tab 2
  output$valuebox_tab2 <- renderUI({
    req(nrow(city_tracts_tab2()) > 0)
    req(input$city_tab2)

    city <- cities() %>% filter(GEOID == input$city_tab2)
    city_name <- city$city_label
    all_tracts <- city_tracts_tab2()
    total_poverty <- sum(all_tracts$children_poverty, na.rm = TRUE)

    # Default to 10 when slider is 0
    n <- input$topn_tab2
    n_display <- if (is.null(n) || n == 0) 10 else n

    # Get top N tracts by children_poverty
    top_tracts <- all_tracts %>%
      arrange(desc(children_poverty)) %>%
      head(n_display)

    n_tracts <- nrow(top_tracts)
    top_poverty <- sum(top_tracts$children_poverty, na.rm = TRUE)
    pct_poverty <- round(top_poverty / total_poverty * 100, 1)

    value_box(
      title = paste0("Top ", n_tracts, " Tracts"),
      value = paste0(pct_poverty, "%"),
      showcase = bsicons::bs_icon("graph-up"),
      theme = "danger",
      p(paste0("of all child poverty in ", city_name)),
      p(class = "small", "Source: 2023 ACS 5-Year")
    )
  })

  # Map for Tab 2
  output$map_tab2 <- renderMapboxgl({
    # Validate base data first (matches Tab 3 pattern)
    req(nrow(city_tracts_tab2()) > 0)

    # Get current theme from dark mode toggle
    current_theme <- if (!is.null(input$dark_mode) && input$dark_mode == "dark") "dark" else "light"

    # Get all city tracts for bounds and breaks calculation
    all_tracts <- city_tracts_tab2()

    # Get display tracts (filtered or all)
    display_tracts <- display_tracts_tab2() %>%
      rowwise() %>%
      mutate(
        popup_content = cpal_popup_html_metrics(
          title = tract_label,
          subtitle = GEOID,
          metrics = c(
            "Children in Poverty" = format(children_poverty, big.mark = ","),
            "Total Children" = format(total_children, big.mark = ","),
            "Poverty Rate" = paste0(poverty_rate_display, "%")
          ),
          theme = current_theme
        )
      ) %>%
      ungroup() %>%
      st_as_sf(wkt = "geometry_wkt", crs = 4326)

    # Use ALL city tracts for bounds (consistent view)
    all_tracts_sf <- all_tracts %>%
      st_as_sf(wkt = "geometry_wkt", crs = 4326)

    # Calculate Jenks natural breaks on ALL city tracts (continuous scale)
    # n=4 gives 5 breaks to match 5 colors for interpolate()
    jenks <- classIntervals(all_tracts$children_poverty, n = 4, style = "jenks")
    breaks <- jenks$brks
    colors <- cpal_palette("coral_seq_5")

    cpal_mapgl(theme = current_theme) |>
      fit_bounds(all_tracts_sf, padding = 20) |>
      add_fill_layer(
        id = "tracts",
        source = display_tracts,
        fill_color = interpolate(
          column = "children_poverty",
          values = breaks,
          stops = colors
        ),
        fill_opacity = 0.7,
        fill_emissive_strength = 1,
        popup = "popup_content",
        hover_options = list(fill_opacity = 1, fill_outline_color = "#000000")
      ) |>
      add_line_layer(
        id = "tract_borders",
        source = display_tracts,
        line_color = "#ffffff",
        line_width = 0.5
      ) |>
      add_cpal_popup_style(theme = current_theme) |>
      add_continuous_legend(
        legend_title = "Children in Poverty",
        values = c(
          format(round(min(breaks)), big.mark = ","),
          format(round(max(breaks)), big.mark = ",")
        ),
        colors = colors,
        position = "bottom-left",
        style = cpal_legend_style(theme = current_theme)
      )
  })

  # Table for Tab 2
  output$table_tab2 <- renderReactable({
    req(nrow(city_tracts_tab2()) > 0)

    data <- city_tracts_tab2() %>%
      select(GEOID, tract_label, children_poverty, total_children, poverty_rate_display)

    max_val <- max(data$children_poverty, na.rm = TRUE)

    reactable(
      data,
      columns = list(
        GEOID = colDef(show = FALSE),
        tract_label = colDef(name = "Tract", sticky = "left", minWidth = 140),
        children_poverty = colDef(
          name = "Children in Poverty",
          cell = function(value) {
            width <- paste0(value / max_val * 100, "%")
            bar_color <- cpal_color("coral")
            div(
              style = list(display = "flex", alignItems = "center"),
              div(style = list(
                background = bar_color,
                width = width,
                height = "14px",
                marginRight = "8px",
                borderRadius = "2px"
              )),
              format(value, big.mark = ",")
            )
          }
        ),
        total_children = colDef(name = "Total Children", format = colFormat(separators = TRUE)),
        poverty_rate_display = colDef(name = "Poverty Rate", format = colFormat(suffix = "%"))
      ),
      highlight = TRUE,
      compact = TRUE,
      defaultPageSize = 20,
      selection = "single",
      onClick = JS("function(rowInfo, column) {
        Shiny.setInputValue('selected_tract_tab2', rowInfo.row.GEOID, {priority: 'event'});
      }")
    )
  })

  # Observer to fly to selected tract on Tab 2
  observeEvent(input$selected_tract_tab2, {
    req(input$selected_tract_tab2)

    selected_tract <- city_tracts_tab2() %>%
      filter(GEOID == input$selected_tract_tab2) %>%
      st_as_sf(wkt = "geometry_wkt", crs = 4326)

    req(nrow(selected_tract) > 0)

    mapboxgl_proxy("map_tab2") |>
      fit_bounds(selected_tract, padding = 100, animate = TRUE)
  })

  # --------------------------------------------------------------------------
  # Tab 3: Where are poverty rates highest?
  # --------------------------------------------------------------------------

  # Filtered tracts for Tab 3
  city_tracts_tab3 <- reactive({
    req(input$city_tab3)
    tracts() %>%
      filter(primary_city_geoid == input$city_tab3) %>%
      filter(!is.na(poverty_rate)) %>%
      arrange(desc(poverty_rate))
  })

  # Display tracts for Tab 3 (all if slider=0, top N otherwise)
  display_tracts_tab3 <- reactive({
    all_tracts <- city_tracts_tab3()
    n <- input$topn_tab3
    if (!is.null(n) && n > 0) {
      all_tracts %>% head(n)
    } else {
      all_tracts
    }
  })

  # Value box stat for Tab 3
  output$valuebox_tab3 <- renderUI({
    req(nrow(city_tracts_tab3()) > 0)
    req(input$city_tab3)

    city <- cities() %>% filter(GEOID == input$city_tab3)
    city_name <- city$city_label
    all_tracts <- city_tracts_tab3()
    total_poverty <- sum(all_tracts$children_poverty, na.rm = TRUE)

    # Default to 10 when slider is 0
    n <- input$topn_tab3
    n_display <- if (is.null(n) || n == 0) 10 else n

    # Get top N tracts by poverty_rate
    top_tracts <- all_tracts %>%
      arrange(desc(poverty_rate)) %>%
      head(n_display)

    n_tracts <- nrow(top_tracts)
    top_poverty <- sum(top_tracts$children_poverty, na.rm = TRUE)
    pct_poverty <- round(top_poverty / total_poverty * 100, 1)

    value_box(
      title = paste0("Top ", n_tracts, " Tracts"),
      value = paste0(pct_poverty, "%"),
      showcase = bsicons::bs_icon("graph-up"),
      theme = "success",
      p(paste0("of all child poverty in ", city_name)),
      p(class = "small", "Source: 2023 ACS 5-Year")
    )
  })

  # Map for Tab 3
  output$map_tab3 <- renderMapboxgl({
    req(nrow(city_tracts_tab3()) > 0)

    # Get current theme from dark mode toggle
    current_theme <- if (!is.null(input$dark_mode) && input$dark_mode == "dark") "dark" else "light"

    # Get all city tracts for bounds and breaks calculation
    all_tracts <- city_tracts_tab3()

    # Get display tracts (filtered or all)
    display_tracts <- display_tracts_tab3() %>%
      rowwise() %>%
      mutate(
        popup_content = cpal_popup_html_metrics(
          title = tract_label,
          subtitle = GEOID,
          metrics = c(
            "Poverty Rate" = paste0(poverty_rate_display, "%"),
            "Children in Poverty" = format(children_poverty, big.mark = ","),
            "Total Children" = format(total_children, big.mark = ",")
          ),
          theme = current_theme
        )
      ) %>%
      ungroup() %>%
      st_as_sf(wkt = "geometry_wkt", crs = 4326)

    # Use ALL city tracts for bounds (consistent view)
    all_tracts_sf <- all_tracts %>%
      st_as_sf(wkt = "geometry_wkt", crs = 4326)

    # Calculate Jenks natural breaks on ALL city tracts (continuous scale)
    # n=4 gives 5 breaks to match 5 colors for interpolate()
    jenks <- classIntervals(all_tracts$poverty_rate, n = 4, style = "jenks")
    breaks <- jenks$brks
    colors <- cpal_palette("sage_seq_5")

    cpal_mapgl(theme = current_theme) |>
      fit_bounds(all_tracts_sf, padding = 20) |>
      add_fill_layer(
        id = "tracts",
        source = display_tracts,
        fill_color = interpolate(
          column = "poverty_rate",
          values = breaks,
          stops = colors
        ),
        fill_opacity = 0.7,
        fill_emissive_strength = 1,
        popup = "popup_content",
        hover_options = list(fill_opacity = 1, fill_outline_color = "#000000")
      ) |>
      add_line_layer(
        id = "tract_borders",
        source = display_tracts,
        line_color = "#ffffff",
        line_width = 0.5
      ) |>
      add_cpal_popup_style(theme = current_theme) |>
      add_continuous_legend(
        legend_title = "Poverty Rate",
        values = c(
          paste0(round(min(breaks)), "%"),
          paste0(round(max(breaks)), "%")
        ),
        colors = colors,
        position = "bottom-left",
        style = cpal_legend_style(theme = current_theme)
      )
  })

  # Table for Tab 3
  output$table_tab3 <- renderReactable({
    req(nrow(city_tracts_tab3()) > 0)

    data <- city_tracts_tab3() %>%
      select(GEOID, tract_label, poverty_rate_display, children_poverty, total_children)

    reactable(
      data,
      columns = list(
        GEOID = colDef(show = FALSE),
        tract_label = colDef(name = "Tract", sticky = "left", minWidth = 140),
        poverty_rate_display = colDef(
          name = "Poverty Rate",
          cell = function(value) {
            width <- paste0(min(value, 100), "%")
            bar_color <- cpal_color("sage")
            div(
              style = list(display = "flex", alignItems = "center"),
              div(style = list(
                background = bar_color,
                width = width,
                height = "14px",
                marginRight = "8px",
                borderRadius = "2px"
              )),
              paste0(value, "%")
            )
          }
        ),
        children_poverty = colDef(name = "In Poverty", format = colFormat(separators = TRUE)),
        total_children = colDef(name = "Total Children", format = colFormat(separators = TRUE))
      ),
      highlight = TRUE,
      compact = TRUE,
      defaultPageSize = 20,
      selection = "single",
      onClick = JS("function(rowInfo, column) {
        Shiny.setInputValue('selected_tract_tab3', rowInfo.row.GEOID, {priority: 'event'});
      }")
    )
  })

  # Observer to fly to selected tract on Tab 3
  observeEvent(input$selected_tract_tab3, {
    req(input$selected_tract_tab3)

    selected_tract <- city_tracts_tab3() %>%
      filter(GEOID == input$selected_tract_tab3) %>%
      st_as_sf(wkt = "geometry_wkt", crs = 4326)

    req(nrow(selected_tract) > 0)

    mapboxgl_proxy("map_tab3") |>
      fit_bounds(selected_tract, padding = 100, animate = TRUE)
  })
}

# =============================================================================
# RUN APP
# =============================================================================

shinyApp(ui = ui, server = server)
