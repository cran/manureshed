# Manureshed Interactive Dashboard
# ================================

library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(DT)

# Load manureshed package
library(manureshed)

# UI Definition
# =============

ui <- dashboardPage(

  # Header
  dashboardHeader(title = "Manureshed Analysis"),

  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Statistics", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Data Table", tabName = "data", icon = icon("table")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
    ),

    hr(),

    h4("Analysis Parameters", style = "padding-left: 15px;"),

    # Scale selection
    selectInput("scale", "Spatial Scale:",
                choices = c("County" = "county",
                            "HUC8 Watershed" = "huc8",
                            "HUC2 Region" = "huc2"),
                selected = "huc8"),

    # Year selection
    sliderInput("year", "Year:",
                min = 2007, max = 2016, value = 2016,
                step = 1, sep = ""),

    # Nutrient selection
    selectInput("nutrient", "Nutrient:",
                choices = c("Nitrogen" = "nitrogen",
                            "Phosphorus" = "phosphorus"),
                selected = "nitrogen"),

    # WWTP inclusion
    checkboxInput("include_wwtp",
                  "Include Wastewater Treatment Plants",
                  value = TRUE),

    # Advanced options
    actionLink("show_advanced", "Show advanced options"),

    conditionalPanel(
      condition = "input.show_advanced % 2 == 1",
      numericInput("threshold", "Cropland Threshold (acres):",
                   value = 1234, min = 0, max = 10000)
    ),

    hr(),

    # Run button
    actionButton("run", "Run Analysis",
                 icon = icon("play"),
                 class = "btn-primary btn-lg btn-block"),

    br(),

    # Download buttons
    downloadButton("download_data", "Download Data",
                   class = "btn-default btn-sm btn-block")

  ),

  # Body
  dashboardBody(
    tabItems(

      # Map tab
      tabItem(tabName = "map",
              fluidRow(
                box(
                  title = "Interactive Map",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  leafletOutput("map", height = "600px")
                )
              ),
              fluidRow(
                box(
                  title = "Legend",
                  status = "info",
                  width = 12,
                  htmlOutput("legend_text")
                )
              )
      ),

      # Statistics tab
      tabItem(tabName = "stats",
              fluidRow(
                valueBoxOutput("n_sources", width = 2),
                valueBoxOutput("n_sink_deficit", width = 2),
                valueBoxOutput("n_sink_fertilizer", width = 2),
                valueBoxOutput("n_balanced", width = 3),
                valueBoxOutput("n_excluded", width = 3)
              ),
              fluidRow(
                box(
                  title = "Classification Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("pie_chart")
                ),
                box(
                  title = "Surplus/Deficit Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("histogram")
                )
              ),
              fluidRow(
                box(
                  title = "Summary Statistics",
                  status = "info",
                  width = 12,
                  verbatimTextOutput("summary_stats")
                )
              )
      ),

      # Data table tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Results Data Table",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("results_table")
                )
              )
      ),

      # Help tab
      tabItem(tabName = "help",
              fluidRow(
                box(
                  title = "About Manureshed",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  h4("Manureshed Analysis Framework"),
                  p("This dashboard provides an interactive interface for analyzing
                    agricultural nutrient balances and wastewater treatment plant
                    nutrient loads across multiple spatial scales."),
                  h4("How to Use:"),
                  tags$ol(
                    tags$li("Select your analysis parameters in the sidebar"),
                    tags$li("Click 'Run Analysis' to process the data"),
                    tags$li("Explore results in the Map, Statistics, and Data tabs"),
                    tags$li("Download results using the download buttons")
                  ),
                  h4("Classifications:"),
                  tags$ul(
                    tags$li(tags$strong("Source:"), " Areas with nutrient surplus"),
                    tags$li(tags$strong("Sink (Deficit):"), " Areas with nutrient deficit (no fertilizer)"),
                    tags$li(tags$strong("Sink (Fertilizer):"), " Areas with deficit despite fertilizer use"),
                    tags$li(tags$strong("Balanced:"), " Areas with balanced nutrient flows"),
                    tags$li(tags$strong("Excluded:"), " Areas below cropland threshold")
                  ),
                  hr(),
                  p("For more information, see:"),
                  p(tags$code("?manureshed")),
                  p("Package version:", as.character(packageVersion("manureshed")))
                )
              )
      )
    )
  )
)


# Server Logic
# ============

server <- function(input, output, session) {

  # Reactive values
  rv <- reactiveValues(
    results = NULL,
    processing = FALSE
  )

  # Run analysis
  observeEvent(input$run, {

    rv$processing <- TRUE

    # Show progress
    withProgress(message = 'Running analysis...', value = 0, {

      incProgress(0.3, detail = "Loading data...")

      # Run analysis
      tryCatch({
        results <- run_builtin_analysis(
          scale = input$scale,
          year = input$year,
          nutrients = input$nutrient,
          include_wwtp = input$include_wwtp,
          cropland_threshold = if(input$show_advanced %% 2 == 1) input$threshold else NULL,
          verbose = FALSE
        )

        incProgress(0.7, detail = "Processing results...")

        rv$results <- results
        rv$processing <- FALSE

        incProgress(1.0, detail = "Complete!")

        showNotification("Analysis complete!", type = "message")

      }, error = function(e) {
        rv$processing <- FALSE
        showNotification(paste("Error:", e$message), type = "error", duration = NULL)
      })
    })
  })

  # Render map
  output$map <- renderLeaflet({
    req(rv$results)

    data <- rv$results$agricultural
    nutrient <- input$nutrient
    class_col <- paste0(toupper(substr(nutrient, 1, 1)), "_class")
    surplus_col <- paste0(toupper(substr(nutrient, 1, 1)), "_surplus")

    # Transform to WGS84 for leaflet
    data_wgs84 <- sf::st_transform(data, 4326)

    # Determine ID column and label based on scale
    # After join, all data has NAME column (standardized)
    if (input$scale == "county") {
      id_col <- "FIPS"          # Capital letters for county
      id_label <- "County FIPS"
      name_label <- "County"
    } else if (input$scale == "huc8") {
      id_col <- "huc8"          # Lowercase for HUC8
      id_label <- "HUC8 ID"
      name_label <- "HUC8 Name"
    } else {  # huc2
      id_col <- "huc2"          # Lowercase for HUC2
      id_label <- "HUC2 ID"
      name_label <- "HUC2 Name"
    }

    # NAME column is standardized across all scales after processing
    name_col <- "NAME"

    # Create popup text with proper labels
    data_wgs84$popup_text <- paste0(
      "<strong>", id_label, ":</strong> ", data_wgs84[[id_col]], "<br/>",
      "<strong>", name_label, ":</strong> ", data_wgs84[[name_col]], "<br/>",
      "<strong>Classification:</strong> ", gsub("_", " ", data_wgs84[[class_col]]), "<br/>",
      "<strong>Surplus:</strong> ",
      format(round(data_wgs84[[surplus_col]], 1), big.mark = ","), " kg<br/>",
      "<strong>Cropland:</strong> ",
      format(round(data_wgs84$cropland, 1), big.mark = ","), " acres"
    )

    # Color palette
    colors <- get_nutrient_colors(nutrient)
    pal <- colorFactor(palette = colors, domain = data_wgs84[[class_col]])

    # Create map
    leaflet(data_wgs84) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(data_wgs84[[class_col]]),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~popup_text,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = data_wgs84[[class_col]],
        title = paste(tools::toTitleCase(nutrient), "Classification"),
        position = "bottomright"
      )
  })


  # Value boxes
  # Sources
  output$n_sources <- renderValueBox({
    req(rv$results)

    # Use checkbox to decide which data to display
    # If unchecked, ALWAYS show agricultural even if integrated exists
    if (input$include_wwtp && "integrated" %in% names(rv$results) &&
        !is.null(rv$results$integrated[[input$nutrient]])) {
      class_col <- paste0("combined_", toupper(substr(input$nutrient, 1, 1)), "_class")
      data <- rv$results$integrated[[input$nutrient]]
    } else {
      class_col <- paste0(toupper(substr(input$nutrient, 1, 1)), "_class")
      data <- rv$results$agricultural
    }

    n <- sum(data[[class_col]] == "Source", na.rm = TRUE)
    valueBox(
      n,
      "Sources",
      icon = icon("arrow-up"),
      color = "red"
    )
  })

  # Sink - Deficit
  output$n_sink_deficit <- renderValueBox({
    req(rv$results)

    if (input$include_wwtp && "integrated" %in% names(rv$results) &&
        !is.null(rv$results$integrated[[input$nutrient]])) {
      class_col <- paste0("combined_", toupper(substr(input$nutrient, 1, 1)), "_class")
      data <- rv$results$integrated[[input$nutrient]]
    } else {
      class_col <- paste0(toupper(substr(input$nutrient, 1, 1)), "_class")
      data <- rv$results$agricultural
    }

    n <- sum(data[[class_col]] == "Sink_Deficit", na.rm = TRUE)
    valueBox(
      n,
      "Sink (Deficit)",
      icon = icon("arrow-down"),
      color = "blue"
    )
  })

  # Sink - Fertilizer
  output$n_sink_fertilizer <- renderValueBox({
    req(rv$results)

    if (input$include_wwtp && "integrated" %in% names(rv$results) &&
        !is.null(rv$results$integrated[[input$nutrient]])) {
      class_col <- paste0("combined_", toupper(substr(input$nutrient, 1, 1)), "_class")
      data <- rv$results$integrated[[input$nutrient]]
    } else {
      class_col <- paste0(toupper(substr(input$nutrient, 1, 1)), "_class")
      data <- rv$results$agricultural
    }

    n <- sum(data[[class_col]] == "Sink_Fertilizer", na.rm = TRUE)
    valueBox(
      n,
      "Sink (Fertilizer)",
      icon = icon("arrow-down"),
      color = "purple"
    )
  })

  # Balanced / Within_Watershed - THE KEY FIX IS HERE
  output$n_balanced <- renderValueBox({
    req(rv$results)

    # Determine the "within" classification name based on scale
    within_class <- if (input$scale == "county") "Within_County" else "Within_Watershed"
    label <- if (input$scale == "county") "Within County" else "Within Watershed"

    # Checkbox decides which data to show
    if (input$include_wwtp && "integrated" %in% names(rv$results) &&
        !is.null(rv$results$integrated[[input$nutrient]])) {
      # Show integrated data
      class_col <- paste0("combined_", toupper(substr(input$nutrient, 1, 1)), "_class")
      data <- rv$results$integrated[[input$nutrient]]
      n <- sum(data[[class_col]] == within_class, na.rm = TRUE)
    } else {
      # Show agricultural data
      class_col <- paste0(toupper(substr(input$nutrient, 1, 1)), "_class")
      data <- rv$results$agricultural
      n <- sum(data[[class_col]] == within_class, na.rm = TRUE)
    }

    valueBox(
      n,
      label,
      icon = icon("balance-scale"),
      color = "green"
    )
  })

  # Excluded
  output$n_excluded <- renderValueBox({
    req(rv$results)

    if (input$include_wwtp && "integrated" %in% names(rv$results) &&
        !is.null(rv$results$integrated[[input$nutrient]])) {
      class_col <- paste0("combined_", toupper(substr(input$nutrient, 1, 1)), "_class")
      data <- rv$results$integrated[[input$nutrient]]
    } else {
      class_col <- paste0(toupper(substr(input$nutrient, 1, 1)), "_class")
      data <- rv$results$agricultural
    }

    n <- sum(data[[class_col]] == "Excluded", na.rm = TRUE)
    valueBox(
      n,
      "Excluded",
      icon = icon("ban"),
      color = "light-blue"
    )
  })

  # Pie chart
  output$pie_chart <- renderPlotly({
    req(rv$results)

    if (input$include_wwtp && "integrated" %in% names(rv$results) &&
        !is.null(rv$results$integrated[[input$nutrient]])) {
      class_col <- paste0("combined_", toupper(substr(input$nutrient, 1, 1)), "_class")
      data <- rv$results$integrated[[input$nutrient]]
    } else {
      class_col <- paste0(toupper(substr(input$nutrient, 1, 1)), "_class")
      data <- rv$results$agricultural
    }

    class_counts <- table(data[[class_col]])

    plot_ly(
      labels = names(class_counts),
      values = as.numeric(class_counts),
      type = 'pie',
      marker = list(colors = get_nutrient_colors(input$nutrient))
    ) %>%
      layout(
        title = paste(tools::toTitleCase(input$nutrient), "Classification"),
        showlegend = TRUE
      )
  })

  # Histogram
  output$histogram <- renderPlotly({
    req(rv$results)

    if (input$include_wwtp && "integrated" %in% names(rv$results) &&
        !is.null(rv$results$integrated[[input$nutrient]])) {
      surplus_col <- paste0("combined_", toupper(substr(input$nutrient, 1, 1)), "_surplus")
      data <- rv$results$integrated[[input$nutrient]]
    } else {
      surplus_col <- paste0(toupper(substr(input$nutrient, 1, 1)), "_surplus")
      data <- rv$results$agricultural
    }

    surplus_data <- data[[surplus_col]]
    surplus_data <- surplus_data[!is.na(surplus_data) & is.finite(surplus_data)]

    plot_ly(x = surplus_data, type = "histogram") %>%
      layout(
        title = paste(tools::toTitleCase(input$nutrient), "Surplus/Deficit"),
        xaxis = list(title = "Surplus (kg, negative = deficit)"),
        yaxis = list(title = "Count")
      )
  })

  # Summary statistics
  output$summary_stats <- renderPrint({
    req(rv$results)

    if (input$include_wwtp && "integrated" %in% names(rv$results) &&
        !is.null(rv$results$integrated[[input$nutrient]])) {
      class_col <- paste0("combined_", toupper(substr(input$nutrient, 1, 1)), "_class")
      surplus_col <- paste0("combined_", toupper(substr(input$nutrient, 1, 1)), "_surplus")
      data <- rv$results$integrated[[input$nutrient]]
      data_type <- "Integrated (Agricultural + WWTP)"
    } else {
      class_col <- paste0(toupper(substr(input$nutrient, 1, 1)), "_class")
      surplus_col <- paste0(toupper(substr(input$nutrient, 1, 1)), "_surplus")
      data <- rv$results$agricultural
      data_type <- "Agricultural Only"
    }

    cat("Analysis Summary\n")
    cat("================\n\n")
    cat("Data type:", data_type, "\n")
    cat("Scale:", input$scale, "\n")
    cat("Year:", input$year, "\n")
    cat("Nutrient:", input$nutrient, "\n\n")

    cat("Total units:", nrow(data), "\n")
    cat("Units excluded:", sum(data[[class_col]] == "Excluded", na.rm = TRUE), "\n\n")

    cat("Classification counts:\n")
    print(table(data[[class_col]]))
    cat("\n")

    cat("Surplus statistics (kg):\n")
    print(summary(data[[surplus_col]]))
  })

  # Data table
  output$results_table <- renderDT({
    req(rv$results)

    data <- rv$results$agricultural
    data_df <- sf::st_drop_geometry(data)

    datatable(
      data_df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        searchHighlight = TRUE
      ),
      filter = 'top'
    )
  })

  # Legend text
  output$legend_text <- renderUI({
    HTML("
      <strong>Classification Legend:</strong><br/>
      <span style='color:#d62728;'></span> Source: Nutrient surplus<br/>
      <span style='color:#1f77b4;'></span> Sink (Deficit): Nutrient deficit, no fertilizer<br/>
      <span style='color:#9467bd;'></span> Sink (Fertilizer): Deficit despite fertilizer<br/>
      <span style='color:#2ca02c;'></span> Balanced: Near-zero balance<br/>
      <span style='color:#7f7f7f;'></span> Excluded: Below cropland threshold
    ")
  })

  # Download data
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("manureshed_", input$scale, "_", input$year, "_",
             input$nutrient, ".csv")
    },
    content = function(file) {
      req(rv$results)
      data <- sf::st_drop_geometry(rv$results$agricultural)
      write.csv(data, file, row.names = FALSE)
    }
  )


}

# Run the application
shinyApp(ui = ui, server = server)
