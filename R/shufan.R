#' @import shiny
shufan_ui <- function() {
  # Load required libraries
  library(ggplot2)
  library(dplyr)
  library(plotly)
  library(DT)
  library(readxl)
  library(leaflet)

  fluidPage(
    titlePanel("Vehicle Traffic and Emission Analysis"),

    sidebarLayout(
      sidebarPanel(
        width = 3,

        # Sheet selection
        selectInput("sheet_select", "Select Time Period:",
                    choices = c("Off-Peak Hour", "Peak Hour"),
                    selected = "Peak Hour"),
        # Pollutant selection
        selectInput("pollutant_select", "Select Pollutant:",
                    choices = c("CO" = "CO Emission Intensity (g/km/h)"),
                    selected = "CO Emission Intensity (g/km/h)"),
        # Traffic flow selection
        selectInput("traffic_select", "Select Vehicle Type:",
                    choices = c("Heavy Duty Vehicles (HDV)" = "HDV Flow (veh/h)",
                                "Light Duty Vehicles (LDV)" = "LDV Flow (veh/h)",
                                "New Energy Vehicles (NEV)" = "NEV Flow (veh/h)",
                                "Total Vehicle Flow" = "Total Vehicle Flow(veh/h)"),
                    selected = "Total Vehicle Flow(veh/h)"),

        # Range slection
        sliderInput("flow_range", "Traffic Flow Range (veh/h):",
                    min = 0, max = 27000, value = c(0, 27000), step = 100),
        sliderInput("emission_range", "Emission Intensity Range (g/km/h):",
                    min = 0, max = 16000, value = c(0, 16000), step = 100),

        # Visualization controls
        checkboxInput("show_trend", "Show Trend Line", value = TRUE),

        # Statistics section header
        h4("Data Summary"),
        helpText("Statistical summaries for selected data")),

      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("Visualization",
                   fluidRow(
                     plotlyOutput("traffic_emission_scatter", height = "600px"))),

          tabPanel("Distribution",
                   fluidRow(
                     column(6,
                            h4("Traffic Flow Distribution"),
                            plotlyOutput("traffic_histogram", height = "300px"),
                            plotlyOutput("traffic_boxplot", height = "300px")),
                     column(6,
                            h4("Emission Intensity Distribution"),
                            plotlyOutput("emission_histogram", height = "300px"),
                            plotlyOutput("emission_boxplot", height = "300px")))),

          tabPanel("Statistics",
                   fluidRow(
                     column(6,
                            h4("CO Statistics"),
                            DTOutput("pollution_stats")),
                     column(6,
                            h4("Traffic Statistics"),
                            DTOutput("traffic_stats"))),
                   br(),
                   fluidRow(
                     column(8,
                            h4("Traffic Statistical Tests"),
                            DTOutput("statistical_tests")))),

          tabPanel("Map",
                   fluidRow(
                     leafletOutput("map_plot", height = "650px"))),

          tabPanel("Vehicle Contribution",
                   fluidRow(
                     column(10,
                            plotlyOutput("vehicle_contribution_plot", height = "400px"))),
                   br(),
                   fluidRow(
                     column(6,
                            DTOutput("vehicle_contribution_table")))),

          tabPanel("Data Table",
                   fluidRow(
                     DTOutput("data_table")))

        )
      )
    )
  )
}

#' @import shiny

shufan_server <- function(input, output, session) {

  # Load data
  loaded_data <- reactive({
    file_path <- system.file("data/Traffic flow and emission data.xlsx", package = 'env405')
    sheets <- excel_sheets(file_path)
    data_list <- list()
    for(sheet in sheets) {
      data_list[[sheet]] <- read_excel(file_path, sheet = sheet)
    }
    return(data_list)
  })

  # Get current filtered data
  current_data <- reactive({
    req(loaded_data(), input$sheet_select)
    data <- loaded_data()[[input$sheet_select]]

    # Apply filters
    traffic_col <- input$traffic_select
    pollutant_col <- input$pollutant_select
    data <- data %>%
      filter(.data[[traffic_col]] >= input$flow_range[1] &
               .data[[traffic_col]] <= input$flow_range[2]) %>%
      filter(.data[[pollutant_col]] >= input$emission_range[1] &
               .data[[pollutant_col]] <= input$emission_range[2])
    return(data)
  })

  # Get all numeric data for correlation analysis
  numeric_data <- reactive({
    data <- current_data()
    req(data)

    # Select only numeric columns for correlation analysis
    numeric_cols <- sapply(data, is.numeric)
    data_numeric <- data[, numeric_cols]

    # Remove columns with zero variance
    variances <- apply(data_numeric, 2, var, na.rm = TRUE)
    data_numeric <- data_numeric[, variances > 0, drop = FALSE]

    return(data_numeric)
  })

  # Main scatter plot
  output$traffic_emission_scatter <- renderPlotly({
    data <- current_data()
    req(data)
    traffic_col <- input$traffic_select
    pollutant_col <- input$pollutant_select

    # Create base plot
    p <- ggplot(data) +
      geom_point(aes(x = .data[[traffic_col]],
                     y = .data[[pollutant_col]],
                     text = paste("Site:", ID,
                                  "<br>Traffic:", round(.data[[traffic_col]], 1), "veh/h",
                                  "<br>Emission:", round(.data[[pollutant_col]], 2), "g/km/h")),
                 color = "steelblue", size = 3, alpha = 0.7)

    # Add trend line if selected
    if (input$show_trend) {
      p <- p + geom_smooth(aes(x = .data[[traffic_col]],
                               y = .data[[pollutant_col]]), data = data, method = "loess", se = FALSE, color = "darkorange", linewidth = 1)
    }

    # Axis labels
    x_label <- gsub(" Flow (veh/h)", "", traffic_col)
    y_label <- gsub(" Emission Intensity (g/km/h)", "", pollutant_col)

    # Add labels and theme
    p <- p +
      labs(title = paste("Relationship between", x_label, "and", y_label),
           subtitle = paste("Time Period:", input$sheet_select),
           x = paste(traffic_col),
           y = paste(pollutant_col)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(size = 10))

    ggplotly(p, tooltip = "text", height = 600) %>%
      layout(hoverlabel = list(bgcolor = "white",
                               font = list(size = 10)),
             margin = list(t = 80))
  })

  # Traffic Flow Histogram
  output$traffic_histogram <- renderPlotly({
    data <- current_data()
    req(data)

    traffic_col <- input$traffic_select

    p <- ggplot(data, aes(x = .data[[traffic_col]])) +
      geom_histogram(aes(y = ..density..),
                     bins = 30,
                     fill = "steelblue",
                     alpha = 0.7,
                     color = "white") +
      geom_density(alpha = 0.2, fill = "orange", color = "darkorange") +
      labs(title = "Traffic Flow Distribution",
           x = traffic_col,
           y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 12),
            axis.title = element_text(size = 10))

    ggplotly(p, height = 300) %>%
      layout(margin = list(t = 50, b = 50))
  })

  # Traffic Flow Boxplot
  output$traffic_boxplot <- renderPlotly({
    data <- current_data()
    req(data)

    traffic_col <- input$traffic_select

    p <- ggplot(data, aes(y = .data[[traffic_col]])) +
      geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "red") +
      labs(title = "Traffic Flow Boxplot",
           y = traffic_col) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 12),
            axis.title = element_text(size = 10),
            axis.text.x = element_blank())

    ggplotly(p, height = 300) %>%
      layout(margin = list(t = 50, b = 50))
  })

  # Emission Intensity Histogram
  output$emission_histogram <- renderPlotly({
    data <- current_data()
    req(data)

    pollutant_col <- input$pollutant_select

    p <- ggplot(data, aes(x = .data[[pollutant_col]])) +
      geom_histogram(aes(y = ..density..),
                     bins = 30,
                     fill = "darkorange",
                     alpha = 0.7,
                     color = "white") +
      geom_density(alpha = 0.2, fill = "steelblue", color = "darkblue") +
      labs(title = "Emission Intensity Distribution",
           x = pollutant_col,
           y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 12),
            axis.title = element_text(size = 10))

    ggplotly(p, height = 300) %>%
      layout(margin = list(t = 50, b = 50))
  })

  # Emission Intensity Boxplot
  output$emission_boxplot <- renderPlotly({
    data <- current_data()
    req(data)

    pollutant_col <- input$pollutant_select

    p <- ggplot(data, aes(y = .data[[pollutant_col]])) +
      geom_boxplot(fill = "darkorange", alpha = 0.7, outlier.color = "red") +
      labs(title = "Emission Intensity Boxplot",
           y = pollutant_col) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 12),
            axis.title = element_text(size = 10),
            axis.text.x = element_blank())

    ggplotly(p, height = 300) %>%
      layout(margin = list(t = 50, b = 50))
  })

  # Pollution statistics
  output$pollution_stats <- renderDT({
    data <- current_data()
    req(data)

    pollutant_col <- input$pollutant_select

    stats <- data %>%
      summarise(
        Mean = round(mean(.data[[pollutant_col]], na.rm = TRUE), 2),
        Median = round(median(.data[[pollutant_col]], na.rm = TRUE), 2),
        SD = round(sd(.data[[pollutant_col]], na.rm = TRUE), 2),
        Min = round(min(.data[[pollutant_col]], na.rm = TRUE), 2),
        Max = round(max(.data[[pollutant_col]], na.rm = TRUE), 2),
        IQR = round(IQR(.data[[pollutant_col]], na.rm = TRUE), 2)
      ) %>%
      t() %>% as.data.frame() %>%
      rename(Value = V1)

    stats$Statistic <- rownames(stats)
    stats <- stats[, c("Statistic", "Value")]

    datatable(stats,
              options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE),
              rownames = FALSE)
  })

  # Traffic statistics
  output$traffic_stats <- renderDT({
    data <- current_data()
    req(data)

    traffic_col <- input$traffic_select

    stats <- data %>%
      summarise(
        Mean = round(mean(.data[[traffic_col]], na.rm = TRUE), 2),
        Median = round(median(.data[[traffic_col]], na.rm = TRUE), 2),
        SD = round(sd(.data[[traffic_col]], na.rm = TRUE), 2),
        Min = round(min(.data[[traffic_col]], na.rm = TRUE), 2),
        Max = round(max(.data[[traffic_col]], na.rm = TRUE), 2),
        IQR = round(IQR(.data[[traffic_col]], na.rm = TRUE), 2)
      ) %>%
      t() %>% as.data.frame() %>%
      rename(Value = V1)

    stats$Statistic <- rownames(stats)
    stats <- stats[, c("Statistic", "Value")]

    datatable(stats,
              options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE),
              rownames = FALSE)
  })

  # Statistical Tests output
  output$statistical_tests <- renderDT({
    data <- current_data()
    req(data, nrow(data) > 2)

    traffic_col <- input$traffic_select
    pollutant_col <- input$pollutant_select

    tryCatch({
      # Get the data for selected columns
      traffic_data <- data[[traffic_col]]
      pollution_data <- data[[pollutant_col]]

      # Perform normality tests (Shapiro-Wilk test)
      normality_traffic <- shapiro.test(traffic_data)
      normality_pollution <- shapiro.test(pollution_data)

      # Perform Spearman's rank correlation test
      spearman_test <- cor.test(traffic_data, pollution_data,
                                method = "spearman", exact = FALSE)

      # Create correlation strength description
      strength <- ifelse(abs(spearman_test$estimate) > 0.7, "Strong",
                         ifelse(abs(spearman_test$estimate) > 0.3, "Moderate", "Weak"))

      # Determine direction and significance
      direction <- if (spearman_test$estimate > 0) "Positive" else "Negative"
      significance <- if (spearman_test$p.value < 0.001) "***" else
        if (spearman_test$p.value < 0.01) "**" else
          if (spearman_test$p.value < 0.05) "*" else "Not significant"

      # Create results table
      tests_df <- data.frame(
        "N" = format(nrow(data), big.mark = ","),
        "Complete" = format(length(traffic_data), big.mark = ","),
        "Traffic Norm" = ifelse(normality_traffic$p.value < 0.05, "Non-normal", "Normal"),
        "Pollution Norm" = ifelse(normality_pollution$p.value < 0.05, "Non-normal", "Normal"),
        "Spearman ρ" = round(spearman_test$estimate, 3),
        "P" = format.pval(spearman_test$p.value, digits = 3),
        "Strength" = strength,
        "Direction" = direction,
        "Sig" = significance,
        check.names = FALSE
      )

      datatable(tests_df,
                options = list(
                  dom = 't',
                  paging = FALSE,
                  searching = FALSE,
                  ordering = FALSE
                ),
                rownames = FALSE,
                caption = "Statistical Test Results") %>%
        formatStyle(columns = c(0:8), fontSize = '12px')

    })
  })

  # Vehicle Contribution
  output$vehicle_contribution_plot <- renderPlotly({
    data <- current_data()
    req(data)
    pollutant_col <- input$pollutant_select

    # Use available flows
    predictors <- c("HDV Flow (veh/h)",
                    "LDV Flow (veh/h)",
                    "NEV Flow (veh/h)")
    predictors <- predictors[predictors %in% colnames(data)]

    # Build regression formula
    formula <- as.formula(
      paste0("`", pollutant_col, "` ~ `",
             paste(predictors, collapse = "` + `"), "`")
    )

    # Fit model
    model <- lm(formula, data = data)

    # Standardized coefficients
    beta <- lm.beta(model)

    beta_df <- data.frame(
      Vehicle = names(beta),
      Standardized_Effect = as.numeric(beta)
    )

    p <- ggplot(beta_df, aes(x = reorder(Vehicle, Standardized_Effect),
                             y = Standardized_Effect, fill = Standardized_Effect)) +
      geom_col() +
      coord_flip() +
      labs(title = "Standardized Contribution of Each Vehicle Type to CO Emissions",
           x = "Vehicle Category",
           y = "Standardized Effect (β)") +
      theme_minimal() +
      theme(plot.title = element_text(size = 11),
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 10))
    ggplotly(p)
  })


  output$vehicle_contribution_table <- renderDT({
    data <- current_data()
    req(data)

    pollutant_col <- input$pollutant_select

    predictors <- c("HDV Flow (veh/h)",
                    "LDV Flow (veh/h)",
                    "NEV Flow (veh/h)")

    predictors <- predictors[predictors %in% colnames(data)]

    formula <- as.formula(
      paste0("`", pollutant_col, "` ~ `",
             paste(predictors, collapse = "` + `"), "`")
    )

    model <- lm(formula, data = data)

    summary_m <- summary(model)

    effect_df <- data.frame(
      Vehicle = predictors,
      Estimate = summary_m$coefficients[-1, 1],
      Std.Error = summary_m$coefficients[-1, 2],
      t.value = summary_m$coefficients[-1, 3],
      p.value = summary_m$coefficients[-1, 4]
    )

    # Add significance marks
    effect_df$Significance <- cut(effect_df$p.value,
                                  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                                  labels = c("***", "**", "*", "ns"))

    datatable(effect_df,
              options = list(dom = 't', paging = FALSE),
              rownames = FALSE)
  })

  # Map visualization
  output$map_plot <- renderLeaflet({
    data <- current_data()
    req(data)

    traffic_col <- input$traffic_select
    pollutant_col <- input$pollutant_select

    # Normalize marker size based on traffic flow
    flow_values <- data[[traffic_col]]
    flow_scaled <- (flow_values - min(flow_values)) /
      (max(flow_values) - min(flow_values) + 1e-10)
    marker_sizes <- 1 + flow_scaled * 10

    # Color scale for emission
    emission_values <- data[[pollutant_col]]
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = emission_values
    )

    # Create popup content
    popup_content <- paste0(
      "<b>Site ID: </b>", data$ID, "<br>",
      "<b>Flow: </b>", round(data[[traffic_col]], 1), " veh/h<br>",
      "<b>Emission: </b>", round(data[[pollutant_col]], 2), " g/km/h<br>",
      "<b>Time Period: </b>", input$sheet_select
    )

    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = marker_sizes,
        color = ~pal(emission_values),
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = popup_content
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~emission_values,
        title = paste(gsub(" Emission Intensity \\(g/km/h\\)", "", pollutant_col),
                      "<br>Emission (g/km/h)")
      ) %>%
      setView(lng = mean(data$Longitude, na.rm = TRUE),
              lat = mean(data$Latitude, na.rm = TRUE),
              zoom = 10)
  })

  # Data table
  output$data_table <- renderDT({
    data <- current_data()
    req(data)

    # Select relevant columns for display
    display_data <- data %>%
      dplyr::select(ID, Longitude, Latitude,
                    `HDV Flow (veh/h)`, `LDV Flow (veh/h)`, `NEV Flow (veh/h)`, `Total Vehicle Flow(veh/h)`,
                    `CO Emission Intensity (g/km/h)`)
    datatable(display_data,
              colnames = c(names(display_data)[1:8]),
              escape = FALSE,
              options = list(scrollX = TRUE, pageLength = 10),
              caption = "Traffic Flow and CO Emission Data")
  })
}

#' Shiny app
#'
#' @importFrom shiny runGadget
#' @return A Shiny app
#' @export
shufan <- function(){
  shiny::runGadget(shufan_ui, shufan_server)
}
