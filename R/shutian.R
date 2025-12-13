#' @import shiny
shutian_ui <- function() {
  # LOAD REQUIRED LIBRARIES
  library(shinythemes)
  library(DT)

  # DEFINE THE USER INTERFACE (UI)
  fluidPage(
    theme = shinytheme("flatly"),

    titlePanel("Interactive Meta-Analysis App"),

    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$h4("1. Upload Your Data"),

        fileInput("file1", "Choose .csv or .xlsx File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv",
                             ".xls",
                             ".xlsx")),

        conditionalPanel(
          condition = "output.fileType == 'csv'",
          checkboxInput("header", "Data has Header?", TRUE),
          radioButtons("sep", "Separator",
                       choices = c(Comma = ",",
                                   Semicolon = ";",
                                   Tab = "\t"),
                       selected = ",")
        ),

        tags$hr(),

        tags$h4("2. Define Analysis"),

        radioButtons("dataType", "Select Data Type:",
                     choices = c("Binary (Events / N)" = "binary",
                                 "Continuous (Mean / SD / N)" = "continuous"),
                     selected = "binary"),

        tags$p(strong("Map your data columns:")),
        uiOutput("dataMappingControls"),

        tags$hr(),

        tags$h4("3. Set Model Parameters"),

        uiOutput("measureControls"),

        radioButtons("model", "Model Type",
                     choices = c("Random effects" = "random",
                                 "Fixed effect" = "fixed"),
                     selected = "random"),

        selectInput("method", "Random Effects Method (if random)",
                    choices = c("DerSimonian-Laird" = "DL",
                                "REML (recommended)" = "REML",
                                "Paule-Mandel" = "PM",
                                "Hartung-Knapp" = "HK"),
                    selected = "REML"),

        tags$hr(),

        actionButton("runAnalysis", "Run Meta-Analysis", class = "btn-primary btn-lg btn-block")
      ),

      mainPanel(
        width = 9,
        tabsetPanel(
          type = "tabs",

          tabPanel(
            "Data Preview",
            icon = icon("table"),
            h4("Uploaded Data Preview"),
            p("Please check to ensure your data loaded correctly before running the analysis."),
            DT::dataTableOutput("contents")
          ),

          tabPanel(
            "Summary Results",
            icon = icon("file-alt"),
            h4("Meta-Analysis Summary"),
            verbatimTextOutput("summary")
          ),

          tabPanel(
            "Forest Plot",
            icon = icon("tree"),
            h4("Forest Plot"),
            p("The diamond represents the pooled effect estimate."),
            plotOutput("forestPlot", height = "800px")
          ),

          tabPanel(
            "Funnel Plot",
            icon = icon("chart-funnel"),
            h4("Funnel Plot (Publication Bias)"),
            p("Asymmetry in this plot can suggest publication bias or other heterogeneity."),
            plotOutput("funnelPlot", height = "600px")
          )
        )
      )
    )
  )
}

#' @import shiny

shutian_server <- function(input, output, session) {
  library(meta)     # For all meta-analysis functions
  library(readxl)   # For reading .xlsx files
  # *Reactive: Load Data
  loaded_data <- reactive({
    req(input$file1)

    inFile <- input$file1

    # Get file extension
    ext <- tools::file_ext(inFile$name)

    # Read file based on extension
    df <- switch(ext,
                 "csv" = read.csv(inFile$datapath,
                                  header = input$header,
                                  sep = input$sep),
                 "xls" = read_excel(inFile$datapath),
                 "xlsx" = read_excel(inFile$datapath),
                 stop("Invalid file type. Please upload a .csv or .xlsx file.")
    )

    return(df)
  })

  # *Output: File Type
  output$fileType <- reactive({
    req(input$file1)
    tools::file_ext(input$file1$name)
  })
  outputOptions(output, "fileType", suspendWhenHidden = FALSE)

  # *Output: Data Preview Table
  output$contents <- DT::renderDataTable({
    df <- loaded_data()
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  # *Output: Dynamic UI for Data Mapping
  output$dataMappingControls <- renderUI({
    req(loaded_data())
    col_names <- colnames(loaded_data())

    if (input$dataType == "binary") {
      tagList(
        selectInput("study_col", "Study/Author Column:", col_names, selected = col_names[1]),
        selectInput("e1_col", "Events (Group 1 / Exposed):", col_names, selected = col_names[2]),
        selectInput("n1_col", "Total N (Group 1 / Exposed):", col_names, selected = col_names[3]),
        selectInput("e2_col", "Events (Group 2 / Control):", col_names, selected = col_names[4]),
        selectInput("n2_col", "Total N (Group 2 / Control):", col_names, selected = col_names[5])
      )
    } else if (input$dataType == "continuous") {
      tagList(
        selectInput("study_col", "Study/Author Column:", col_names, selected = col_names[1]),
        selectInput("n1_col", "N (Group 1 / Exposed):", col_names, selected = col_names[2]),
        selectInput("m1_col", "Mean (Group 1 / Exposed):", col_names, selected = col_names[3]),
        selectInput("sd1_col", "SD (Group 1 / Exposed):", col_names, selected = col_names[4]),
        selectInput("n2_col", "N (Group 2 / Control):", col_names, selected = col_names[5]),
        selectInput("m2_col", "Mean (Group 2 / Control):", col_names, selected = col_names[6]),
        selectInput("sd2_col", "SD (Group 2 / Control):", col_names, selected = col_names[7])
      )
    }
  })

  # *Output: Dynamic UI for Effect Measure
  output$measureControls <- renderUI({
    if (input$dataType == "binary") {
      selectInput("measure", "Effect Measure",
                  choices = c("Odds Ratio" = "OR",
                              "Risk Ratio" = "RR",
                              "Risk Difference" = "RD"),
                  selected = "OR")
    } else if (input$dataType == "continuous") {
      selectInput("measure", "Effect Measure",
                  choices = c("Mean Difference" = "MD",
                              "Standardized Mean Difference (Hedges' g)" = "SMD"),
                  selected = "MD")
    }
  })

  # *Reactive: Run Analysis
  meta_results <- eventReactive(input$runAnalysis, {
    withProgress(message = 'Running Meta-Analysis...', value = 0.5, {
      df <- loaded_data()

      tryCatch({
        if (input$dataType == "binary") {
          e1 <- as.numeric(df[[input$e1_col]])
          n1 <- as.numeric(df[[input$n1_col]])
          e2 <- as.numeric(df[[input$e2_col]])
          n2 <- as.numeric(df[[input$n2_col]])
          studlab <- df[[input$study_col]]

          m <- metabin(event.e = e1,
                       n.e = n1,
                       event.c = e2,
                       n.c = n2,
                       studlab = studlab,
                       sm = input$measure,
                       comb.fixed = (input$model == "fixed"),
                       comb.random = (input$model == "random"),
                       method.tau = input$method,
                       hakn = (input$method == "HK"))
        } else if (input$dataType == "continuous") {
          n1 <- as.numeric(df[[input$n1_col]])
          m1 <- as.numeric(df[[input$m1_col]])
          sd1 <- as.numeric(df[[input$sd1_col]])
          n2 <- as.numeric(df[[input$n2_col]])
          m2 <- as.numeric(df[[input$m2_col]])
          sd2 <- as.numeric(df[[input$sd2_col]])
          studlab <- df[[input$study_col]]

          m <- metacont(n.e = n1,
                        mean.e = m1,
                        sd.e = sd1,
                        n.c = n2,
                        mean.c = m2,
                        sd.c = sd2,
                        studlab = studlab,
                        sm = input$measure,
                        comb.fixed = (input$model == "fixed"),
                        comb.random = (input$model == "random"),
                        method.tau = input$method,
                        hakn = (input$method == "HK"))
        }

        incProgress(1, detail = "Done.")
        return(m)

      }, error = function(e) {
        showModal(modalDialog(
          title = "Analysis Error",
          paste("An error occurred. Please check your data and column mappings.",
                "Common errors include:",
                "\n- Mapping a non-numeric column (e.g., text) to a numeric input (e.g., 'Events' or 'Mean').",
                "\n- Having missing values (NA) in your data.",
                "\n\nError details: ", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      })
    })
  })

  # *Output: Summary Text
  output$summary <- renderPrint({
    m <- meta_results()
    req(m)
    summary(m)
  })

  # *Output: Forest Plot
  output$forestPlot <- renderPlot({
    m <- meta_results()
    req(m)
    forest(m,
           cex = 1,
           cex.studlab = 1,
           cex.summary = 1.2)
  })

  # *Output: Funnel Plot
  output$funnelPlot <- renderPlot({
    m <- meta_results()
    req(m)
    funnel(m)
  })
}

#' Shiny app
#'
#' @importFrom shiny runGadget
#' @return A Shiny app
#' @export
shutian <- function(){
  shiny::runGadget(shutian_ui, shutian_server)
}
