#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# This example is inspired by Grolemund, G. (2015). Learn Shiny - Video Tutorials.
# URL: https://shiny.rstudio.com/tutorial/

library(shiny)
library(shinyalert)
library(ggplot2)

# Define UI
ui <- fluidPage(
  # Title
  titlePanel("Oral Microbiome Analysis of Substance Users: Exploring Microbial Profiles"),

  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      tags$p("This Shiny App is part of the OralMicrobiomeSubstanceUse package.
             It allows users to upload microbiome datasets, visualize microbial profiles,
             and compare microbial communities across substance user groups."),
      br(),

      tags$b("Description:"),
      tags$p("The OralMicrobiomeSubstanceUse package provides tools for analyzing microbiome data.
             This Shiny app enables users to upload microbiome datasets, visualize taxonomic profiles,
             and compare microbial communities across different groups."),
      br(),
      br(),

      tags$p("Instructions: Use the inputs below to upload data and choose analysis options. Default values are provided.
             Press 'Run Analysis' to generate results and navigate through the tabs to explore plots and summaries."),
      br(),

      shinyalert::useShinyalert(),  # Set up shinyalert

      # Input fields
      fileInput(inputId = "datafile",
                label = "Upload Microbiome Data (CSV).",
                accept = c(".csv")),
      selectInput(inputId = "analysis_type",
                  label = "Choose Analysis Type:",
                  choices = c("Taxonomic Visualization", "Substance Use Comparison")),
      actionButton(inputId = "run_analysis",
                   label = "Run Analysis"),
      br(),

      # Example datasets
      tags$p("Example Datasets:"),
      uiOutput("example_data1"),
      actionButton(inputId = "data1_info",
                   label = "Dataset 1 Details"),
      uiOutput("example_data2"),
      actionButton(inputId = "data2_info",
                   label = "Dataset 2 Details"),
      br(),

      # Download button
      downloadButton(outputId = "download_results",
                     label = "Download Results")
    ),

    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Microbial Profiles",
                           h3("Taxonomic Visualization"),
                           plotOutput("taxonomic_plot")),
                  tabPanel("Summary Statistics",
                           h3("Microbial Community Comparison"),
                           tableOutput("summary_table")),
                  tabPanel("Group Comparisons",
                           h3("Boxplot of Microbial Communities"),
                           plotOutput("group_comparison_plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive: Load uploaded or example data
  dataset <- reactive({
    if (is.null(input$datafile)) {
      # Load example data if no file is uploaded
      read.csv(system.file("extdata", "sample_data.csv", package = "OralMicrobiomeSubstanceUse"))
    } else {
      read.csv(input$datafile$datapath)
    }
  })

  # Reactive: Perform analysis
  analysis_results <- eventReactive(input$run_analysis, {
    req(dataset())
    data <- dataset()

    if (input$analysis_type == "Taxonomic Visualization") {
      list(
        plot = ggplot(data, aes(x = Taxon, y = Abundance, fill = Group)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          labs(title = "Taxonomic Visualization", x = "Taxon", y = "Abundance")
      )
    } else {
      list(
        plot = ggplot(data, aes(x = Group, y = Abundance, fill = Substance)) +
          geom_boxplot() +
          theme_minimal() +
          labs(title = "Substance Use Comparison", x = "Group", y = "Abundance")
      )
    }
  })

  # Output: Plot
  output$taxonomic_plot <- renderPlot({
    req(analysis_results())
    analysis_results()$plot
  })

  # Output: Table summary
  output$summary_table <- renderTable({
    req(dataset())
    summary(dataset())
  })

  # Output: Boxplot comparison
  output$group_comparison_plot <- renderPlot({
    req(dataset())
    ggplot(dataset(), aes(x = Group, y = Abundance, fill = Substance)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Microbial Community Boxplot", x = "Group", y = "Abundance")
  })

  # Download results
  output$download_results <- downloadHandler(
    filename = function() { paste("analysis_results", ".csv", sep = "") },
    content = function(file) {
      write.csv(dataset(), file)
    }
  )

  # Example data links and info
  output$example_data1 <- renderUI({
    a("Example Dataset 1", href = "https://github.com/Maryam-hkiabi/OralMicrobiomeSubstanceUse/raw/main/inst/extdata/sample_data.csv")
  })

  observeEvent(input$data1_info, {
    shinyalert(
      title = "Dataset 1 Details",
      text = "This is a sample dataset provided with the OralMicrobiomeSubstanceUse package.
              It contains rows representing microbial taxa and columns representing sample groups (e.g., substance users vs. non-users).",
      type = "info"
    )
  })

  output$example_data2 <- renderUI({
    a("Example Dataset 2", href = "https://github.com/Maryam-hkiabi/OralMicrobiomeSubstanceUse/raw/main/inst/extdata/sample_data2.csv")
  })

  observeEvent(input$data2_info, {
    shinyalert(
      title = "Dataset 2 Details",
      text = "This dataset is a larger example illustrating microbial abundance differences across various substance use types.
              Provided for testing purposes.",
      type = "info"
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
