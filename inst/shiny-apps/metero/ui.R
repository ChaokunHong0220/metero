# User interface for metero Shiny application

# Define UI
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard header
  dashboardHeader(
    title = "metero - AMR Analysis",
    tags$li(class = "dropdown",
            tags$a(href = "https://github.com/yourusername/metero", 
                   target = "_blank", 
                   tags$img(src = "metero_logo.png", height = "30px"),
                   "v0.1.0"))
  ),
  
  # Dashboard sidebar
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Import", tabName = "data_import", icon = icon("file-import")),
      menuItem("Data Overview", tabName = "data_overview", icon = icon("table")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Reports", tabName = "reports", icon = icon("file-alt")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
    ),
    
    # Conditional filter panel that appears after data is loaded
    conditionalPanel(
      condition = "input.sidebar == 'analysis' || input.sidebar == 'visualization'",
      div(
        h4("Filter Data"),
        uiOutput("pathogen_selector"),
        uiOutput("antibiotic_selector"),
        uiOutput("country_selector"),
        uiOutput("year_selector")
      )
    )
  ),
  
  # Dashboard body
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .dashboard-title {
          font-weight: bold;
          color: #3c8dbc;
        }
      "))
    ),
    
    tabItems(
      # Home tab
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            width = 12,
            title = "Welcome to metero", status = "primary", solidHeader = TRUE,
            p("metero is an integrated toolkit for analyzing Antimicrobial Resistance (AMR) data."),
            p("Use this application to:"),
            tags$ul(
              tags$li("Import and manage AMR data"),
              tags$li("Perform meta-analysis"),
              tags$li("Create visualizations"),
              tags$li("Generate reports")
            ),
            p("To get started, click on 'Data Import' in the sidebar.")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Current Data Summary", status = "info", solidHeader = TRUE,
            uiOutput("data_summary")
          )
        )
      ),
      
      # Data Import tab
      tabItem(
        tabName = "data_import",
        fluidRow(
          box(
            width = 6,
            title = "Import Data File", status = "primary", solidHeader = TRUE,
            fileInput("file_input", "Choose a file",
                      accept = c("text/csv", "text/comma-separated-values",
                                 "text/tab-separated-values", "text/plain",
                                 ".csv", ".tsv", ".xlsx", ".xls")),
            selectInput("file_type", "File Type", 
                        choices = c("Auto-detect" = "auto", "CSV" = "csv", 
                                    "Excel" = "excel", "RData" = "rdata")),
            numericInput("sheet_number", "Sheet Number (Excel only)", 1, min = 1),
            checkboxInput("header", "File has header row", TRUE),
            numericInput("skip_rows", "Skip Rows", 0, min = 0),
            actionButton("load_file", "Load File", class = "btn-primary")
          ),
          box(
            width = 6,
            title = "Column Mapping", status = "primary", solidHeader = TRUE,
            uiOutput("column_mapping")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Preview Data", status = "info", solidHeader = TRUE,
            DT::dataTableOutput("preview_data")
          )
        )
      ),
      
      # Data Overview tab
      tabItem(
        tabName = "data_overview",
        fluidRow(
          valueBoxOutput("total_records_box", width = 3),
          valueBoxOutput("unique_pathogens_box", width = 3),
          valueBoxOutput("unique_antibiotics_box", width = 3),
          valueBoxOutput("countries_box", width = 3)
        ),
        fluidRow(
          box(
            width = 12,
            title = "Dataset Contents", status = "primary", solidHeader = TRUE,
            DT::dataTableOutput("data_table")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Data Quality", status = "warning", solidHeader = TRUE,
            uiOutput("data_quality")
          ),
          box(
            width = 6,
            title = "Data Completeness", status = "warning", solidHeader = TRUE,
            plotOutput("completeness_chart")
          )
        )
      ),
      
      # Analysis tab
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(
            width = 4,
            title = "Meta-Analysis Settings", status = "primary", solidHeader = TRUE,
            selectInput("meta_method", "Meta-Analysis Method",
                        choices = c("Random Effects" = "random", 
                                    "Fixed Effects" = "fixed")),
            selectInput("group_by", "Group Results By",
                        choices = c("Pathogen" = "pathogen", 
                                    "Antibiotic" = "antibiotic",
                                    "Country" = "country",
                                    "Year" = "year"),
                        multiple = TRUE),
            checkboxInput("heterogeneity", "Analyze Heterogeneity", TRUE),
            checkboxInput("subgroup", "Perform Subgroup Analysis", FALSE),
            conditionalPanel(
              condition = "input.subgroup == true",
              selectInput("subgroup_var", "Subgroup Variable",
                          choices = c("Country" = "country", 
                                      "Year" = "year",
                                      "Region" = "region"))
            ),
            actionButton("run_analysis", "Run Analysis", class = "btn-primary")
          ),
          box(
            width = 8,
            title = "Analysis Results", status = "info", solidHeader = TRUE,
            tabsetPanel(
              id = "analysis_tabs",
              tabPanel("Summary", verbatimTextOutput("analysis_summary")),
              tabPanel("Pooled Rates", DT::dataTableOutput("pooled_rates")),
              tabPanel("Forest Plot", plotOutput("analysis_forest_plot", height = "600px")),
              tabPanel("Heterogeneity", verbatimTextOutput("heterogeneity_results"))
            )
          )
        )
      ),
      
      # Visualization tab
      tabItem(
        tabName = "visualization",
        fluidRow(
          box(
            width = 3,
            title = "Visualization Settings", status = "primary", solidHeader = TRUE,
            selectInput("viz_type", "Visualization Type", 
                        choices = c("Geographic Map" = "geo_map",
                                    "Forest Plot" = "forest",
                                    "Resistance Heatmap" = "heatmap",
                                    "Time Trend Plot" = "trend")),
            
            # Geo Map Options
            conditionalPanel(
              condition = "input.viz_type == 'geo_map'",
              selectInput("map_region", "Map Region",
                          choices = c("World" = "world",
                                      "South Asia" = "south_asia")),
              selectInput("color_palette", "Color Palette",
                          choices = c("Viridis" = "viridis",
                                      "Red-Blue" = "red_blue",
                                      "Red-Green" = "red_green"))
            ),
            
            # Forest Plot Options
            conditionalPanel(
              condition = "input.viz_type == 'forest'",
              checkboxInput("sort_forest", "Sort by Effect Size", TRUE),
              numericInput("max_studies", "Maximum Studies", 20, min = 5, max = 100)
            ),
            
            # Heatmap Options
            conditionalPanel(
              condition = "input.viz_type == 'heatmap'",
              selectInput("cluster_rows", "Cluster By Rows",
                          choices = c("None" = "none",
                                      "Hierarchical" = "hclust")),
              selectInput("cluster_cols", "Cluster By Columns",
                          choices = c("None" = "none",
                                      "Hierarchical" = "hclust"))
            ),
            
            # Trend Plot Options
            conditionalPanel(
              condition = "input.viz_type == 'trend'",
              selectInput("trend_time_var", "Time Variable", choices = "year"),
              selectInput("trend_group", "Group By",
                          choices = c("None" = "none",
                                      "Pathogen" = "pathogen",
                                      "Antibiotic" = "antibiotic",
                                      "Country" = "country")),
              selectInput("smooth_method", "Smoothing Method",
                          choices = c("None" = "none",
                                      "LOESS" = "loess",
                                      "Linear" = "lm"))
            ),
            
            actionButton("generate_viz", "Generate", class = "btn-primary"),
            br(), br(),
            downloadButton("download_viz", "Download Plot")
          ),
          box(
            width = 9,
            title = "Visualization", status = "info", solidHeader = TRUE,
            plotOutput("visualization_plot", height = "600px")
          )
        )
      ),
      
      # Reports tab
      tabItem(
        tabName = "reports",
        fluidRow(
          box(
            width = 4,
            title = "Report Settings", status = "primary", solidHeader = TRUE,
            selectInput("report_type", "Report Type",
                        choices = c("Summary Report" = "summary",
                                    "Regional Analysis" = "regional",
                                    "Pathogen Profile" = "pathogen",
                                    "Antibiotic Profile" = "antibiotic")),
            
            # Additional settings based on report type
            conditionalPanel(
              condition = "input.report_type == 'regional'",
              selectInput("report_region", "Region", choices = c("Global", "South Asia"))
            ),
            
            conditionalPanel(
              condition = "input.report_type == 'pathogen'",
              uiOutput("report_pathogen_selector")
            ),
            
            conditionalPanel(
              condition = "input.report_type == 'antibiotic'",
              uiOutput("report_antibiotic_selector")
            ),
            
            selectInput("report_format", "Output Format",
                        choices = c("HTML" = "html", 
                                    "PDF" = "pdf", 
                                    "Word" = "docx")),
            actionButton("generate_report", "Generate Report", class = "btn-primary")
          ),
          box(
            width = 8,
            title = "Report Preview", status = "info", solidHeader = TRUE,
            htmlOutput("report_preview")
          )
        )
      ),
      
      # Help tab
      tabItem(
        tabName = "help",
        fluidRow(
          box(
            width = 12,
            title = "metero Help", status = "primary", solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Overview", 
                       includeMarkdown(system.file("shiny-apps/metero/www/help_overview.md", package = "metero"))),
              tabPanel("Data Import", 
                       includeMarkdown(system.file("shiny-apps/metero/www/help_data_import.md", package = "metero"))),
              tabPanel("Analysis", 
                       includeMarkdown(system.file("shiny-apps/metero/www/help_analysis.md", package = "metero"))),
              tabPanel("Visualization", 
                       includeMarkdown(system.file("shiny-apps/metero/www/help_visualization.md", package = "metero"))),
              tabPanel("FAQ", 
                       includeMarkdown(system.file("shiny-apps/metero/www/help_faq.md", package = "metero")))
            )
          )
        )
      )
    )
  )
) 