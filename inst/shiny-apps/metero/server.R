# Server logic for metero Shiny application

server <- function(input, output, session) {
  
  # Reactive values to store data and results
  values <- reactiveValues(
    raw_data = NULL,       # Raw imported data
    mapped_data = NULL,    # Data after mapping columns
    filtered_data = NULL,  # Data after filtering
    analysis_results = NULL, # Results of analysis
    column_map = NULL,     # Map between user columns and standard names
    preview_data = NULL    # Preview data for import
  )
  
  # Function to initialize data from environment if available
  observe({
    metero_data <- get_app_data()
    if (!is.null(metero_data)) {
      values$raw_data <- metero_data
      values$mapped_data <- metero_data
      updateTabItems(session, "sidebar", "data_overview")
    }
  })
  
  # DATA IMPORT TAB ----
  
  # Preview raw data when file is selected
  observeEvent(input$file_input, {
    req(input$file_input)
    
    file <- input$file_input
    
    # Determine file type
    file_type <- if (input$file_type == "auto") {
      ext <- tolower(tools::file_ext(file$name))
      if (ext %in% c("csv", "txt")) {
        "csv"
      } else if (ext %in% c("xls", "xlsx")) {
        "excel"
      } else if (ext == "rdata" || ext == "rda") {
        "rdata"
      } else {
        "csv"  # Default to CSV
      }
    } else {
      input$file_type
    }
    
    # Read the file based on type (only first 100 rows for preview)
    preview_data <- tryCatch({
      if (file_type == "csv") {
        utils::read.csv(file$datapath, header = input$header, 
                        skip = input$skip_rows, nrows = 100)
      } else if (file_type == "excel") {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          stop("Package 'readxl' is required to import Excel files.")
        }
        readxl::read_excel(file$datapath, sheet = input$sheet_number, 
                          skip = input$skip_rows, n_max = 100)
      } else if (file_type == "rdata") {
        env <- new.env()
        load(file$datapath, envir = env)
        
        # Return first data frame
        first_df <- NULL
        for (obj_name in ls(env)) {
          obj <- get(obj_name, envir = env)
          if (is.data.frame(obj)) {
            first_df <- obj
            break
          }
        }
        if (is.null(first_df)) {
          stop("No data frames found in RData file")
        }
        first_df
      }
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
      return(NULL)
    })
    
    if (!is.null(preview_data)) {
      values$preview_data <- preview_data
    }
  })
  
  # Display preview data
  output$preview_data <- DT::renderDataTable({
    req(values$preview_data)
    DT::datatable(values$preview_data, options = list(scrollX = TRUE, pageLength = 5))
  })
  
  # Generate column mapping UI
  output$column_mapping <- renderUI({
    req(values$preview_data)
    
    # Standard column names to map to
    std_columns <- c(
      "study_id" = "Study ID",
      "pathogen" = "Pathogen",
      "pathogen_name" = "Pathogen Name",
      "antibiotic" = "Antibiotic",
      "antibiotic_name" = "Antibiotic Name",
      "antibiotic_class" = "Antibiotic Class",
      "resistance_rate" = "Resistance Rate",
      "sample_count" = "Sample Count",
      "resistant_count" = "Resistant Count",
      "country" = "Country",
      "region" = "Region",
      "year" = "Year",
      "author" = "Author",
      "population_type" = "Population Type",
      "setting" = "Setting"
    )
    
    # Get columns from preview data
    data_columns <- names(values$preview_data)
    data_columns_named <- c("(None)" = "", setNames(data_columns, data_columns))
    
    # Create select inputs for each standard column
    mapping_inputs <- lapply(names(std_columns), function(std_col) {
      # Try to guess the mapping based on name similarity
      suggested <- data_columns[grep(paste0("^", std_col), tolower(data_columns))]
      suggested <- if (length(suggested) > 0) suggested[1] else ""
      
      selectInput(
        inputId = paste0("map_", std_col),
        label = std_columns[std_col],
        choices = data_columns_named,
        selected = suggested
      )
    })
    
    # Combine all inputs
    div(
      p("Map your data columns to standard metero columns:"),
      do.call(tagList, mapping_inputs),
      p("Required fields: Pathogen, Antibiotic, and Resistance Rate or Sample Count + Resistant Count")
    )
  })
  
  # Load full data when button is clicked
  observeEvent(input$load_file, {
    req(input$file_input)
    
    # Get selected file type
    file_type <- if (input$file_type == "auto") {
      ext <- tolower(tools::file_ext(input$file_input$name))
      if (ext %in% c("csv", "txt")) {
        "csv"
      } else if (ext %in% c("xls", "xlsx")) {
        "excel"
      } else if (ext == "rdata" || ext == "rda") {
        "rdata"
      } else {
        "csv"  # Default to CSV
      }
    } else {
      input$file_type
    }
    
    # Read the full file
    full_data <- tryCatch({
      if (file_type == "csv") {
        utils::read.csv(input$file_input$datapath, header = input$header, 
                        skip = input$skip_rows)
      } else if (file_type == "excel") {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          stop("Package 'readxl' is required to import Excel files.")
        }
        readxl::read_excel(input$file_input$datapath, sheet = input$sheet_number, 
                          skip = input$skip_rows)
      } else if (file_type == "rdata") {
        env <- new.env()
        load(input$file_input$datapath, envir = env)
        
        # Return first data frame
        first_df <- NULL
        for (obj_name in ls(env)) {
          obj <- get(obj_name, envir = env)
          if (is.data.frame(obj)) {
            first_df <- obj
            break
          }
        }
        if (is.null(first_df)) {
          stop("No data frames found in RData file")
        }
        first_df
      }
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
      return(NULL)
    })
    
    if (!is.null(full_data)) {
      values$raw_data <- full_data
      
      # Build column mapping from UI inputs
      column_map <- list()
      for (std_col in c("study_id", "pathogen", "pathogen_name", "antibiotic", "antibiotic_name",
                       "antibiotic_class", "resistance_rate", "sample_count", "resistant_count",
                       "country", "region", "year", "author", "population_type", "setting")) {
        user_col <- input[[paste0("map_", std_col)]]
        if (!is.null(user_col) && user_col != "") {
          column_map[[std_col]] <- user_col
        }
      }
      
      values$column_map <- column_map
      
      # Try to import and standardize the data
      tryCatch({
        # Use import_amr_data from the metero package
        mapped_data <- import_amr_data(
          data = full_data,
          mapping = column_map,
          domain = "human"  # Default to human for now
        )
        
        values$mapped_data <- mapped_data
        
        # Navigate to data overview
        updateTabItems(session, "sidebar", "data_overview")
        
        showNotification("Data imported successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error standardizing data:", e$message), type = "error")
      })
    }
  })
  
  # HOME TAB ----
  
  # Data summary on home page
  output$data_summary <- renderUI({
    if (is.null(values$mapped_data)) {
      p("No data loaded. Go to 'Data Import' tab to load data.")
    } else {
      meta <- format_metadata(values$mapped_data)
      
      div(
        p(paste("Current dataset contains", meta$n_records, "AMR records.")),
        p(HTML(paste(
          "Dataset includes <b>", meta$pathogens, "</b> pathogens, <b>", 
          meta$antibiotics, "</b> antibiotics across <b>", 
          meta$countries, "</b> countries for the years <b>", meta$years, "</b>."
        ))),
        p("Use the sidebar to navigate to analysis and visualization tools.")
      )
    }
  })
  
  # DATA OVERVIEW TAB ----
  
  # Value boxes
  output$total_records_box <- renderValueBox({
    req(values$mapped_data)
    valueBox(
      nrow(values$mapped_data),
      "Total Records",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$unique_pathogens_box <- renderValueBox({
    req(values$mapped_data)
    
    pathogen_col <- if ("pathogen_name" %in% names(values$mapped_data)) "pathogen_name" 
                   else if ("pathogen" %in% names(values$mapped_data)) "pathogen"
                   else NULL
    
    if (is.null(pathogen_col)) {
      return(valueBox(0, "Pathogens", icon = icon("bug"), color = "red"))
    }
    
    valueBox(
      length(unique(values$mapped_data[[pathogen_col]])),
      "Unique Pathogens",
      icon = icon("bug"),
      color = "green"
    )
  })
  
  output$unique_antibiotics_box <- renderValueBox({
    req(values$mapped_data)
    
    antibiotic_col <- if ("antibiotic_name" %in% names(values$mapped_data)) "antibiotic_name" 
                     else if ("antibiotic" %in% names(values$mapped_data)) "antibiotic"
                     else NULL
    
    if (is.null(antibiotic_col)) {
      return(valueBox(0, "Antibiotics", icon = icon("prescription"), color = "red"))
    }
    
    valueBox(
      length(unique(values$mapped_data[[antibiotic_col]])),
      "Unique Antibiotics",
      icon = icon("prescription"),
      color = "yellow"
    )
  })
  
  output$countries_box <- renderValueBox({
    req(values$mapped_data)
    
    if (!("country" %in% names(values$mapped_data))) {
      return(valueBox(0, "Countries", icon = icon("globe"), color = "red"))
    }
    
    valueBox(
      length(unique(values$mapped_data$country)),
      "Countries",
      icon = icon("globe"),
      color = "purple"
    )
  })
  
  # Data table
  output$data_table <- DT::renderDataTable({
    req(values$mapped_data)
    DT::datatable(values$mapped_data, 
                options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Placeholder message for other tabs - to be implemented in future updates
  observe({
    if (input$sidebar %in% c("analysis", "visualization", "reports")) {
      showNotification("This feature will be fully implemented in a future update.", 
                      type = "warning", duration = 5)
    }
  })
} 