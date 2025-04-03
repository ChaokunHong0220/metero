# Server logic for METEOR Shiny application

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
    meteor_data <- get_app_data()
    if (!is.null(meteor_data)) {
      values$raw_data <- meteor_data
      values$mapped_data <- meteor_data
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
        
        # If there's only one object in the environment, return it
        if (length(ls(env)) == 1) {
          get(ls(env)[1], envir = env)
        } else {
          # If there are multiple objects, try to find a data frame
          data_frames <- ls(env)[sapply(ls(env), function(x) is.data.frame(get(x, envir = env)))]
          if (length(data_frames) == 1) {
            get(data_frames[1], envir = env)
          } else {
            stop("Multiple data frames found in RData file. Please extract the desired data frame manually.")
          }
        }
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
      p("Map your data columns to standard METEOR columns:"),
      do.call(tagList, mapping_inputs),
      p("Required fields: Pathogen, Antibiotic, Resistance Rate or Sample Count + Resistant Count")
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
        
        # If there's only one object in the environment, return it
        if (length(ls(env)) == 1) {
          get(ls(env)[1], envir = env)
        } else {
          # If there are multiple objects, try to find a data frame
          data_frames <- ls(env)[sapply(ls(env), function(x) is.data.frame(get(x, envir = env)))]
          if (length(data_frames) == 1) {
            get(data_frames[1], envir = env)
          } else {
            stop("Multiple data frames found in RData file. Please extract the desired data frame manually.")
          }
        }
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
      
      # Import data using metero's import_amr_data function
      tryCatch({
        mapped_data <- import_amr_data(
          data = full_data,
          mapping = column_map,
          domain = "human"  # Default to human, can be extended later
        )
        values$mapped_data <- mapped_data
        
        # Update filter controls
        updateSelectInput(session, "pathogen_selector", 
                         choices = get_pathogens(mapped_data))
        updateSelectInput(session, "antibiotic_selector", 
                         choices = get_antibiotics(mapped_data))
        updateSelectInput(session, "country_selector", 
                         choices = get_countries(mapped_data))
        updateSelectInput(session, "year_selector", 
                         choices = get_years(mapped_data))
        
        # Navigate to data overview
        updateTabItems(session, "sidebar", "data_overview")
        
        showNotification("Data imported successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error standardizing data:", e$message), type = "error")
      })
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
    
    pathogen_col <- if ("pathogen_name" %in% names(values$mapped_data)) {
      "pathogen_name"
    } else if ("pathogen" %in% names(values$mapped_data)) {
      "pathogen"
    } else {
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
    
    antibiotic_col <- if ("antibiotic_name" %in% names(values$mapped_data)) {
      "antibiotic_name"
    } else if ("antibiotic" %in% names(values$mapped_data)) {
      "antibiotic"
    } else {
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
  
  # Data quality info
  output$data_quality <- renderUI({
    req(values$mapped_data)
    
    # Perform data quality check using metero function
    quality_check <- check_data_quality(values$mapped_data, domain = "human")
    
    # Extract information
    quality_score <- quality_check$quality_score * 100
    issues <- c(quality_check$consistency, quality_check$issues)
    recommendations <- quality_check$recommendations
    
    div(
      h4(paste("Quality Score:", round(quality_score), "/ 100")),
      
      h5("Issues:"),
      if (length(issues) == 0) {
        p("No major issues detected")
      } else {
        tags$ul(
          lapply(issues, function(issue) tags$li(issue))
        )
      },
      
      h5("Recommendations:"),
      if (length(recommendations) == 0) {
        p("No specific recommendations")
      } else {
        tags$ul(
          lapply(recommendations, function(rec) tags$li(rec))
        )
      }
    )
  })
  
  # Completeness chart
  output$completeness_chart <- renderPlot({
    req(values$mapped_data)
    
    # Calculate completeness for important columns
    data <- values$mapped_data
    important_cols <- c("study_id", "pathogen", "pathogen_name", "antibiotic", "antibiotic_name",
                      "resistance_rate", "sample_count", "resistant_count", "country", "year")
    
    # Filter to only columns that exist in the data
    important_cols <- important_cols[important_cols %in% names(data)]
    
    if (length(important_cols) == 0) {
      return(NULL)
    }
    
    # Calculate completeness
    completeness <- sapply(important_cols, function(col) {
      sum(!is.na(data[[col]])) / nrow(data) * 100
    })
    
    # Create data frame for plotting
    plot_data <- data.frame(
      column = important_cols,
      completeness = completeness
    )
    
    # Plot
    ggplot2::ggplot(plot_data, ggplot2::aes(x = reorder(column, completeness), y = completeness)) +
      ggplot2::geom_bar(stat = "identity", fill = "#3c8dbc") +
      ggplot2::geom_text(ggplot2::aes(label = paste0(round(completeness), "%")), 
                         hjust = -0.2, size = 4) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "Completeness (%)") +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()) +
      ggplot2::scale_y_continuous(limits = c(0, 110))  # Extra space for labels
  })
  
  # DATA FILTER UI ELEMENTS ----
  
  # Pathogen selector
  output$pathogen_selector <- renderUI({
    req(values$mapped_data)
    pathogens <- get_pathogens(values$mapped_data)
    if (is.null(pathogens)) return(NULL)
    
    selectInput("pathogens", "Pathogens", choices = pathogens, multiple = TRUE)
  })
  
  # Antibiotic selector
  output$antibiotic_selector <- renderUI({
    req(values$mapped_data)
    antibiotics <- get_antibiotics(values$mapped_data)
    if (is.null(antibiotics)) return(NULL)
    
    selectInput("antibiotics", "Antibiotics", choices = antibiotics, multiple = TRUE)
  })
  
  # Country selector
  output$country_selector <- renderUI({
    req(values$mapped_data)
    countries <- get_countries(values$mapped_data)
    if (is.null(countries)) return(NULL)
    
    # Check for South Asian countries
    south_asia_present <- any(countries %in% south_asian_countries)
    
    selectInput("countries", "Countries", 
               choices = c(
                 if (south_asia_present) "South Asia (All)" else NULL,
                 countries
               ), 
               multiple = TRUE)
  })
  
  # Year selector
  output$year_selector <- renderUI({
    req(values$mapped_data)
    years <- get_years(values$mapped_data)
    if (is.null(years)) return(NULL)
    
    sliderInput("years", "Year Range", 
               min = min(years), max = max(years),
               value = c(min(years), max(years)),
               step = 1, round = TRUE)
  })
  
  # Apply filters to data
  observe({
    # Only run if we have data and are on analysis or visualization tabs
    req(values$mapped_data)
    if (!(input$sidebar %in% c("analysis", "visualization"))) return()
    
    # Special handling for "South Asia (All)"
    countries <- input$countries
    if (!is.null(countries) && "South Asia (All)" %in% countries) {
      # Replace South Asia with the individual countries
      south_asia_idx <- which(countries == "South Asia (All)")
      countries <- c(
        countries[-south_asia_idx],
        intersect(south_asian_countries, get_countries(values$mapped_data))
      )
    }
    
    # Convert year range to list of years
    years <- NULL
    if (!is.null(input$years)) {
      years <- input$years[1]:input$years[2]
    }
    
    # Apply filters
    filtered <- filter_data(
      values$mapped_data,
      pathogens = input$pathogens,
      antibiotics = input$antibiotics,
      countries = countries,
      years = years
    )
    
    values$filtered_data <- filtered
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
  
  # ANALYSIS TAB ----
  
  # Run analysis on click
  observeEvent(input$run_analysis, {
    req(values$filtered_data)
    
    # Make sure at least one grouping variable is selected
    if (length(input$group_by) == 0) {
      showNotification("Please select at least one variable to group by", type = "warning")
      return()
    }
    
    # Determine grouping column names based on what's in the data
    group_cols <- c()
    for (group in input$group_by) {
      if (group == "pathogen") {
        col <- if ("pathogen_name" %in% names(values$filtered_data)) "pathogen_name" else "pathogen"
        if (col %in% names(values$filtered_data)) group_cols <- c(group_cols, col)
      } else if (group == "antibiotic") {
        col <- if ("antibiotic_name" %in% names(values$filtered_data)) "antibiotic_name" else "antibiotic"
        if (col %in% names(values$filtered_data)) group_cols <- c(group_cols, col)
      } else if (group %in% names(values$filtered_data)) {
        group_cols <- c(group_cols, group)
      }
    }
    
    if (length(group_cols) == 0) {
      showNotification("None of the selected grouping variables are available in the data", type = "warning")
      return()
    }
    
    # Run the analysis
    tryCatch({
      
      # Perform meta-analysis
      results <- calculate_pooled_rate(
        data = values$filtered_data,
        by = group_cols,
        method = input$meta_method
      )
      
      # Perform heterogeneity analysis if requested
      if (input$heterogeneity) {
        het_results <- analyze_heterogeneity(results)
        results$heterogeneity <- het_results
      }
      
      # Perform subgroup analysis if requested
      if (input$subgroup && input$subgroup_var %in% names(values$filtered_data)) {
        subgroup_results <- perform_subgroup_analysis(
          data = results,
          by = input$subgroup_var
        )
        results$subgroup <- subgroup_results
      }
      
      values$analysis_results <- results
      showNotification("Analysis completed successfully", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error in analysis:", e$message), type = "error")
    })
  })
  
  # Render analysis summary
  output$analysis_summary <- renderPrint({
    req(values$analysis_results)
    print(summary(values$analysis_results))
  })
  
  # Render pooled rates table
  output$pooled_rates <- DT::renderDataTable({
    req(values$analysis_results)
    
    # Extract the results data frame
    results_df <- values$analysis_results
    
    # Format resistance rates as percentages
    results_df$pooled_rate <- paste0(round(results_df$pooled_rate * 100, 1), "%")
    results_df$ci_lower <- paste0(round(results_df$ci_lower * 100, 1), "%")
    results_df$ci_upper <- paste0(round(results_df$ci_upper * 100, 1), "%")
    
    DT::datatable(results_df, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Render forest plot
  output$analysis_forest_plot <- renderPlot({
    req(values$analysis_results)
    
    # Generate forest plot
    create_forest_plot(values$analysis_results)
  })
  
  # Render heterogeneity results
  output$heterogeneity_results <- renderPrint({
    req(values$analysis_results, values$analysis_results$heterogeneity)
    print(values$analysis_results$heterogeneity)
  })
  
  # VISUALIZATION TAB ----
  
  # Generate visualization
  observeEvent(input$generate_viz, {
    req(values$filtered_data)
    
    # Generate the requested visualization
    tryCatch({
      viz_type <- input$viz_type
      
      if (viz_type == "geo_map") {
        # Geographic map
        pathogen_col <- if ("pathogen_name" %in% names(values$filtered_data)) "pathogen_name" else "pathogen"
        antibiotic_col <- if ("antibiotic_name" %in% names(values$filtered_data)) "antibiotic_name" else "antibiotic"
        
        # Get first selected pathogen and antibiotic if there are selections
        pathogen <- if (length(input$pathogens) > 0) input$pathogens[1] else NULL
        antibiotic <- if (length(input$antibiotics) > 0) input$antibiotics[1] else NULL
        
        plot <- create_geo_map(
          data = values$filtered_data,
          pathogen = pathogen,
          antibiotic = antibiotic,
          map_region = input$map_region,
          color_palette = input$color_palette
        )
      } else if (viz_type == "forest") {
        # Forest plot - need to run analysis first
        if (is.null(values$analysis_results)) {
          showNotification("Please run an analysis first", type = "warning")
          return(NULL)
        }
        
        plot <- create_forest_plot(
          values$analysis_results,
          max_studies = input$max_studies,
          sort_by = if (input$sort_forest) "effect" else NULL
        )
      } else if (viz_type == "heatmap") {
        # Resistance heatmap
        plot <- create_resistance_heatmap(
          values$filtered_data,
          cluster_rows = input$cluster_rows,
          cluster_cols = input$cluster_cols
        )
      } else if (viz_type == "trend") {
        # Time trend plot
        time_var <- input$trend_time_var
        
        # Get grouping variable
        group_var <- if (input$trend_group != "none") {
          if (input$trend_group == "pathogen") {
            if ("pathogen_name" %in% names(values$filtered_data)) "pathogen_name" else "pathogen"
          } else if (input$trend_group == "antibiotic") {
            if ("antibiotic_name" %in% names(values$filtered_data)) "antibiotic_name" else "antibiotic"
          } else {
            input$trend_group
          }
        } else {
          NULL
        }
        
        # Check that time variable exists
        if (!(time_var %in% names(values$filtered_data))) {
          showNotification("Time variable not found in data", type = "error")
          return(NULL)
        }
        
        # Check that grouping variable exists if specified
        if (!is.null(group_var) && !(group_var %in% names(values$filtered_data))) {
          showNotification("Grouping variable not found in data", type = "error")
          return(NULL)
        }
        
        # Create smoothing method argument
        smooth <- if (input$smooth_method == "none") NULL else input$smooth_method
        
        plot <- create_trend_plot(
          data = values$filtered_data,
          time_var = time_var,
          group_vars = if (!is.null(group_var)) group_var else NULL,
          smooth_method = smooth
        )
      }
      
      # Save the plot for downloading
      values$current_plot <- plot
      
      # Return the plot
      output$visualization_plot <- renderPlot({
        print(plot)
      })
      
    }, error = function(e) {
      showNotification(paste("Error generating visualization:", e$message), type = "error")
    })
  })
  
  # Download current visualization
  output$download_viz <- downloadHandler(
    filename = function() {
      paste("meteor-plot-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".png", sep = "")
    },
    content = function(file) {
      req(values$current_plot)
      ggsave(file, values$current_plot, width = 10, height = 7, dpi = 300)
    }
  )
  
  # REPORTS TAB ----
  
  # Update report pathogen selector
  output$report_pathogen_selector <- renderUI({
    req(values$mapped_data)
    pathogens <- get_pathogens(values$mapped_data)
    if (is.null(pathogens)) return(NULL)
    
    selectInput("report_pathogen", "Pathogen", choices = pathogens)
  })
  
  # Update report antibiotic selector
  output$report_antibiotic_selector <- renderUI({
    req(values$mapped_data)
    antibiotics <- get_antibiotics(values$mapped_data)
    if (is.null(antibiotics)) return(NULL)
    
    selectInput("report_antibiotic", "Antibiotic", choices = antibiotics)
  })
  
  # Generate report
  observeEvent(input$generate_report, {
    req(values$mapped_data)
    
    # This is a placeholder for actual report generation
    # In a full implementation, this would use rmarkdown to generate the report
    
    report_html <- HTML(paste(
      "<div style='padding: 20px;'>",
      "<h3>Report Preview</h3>",
      "<p>This is a placeholder for the actual report that would be generated.</p>",
      "<p>In the full implementation, this would generate a", input$report_format, "report for:",
      "<ul>",
      "<li>Report Type:", input$report_type, "</li>",
      if (input$report_type == "regional") paste("<li>Region:", input$report_region, "</li>") else "",
      if (input$report_type == "pathogen") paste("<li>Pathogen:", input$report_pathogen, "</li>") else "",
      if (input$report_type == "antibiotic") paste("<li>Antibiotic:", input$report_antibiotic, "</li>") else "",
      "</ul>",
      "</p>",
      "</div>"
    ))
    
    output$report_preview <- renderUI(report_html)
  })
  
} 