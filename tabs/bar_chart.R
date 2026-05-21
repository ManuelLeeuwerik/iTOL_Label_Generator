  # ---- Bar chart tab: Column settings UI ----
  output$bar_column_settings_ui <- renderUI({
    req(input$data_cols)
    
    df <- isolate(data())
    
    # Filter to numeric or convertible-to-numeric columns
    numeric_cols <- c()
    na_counts <- list()
    
    for(col in input$data_cols) {
      col_data <- df[[col]]
      
      if(is.numeric(col_data)) {
        numeric_cols <- c(numeric_cols, col)
        na_counts[[col]] <- sum(is.na(col_data))
      } else {
        converted <- suppressWarnings(as.numeric(col_data))
        non_na_count <- sum(!is.na(converted))
        
        if(non_na_count > 0) {
          numeric_cols <- c(numeric_cols, col)
          na_counts[[col]] <- sum(is.na(converted))
        }
      }
    }
    
    if(length(numeric_cols) == 0) {
      return(
        div(
          class = "info-box",
          style = "background-color: #fff3cd; border-left-color: #ffc107;",
          p(icon("exclamation-triangle"), "No numeric columns selected. Please select at least one numeric column to generate bar charts.")
        )
      )
    }
    
    # Show info about NA filtering if any columns have NAs
    info_messages <- tagList()
    for(col in numeric_cols) {
      if(na_counts[[col]] > 0) {
        info_messages <- tagList(
          info_messages,
          div(
            class = "help-text",
            style = "color: #856404; margin-bottom: 0.5rem;",
            icon("info-circle"),
            sprintf(" Column '%s': %d NA/non-numeric value(s) will be filtered out", col, na_counts[[col]])
          )
        )
      }
    }
    
    # Create accordion for each numeric column
    accordion_items <- lapply(seq_along(numeric_cols), function(idx) {
      col <- numeric_cols[idx]
      
      # Get current settings
      current_bar_color <- isolate(input[[paste0("bar_color_", col)]])
      if(is.null(current_bar_color)) current_bar_color <- "#2C5F8D"
      
      # Create accordion item
      accordion_panel(
        title = col,
        value = paste0("bar_panel_", idx),
        
        # Dataset label
        div(
          style = "margin-bottom: 1rem;",
          textInput(
            paste0("bar_label_", col),
            "Dataset Label",
            value = isolate(input[[paste0("bar_label_", col)]]) %||% col,
            width = "250px"
          ),
          div(class = "help-text",
              "Label used in the legend table")
        ),
        
        # Color selection
        div(
          style = "margin-bottom: 1rem;",
          colourInput(
            paste0("bar_color_", col),
            "Bar Color",
            value = current_bar_color,
            showColour = "both",
            palette = "square",
            returnName = FALSE
          )
        ),
        
        # Scale lines configuration
        div(
          style = "margin-bottom: 1rem;",
          textInput(
            paste0("bar_scale_", col),
            "Scale Lines (comma-separated values)",
            value = isolate(input[[paste0("bar_scale_", col)]]) %||% "",
            placeholder = "e.g., 10,50,100"
          ),
          div(class = "help-text",
              "Optional: Specify values where scale lines will be drawn")
        ),
        
        tags$hr(),
        
        # ---- COLLAPSIBLE ADVANCED SETTINGS SECTION ----
        tags$details(
          tags$summary(
            style = "cursor: pointer; font-weight: 600; color: #2C5F8D; margin: 0.5rem 0; display: flex; align-items: center; gap: 0.5rem;",
            icon("cog"),
            "Advanced iTOL Settings"
          ),
          
          div(
            style = "padding: 1rem; background-color: #f8f9fa; border-radius: 0.25rem; margin-top: 0.5rem; border: 1px solid #dee2e6;",
            
            # Legend settings
            tags$h6(style = "color: #2C5F8D;", "Legend Settings"),
            
            div(
              style = "margin-bottom: 1rem;",
              textInput(
                paste0("bar_legend_title_", col),
                "Legend Title",
                value = isolate(input[[paste0("bar_legend_title_", col)]]) %||% col,
                width = "250px"
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("bar_legend_scale_", col),
                "Legend Scale Factor",
                value = isolate(input[[paste0("bar_legend_scale_", col)]]) %||% 1,
                min = 0.1,
                max = 5,
                step = 0.1,
                width = "150px"
              )
            ),
            
            tags$hr(),
            
            # Bar dimensions and spacing
            tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Bar Dimensions & Spacing"),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("bar_width_", col),
                "Maximum Bar Width",
                value = isolate(input[[paste0("bar_width_", col)]]) %||% 1000,
                min = 50,
                max = 5000,
                step = 50,
                width = "150px"
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("bar_margin_", col),
                "Left Margin",
                value = isolate(input[[paste0("bar_margin_", col)]]) %||% 0,
                min = -200,
                max = 200,
                step = 1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Spacing to next dataset (can be negative for overlapping)")
            ),
            
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("bar_log_scale_", col),
                "Use logarithmic scale",
                value = isolate(input[[paste0("bar_log_scale_", col)]]) %||% FALSE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("bar_dashed_lines_", col),
                "Show dashed lines to leaf labels",
                value = isolate(input[[paste0("bar_dashed_lines_", col)]]) %||% FALSE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("bar_height_factor_", col),
                "Bar Height Factor",
                value = isolate(input[[paste0("bar_height_factor_", col)]]) %||% 1,
                min = 0.1,
                max = 5,
                step = 0.1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Multiplication factor for bar height")
            ),
            
            tags$hr(),

            # Value display settings
            tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Value Display"),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("bar_show_value_", col),
                "Display individual values above bars",
                value = isolate(input[[paste0("bar_show_value_", col)]]) %||% TRUE
              )
            ),
            
            
            conditionalPanel(
              condition = sprintf("input['bar_show_value_%s']", col),
              
              div(
                style = "margin-bottom: 1rem;",
                selectInput(
                  paste0("bar_label_position_", col),
                  "Label Position",
                  choices = c(
                    "Outside Right" = "outside-right",
                    "Outside Left" = "outside-left",
                    "Left" = "left",
                    "Center" = "center",
                    "Right" = "right",
                    "Dataset Center" = "dataset-center"
                  ),
                  selected = isolate(input[[paste0("bar_label_position_", col)]]) %||% "left",
                  width = "200px"
                )
              ),
          
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("bar_label_shift_x_", col),
                  "Label Horizontal Shift",
                  value = isolate(input[[paste0("bar_label_shift_x_", col)]]) %||% 0,
                  min = -200,
                  max = 200,
                  step = 1,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("bar_label_shift_y_", col),
                  "Label Vertical Shift",
                  value = isolate(input[[paste0("bar_label_shift_y_", col)]]) %||% 0,
                  min = -200,
                  max = 200,
                  step = 1,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                checkboxInput(
                  paste0("bar_label_auto_color_", col),
                  "Automatic label color (white/black based on bar darkness)",
                  value = isolate(input[[paste0("bar_label_auto_color_", col)]]) %||% TRUE
                )
              ),
              
              conditionalPanel(
                condition = sprintf("!input['bar_label_auto_color_%s']", col),
                
                div(
                  style = "margin-bottom: 1rem;",
                  colourInput(
                    paste0("bar_label_color_", col),
                    "Value Label Color",
                    value = isolate(input[[paste0("bar_label_color_", col)]]) %||% "#0000ff",
                    showColour = "both",
                    palette = "square",
                    returnName = FALSE
                  )
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("bar_label_size_factor_", col),
                  "Label Size Factor",
                  value = isolate(input[[paste0("bar_label_size_factor_", col)]]) %||% 1,
                  min = 0.1,
                  max = 5,
                  step = 0.1,
                  width = "150px"
                )
              )
            ),
            
            tags$hr(),
            
            # Bar positioning
            tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Bar Positioning"),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("bar_shift_", col),
                "Bar Vertical Shift",
                value = isolate(input[[paste0("bar_shift_", col)]]) %||% 0,
                min = -100,
                max = 100,
                step = 1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Move all bars up/down by a fixed amount")
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("bar_zero_", col),
                "Bar Zero Point",
                value = isolate(input[[paste0("bar_zero_", col)]]) %||% 0,
                step = 0.1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Starting point for bars. Values smaller than this will extend left")
            ),
            
            tags$hr(),
            
            # Border settings
            tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Bar Border"),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("bar_border_width_", col),
                "Border Width",
                value = isolate(input[[paste0("bar_border_width_", col)]]) %||% 0,
                min = 0,
                max = 10,
                step = 0.5,
                width = "150px"
              ),
              div(class = "help-text",
                  "Width of border around bars (0 = no border)")
            ),
            
            conditionalPanel(
              condition = sprintf("input['bar_border_width_%s'] > 0", col),
              
              div(
                style = "margin-bottom: 0;",
                colourInput(
                  paste0("bar_border_color_", col),
                  "Border Color",
                  value = isolate(input[[paste0("bar_border_color_", col)]]) %||% "#0000ff",
                  showColour = "both",
                  palette = "square",
                  returnName = FALSE
                )
              )
            )
          )
        )
      )
    })
    
    # Return accordion with NA info
    tagList(
      if(length(info_messages) > 0) {
        div(
          class = "info-box",
          style = "background-color: #fff3cd; border-left-color: #ffc107; margin-bottom: 1rem;",
          p(tags$strong("Value Filtering:")),
          info_messages
        )
      },
      accordion(
        id = "bar_accordion",
        multiple = TRUE,
        !!!accordion_items
      )
    )
  })

  # ---- Generate bar chart outputs ----
  bar_outputs <- reactive({
    req(data(), input$id_col, input$data_cols)
    
    df <- data()
    output_list <- list()
    
    # Filter to numeric or convertible-to-numeric columns
    numeric_cols <- c()
    for(col in input$data_cols) {
      col_data <- df[[col]]
      
      if(is.numeric(col_data)) {
        numeric_cols <- c(numeric_cols, col)
      } else {
        converted <- suppressWarnings(as.numeric(col_data))
        if(sum(!is.na(converted)) > 0) {
          numeric_cols <- c(numeric_cols, col)
        }
      }
    }
    
    if(length(numeric_cols) == 0) return(NULL)
    
    for(col in numeric_cols) {
      # Get basic settings
      bar_color <- input[[paste0("bar_color_", col)]] %||% "#2C5F8D"
      bar_scale <- input[[paste0("bar_scale_", col)]] %||% ""
      
      # Get advanced settings
      bar_width <- input[[paste0("bar_width_", col)]] %||% 1000
      bar_margin <- input[[paste0("bar_margin_", col)]] %||% 0
      bar_height_factor <- input[[paste0("bar_height_factor_", col)]] %||% 1
      bar_shift <- input[[paste0("bar_shift_", col)]] %||% 0
      bar_zero <- input[[paste0("bar_zero_", col)]] %||% 0
      bar_border_width <- input[[paste0("bar_border_width_", col)]] %||% 0
      bar_border_color <- input[[paste0("bar_border_color_", col)]] %||% "#0000ff"
      bar_log_scale <- input[[paste0("bar_log_scale_", col)]] %||% FALSE
      bar_dashed_lines <- input[[paste0("bar_dashed_lines_", col)]] %||% FALSE
      bar_show_value <- input[[paste0("bar_show_value_", col)]] %||% TRUE
      bar_label_position <- input[[paste0("bar_label_position_", col)]] %||% "outside-right"
      bar_label_shift_x <- input[[paste0("bar_label_shift_x_", col)]] %||% 0
      bar_label_shift_y <- input[[paste0("bar_label_shift_y_", col)]] %||% 0
      bar_label_auto_color <- input[[paste0("bar_label_auto_color_", col)]] %||% TRUE
      bar_label_color <- input[[paste0("bar_label_color_", col)]] %||% "#0000ff"
      bar_label_size_factor <- input[[paste0("bar_label_size_factor_", col)]] %||% 1
      bar_legend_title <- input[[paste0("bar_legend_title_", col)]] %||% col
      bar_legend_scale <- input[[paste0("bar_legend_scale_", col)]] %||% 1
      
      # Build iTOL DATASET_SIMPLEBAR format
      content <- c("DATASET_SIMPLEBAR")
      content <- c(content, "SEPARATOR COMMA")
      content <- c(content, paste("DATASET_LABEL", paste(input$dataset_label, "-", col), sep = ","))
      content <- c(content, paste("COLOR", bar_color, sep = ","))
      content <- c(content, "")
      
      # Scale lines
      if(bar_scale != "" && !is.na(bar_scale)) {
        scale_values <- trimws(unlist(strsplit(bar_scale, ",")))
        scale_line <- paste(scale_values, collapse = ",")
        content <- c(content, paste("DATASET_SCALE", scale_line, sep = ","))
      }
      content <- c(content, "")
      
      # Legend settings
      content <- c(content, paste("LEGEND_TITLE", bar_legend_title, sep = ","))
      content <- c(content, paste("LEGEND_SCALE", bar_legend_scale, sep = ","))
      content <- c(content, paste("LEGEND_SHAPES", "1", sep = ","))
      content <- c(content, paste("LEGEND_COLORS", bar_color, sep = ","))
      content <- c(content, paste("LEGEND_LABELS", col, sep = ","))
      content <- c(content, "")
      
      # Advanced display settings
      content <- c(content, paste("WIDTH", bar_width, sep = ","))
      content <- c(content, paste("MARGIN", bar_margin, sep = ","))
      if(bar_log_scale) {
        content <- c(content, "LOG_SCALE,1")
      }
      if(bar_dashed_lines) {
        content <- c(content, "DASHED_LINES,1")
      }
      content <- c(content, paste("HEIGHT_FACTOR", bar_height_factor, sep = ","))
      content <- c(content, "")
      
      # Value display settings
      content <- c(content, paste("SHOW_VALUE", if(bar_show_value) "1" else "0", sep = ","))
      
      if(bar_show_value) {
        content <- c(content, paste("LABEL_POSITION", bar_label_position, sep = ","))
        content <- c(content, paste("LABEL_SHIFT_X", bar_label_shift_x, sep = ","))
        content <- c(content, paste("LABEL_SHIFT_Y", bar_label_shift_y, sep = ","))
        
        if(bar_label_auto_color) {
          content <- c(content, "LABEL_AUTO_COLOR,1")
        } else {
          content <- c(content, "LABEL_AUTO_COLOR,0")
          content <- c(content, paste("BAR_LABEL_COLOR", bar_label_color, sep = ","))
        }
        
        content <- c(content, paste("LABEL_SIZE_FACTOR", bar_label_size_factor, sep = ","))
      }
      content <- c(content, "")
      
      # Bar positioning
      content <- c(content, paste("BAR_SHIFT", bar_shift, sep = ","))
      content <- c(content, paste("BAR_ZERO", bar_zero, sep = ","))
      content <- c(content, "")
      
      # Border settings
      if(bar_border_width > 0) {
        content <- c(content, paste("BORDER_WIDTH", bar_border_width, sep = ","))
        content <- c(content, paste("BORDER_COLOR", bar_border_color, sep = ","))
      }
      content <- c(content, "")
      
      content <- c(content, "DATA")
      
      # Get column data and convert to numeric if needed
      col_data <- df[[col]]
      if(!is.numeric(col_data)) {
        col_data <- suppressWarnings(as.numeric(col_data))
      }
      
      # Data: ID followed by numeric value - only include valid numerics
      for(i in 1:nrow(df)) {
        id <- as.character(df[[input$id_col]][i])
        val <- col_data[i]
        
        if(!is.na(val)) {
          content <- c(content, paste(id, val, sep = ","))
        }
      }
          
      output_list[[col]] <- paste(content, collapse = "\n")
    }
    
    return(output_list)
  })
    # ---- Bar chart download card ----
  output$bar_download_card <- renderUI({
    req(bar_outputs())
    content_list <- bar_outputs()
    
    if(length(content_list) == 0) return(NULL)
    
    card(
      card_header("Download Bar Chart Annotations"),
      card_body(
        if(length(content_list) == 1) {
          centered_download_button(
            "download_bar_single", 
            "Download Bar Chart File"
          )
        } else {
          tagList(
            centered_download_button(
              "download_bar_zip",
              "Download All Bar Chart Files (ZIP)",
              icon_name = "file-zipper"
            ),
            tags$br(),
            tags$br(),
            div(class = "help-text-center",
                "Or download each annotation file separately:"),
            tags$br(),
            lapply(names(content_list), function(name) {
              tags$div(
                style = "margin-bottom: 0.5rem;",
                centered_download_button(
                  paste0("download_bar_", safe_id(name)),
                  paste0(name, "_bar.txt"),
                  class = "btn-primary btn-sm"
                )
              )
            })
          )
        }
      )
    )
  })


    # Single bar chart file
  output$download_bar_single <- downloadHandler(
    filename = function() {
      paste0(input$dataset_label, "_bar.txt")
    },
    content = function(file) {
      content_list <- bar_outputs()
      writeLines(content_list[[1]], file)
    }
  )
  
  # All bar chart files as ZIP
  output$download_bar_zip <- downloadHandler(
    filename = function() {
      paste0(input$dataset_label, "_bar_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      content_list <- bar_outputs()
      temp_dir <- tempdir()
      temp_files <- c()
      
      for(name in names(content_list)) {
        temp_file <- file.path(temp_dir, paste0(name, "_bar.txt"))
        writeLines(content_list[[name]], temp_file)
        temp_files <- c(temp_files, temp_file)
      }
      
      zip::zip(
        zipfile = file,
        files = basename(temp_files),
        root = temp_dir,
        mode = "cherry-pick"
      )
      
      unlink(temp_files)
    }
  )
  
  # Individual bar chart files
  observe({
    req(input$data_cols)
    
    tryCatch({
      content_list <- bar_outputs()
      req(content_list)
      
      if(length(content_list) > 1) {
        lapply(names(content_list), function(name) {
          local({
            my_name <- name
            output[[paste0("download_bar_", safe_id(my_name))]] <- downloadHandler(
              filename = function() {
                paste0(my_name, "_bar.txt")
              },
              content = function(file) {
                writeLines(content_list[[my_name]], file)
              }
            )
          })
        })
      }
    }, error = function(e) {
      NULL
    })
  })