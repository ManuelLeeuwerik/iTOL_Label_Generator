  # ---- Binary tab: Column settings UI ----
  output$binary_column_settings_ui <- renderUI({
    req(input$data_cols)
    
    df <- isolate(data())
    
    # Create accordion for each column
    accordion_items <- lapply(seq_along(input$data_cols), function(idx) {
      col <- input$data_cols[idx]
      col_values <- unique(sapply(as.character(df[[col]]), standardize_value))
      col_values <- col_values[!is.na(col_values)]  # Filter out NA values

      # Get current settings
      current_binary_shape <- isolate(input[[paste0("binary_shape_", col)]])
      current_binary_color <- isolate(input[[paste0("binary_color_", col)]])
      current_binary_filled <- isolate(input[[paste0("binary_filled_", col)]])
      
      if(is.null(current_binary_shape)) current_binary_shape <- 2
      if(is.null(current_binary_color)) current_binary_color <- "#3498DB"
      if(is.null(current_binary_filled)) current_binary_filled <- TRUE
      
      # Create accordion item
      accordion_panel(
        title = col,
        value = paste0("binary_panel_", idx),
        
        # Shape selection
        selectInput(
          paste0("binary_shape_", col),
          "Shape",
          choices = symbol_names,
          selected = current_binary_shape,
          width = "200px"
        ),
        
        # Color selection
        colourInput(
          paste0("binary_color_", col),
          "Color",
          value = current_binary_color,
          showColour = "both",
          palette = "square",
          returnName = FALSE
        ),
        
        # Filled/empty option
        div(
          style = "margin-bottom: 1rem;",
          checkboxInput(
            paste0("binary_filled_", col),
            "Show only filled shapes (hide empty shapes)",
            value = current_binary_filled
          )
        ),
        
        tags$hr(),
        
        # Value selection mode
        radioButtons(
          paste0("binary_mode_", col),
          "Value Selection Mode",
          choices = c(
            "Include specific values (show presence)" = "include",
            "Exclude specific values (show absence)" = "exclude",
            "All values as separate fields" = "all"
          ),
          selected = isolate(input[[paste0("binary_mode_", col)]]) %||% "all"
        ),
        
        # Value selection (conditional)
        conditionalPanel(
          condition = sprintf("input['binary_mode_%s'] != 'all'", col),
          checkboxGroupInput(
            paste0("binary_values_", col),
            "Select Values",
            choices = col_values,
            selected = isolate(input[[paste0("binary_values_", col)]]) %||% col_values[1],
            width = "200px"
          )
        ),
        
        tags$hr(),
        
        # ---- COLLAPSIBLE ADVANCED SETTINGS SECTION ----
        tags$details(
          # Summary (clickable header) - CLOSED by default
          tags$summary(
            style = "cursor: pointer; font-weight: 600; color: #2C5F8D; margin: 0.5rem 0; display: flex; align-items: center; gap: 0.5rem;",
            icon("cog"),
            "Advanced iTOL Settings"
          ),
          
          # Content (hidden by default)
          div(
            style = "padding: 1rem; background-color: #f8f9fa; border-radius: 0.25rem; margin-top: 0.5rem; border: 1px solid #dee2e6;",
            
            # Legend settings
            tags$h6(style = "color: #2C5F8D;", "Legend Settings"),
            
            div(
              style = "margin-bottom: 1rem;",
              textInput(
                paste0("binary_legend_title_", col),
                "Legend Title",
                value = isolate(input[[paste0("binary_legend_title_", col)]]) %||% col,
                width = "250px"
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("binary_legend_visible_", col),
                "Show legend initially",
                value = isolate(input[[paste0("binary_legend_visible_", col)]]) %||% TRUE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("binary_legend_horizontal_", col),
                "Horizontal legend layout",
                value = isolate(input[[paste0("binary_legend_horizontal_", col)]]) %||% FALSE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("binary_legend_scale_", col),
                "Legend Scale Factor",
                value = isolate(input[[paste0("binary_legend_scale_", col)]]) %||% 1,
                min = 0.1,
                max = 5,
                step = 0.1,
                width = "150px"
              )
            ),
            
            tags$hr(),

            tags$h6(style = "color: #2C5F8D;", "Symbol Settings"),

            # Symbol height factor
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("binary_height_factor_", col),
                "Symbol Height Factor",
                value = isolate(input[[paste0("binary_height_factor_", col)]]) %||% 1,
                min = 0.1,
                max = 5,
                step = 0.1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Multiplication factor for symbol height (values <1 decrease, >1 increase)")
            ),
            
            # Symbol spacing
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("binary_symbol_spacing_", col),
                "Symbol Spacing",
                value = isolate(input[[paste0("binary_symbol_spacing_", col)]]) %||% 10,
                min = 0,
                max = 100,
                step = 1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Spacing between individual binary levels when there's more than one")
            ),
            
            # Margin
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("binary_margin_", col),
                "Left Margin",
                value = isolate(input[[paste0("binary_margin_", col)]]) %||% 0,
                min = -100,
                max = 100,
                step = 1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Increase/decrease spacing to next dataset (can be negative for overlapping)")
            ),
            
            tags$hr(),
            
            # Grid settings
            tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Grid Settings"),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("binary_horizontal_grid_", col),
                "Show horizontal grid",
                value = isolate(input[[paste0("binary_horizontal_grid_", col)]]) %||% FALSE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("binary_vertical_grid_", col),
                "Show vertical grid",
                value = isolate(input[[paste0("binary_vertical_grid_", col)]]) %||% FALSE
              )
            ),
            
            conditionalPanel(
              condition = sprintf("input['binary_horizontal_grid_%s'] || input['binary_vertical_grid_%s']", col, col),
              
              div(
                style = "margin-bottom: 1rem;",
                colourInput(
                  paste0("binary_grid_color_", col),
                  "Grid Line Color",
                  value = isolate(input[[paste0("binary_grid_color_", col)]]) %||% "#0000ff",
                  showColour = "both",
                  palette = "square",
                  returnName = FALSE
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("binary_grid_width_", col),
                  "Grid Line Width",
                  value = isolate(input[[paste0("binary_grid_width_", col)]]) %||% 0.6,
                  min = 0.1,
                  max = 10,
                  step = 0.1,
                  width = "150px"
                )
              )
            ),
            
            tags$hr(),
            
            # Label settings
            tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Label Settings"),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("binary_show_labels_", col),
                "Show field labels",
                value = isolate(input[[paste0("binary_show_labels_", col)]]) %||% TRUE
              )
            ),
            
            conditionalPanel(
              condition = sprintf("input['binary_show_labels_%s']", col),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("binary_label_size_factor_", col),
                  "Label Size Factor",
                  value = isolate(input[[paste0("binary_label_size_factor_", col)]]) %||% 1,
                  min = 0.1,
                  max = 5,
                  step = 0.1,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("binary_label_rotation_", col),
                  "Label Rotation (degrees)",
                  value = isolate(input[[paste0("binary_label_rotation_", col)]]) %||% 0,
                  min = -180,
                  max = 180,
                  step = 1,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("binary_label_shift_", col),
                  "Label Horizontal Shift",
                  value = isolate(input[[paste0("binary_label_shift_", col)]]) %||% 0,
                  min = -200,
                  max = 200,
                  step = 1,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                checkboxInput(
                  paste0("binary_label_align_to_tree_", col),
                  "Align labels to tree circle (circular mode only)",
                  value = isolate(input[[paste0("binary_label_align_to_tree_", col)]]) %||% FALSE
                )
              )
            ),
            
            tags$hr(),
            
            # Additional options
            tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Additional Options"),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("binary_dashed_lines_", col),
                "Show dashed lines to leaf labels",
                value = isolate(input[[paste0("binary_dashed_lines_", col)]]) %||% FALSE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("binary_align_to_labels_", col),
                "Align symbols to end of leaf labels",
                value = isolate(input[[paste0("binary_align_to_labels_", col)]]) %||% FALSE
              ),
              div(class = "help-text",
                  "Individual dataset fields will not be aligned to each other")
            )
          )
        )
      )
    })
    
    # Return accordion
    accordion(
      id = "binary_accordion",
      multiple = TRUE,
      !!!accordion_items
    )
  })

  # ---- Generate binary outputs ----
  binary_outputs <- reactive({
    # Validate all requirements
    req(data(), input$id_col, input$data_cols)
    req(length(input$data_cols) > 0)
    
    # Only isolate the data frame, not the inputs
    df <- data()
    output_list <- list()
    
    for(col in input$data_cols) {
      col_values <- unique(sapply(as.character(df[[col]]), standardize_value))
      # Remove NA values from col_values
      col_values <- col_values[!is.na(col_values)]
      
      if(length(col_values) == 0) next  # Skip columns with no valid values
      
      # Get settings WITHOUT isolation - we want these to be reactive
      binary_mode <- input[[paste0("binary_mode_", col)]]
      binary_shape <- input[[paste0("binary_shape_", col)]]
      binary_color <- input[[paste0("binary_color_", col)]]
      binary_filled <- input[[paste0("binary_filled_", col)]]
      selected_values <- input[[paste0("binary_values_", col)]]
      
      # Get advanced settings
      binary_height_factor <- input[[paste0("binary_height_factor_", col)]] %||% 1
      binary_symbol_spacing <- input[[paste0("binary_symbol_spacing_", col)]] %||% 10
      binary_margin <- input[[paste0("binary_margin_", col)]] %||% 0
      binary_horizontal_grid <- input[[paste0("binary_horizontal_grid_", col)]] %||% FALSE
      binary_vertical_grid <- input[[paste0("binary_vertical_grid_", col)]] %||% FALSE
      binary_grid_color <- input[[paste0("binary_grid_color_", col)]] %||% "#0000ff"
      binary_grid_width <- input[[paste0("binary_grid_width_", col)]] %||% 0.6
      binary_legend_title <- input[[paste0("binary_legend_title_", col)]] %||% col
      binary_legend_visible <- input[[paste0("binary_legend_visible_", col)]] %||% TRUE
      binary_legend_horizontal <- input[[paste0("binary_legend_horizontal_", col)]] %||% FALSE
      binary_legend_scale <- input[[paste0("binary_legend_scale_", col)]] %||% 1
      binary_show_labels <- input[[paste0("binary_show_labels_", col)]] %||% TRUE
      binary_label_size_factor <- input[[paste0("binary_label_size_factor_", col)]] %||% 1
      binary_label_rotation <- input[[paste0("binary_label_rotation_", col)]] %||% 0
      binary_label_shift <- input[[paste0("binary_label_shift_", col)]] %||% 0
      binary_label_align_to_tree <- input[[paste0("binary_label_align_to_tree_", col)]] %||% FALSE
      binary_dashed_lines <- input[[paste0("binary_dashed_lines_", col)]] %||% FALSE
      binary_align_to_labels <- input[[paste0("binary_align_to_labels_", col)]] %||% FALSE

      if(is.null(binary_mode)) binary_mode <- "all"
      if(is.null(binary_shape)) binary_shape <- 2
      if(is.null(binary_color)) binary_color <- "#3498DB"
      if(is.null(binary_filled)) binary_filled <- TRUE
      if(is.null(selected_values) && binary_mode != "all") selected_values <- col_values[1]
      
      # Determine which values to include
      if(binary_mode == "all") {
        fields <- col_values
      } else if(binary_mode == "include") {
        fields <- selected_values
      } else {  # exclude
        fields <- setdiff(col_values, selected_values)
      }
      
      # Build iTOL DATASET_BINARY format
      content <- c("DATASET_BINARY")
      content <- c(content, "SEPARATOR TAB")
      content <- c(content, paste("DATASET_LABEL", paste(input$dataset_label, "-", col, "binary"), sep = "\t"))
      content <- c(content, paste("COLOR", binary_color, sep = "\t"))
      content <- c(content, "")
      
      # Field configuration
      field_shapes <- rep(binary_shape, length(fields))
      content <- c(content, paste("FIELD_SHAPES", paste(field_shapes, collapse = "\t"), sep = "\t"))
      content <- c(content, paste("FIELD_LABELS", paste(fields, collapse = "\t"), sep = "\t"))
      
      # Field colors (one per field)
      field_colors <- rep(binary_color, length(fields))
      content <- c(content, paste("FIELD_COLORS", paste(field_colors, collapse = "\t"), sep = "\t"))
      
      content <- c(content, "")
      
      # Legend (use advanced settings)
      content <- c(content, paste("LEGEND_TITLE", binary_legend_title, sep = "\t"))
      content <- c(content, paste("LEGEND_SCALE", binary_legend_scale, sep = "\t"))
      content <- c(content, paste("LEGEND_VISIBLE", if(binary_legend_visible) "1" else "0", sep = "\t"))
      if(binary_legend_horizontal) {
        content <- c(content, paste("LEGEND_HORIZONTAL", "1", sep = "\t"))
      }
      content <- c(content, paste("LEGEND_SHAPES", paste(field_shapes, collapse = "\t"), sep = "\t"))
      content <- c(content, paste("LEGEND_COLORS", paste(field_colors, collapse = "\t"), sep = "\t"))
      content <- c(content, paste("LEGEND_LABELS", paste(fields, collapse = "\t"), sep = "\t"))
      content <- c(content, "")

      # Advanced display settings
      content <- c(content, paste("HEIGHT_FACTOR", binary_height_factor, sep = "\t"))
      content <- c(content, paste("SYMBOL_SPACING", binary_symbol_spacing, sep = "\t"))
      content <- c(content, paste("MARGIN", binary_margin, sep = "\t"))
      content <- c(content, "")

      # Grid settings
      content <- c(content, paste("HORIZONTAL_GRID", if(binary_horizontal_grid) "1" else "0", sep = "\t"))
      content <- c(content, paste("VERTICAL_GRID", if(binary_vertical_grid) "1" else "0", sep = "\t"))
      if(binary_horizontal_grid || binary_vertical_grid) {
        content <- c(content, paste("GRID_COLOR", binary_grid_color, sep = "\t"))
        content <- c(content, paste("GRID_WIDTH", binary_grid_width, sep = "\t"))
      }
      content <- c(content, "")

      # Label settings
      content <- c(content, paste("SHOW_LABELS", if(binary_show_labels) "1" else "0", sep = "\t"))
      content <- c(content, paste("SIZE_FACTOR", binary_label_size_factor, sep = "\t"))
      content <- c(content, paste("LABEL_ROTATION", binary_label_rotation, sep = "\t"))
      content <- c(content, paste("LABEL_SHIFT", binary_label_shift, sep = "\t"))
      content <- c(content, paste("LABEL_ALIGN_TO_TREE", if(binary_label_align_to_tree) "1" else "0", sep = "\t"))
      content <- c(content, "")

      # Additional options (only add if enabled)
      if(binary_dashed_lines) {
        content <- c(content, paste("DASHED_LINES", "1", sep = "\t"))
      }
      if(binary_align_to_labels) {
        content <- c(content, paste("ALIGN_TO_LABELS", "1", sep = "\t"))
      }
      content <- c(content, "")

      content <- c(content, "DATA")
      
      # Data: ID followed by binary values (1, 0, or -1)
      for(i in 1:nrow(df)) {
        id <- as.character(df[[input$id_col]][i])
        val <- standardize_value(df[[col]][i])
        
        # Create binary vector with proper NA handling
        binary_vec <- sapply(fields, function(field) {
          # Check if val is NA first
          if(is.na(val)) {
            if(binary_filled) {
              return(-1)  # Omit shape for NA values
            } else {
              return(0)  # Empty shape for NA values
            }
          } else if(val == field) {
            return(1)  # Filled shape
          } else if(binary_filled) {
            return(-1)  # Omit shape
          } else {
            return(0)  # Empty shape
          }
        })
        
        content <- c(content, paste(c(id, binary_vec), collapse = "\t"))
      }
      
      output_list[[col]] <- paste(content, collapse = "\n")
    }
    
    return(output_list)
  })
  
    # Single binary file
  output$download_binary_single <- downloadHandler(
    filename = function() {
      paste0(input$dataset_label, "_binary.txt")
    },
    content = function(file) {
      content_list <- binary_outputs()
      writeLines(content_list[[1]], file)
    }
  )
  
  # All binary files as ZIP
  output$download_binary_zip <- downloadHandler(
    filename = function() {
      paste0(input$dataset_label, "_binary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      content_list <- binary_outputs()
      temp_dir <- tempdir()
      temp_files <- c()
      
      for(name in names(content_list)) {
        temp_file <- file.path(temp_dir, paste0(name, "_binary.txt"))
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
  
  # Individual binary files
  observe({
    # Add requirement and validation
    req(input$data_cols)
    
    # Use try-catch to prevent errors
    tryCatch({
      content_list <- binary_outputs()
      req(content_list)
      
      if(length(content_list) > 1) {
        lapply(names(content_list), function(name) {
          local({
            my_name <- name
            output[[paste0("download_binary_", safe_id(my_name))]] <- downloadHandler(
              filename = function() {
                paste0(my_name, "_binary.txt")
              },
              content = function(file) {
                writeLines(content_list[[my_name]], file)
              }
            )
          })
        })
      }
    }, error = function(e) {
      # Silently handle errors during initial load
      NULL
    })
  })


  # ---- Binary download card ----
  output$binary_download_card <- renderUI({
    req(binary_outputs())
    content_list <- binary_outputs()
    
    card(
      card_header("Download Binary Annotations"),
      card_body(
        if(length(content_list) == 1) {
          centered_download_button(
            "download_binary_single", 
            "Download Binary File"
          )
        } else {
          tagList(
            centered_download_button(
              "download_binary_zip",
              "Download All Binary Files (ZIP)",
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
                  paste0("download_binary_", safe_id(name)),
                  paste0(name, "_binary.txt"),
                  class = "btn-primary btn-sm"
                )
              )
            })
          )
        }
      )
    )
  })