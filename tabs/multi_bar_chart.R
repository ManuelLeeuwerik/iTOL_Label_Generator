  # ---- Multi-bar tab: Settings UI ----
  output$multibar_settings_ui <- renderUI({
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
          p(icon("exclamation-triangle"), "No numeric columns selected. Please select at least one numeric column to generate multi-value bar charts.")
        )
      )
    }
    
    # Show info about NA filtering if any columns have NAs
    info_messages <- tagList()
    has_nas <- FALSE
    for(col in numeric_cols) {
      if(na_counts[[col]] > 0) {
        has_nas <- TRUE
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
    
    # Get current settings
    current_selected <- isolate(input$multibar_fields)
    current_layout <- isolate(input$multibar_layout)
    current_show_value <- isolate(input$multibar_show_value)
    current_label_position <- isolate(input$multibar_label_position)
    current_auto_color <- isolate(input$multibar_auto_color)
    current_label_color <- isolate(input$multibar_label_color)
    
    if(is.null(current_selected)) current_selected <- numeric_cols[1:min(3, length(numeric_cols))]
    if(is.null(current_layout)) current_layout <- "stacked"
    if(is.null(current_show_value)) current_show_value <- TRUE
    if(is.null(current_label_position)) current_label_position <- "left"
    if(is.null(current_auto_color)) current_auto_color <- TRUE
    if(is.null(current_label_color)) current_label_color <- "#000000"
    
    tagList(
      # NA filtering info box
      if(has_nas) {
        div(
          class = "info-box",
          style = "background-color: #fff3cd; border-left-color: #ffc107; margin-bottom: 1rem;",
          p(tags$strong("Value Filtering:")),
          info_messages
        )
      },
      
      card(
        card_header("Field Selection"),
        card_body(
          selectizeInput(
            "multibar_fields",
            "Select Numeric Columns to Display",
            choices = numeric_cols,
            selected = current_selected,
            multiple = TRUE,
            options = list(
              placeholder = 'Select 2 or more columns',
              plugins = list('remove_button')
            )
          ),
          div(class = "help-text",
              "Select multiple numeric columns to display as a multi-value bar chart")
        )
      ),
      
      card(
        card_header("Dataset Label"),
        card_body(
          textInput(
            "multibar_dataset_label",
            NULL,
            value = isolate(input$multibar_dataset_label) %||% "multibar",
            placeholder = "Enter dataset label"
          ),
          div(class = "help-text",
              "Label for this multi-value bar chart dataset")
        )
      ),
      
      card(
        card_header("Field Colors"),
        card_body(
          uiOutput("multibar_color_inputs")
        )
      ),
      
      card(
        card_header("Display Options"),
        card_body(
          # Bar layout mode selection
          radioButtons(
            "multibar_layout",
            "Bar Layout Mode",
            choices = c(
              "Stacked (default - values stacked vertically)" = "stacked",
              "Aligned (fields displayed side-by-side)" = "aligned",
              "Side Stacked (fields next to each other, slightly offset)" = "side_stacked"
            ),
            selected = current_layout
          ),
          
          div(class = "help-text",
              "Choose how multiple fields are displayed in the bar chart"),
          
          tags$hr(),
          
          checkboxInput(
            "multibar_na_to_zero",
            "Convert missing values (NA) to 0.0",
            value = isolate(input$multibar_na_to_zero) %||% TRUE
          ),
          div(class = "help-text",
              style = "margin-top: -0.5rem; margin-bottom: 1rem;",
              "When checked, missing values will be displayed as 0. When unchecked, samples with any missing values will be excluded."),
              
          tags$hr(),

          textInput(
            "multibar_scale",
            "Scale Lines (comma-separated values)",
            value = isolate(input$multibar_scale) %||% "",
            placeholder = "e.g., 10,50,100"
          ),
          div(class = "help-text",
              "Optional: Specify values where scale lines will be drawn"),
          
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
                  "multibar_legend_title",
                  "Legend Title",
                  value = isolate(input$multibar_legend_title) %||% "Multi-bar Legend",
                  width = "250px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  "multibar_legend_scale",
                  "Legend Scale Factor",
                  value = isolate(input$multibar_legend_scale) %||% 1,
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
                  "multibar_width",
                  "Maximum Bar Width",
                  value = isolate(input$multibar_width) %||% 1000,
                  min = 50,
                  max = 5000,
                  step = 50,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  "multibar_margin",
                  "Left Margin",
                  value = isolate(input$multibar_margin) %||% 0,
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
                  "multibar_log_scale",
                  "Use logarithmic scale",
                  value = isolate(input$multibar_log_scale) %||% FALSE
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                checkboxInput(
                  "multibar_dashed_lines",
                  "Show dashed lines to leaf labels",
                  value = isolate(input$multibar_dashed_lines) %||% FALSE
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  "multibar_height_factor",
                  "Bar Height Factor",
                  value = isolate(input$multibar_height_factor) %||% 1,
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
                  "multibar_show_value",
                  "Display individual values inside bars",
                  value = FALSE
                )
              ),
              
              conditionalPanel(
                condition = "input.multibar_show_value",
                
                div(
                  style = "margin-bottom: 1rem;",
                  selectInput(
                    "multibar_label_position",
                    "Label Position",
                    choices = c(
                      "Left" = "left",
                      "Center" = "center",
                      "Right" = "right"
                    ),
                    selected = current_label_position,
                    width = "200px"
                  )
                ),
                
                div(
                  style = "margin-bottom: 1rem;",
                  numericInput(
                    "multibar_label_shift_x",
                    "Label Horizontal Shift",
                    value = isolate(input$multibar_label_shift_x) %||% 0,
                    min = -200,
                    max = 200,
                    step = 1,
                    width = "150px"
                  )
                ),
                
                div(
                  style = "margin-bottom: 1rem;",
                  numericInput(
                    "multibar_label_shift_y",
                    "Label Vertical Shift",
                    value = isolate(input$multibar_label_shift_y) %||% 0,
                    min = -200,
                    max = 200,
                    step = 1,
                    width = "150px"
                  )
                ),
                
                div(
                  style = "margin-bottom: 1rem;",
                  numericInput(
                    "multibar_label_size_factor",
                    "Label Size Factor",
                    value = isolate(input$multibar_label_size_factor) %||% 1,
                    min = 0.1,
                    max = 5,
                    step = 0.1,
                    width = "150px"
                  )
                ),
                
                div(
                  style = "margin-bottom: 1rem;",
                  checkboxInput(
                    "multibar_auto_color",
                    "Automatic label color (white/black based on bar darkness)",
                    value = current_auto_color
                  )
                ),
                
                conditionalPanel(
                  condition = "!input.multibar_auto_color",
                  
                  div(
                    style = "margin-bottom: 1rem;",
                    colourInput(
                      "multibar_label_color",
                      "Value Label Color",
                      value = current_label_color,
                      showColour = "both",
                      palette = "square",
                      returnName = FALSE
                    )
                  )
                )
              ),
              
              tags$hr(),
              
              # Bar positioning
              tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Bar Positioning"),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  "multibar_shift",
                  "Bar Vertical Shift",
                  value = isolate(input$multibar_shift) %||% 0,
                  min = -100,
                  max = 100,
                  step = 1,
                  width = "150px"
                ),
                div(class = "help-text",
                    "Move all bars up/down by a fixed amount")
              ),
              
              tags$hr(),
              
              # Border settings
              tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Bar Border"),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  "multibar_border_width",
                  "Border Width",
                  value = isolate(input$multibar_border_width) %||% 0,
                  min = 0,
                  max = 10,
                  step = 0.5,
                  width = "150px"
                ),
                div(class = "help-text",
                    "Width of border around bars (0 = no border)")
              ),
              
              conditionalPanel(
                condition = "input.multibar_border_width > 0",
                
                div(
                  style = "margin-bottom: 0;",
                  colourInput(
                    "multibar_border_color",
                    "Border Color",
                    value = isolate(input$multibar_border_color) %||% "#0000ff",
                    showColour = "both",
                    palette = "square",
                    returnName = FALSE
                  )
                )
              )
            )
          )
        )
      )
    )
    })

  # ---- Multi-bar color inputs ----
  output$multibar_color_inputs <- renderUI({
    req(input$multibar_fields)
    
    if(length(input$multibar_fields) == 0) {
      return(
        div(class = "help-text", "Select fields to configure colors")
      )
    }
    
    # Default colors
    default_colors <- c("#ff5757ff", "#86fc86ff", "#5858ffff", "#ff9bffff", "#7affffff", "#ffff88ff")
    
    tagList(
      lapply(seq_along(input$multibar_fields), function(idx) {
        field <- input$multibar_fields[idx]
        default_color <- default_colors[((idx - 1) %% length(default_colors)) + 1]
        current_color <- isolate(input[[paste0("multibar_field_color_", safe_id(field))]])
        if(is.null(current_color)) current_color <- default_color
        
        div(
          class = "value-config",
          div(class = "value-label", field),
          colourInput(
            paste0("multibar_field_color_", safe_id(field)),
            NULL,
            value = current_color,
            showColour = "both",
            palette = "square",
            returnName = FALSE
          )
        )
      })
    )
  })

    # ---- Generate multi-bar outputs ----
    multibar_output <- reactive({
    req(data(), input$id_col, input$multibar_fields)
    
    if(length(input$multibar_fields) < 1) return(NULL)
    
    df <- data()
    fields <- input$multibar_fields
    
    # Get basic settings
    multibar_scale <- input$multibar_scale %||% ""
    multibar_na_to_zero <- input$multibar_na_to_zero %||% TRUE
    multibar_layout <- input$multibar_layout %||% "stacked"
    
    # Get advanced settings
    multibar_width <- input$multibar_width %||% 1000
    multibar_margin <- input$multibar_margin %||% 0
    multibar_log_scale <- input$multibar_log_scale %||% FALSE
    multibar_dashed_lines <- input$multibar_dashed_lines %||% FALSE
    multibar_height_factor <- input$multibar_height_factor %||% 1
    multibar_show_value <- input$multibar_show_value %||% TRUE
    multibar_label_position <- input$multibar_label_position %||% "left"
    multibar_label_shift_x <- input$multibar_label_shift_x %||% 0
    multibar_label_shift_y <- input$multibar_label_shift_y %||% 0
    multibar_label_size_factor <- input$multibar_label_size_factor %||% 1
    multibar_auto_color <- input$multibar_auto_color %||% TRUE
    multibar_label_color <- input$multibar_label_color %||% "#0000ff"
    multibar_shift <- input$multibar_shift %||% 0
    multibar_border_width <- input$multibar_border_width %||% 0
    multibar_border_color <- input$multibar_border_color %||% "#0000ff"
    
    # Build iTOL DATASET_MULTIBAR format
    content <- c("DATASET_MULTIBAR")
    content <- c(content, "SEPARATOR COMMA")
    
    # Get dataset label
    multibar_dataset_label <- input$multibar_dataset_label %||% "multibar"
    content <- c(content, paste("DATASET_LABEL", multibar_dataset_label, sep = ","))
    content <- c(content, paste("COLOR", "#2C5F8D", sep = ","))
    content <- c(content, "")
    
    # Get field colors
    field_colors <- sapply(fields, function(field) {
        color <- input[[paste0("multibar_field_color_", safe_id(field))]]
        if(is.null(color)) "#3498DB" else color
    })
    
    content <- c(content, paste("FIELD_COLORS", paste(field_colors, collapse = ","), sep = ","))
    content <- c(content, paste("FIELD_LABELS", paste(fields, collapse = ","), sep = ","))
    content <- c(content, "")
    
    # Add scale lines if specified
    if(multibar_scale != "" && !is.na(multibar_scale)) {
        scale_values <- trimws(unlist(strsplit(multibar_scale, ",")))
        scale_line <- paste(scale_values, collapse = ",")
        content <- c(content, paste("DATASET_SCALE", scale_line, sep = ","))
    }
    content <- c(content, "")
    
    # Legend settings
    multibar_legend_title <- input$multibar_legend_title %||% "Multi-bar Legend"
    multibar_legend_scale <- input$multibar_legend_scale %||% 1

    content <- c(content, paste("LEGEND_TITLE", multibar_legend_title, sep = ","))
    content <- c(content, paste("LEGEND_SCALE", multibar_legend_scale, sep = ","))
    content <- c(content, paste("LEGEND_SHAPES", paste(rep("1", length(fields)), collapse = ","), sep = ","))
    content <- c(content, paste("LEGEND_COLORS", paste(field_colors, collapse = ","), sep = ","))
    content <- c(content, paste("LEGEND_LABELS", paste(fields, collapse = ","), sep = ","))
    content <- c(content, "")
    
    # Advanced display settings
    content <- c(content, paste("WIDTH", multibar_width, sep = ","))
    content <- c(content, paste("MARGIN", multibar_margin, sep = ","))
    if(multibar_log_scale) {
        content <- c(content, "LOG_SCALE,1")
    }
    if(multibar_dashed_lines) {
        content <- c(content, "DASHED_LINES,1")
    }
    content <- c(content, paste("HEIGHT_FACTOR", multibar_height_factor, sep = ","))
    content <- c(content, "")
    
    # Layout mode
    if(multibar_layout == "aligned") {
        content <- c(content, "ALIGN_FIELDS,1")
    } else if(multibar_layout == "side_stacked") {
        content <- c(content, "ALIGN_FIELDS,0")
        content <- c(content, "SIDE_STACKED,1")
    } else {  # stacked (default)
        content <- c(content, "ALIGN_FIELDS,0")
    }
    content <- c(content, "")
    
    # Border settings
    if(multibar_border_width > 0) {
        content <- c(content, paste("BORDER_WIDTH", multibar_border_width, sep = ","))
        content <- c(content, paste("BORDER_COLOR", multibar_border_color, sep = ","))
    }
    content <- c(content, "")
    
    # Value display settings
    content <- c(content, paste("SHOW_VALUE", if(multibar_show_value) "1" else "0", sep = ","))
    if(multibar_show_value) {
        content <- c(content, paste("LABEL_POSITION", multibar_label_position, sep = ","))
        content <- c(content, paste("LABEL_SHIFT_X", multibar_label_shift_x, sep = ","))
        content <- c(content, paste("LABEL_SHIFT_Y", multibar_label_shift_y, sep = ","))
        content <- c(content, paste("LABEL_SIZE_FACTOR", multibar_label_size_factor, sep = ","))
        
        if(multibar_auto_color) {
        content <- c(content, "LABEL_AUTO_COLOR,1")
        } else {
        content <- c(content, "LABEL_AUTO_COLOR,0")
        content <- c(content, paste("BAR_LABEL_COLOR", multibar_label_color, sep = ","))
        }
    }
    content <- c(content, "")
    
    # Bar positioning
    content <- c(content, paste("BAR_SHIFT", multibar_shift, sep = ","))
    content <- c(content, "")
    
    content <- c(content, "SHOW_LABELS,0")
    content <- c(content, "")
    content <- c(content, "DATA")
    
    # Data: ID followed by multiple numeric values
    for(i in 1:nrow(df)) {
        id <- as.character(df[[input$id_col]][i])
        
        # Get values for each field, converting to numeric if needed
        values <- sapply(fields, function(field) {
        col_data <- df[[field]]
        if(!is.numeric(col_data)) {
            col_data <- suppressWarnings(as.numeric(col_data))
        }
        val <- col_data[i]
        
        # Handle NA based on user preference
        if(!is.na(val)) {
            return(as.character(val))
        } else {
            if(multibar_na_to_zero) {
            return("0.0")
            } else {
            return(NA_character_)
            }
        }
        })
        
        # Include row based on NA handling mode
        if(multibar_na_to_zero) {
        # Always include row (NAs converted to 0)
        content <- c(content, paste(c(id, values), collapse = ","))
        } else {
        # Only include row if at least one value is non-NA
        if(any(!is.na(values))) {
            values[is.na(values)] <- ""
            content <- c(content, paste(c(id, values), collapse = ","))
        }
        }
    }
    
    return(paste(content, collapse = "\n"))
        })
    
    # ---- Multi-bar download card ----
    output$multibar_download_card <- renderUI({
        req(multibar_output())
        
        card(
        card_header("Download Multi-Value Bar Chart"),
        card_body(
            centered_download_button(
            "download_multibar",
            "Download Multi-Bar Chart File"
            )
        )
        )
    })

    # ---- Multi-bar download handler ----
    output$download_multibar <- downloadHandler(
        filename = function() {
        paste0(input$dataset_label, "_multibar.txt")
        },
        content = function(file) {
        writeLines(multibar_output(), file)
        }
    )