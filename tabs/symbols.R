  # ---- Symbol tab: Column settings UI ----
  output$symbol_column_settings_ui <- renderUI({
    req(input$data_cols)
    
    df <- isolate(data())
    brewer_pals <- get_brewer_palettes()
    
    # Create accordion for each column
    accordion_items <- lapply(seq_along(input$data_cols), function(idx) {
      col <- input$data_cols[idx]
      col_data <- df[[col]]
      
      # Determine if column is numeric (same logic as bar chart tab)
      is_numeric_col <- is.numeric(col_data)
      if(!is_numeric_col) {
        # Try converting to numeric
        converted <- suppressWarnings(as.numeric(col_data))
        non_na_count <- sum(!is.na(converted))
        # If at least one value converts successfully, treat as numeric
        is_numeric_col <- non_na_count > 0
      }
      
      # Get unique values (after standardization and NA removal)
      if(is_numeric_col) {
        # For numeric columns, convert and remove NAs
        if(!is.numeric(col_data)) {
          col_data <- suppressWarnings(as.numeric(col_data))
        }
        col_values <- unique(col_data[!is.na(col_data)])
        col_values <- sort(col_values)  # Sort numeric values
      } else {
        # For qualitative columns, standardize and remove NAs
        col_values <- unique(sapply(as.character(col_data), standardize_value))
        col_values <- col_values[!is.na(col_values)]
      }
      
      # Build palette choices based on column type
      if(is_numeric_col) {
        # Sequential palettes only for numeric data
        palette_choices <- c("Select a palette" = "", brewer_pals$Sequential)
        default_palette <- "Blues"
      } else {
        # Qualitative palettes only for categorical data
        palette_choices <- c("Select a palette" = "", brewer_pals$Qualitative)
        default_palette <- "Set1"
      }
      
      # Get current settings
      current_color_mode <- isolate(input[[paste0("color_mode_", col)]])
      current_brewer_pal <- isolate(input[[paste0("brewer_palette_", col)]])
      current_symbol_mode <- isolate(input[[paste0("symbol_mode_", col)]])
      current_auto_symbol <- isolate(input[[paste0("auto_symbol_", col)]])
      
      if(is.null(current_color_mode)) {
        current_color_mode <- "ColorBrewer" 
      }
      if(is.null(current_brewer_pal)) current_brewer_pal <- default_palette
      if(is.null(current_symbol_mode)) current_symbol_mode <- "Auto"
      if(is.null(current_auto_symbol)) current_auto_symbol <- 1
      
      # Create accordion item with advanced settings in side-by-side layout
      accordion_panel(
        title = paste0(col, if(is_numeric_col) " (Numeric)" else " (Categorical)"),
        value = paste0("panel_", idx),
        
        # Info about column type
        div(
          class = "info-box",
          style = "margin-bottom: 1rem; font-size: 0.85rem;",
          p(
            icon(if(is_numeric_col) "hashtag" else "font"),
            if(is_numeric_col) {
              sprintf("Numeric column with %d unique values (NA values filtered)", length(col_values))
            } else {
              sprintf("Categorical column with %d unique values (NA values filtered)", length(col_values))
            }
          )
        ),
        
        # Color mode selection
        radioGroupButtons(
          paste0("color_mode_", col),
          "Color Mode",
          choices = c("ColorBrewer" = "ColorBrewer", 
                      "Manual" = "Manual",
                      "Hue Scale" = "Hue Scale"),
          selected = current_color_mode,
          justified = FALSE,
          size = "xs",
          status = "primary",
          width = "100%",
          individual = TRUE
        ),
        
        # ColorBrewer palette selector
        conditionalPanel(
          condition = sprintf("input['color_mode_%s'] == 'ColorBrewer'", col),
          selectInput(
            paste0("brewer_palette_", col),
            if(is_numeric_col) "Select Sequential Palette" else "Select Qualitative Palette",
            choices = palette_choices,
            selected = current_brewer_pal,
            width = "200px"
          ),
          div(class = "help-text",
              if(is_numeric_col) {
                "Sequential palettes are recommended for numeric data"
              } else {
                "Qualitative palettes are recommended for categorical data"
              }
          )
        ),
        
        tags$hr(),
        
        # Symbol mode selection
        radioGroupButtons(
          paste0("symbol_mode_", col),
          "Symbol Mode",
          choices = c("Auto" = "Auto", "Manual" = "Manual"),
          selected = current_symbol_mode,
          justified = FALSE,
          size = "xs",
          status = "primary",
          width = "75%",
          individual = TRUE
        ),
        
        # Auto symbol selector
        conditionalPanel(
          condition = sprintf("input['symbol_mode_%s'] == 'Auto'", col),
          selectInput(
            paste0("auto_symbol_", col),
            "Symbol for All Values",
            choices = symbol_names,
            selected = current_auto_symbol,
            width = "200px"
          )
        ),
        
        tags$hr(),
        
        # Fill/empty option for symbols
        checkboxInput(
          paste0("symbol_filled_", col),
          "Fill symbols (uncheck for outline only)",
          value = isolate(input[[paste0("symbol_filled_", col)]]) %||% TRUE
        ),
        
        # Manual configuration
        conditionalPanel(
          condition = sprintf("input['color_mode_%s'] == 'Manual' || input['symbol_mode_%s'] == 'Manual'", col, col),
          tags$h6("Configure Individual Values"),
          lapply(col_values, function(val) {
            val_display <- if(is_numeric_col) as.character(val) else val
            val_id <- safe_id(paste(col, val_display, sep = "_"))
            
            current_color <- isolate(input[[paste0("color_", val_id)]])
            current_symbol <- isolate(input[[paste0("symbol_", val_id)]])
            
            if(is.null(current_color)) current_color <- "#3498DB"
            if(is.null(current_symbol)) current_symbol <- 1
            
            div(
              class = "value-config",
              div(class = "value-label", val_display),
              conditionalPanel(
                condition = sprintf("input['color_mode_%s'] == 'Manual'", col),
                colourInput(
                  paste0("color_", val_id),
                  NULL,
                  value = current_color,
                  showColour = "both",
                  palette = "square",
                  returnName = FALSE
                )
              ),
              conditionalPanel(
                condition = sprintf("input['symbol_mode_%s'] == 'Manual'", col),
                selectInput(
                  paste0("symbol_", val_id),
                  NULL,
                  choices = symbol_names,
                  selected = current_symbol,
                  width = "120px"
                )
              )
            )
          })
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
                paste0("symbol_legend_title_", col),
                "Legend Title",
                value = isolate(input[[paste0("symbol_legend_title_", col)]]) %||% col,
                width = "250px"
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("symbol_legend_visible_", col),
                "Show legend initially",
                value = isolate(input[[paste0("symbol_legend_visible_", col)]]) %||% TRUE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("symbol_legend_horizontal_", col),
                "Horizontal legend layout",
                value = isolate(input[[paste0("symbol_legend_horizontal_", col)]]) %||% FALSE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("symbol_legend_scale_", col),
                "Legend Scale Factor",
                value = isolate(input[[paste0("symbol_legend_scale_", col)]]) %||% 1,
                min = 0.1,
                max = 5,
                step = 0.1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Scale factor for legend symbols")
            ),
            
            tags$hr(),

            tags$h6(style = "color: #2C5F8D;", "Symbol Settings"),

            # Maximum symbol size
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("symbol_max_size_", col),
                "Maximum Symbol Size",
                value = isolate(input[[paste0("symbol_max_size_", col)]]) %||% 14,
                min = 1,
                max = 100,
                step = 1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Largest symbol will be displayed with this size (in pixels)")
            ),
            
            # Gradient fill
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("symbol_gradient_", col),
                "Use gradient fill (instead of solid color)",
                value = isolate(input[[paste0("symbol_gradient_", col)]]) %||% FALSE
              )
            ),
            
            # Symbol spacing (for external symbols)
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("symbol_spacing_", col),
                "Symbol Column Spacing",
                value = isolate(input[[paste0("symbol_spacing_", col)]]) %||% 10,
                min = 0,
                max = 100,
                step = 1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Spacing between symbol columns (only for external symbols)")
            ),

            # Symbol position
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("symbol_position_", col),
                "Symbol Position",
                value = isolate(input[[paste0("symbol_position_", col)]]) %||% -1,
                min = -10,
                max = 1,
                step = 0.1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Position: 0-1 = on branch (0=start, 0.5=middle, 1=end), negative = external column (-1=first, -2=second, etc.)")
            ),
            
            tags$hr(),
            
            # Label settings
            tags$h6(style = "color: #2C5F8D; margin-top = 1rem;", "Label Settings"),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("symbol_show_labels_", col),
                "Show dataset label",
                value = isolate(input[[paste0("symbol_show_labels_", col)]]) %||% FALSE
              )
            ),
            
            conditionalPanel(
              condition = sprintf("input['symbol_show_labels_%s']", col),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("symbol_label_size_factor_", col),
                  "Label Size Factor",
                  value = isolate(input[[paste0("symbol_label_size_factor_", col)]]) %||% 1,
                  min = 0.1,
                  max = 5,
                  step = 0.1,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("symbol_label_rotation_", col),
                  "Label Rotation (degrees)",
                  value = isolate(input[[paste0("symbol_label_rotation_", col)]]) %||% 0,
                  min = -180,
                  max = 180,
                  step = 1,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("symbol_label_shift_", col),
                  "Label Horizontal Shift",
                  value = isolate(input[[paste0("symbol_label_shift_", col)]]) %||% 0,
                  min = -200,
                  max = 200,
                  step = 1,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                checkboxInput(
                  paste0("symbol_label_align_to_tree_", col),
                  "Align label to tree circle (circular mode only)",
                  value = isolate(input[[paste0("symbol_label_align_to_tree_", col)]]) %||% FALSE
                )
              )
            )
          )
        )
      )
    })
    
    # Return accordion
    accordion(
      id = "column_accordion",
      multiple = TRUE,
      !!!accordion_items
    )
  })
    
  # ---- ColorBrewer plot for symbol tab ----
  output$brewer_plot_symbol <- renderPlot({
    # Create layout for two separate sections
    par(mfrow = c(2, 1), mar = c(1, 10, 3, 2))
    
    # Sequential palettes
    display.brewer.all(type = "seq")
    title("Sequential Palettes (for Numeric Data)", cex.main = 1.2, font.main = 2)
    
    # Qualitative palettes
    display.brewer.all(type = "qual")
    title("Qualitative Palettes (for Categorical Data)", cex.main = 1.2, font.main = 2)
    
  }, res = 96, height = 600)
  
  # ---- Generate symbol outputs ----
  symbol_outputs <- reactive({
    req(data(), input$id_col, input$data_cols)
    
    df <- data()
    output_list <- list()
    
    for(col in input$data_cols) {
      col_data <- df[[col]]
      
      # Determine if column is numeric (same logic as bar chart and UI)
      is_numeric_col <- is.numeric(col_data)
      if(!is_numeric_col) {
        converted <- suppressWarnings(as.numeric(col_data))
        non_na_count <- sum(!is.na(converted))
        is_numeric_col <- non_na_count > 0
      }
      
      # Get unique values with NA filtering
      if(is_numeric_col) {
        if(!is.numeric(col_data)) {
          col_data_clean <- suppressWarnings(as.numeric(col_data))
        } else {
          col_data_clean <- col_data
        }
        col_values <- unique(col_data_clean[!is.na(col_data_clean)])
        col_values <- sort(col_values)
      } else {
        col_values <- unique(sapply(as.character(col_data), standardize_value))
        col_values <- col_values[!is.na(col_values)]
      }
      
      # Get color and symbol settings
      color_mode <- input[[paste0("color_mode_", col)]]
      symbol_mode <- input[[paste0("symbol_mode_", col)]]
      symbol_filled <- input[[paste0("symbol_filled_", col)]]
      
      if(is.null(color_mode)) color_mode <- "ColorBrewer"
      if(is.null(symbol_mode)) symbol_mode <- "Auto"
      if(is.null(symbol_filled)) symbol_filled <- TRUE
      
      # Generate colors
      if(color_mode == "ColorBrewer") {
        brewer_pal <- input[[paste0("brewer_palette_", col)]]
        if(is.null(brewer_pal)) brewer_pal <- if(is_numeric_col) "Blues" else "Set1"
        n_colors <- max(3, min(length(col_values), 12))
        colors <- suppressWarnings(brewer.pal(n_colors, brewer_pal))
        if(length(col_values) > length(colors)) {
          colors <- colorRampPalette(colors)(length(col_values))
        }
        color_map <- setNames(colors[1:length(col_values)], as.character(col_values))
      } else if(color_mode == "Manual") {
        color_map <- setNames(
          sapply(col_values, function(val) {
            val_display <- if(is_numeric_col) as.character(val) else val
            val_id <- safe_id(paste(col, val_display, sep = "_"))
            color <- input[[paste0("color_", val_id)]]
            if(is.null(color)) "#3498DB" else color
          }),
          as.character(col_values)
        )
      } else {  # Hue Scale
        colors <- hue_pal()(length(col_values))
        color_map <- setNames(colors, as.character(col_values))
      }
      
      # Generate symbols
      if(symbol_mode == "Auto") {
        auto_symbol <- input[[paste0("auto_symbol_", col)]]
        if(is.null(auto_symbol)) auto_symbol <- 1
        symbol_map <- setNames(rep(auto_symbol, length(col_values)), as.character(col_values))
      } else {  # Manual
        symbol_map <- setNames(
          sapply(col_values, function(val) {
            val_display <- if(is_numeric_col) as.character(val) else val
            val_id <- safe_id(paste(col, val_display, sep = "_"))
            symbol <- input[[paste0("symbol_", val_id)]]
            if(is.null(symbol)) 2 else symbol
          }),
          as.character(col_values)
        )
      }
      
      # Get advanced settings
      symbol_max_size <- input[[paste0("symbol_max_size_", col)]] %||% 14
      symbol_gradient <- input[[paste0("symbol_gradient_", col)]] %||% FALSE
      symbol_spacing <- input[[paste0("symbol_spacing_", col)]] %||% 10
      symbol_legend_title <- input[[paste0("symbol_legend_title_", col)]] %||% col
      symbol_legend_visible <- input[[paste0("symbol_legend_visible_", col)]] %||% TRUE
      symbol_legend_horizontal <- input[[paste0("symbol_legend_horizontal_", col)]] %||% FALSE
      symbol_legend_scale <- input[[paste0("symbol_legend_scale_", col)]] %||% 1
      symbol_show_labels <- input[[paste0("symbol_show_labels_", col)]] %||% FALSE
      symbol_label_size_factor <- input[[paste0("symbol_label_size_factor_", col)]] %||% 1
      symbol_label_rotation <- input[[paste0("symbol_label_rotation_", col)]] %||% 0
      symbol_label_shift <- input[[paste0("symbol_label_shift_", col)]] %||% 0
      symbol_label_align_to_tree <- input[[paste0("symbol_label_align_to_tree_", col)]] %||% FALSE
      symbol_position <- input[[paste0("symbol_position_", col)]] %||% -1

      # Build iTOL DATASET_SYMBOL format
      content <- c("DATASET_SYMBOL")
      content <- c(content, "SEPARATOR TAB")
      content <- c(content, paste("DATASET_LABEL", paste(input$dataset_label, "-", col), sep = "\t"))
      content <- c(content, paste("COLOR", "#2C5F8D", sep = "\t"))
      content <- c(content, "")
      
      # Legend settings (with advanced options)
      content <- c(content, paste("LEGEND_TITLE", symbol_legend_title, sep = "\t"))
      content <- c(content, paste("LEGEND_SCALE", symbol_legend_scale, sep = "\t"))
      content <- c(content, paste("LEGEND_VISIBLE", if(symbol_legend_visible) "1" else "0", sep = "\t"))
      if(symbol_legend_horizontal) {
        content <- c(content, paste("LEGEND_HORIZONTAL", "1", sep = "\t"))
      }
      content <- c(content, paste("LEGEND_SHAPES", paste(symbol_map, collapse = "\t"), sep = "\t"))
      content <- c(content, paste("LEGEND_COLORS", paste(color_map, collapse = "\t"), sep = "\t"))
      content <- c(content, paste("LEGEND_LABELS", paste(names(color_map), collapse = "\t"), sep = "\t"))
      content <- c(content, "")
      
      # Symbol display settings (with advanced options)
      content <- c(content, paste("MAXIMUM_SIZE", symbol_max_size, sep = "\t"))
      content <- c(content, paste("GRADIENT_FILL", if(symbol_gradient) "1" else "0", sep = "\t"))
      content <- c(content, paste("SYMBOL_SPACING", symbol_spacing, sep = "\t"))
      content <- c(content, "")
      
      # Label settings (with advanced options)
      content <- c(content, paste("SHOW_LABELS", if(symbol_show_labels) "1" else "0", sep = "\t"))
      content <- c(content, paste("LABEL_SIZE_FACTOR", symbol_label_size_factor, sep = "\t"))
      content <- c(content, paste("LABEL_ROTATION", symbol_label_rotation, sep = "\t"))
      content <- c(content, paste("LABEL_SHIFT", symbol_label_shift, sep = "\t"))
      content <- c(content, paste("LABEL_ALIGN_TO_TREE", if(symbol_label_align_to_tree) "1" else "0", sep = "\t"))
      content <- c(content, "")
      content <- c(content, "DATA")
      
      # Data format: ID, symbol, size, color, fill, position, label
      for(i in 1:nrow(df)) {
        id <- as.character(df[[input$id_col]][i])
        
        # Get value and handle numeric vs categorical appropriately
        if(is_numeric_col) {
          if(!is.numeric(col_data)) {
            val <- suppressWarnings(as.numeric(df[[col]][i]))
          } else {
            val <- col_data[i]
          }
          val_display <- if(!is.na(val)) as.character(val) else NA_character_
          val_key <- val
        } else {
          val <- standardize_value(df[[col]][i])
          val_display <- val
          val_key <- val
        }
        
        # Skip NA values
        if(!is.na(val_key)) {
          val_key_char <- as.character(val_key)
          symbol <- symbol_map[val_key_char]
          color <- color_map[val_key_char]
          fill_value <- if(symbol_filled) "1" else "0"
          content <- c(content, paste(id, symbol, "1", color, fill_value, symbol_position, val_display, sep = "\t"))
        }
      }
      
      output_list[[col]] <- paste(content, collapse = "\n")
    }
    
    return(output_list)
    })
    
    # ---- Symbol download card ----
    output$symbol_download_card <- renderUI({
      req(symbol_outputs())
      content_list <- symbol_outputs()
      
      card(
        card_header("Download Symbol Annotations"),
        card_body(
          if(length(content_list) == 1) {
            centered_download_button(
              "download_symbol_single", 
              "Download Symbol File"
            )
          } else {
            tagList(
              centered_download_button(
                "download_symbols_zip",
                "Download All Symbol Files (ZIP)",
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
                    paste0("download_symbol_", safe_id(name)),
                    paste0(name, ".txt"),
                    class = "btn-primary btn-sm"
                  )
                )
              })
            )
          }
        )
      )
    })

    # Single symbol file
    output$download_symbol_single <- downloadHandler(
      filename = function() {
        paste0(input$dataset_label, "_symbol.txt")
      },
      content = function(file) {
        content_list <- symbol_outputs()
        writeLines(content_list[[1]], file)
      }
    )
    
    # All symbol files as ZIP
    output$download_symbols_zip <- downloadHandler(
      filename = function() {
        paste0(input$dataset_label, "_symbols_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        content_list <- symbol_outputs()
        temp_dir <- tempdir()
        temp_files <- c()
        
        for(name in names(content_list)) {
          temp_file <- file.path(temp_dir, paste0(name, ".txt"))
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
    
    # Individual symbol files
    observe({
      req(input$data_cols)
      
      tryCatch({
        content_list <- symbol_outputs()
        req(content_list)
        
        if(length(content_list) > 1) {
          lapply(names(content_list), function(name) {
            local({
              my_name <- name
              output[[paste0("download_symbol_", safe_id(my_name))]] <- downloadHandler(
                filename = function() {
                  paste0(my_name, ".txt")
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