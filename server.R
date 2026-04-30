
# ---------- Server ----------

server <- function(input, output, session) {
  
  # ---- Data reactive with sheet selection ----
  data <- reactive({
    req(input$file)
    file <- input$file$datapath
    ext <- tolower(tools::file_ext(input$file$name))
    tryCatch({
      df <- if (ext == "tsv") {
        read_tsv(file, show_col_types = FALSE)
      } else if (ext == "csv") {
        read_csv(file, show_col_types = FALSE)
      } else if (ext == "xlsx") {
        # Check number of sheets
        sheets <- excel_sheets(file)
        
        if(length(sheets) > 1) {
          # Multiple sheets - need selection
          req(input$excel_sheet)
          read_excel(file, sheet = input$excel_sheet)
        } else {
          # Single sheet - read directly
          read_excel(file)
        }
      } else {
        stop("Unsupported file format")
      }
      
      # Sanitize column names
      names(df) <- sapply(names(df), sanitize_colname)
      
      # Check for duplicate names after sanitization
      if(any(duplicated(names(df)))) {
        # Make names unique by adding numeric suffix
        names(df) <- make.unique(names(df), sep = "_")
      }
      
      return(df)
      
    }, error = function(e) {
      NULL
    })
  })

  # ---- Reactive to detect Excel sheets ----
  excel_sheets_list <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    
    if(ext == "xlsx") {
      sheets <- excel_sheets(input$file$datapath)
      if(length(sheets) > 1) {
        return(sheets)
      }
    }
    return(NULL)
  })

  # ---- Show modal for sheet selection ----
  observeEvent(excel_sheets_list(), {
    sheets <- excel_sheets_list()
    
    if(!is.null(sheets)) {
      showModal(
        modalDialog(
          title = "Select Excel Sheet",
          size = "m",
          easyClose = FALSE,
          
          div(
            class = "info-box",
            p(icon("info-circle"), 
              "This Excel file contains multiple sheets. Please select which sheet to import.")
          ),
          
          selectInput(
            "excel_sheet",
            "Choose a sheet:",
            choices = sheets,
            selected = sheets[1]
          ),
          
          footer = tagList(
            actionButton("confirm_sheet", "Load Sheet", class = "btn-success")
          )
        )
      )
    }
  })

  # ---- Close modal when sheet is confirmed ----
  observeEvent(input$confirm_sheet, {
    removeModal()
  })
  
  # ---- Data preview table ----
  output$table <- renderDT({
    req(data())
    datatable(
      data(),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv')
      ),
      class = 'cell-border stripe',
      rownames = FALSE
    )
  })
  
  # ---- Column selection card ----
  output$column_selection_card <- renderUI({
    req(data())
    cols <- names(data())
    
    card(
      card_header("Column Selection"),
      card_body(
        selectInput(
          "id_col",
          "ID Column",
          choices = cols,
          selected = cols[1]
        ),
        div(class = "help-text",
            "Select the column containing unique identifiers"),
        
        tags$br(),
        
        selectizeInput(
          "data_cols",
          "Columns to Visualize/Annotate",
          choices = cols,
          multiple = TRUE,
          options = list(
            placeholder = 'Select one or more columns',
            plugins = list('remove_button')
          )
        )
      )
    )
  })
  
  # ---- Dataset label card ----
  output$dataset_label_card <- renderUI({
    req(data())
    
    card(
      card_header("Dataset Label"),
      card_body(
        textInput(
          "dataset_label", 
          NULL,
          value = "My Dataset",
          placeholder = "Enter a descriptive name"
        ),
        div(class = "help-text",
            "This label will appear in iTOL annotations")
      )
    )
  })

    # ---- Label column selection UI ----
  output$label_column_selection <- renderUI({
    req(data())
    cols <- names(data())
    
    tagList(
      selectInput(
        "old_label_col",
        "ID Column",
        choices = cols,
        selected = input$id_col
      ),
      div(class = "help-text",
          "Column containing the labels currently/originally in your tree"),
      
      tags$br(),
      
      selectInput(
        "new_label_col",
        "New Tip Label Column",
        choices = cols,
        selected = if(length(cols) > 1) cols[2] else cols[1]
      ),
      div(class = "help-text",
          "Column containing the new tip labels to use")
    )
  })

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
      
      # Create accordion item
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
            selected = current_brewer_pal
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
            width = "80%"
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
    
    if(is.null(color_mode)) {
      color_mode <- "ColorBrewer"
    }
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
    
    # Build iTOL DATASET_SYMBOL format
    content <- c("DATASET_SYMBOL")
    content <- c(content, "SEPARATOR TAB")
    content <- c(content, paste("DATASET_LABEL", paste(input$dataset_label, "-", col), sep = "\t"))
    content <- c(content, paste("COLOR", "#2C5F8D", sep = "\t"))
    content <- c(content, "")
    content <- c(content, paste("LEGEND_TITLE", col, sep = "\t"))
    content <- c(content, paste("LEGEND_SHAPES", paste(symbol_map, collapse = "\t"), sep = "\t"))
    content <- c(content, paste("LEGEND_COLORS", paste(color_map, collapse = "\t"), sep = "\t"))
    content <- c(content, paste("LEGEND_LABELS", paste(names(color_map), collapse = "\t"), sep = "\t"))
    content <- c(content, "")
    
    # Set maximum symbol size (works well for rectangular tree, adjust as needed for circular)
    content <- c(content, "MAXIMUM_SIZE\t14")
    content <- c(content, "")

    # Label settings
    content <- c(content, "SHOW_LABELS\t0")
    content <- c(content, "LABEL_SIZE_FACTOR\t1")
    content <- c(content, "LABEL_ROTATION\t0")
    content <- c(content, "LABEL_SHIFT\t0")
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
        content <- c(content, paste(id, symbol, "1", color, fill_value, "-1", val_display, sep = "\t"))
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
            selected = isolate(input[[paste0("binary_values_", col)]]) %||% col_values[1]
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
      
      # Legend
      content <- c(content, paste("LEGEND_TITLE", col, sep = "\t"))
      content <- c(content, paste("LEGEND_SHAPES", paste(field_shapes, collapse = "\t"), sep = "\t"))
      content <- c(content, paste("LEGEND_COLORS", paste(field_colors, collapse = "\t"), sep = "\t"))
      content <- c(content, paste("LEGEND_LABELS", paste(fields, collapse = "\t"), sep = "\t"))
      content <- c(content, "")
      
      # Grid settings - disable both
      content <- c(content, "HORIZONTAL_GRID\t0")
      content <- c(content, "VERTICAL_GRID\t0")
      content <- c(content, "")
      
      content <- c(content, "SHOW_LABELS\t1")
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



    # ---- Bar chart tab: Column settings UI ----
  output$bar_column_settings_ui <- renderUI({
    req(input$data_cols)
    
    df <- isolate(data())
    
    # Filter to numeric or convertible-to-numeric columns
    numeric_cols <- c()
    na_counts <- list()
    
    for(col in input$data_cols) {
      col_data <- df[[col]]
      
      # Check if already numeric
      if(is.numeric(col_data)) {
        numeric_cols <- c(numeric_cols, col)
        na_counts[[col]] <- sum(is.na(col_data))
      } else {
        # Try to convert to numeric
        converted <- suppressWarnings(as.numeric(col_data))
        
        # Count how many values successfully converted (non-NA after conversion)
        non_na_count <- sum(!is.na(converted))
        
        # If at least one value converts successfully, consider it numeric
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
        
        # Color selection
        colourInput(
          paste0("bar_color_", col),
          "Bar Color",
          value = current_bar_color,
          showColour = "both",
          palette = "square",
          returnName = FALSE
        ),
        
        # Scale lines configuration
        textInput(
          paste0("bar_scale_", col),
          "Scale Lines (comma-separated values)",
          value = isolate(input[[paste0("bar_scale_", col)]]) %||% "",
          placeholder = "e.g., 10,50,100"
        ),
        
        div(class = "help-text",
            "Optional: Specify values where scale lines will be drawn"),
        
        tags$br(),
        
        # Show values checkbox
        div(
          style = "margin-bottom: 1rem;",
          checkboxInput(
            paste0("bar_show_value_", col),
            "Show values on bars",
            value = isolate(input[[paste0("bar_show_value_", col)]]) %||% TRUE
          )
        ),
        
        # Value label settings (only show if show_value is checked)
        conditionalPanel(
          condition = paste0("input.bar_show_value_", col),
          selectInput(
            paste0("bar_value_position_", col),
            "Value Label Position",
            choices = c(
              "Outside Right" = "outside-right",
              "Outside Left" = "outside-left", 
              "Left" = "left",
              "Center" = "center",
              "Right" = "right",
              "Dataset Center" = "dataset-center"
            ),
            selected = isolate(input[[paste0("bar_value_position_", col)]]) %||% "left"
          ),
          
          div(
            style = "margin-bottom: 1rem;",
            checkboxInput(
              paste0("bar_auto_color_", col),
              "Automatic label color (white/black based on bar darkness)",
              value = isolate(input[[paste0("bar_auto_color_", col)]]) %||% TRUE
            )
          ),
          
          # Only show manual color picker if auto color is OFF
          conditionalPanel(
            condition = paste0("!input.bar_auto_color_", col),
            colourInput(
              paste0("bar_label_color_", col),
              "Value Label Color",
              value = isolate(input[[paste0("bar_label_color_", col)]]) %||% "#000000",
              showColour = "both",
              palette = "square",
              returnName = FALSE
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
        # Try to convert to numeric
        converted <- suppressWarnings(as.numeric(col_data))
        # If at least one value converts, include it
        if(sum(!is.na(converted)) > 0) {
          numeric_cols <- c(numeric_cols, col)
        }
      }
    }
    
    if(length(numeric_cols) == 0) return(NULL)
    
    for(col in numeric_cols) {
      # Get settings
      bar_color <- input[[paste0("bar_color_", col)]]
      bar_scale <- input[[paste0("bar_scale_", col)]]
      
      if(is.null(bar_color)) bar_color <- "#2C5F8D"
      if(is.null(bar_scale)) bar_scale <- ""
      
      # Build iTOL DATASET_SIMPLEBAR format
      content <- c("DATASET_SIMPLEBAR")
      content <- c(content, "SEPARATOR TAB")
      content <- c(content, paste("DATASET_LABEL", paste(input$dataset_label, "-", col), sep = "\t"))
      content <- c(content, paste("COLOR", bar_color, sep = "\t"))
      # Add legend
      content <- c(content, "")
      content <- c(content, paste("LEGEND_TITLE", col, sep = "\t"))
      content <- c(content, paste("LEGEND_SHAPES", "1", sep = "\t"))
      content <- c(content, paste("LEGEND_COLORS", bar_color, sep = "\t"))
      content <- c(content, paste("LEGEND_LABELS", col, sep = "\t"))
      
      # Add scale lines if specified (convert comma-separated to tab-separated)
      if(bar_scale != "" && !is.na(bar_scale)) {
        scale_values <- trimws(unlist(strsplit(bar_scale, ",")))
        scale_line <- paste(scale_values, collapse = "\t")
        content <- c(content, paste("DATASET_SCALE", scale_line, sep = "\t"))
      }
      
      content <- c(content, "")
      content <- c(content, "SHOW_LABELS\t0")
      
      # Show value settings
      bar_show_value <- input[[paste0("bar_show_value_", col)]]
      if(is.null(bar_show_value)) bar_show_value <- TRUE
      
      if(bar_show_value) {
        content <- c(content, "SHOW_VALUE\t1")
        
        bar_value_position <- input[[paste0("bar_value_position_", col)]]
        if(!is.null(bar_value_position)) {
          content <- c(content, paste("LABEL_POSITION", bar_value_position, sep = "\t"))
        }
        
        bar_auto_color <- input[[paste0("bar_auto_color_", col)]]
        if(is.null(bar_auto_color)) bar_auto_color <- TRUE
        
        if(bar_auto_color) {
          content <- c(content, "LABEL_AUTO_COLOR\t1")
        } else {
          content <- c(content, "LABEL_AUTO_COLOR\t0")
          bar_label_color <- input[[paste0("bar_label_color_", col)]]
          if(!is.null(bar_label_color)) {
            content <- c(content, paste("BAR_LABEL_COLOR", bar_label_color, sep = "\t"))
          }
        }
      } else {
        content <- c(content, "SHOW_VALUE\t0")
      }
    
      content <- c(content, "")
      content <- c(content, "")
      content <- c(content, "DATA")
      
      # Get column data and convert to numeric if needed
      col_data <- df[[col]]
      if(!is.numeric(col_data)) {
        col_data <- suppressWarnings(as.numeric(col_data))
      }
      
      # Count filtered values
      na_count <- 0
      
      # Data: ID followed by numeric value - only include valid numerics
      for(i in 1:nrow(df)) {
        id <- as.character(df[[input$id_col]][i])
        val <- col_data[i]
        
        # Only include non-NA numeric values
        if(!is.na(val)) {
          content <- c(content, paste(id, val, sep = "\t"))
        } else {
          na_count <- na_count + 1
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

  # ---- Generate metadata output ----
  metadata_output <- reactive({
    req(data(), input$id_col, input$data_cols)
    df <- data()
    
    content <- c("METADATA")
    content <- c(content, "SEPARATOR TAB")
    content <- c(content, paste("FIELD_LABELS", paste(input$data_cols, collapse = "\t"), sep = "\t"))
    content <- c(content, "")
    content <- c(content, "DATA")
    
    for(i in 1:nrow(df)) {
      id <- as.character(df[[input$id_col]][i])
      values <- sapply(input$data_cols, function(col) as.character(df[[col]][i]))
      content <- c(content, paste(c(id, values), collapse = "\t"))
    }
    
    paste(content, collapse = "\n")
  })
  
  # ---- Metadata preview ----
  output$metadata_preview_ui <- renderUI({
    req(metadata_output())
    
    card(
      card_header("Preview: metadata.txt"),
      card_body(
        tags$pre(
          style = "max-height: 400px; overflow-y: auto; background-color: #f8f9fa; padding: 1rem; border-radius: 0.25rem; border: 1px solid #dee2e6;",
          metadata_output()
        )
      )
    )
  })
  
  # ---- Metadata download card ----
  output$metadata_download_card <- renderUI({
    req(metadata_output())
    
    card(
      card_header("Download Metadata"),
      card_body(
        centered_download_button(
          "download_metadata",
          "Download metadata.txt"
        )
      )
    )
  })
  
  # ---- Generate labels output ----
  labels_output <- reactive({
    req(data(), input$old_label_col, input$new_label_col)
    df <- data()
    
    content <- c("LABELS")
    content <- c(content, "SEPARATOR TAB")
    content <- c(content, "DATA")
    
    for(i in 1:nrow(df)) {
      old_label <- as.character(df[[input$old_label_col]][i])
      new_label <- as.character(df[[input$new_label_col]][i])
      content <- c(content, paste(old_label, new_label, sep = "\t"))
    }
    
    paste(content, collapse = "\n")
  })
  
    # ---- Labels preview ----
  output$labels_preview_ui <- renderUI({
    req(labels_output())
    
    card(
      card_header("Preview: labels.txt"),
      card_body(
        tags$pre(
          style = "max-height: 400px; overflow-y: auto; background-color: #f8f9fa; padding: 1rem; border-radius: 0.25rem; border: 1px solid #dee2e6;",
          labels_output()
        )
      )
    )
  })
  
  # ---- Labels download card ----
  output$labels_download_card <- renderUI({
    req(labels_output())
    
    card(
      card_header("Download Labels"),
      card_body(
        centered_download_button(
          "download_labels",
          "Download labels.txt"
        )
      )
    )
  })
  
  # ---- Download handlers ----
  
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
    current_align <- isolate(input$multibar_align)
    current_side_stacked <- isolate(input$multibar_side_stacked)
    current_show_value <- isolate(input$multibar_show_value)
    current_label_position <- isolate(input$multibar_label_position)
    current_auto_color <- isolate(input$multibar_auto_color)
    current_label_color <- isolate(input$multibar_label_color)
    
    if(is.null(current_selected)) current_selected <- numeric_cols[1:min(3, length(numeric_cols))]
    if(is.null(current_align)) current_align <- FALSE
    if(is.null(current_side_stacked)) current_side_stacked <- FALSE
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
            selected = isolate(input$multibar_layout) %||% "stacked"
          ),
          
          div(class = "help-text",
              "Choose how multiple fields are displayed in the bar chart"),
          
          tags$hr(),
          
          div(
            style = "margin-bottom: 1rem;",
            checkboxInput(
              "multibar_na_to_zero",
              "Convert missing values (NA) to 0.0 (uncheck to exclude samples with missing data)",
              value = isolate(input$multibar_na_to_zero) %||% TRUE
            )
          ),
          div(class = "help-text",
              "When checked, missing values will be displayed as 0 in the chart. When unchecked, samples with any missing values will be excluded."),

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
          
          div(
            style = "margin-bottom: 1rem;",
            checkboxInput(
              "multibar_show_value",
              "Show values on bars",
              value = current_show_value
            )
          ),
          
          conditionalPanel(
            condition = "input.multibar_show_value",
            selectInput(
              "multibar_label_position",
              "Value Label Position",
              choices = c(
                "Left" = "left",
                "Center" = "center",
                "Right" = "right"
              ),
              selected = current_label_position
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
  
  # Get settings
  multibar_align <- input$multibar_align
  multibar_side_stacked <- input$multibar_side_stacked
  multibar_scale <- input$multibar_scale
  multibar_show_value <- input$multibar_show_value
  multibar_label_position <- input$multibar_label_position
  multibar_auto_color <- input$multibar_auto_color
  multibar_label_color <- input$multibar_label_color
  
  if(is.null(multibar_align)) multibar_align <- FALSE
  if(is.null(multibar_side_stacked)) multibar_side_stacked <- FALSE
  if(is.null(multibar_scale)) multibar_scale <- ""
  if(is.null(multibar_show_value)) multibar_show_value <- TRUE
  if(is.null(multibar_label_position)) multibar_label_position <- "left"
  if(is.null(multibar_auto_color)) multibar_auto_color <- TRUE
  if(is.null(multibar_label_color)) multibar_label_color <- "#000000"
  
  # Build iTOL DATASET_MULTIBAR format
  content <- c("DATASET_MULTIBAR")
  content <- c(content, "SEPARATOR TAB")
  content <- c(content, paste("DATASET_LABEL", paste(input$dataset_label, "- multibar"), sep = "\t"))
  content <- c(content, paste("COLOR", "#2C5F8D", sep = "\t"))
  content <- c(content, "")
  
  # Get field colors
  field_colors <- sapply(fields, function(field) {
    color <- input[[paste0("multibar_field_color_", safe_id(field))]]
    if(is.null(color)) "#3498DB" else color
  })
  
  content <- c(content, paste("FIELD_COLORS", paste(field_colors, collapse = "\t"), sep = "\t"))
  content <- c(content, paste("FIELD_LABELS", paste(fields, collapse = "\t"), sep = "\t"))
  
  # Add legend
  content <- c(content, "")
  content <- c(content, paste("LEGEND_TITLE", "Dataset legend", sep = "\t"))
  content <- c(content, paste("LEGEND_SHAPES", paste(rep("1", length(fields)), collapse = "\t"), sep = "\t"))
  content <- c(content, paste("LEGEND_COLORS", paste(field_colors, collapse = "\t"), sep = "\t"))
  content <- c(content, paste("LEGEND_LABELS", paste(fields, collapse = "\t"), sep = "\t"))
  content <- c(content, "")
  
  # Add scale lines if specified
  if(multibar_scale != "" && !is.na(multibar_scale)) {
    scale_values <- trimws(unlist(strsplit(multibar_scale, ",")))
    scale_line <- paste(scale_values, collapse = "\t")
    content <- c(content, paste("DATASET_SCALE", scale_line, sep = "\t"))
    content <- c(content, "")
  }
  
  multibar_layout <- input$multibar_layout
  if(is.null(multibar_layout)) multibar_layout <- "stacked"

  if(multibar_layout == "aligned") {
    content <- c(content, paste("ALIGN_FIELDS", "1", sep = "\t"))
  } else if(multibar_layout == "side_stacked") {
    content <- c(content, paste("ALIGN_FIELDS", "0", sep = "\t"))
    content <- c(content, paste("SIDE_STACKED", "1", sep = "\t"))
  } else {  # stacked (default)
    content <- c(content, paste("ALIGN_FIELDS", "0", sep = "\t"))
  }
  content <- c(content, "")
  content <- c(content, paste("SHOW_LABELS", "0", sep = "\t"))
  
  # Value display settings
  if(multibar_show_value) {
    content <- c(content, paste("SHOW_VALUE", "1", sep = "\t"))
    content <- c(content, paste("LABEL_POSITION", multibar_label_position, sep = "\t"))
    
    if(multibar_auto_color) {
      content <- c(content, paste("LABEL_AUTO_COLOR", "1", sep = "\t"))
    } else {
      content <- c(content, paste("LABEL_AUTO_COLOR", "0", sep = "\t"))
      content <- c(content, paste("BAR_LABEL_COLOR", multibar_label_color, sep = "\t"))
    }
  } else {
    content <- c(content, paste("SHOW_VALUE", "0", sep = "\t"))
  }
  
  content <- c(content, "")
  content <- c(content, "DATA")
  

  # Data: ID followed by multiple numeric values
  multibar_na_to_zero <- input$multibar_na_to_zero
  if(is.null(multibar_na_to_zero)) multibar_na_to_zero <- TRUE
  
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
      content <- c(content, paste(c(id, values), collapse = "\t"))
    } else {
      # Only include row if at least one value is non-NA
      if(any(!is.na(values))) {
        values[is.na(values)] <- ""
        content <- c(content, paste(c(id, values), collapse = "\t"))
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
      
  # Metadata download
  output$download_metadata <- downloadHandler(
    filename = function() {
      "metadata.txt"
    },
    content = function(file) {
      writeLines(metadata_output(), file)
    }
  )
  
  # Labels download
  output$download_labels <- downloadHandler(
    filename = function() {
      "labels.txt"
    },
    content = function(file) {
      writeLines(labels_output(), file)
    }
  )
}