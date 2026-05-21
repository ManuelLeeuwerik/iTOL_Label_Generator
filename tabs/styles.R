 # ---- Label column selection UI ----
  output$label_column_selection <- renderUI({
    req(data())
    req(input$data_cols)
    cols <- names(data())
    
    tagList(
      selectInput(
        "old_label_col",
        "ID Column",
        choices = cols,
        selected = input$id_col,
        width = "200px"
      ),
      div(class = "help-text",
          "Column containing the labels currently/originally in your tree"),
      
      tags$br(),
      
      selectInput(
        "new_label_col",
        "New Tip Label Column",
        choices = cols,
        selected = if(length(cols) > 1) cols[2] else cols[1],
        width = "200px"
      ),
      div(class = "help-text",
          "Column containing the new tip labels to use")
    )
  })

  # ---- Style tab: Column settings UI ----
  output$style_column_settings_ui <- renderUI({
    req(input$data_cols)
    
    df <- isolate(data())
    
    # Create accordion for each column
    accordion_items <- lapply(seq_along(input$data_cols), function(idx) {
      col <- input$data_cols[idx]
      col_values <- unique(sapply(as.character(df[[col]]), standardize_value))
      col_values <- col_values[!is.na(col_values)]
      
      accordion_panel(
        title = col,
        value = paste0("style_panel_", idx),
        
        div(
          class = "info-box",
          style = "margin-bottom: 1rem; font-size: 0.85rem;",
          p(
            icon("info-circle"),
            sprintf("Configure label styles for %d unique values", length(col_values))
          )
        ),
        
        # Manual configuration for each value
        tags$h6("Configure Label Styles"),
        lapply(col_values, function(val) {
          val_id <- safe_id(paste(col, val, sep = "_"))
          
          current_color <- isolate(input[[paste0("style_color_", val_id)]])
          current_style <- isolate(input[[paste0("style_font_", val_id)]])
          current_bg <- isolate(input[[paste0("style_bg_", val_id)]])
          current_size <- isolate(input[[paste0("style_size_", val_id)]])
          
          if(is.null(current_color)) current_color <- "#0000ff"
          if(is.null(current_style)) current_style <- "normal"
          if(is.null(current_bg)) current_bg <- "#FFFFFF00"
          if(is.null(current_size)) current_size <- 1
          
          div(
            style = "border: 1px solid #dee2e6; padding: 1rem; margin-bottom: 1rem; border-radius: 0.25rem; background-color: #f8f9fa;",
            tags$h6(style = "color: #2C5F8D; margin-top: 0;", val),
            
            div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 1rem; margin-bottom: 0.5rem;",
              
              div(
                colourInput(
                  paste0("style_color_", val_id),
                  "Label Color",
                  value = current_color,
                  showColour = "both",
                  palette = "square",
                  returnName = FALSE
                )
              ),
              
              div(
                selectInput(
                  paste0("style_font_", val_id),
                  "Font Style",
                  choices = c(
                    "Normal" = "normal",
                    "Bold" = "bold",
                    "Italic" = "italic",
                    "Bold Italic" = "bold-italic"
                  ),
                  selected = current_style,
                  width = "200px"
                )
              )
            ),
            
            div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 1rem;",
              
              div(
                colourInput(
                  paste0("style_bg_", val_id),
                  "Background Color (optional)",
                  value = current_bg,
                  showColour = "both",
                  palette = "square",
                  returnName = FALSE,
                  allowTransparent = TRUE
                ),
                div(class = "help-text", "Leave transparent for no background")
              ),
              
              div(
                numericInput(
                  paste0("style_size_", val_id),
                  "Size Factor",
                  value = current_size,
                  min = 0.1,
                  max = 5,
                  step = 0.1,
                  width = "200px"
                ),
                div(class = "help-text", "Relative to global font size")
              )
            )
          )
        }),
        
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
                paste0("style_legend_title_", col),
                "Legend Title",
                value = isolate(input[[paste0("style_legend_title_", col)]]) %||% col,
                width = "250px"
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("style_legend_visible_", col),
                "Show legend initially",
                value = isolate(input[[paste0("style_legend_visible_", col)]]) %||% TRUE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("style_legend_horizontal_", col),
                "Horizontal legend layout",
                value = isolate(input[[paste0("style_legend_horizontal_", col)]]) %||% FALSE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("style_legend_scale_", col),
                "Legend Scale Factor",
                value = isolate(input[[paste0("style_legend_scale_", col)]]) %||% 1,
                min = 0.1,
                max = 5,
                step = 0.1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Scale factor for legend symbols")
            )
          )
        )
      )
    })
    
    accordion(
      id = "style_accordion",
      multiple = TRUE,
      !!!accordion_items
    )
  })

  # ---- Generate style outputs ----
  style_outputs <- reactive({
    req(data(), input$id_col, input$data_cols)
    
    df <- data()
    output_list <- list()
    
    for(col in input$data_cols) {
      col_values <- unique(sapply(as.character(df[[col]]), standardize_value))
      col_values <- col_values[!is.na(col_values)]
      
      if(length(col_values) == 0) next
      
      # Build iTOL DATASET_STYLE format
      content <- c("DATASET_STYLE")
      content <- c(content, "SEPARATOR COMMA")
      content <- c(content, paste("DATASET_LABEL", paste(input$dataset_label, "-", col, "style"), sep = ","))
      content <- c(content, paste("COLOR", "#0000ff", sep = ","))
      content <- c(content, "")
      
      # Get legend colors
      legend_colors <- sapply(col_values, function(val) {
        val_id <- safe_id(paste(col, val, sep = "_"))
        input[[paste0("style_color_", val_id)]] %||% "#0000ff"
      })
      
      # Get advanced legend settings
      style_legend_title <- input[[paste0("style_legend_title_", col)]] %||% col
      style_legend_visible <- input[[paste0("style_legend_visible_", col)]] %||% TRUE
      style_legend_horizontal <- input[[paste0("style_legend_horizontal_", col)]] %||% FALSE
      style_legend_scale <- input[[paste0("style_legend_scale_", col)]] %||% 1
      
      # Add legend
      content <- c(content, paste("LEGEND_TITLE", style_legend_title, sep = ","))
      content <- c(content, paste("LEGEND_SCALE", style_legend_scale, sep = ","))
      content <- c(content, paste("LEGEND_VISIBLE", if(style_legend_visible) "1" else "0", sep = ","))
      if(style_legend_horizontal) {
        content <- c(content, paste("LEGEND_HORIZONTAL", "1", sep = ","))
      }
      content <- c(content, paste("LEGEND_SHAPES", paste(rep("1", length(col_values)), collapse = ","), sep = ","))
      content <- c(content, paste("LEGEND_COLORS", paste(legend_colors, collapse = ","), sep = ","))
      content <- c(content, paste("LEGEND_LABELS", paste(col_values, collapse = ","), sep = ","))
      content <- c(content, "")
      
      content <- c(content, "DATA")
      
      # For each row, check if value matches and apply styling
      for(i in 1:nrow(df)) {
        id <- as.character(df[[input$id_col]][i])
        val <- standardize_value(df[[col]][i])
        
        if(!is.na(val) && val %in% col_values) {
          val_id <- safe_id(paste(col, val, sep = "_"))
          
          color <- input[[paste0("style_color_", val_id)]] %||% "#0000ff"
          font_style <- input[[paste0("style_font_", val_id)]] %||% "normal"
          bg_color <- input[[paste0("style_bg_", val_id)]] %||% "#FFFFFF00"
          size_factor <- input[[paste0("style_size_", val_id)]] %||% 1
          
          # Format: ID,TYPE,WHAT,COLOR,WIDTH_OR_SIZE_FACTOR,STYLE,BACKGROUND_COLOR
          line_parts <- c(
            id,
            "label",
            "node",
            color,
            as.character(size_factor),
            font_style
          )
          
          # Add background color only if specified
          if(!is.null(bg_color) && bg_color != "" && bg_color != "transparent") {
            line_parts <- c(line_parts, bg_color)
          }
          
          content <- c(content, paste(line_parts, collapse = ","))
        }
      }
      
      output_list[[col]] <- paste(content, collapse = "\n")
    }
    
    return(output_list)
  })

  # ---- Style download card ----
  output$style_download_card <- renderUI({
    req(style_outputs())
    content_list <- style_outputs()
    
    if(length(content_list) == 0) return(NULL)
    
    card(
      card_header("Download Label Style Annotations"),
      card_body(
        if(length(content_list) == 1) {
          centered_download_button(
            "download_style_single", 
            "Download Style File"
          )
        } else {
          tagList(
            centered_download_button(
              "download_style_zip",
              "Download All Style Files (ZIP)",
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
                  paste0("download_style_", safe_id(name)),
                  paste0(name, "_style.txt"),
                  class = "btn-primary btn-sm"
                )
              )
            })
          )
        }
      )
    )
  })

  # Single style file
  output$download_style_single <- downloadHandler(
    filename = function() {
      paste0(input$dataset_label, "_style.txt")
    },
    content = function(file) {
      content_list <- style_outputs()
      writeLines(content_list[[1]], file)
    }
  )

  # All style files as ZIP
  output$download_style_zip <- downloadHandler(
    filename = function() {
      paste0(input$dataset_label, "_style_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      content_list <- style_outputs()
      temp_dir <- tempdir()
      temp_files <- c()
      
      for(name in names(content_list)) {
        temp_file <- file.path(temp_dir, paste0(name, "_style.txt"))
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

  # Individual style files
  observe({
    req(input$data_cols)
    
    tryCatch({
      content_list <- style_outputs()
      req(content_list)
      
      if(length(content_list) > 1) {
        lapply(names(content_list), function(name) {
          local({
            my_name <- name
            output[[paste0("download_style_", safe_id(my_name))]] <- downloadHandler(
              filename = function() {
                paste0(my_name, "_style.txt")
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