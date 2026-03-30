# app.R
library(shiny)
library(readr)
library(readxl)
library(DT)
library(dplyr)
library(scales)
library(digest)
library(colourpicker)
library(RColorBrewer)

# ---------- Helpers ----------
safe_id <- function(x) {
  paste0("id_", digest(x))
}

symbol_names <- c(
  "square"=1,
  "circle"=2,
  "star"=3,
  "triangle_right"=4,
  "triangle_left"=5,
  "checkmark"=6
)

# Standardize NA-like values to "Unknown"
standardize_value <- function(x) {
  if (is.na(x) || is.null(x)) return("Unknown")
  x_char <- as.character(x)
  if (x_char == "NA" || x_char == "N/A" || x_char == "unknown" || x_char == "" || grepl("^\\s+$", x_char)) {
    return("Unknown")
  }
  return(x_char)
}

# Get ColorBrewer palettes organized by type
get_brewer_palettes <- function() {
  list(
    "Sequential" = c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", 
                     "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", 
                     "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
    "Qualitative" = c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
    "Diverging" = c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral")
  )
}

# ---------- UI ----------
ui <- fluidPage(
  titlePanel("iTOL SYMBOL & METADATA Generator"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload metadata",
                accept = c(".tsv",".csv",".txt",".xlsx")),

      uiOutput("column_ui"),
      
      textInput("dataset_label", "Dataset label", value = "My Dataset"),
      
      radioButtons("output_type", "Output type",
                   choices = c("SYMBOL", "METADATA", "CHANGE_LABEL"), 
                   selected = "SYMBOL"),

      # SYMBOL-specific controls
      conditionalPanel(
        condition = "input.output_type == 'SYMBOL'",
        numericInput("max_size","Symbol size", 5, min=1)
      ),

      uiOutput("download_ui")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", 
                 DTOutput("table")),
        tabPanel("Symbol Settings",
         conditionalPanel(
           condition = "input.output_type == 'SYMBOL'",
           uiOutput("column_settings_ui")
         ),
         conditionalPanel(
           condition = "input.output_type == 'METADATA'",
           p("METADATA output will use raw values from selected columns")
         ),
         conditionalPanel(
           condition = "input.output_type == 'CHANGE_LABEL'",
           p("CHANGE_LABEL will replace current labels with new labels from selected columns")
         )
        ),
        tabPanel("ColorBrewer Palettes",
                 plotOutput("brewer_plot", height = "800px")),
        tabPanel("Output Preview",
                 uiOutput("preview_ui"))
      )
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session){

  # ---- Load data ----
  data <- reactive({
    req(input$file)
    file <- input$file$datapath
    ext <- tools::file_ext(input$file$name)

    if(ext %in% c("tsv","txt")) read_tsv(file, show_col_types = FALSE)
    else if(ext=="csv") read_csv(file, show_col_types = FALSE)
    else if(ext=="xlsx") read_excel(file)
    else stop("Unsupported format")
  })

  # ---- Table ----
  output$table <- renderDT({
    req(data())
    datatable(data(), options=list(scrollX=TRUE))
  })

  # ---- Column selection ----
  output$column_ui <- renderUI({
    req(data())
    cols <- names(data())

    tagList(
      # Show different inputs based on output type
      conditionalPanel(
        condition = "input.output_type != 'CHANGE_LABEL'",
        selectInput("id_col","ID column", choices=cols),
        selectizeInput("data_cols","Columns to visualize",
                       choices=cols, multiple=TRUE)
      ),
      conditionalPanel(
        condition = "input.output_type == 'CHANGE_LABEL'",
        selectInput("old_label_col","Current label column", choices=cols),
        selectInput("new_label_col","New label column", choices=cols)
      )
    )
  })

  # ---- Display all ColorBrewer palettes ----
  output$brewer_plot <- renderPlot({
    display.brewer.all()
  })

  # ---- Per-column settings UI ----
  output$column_settings_ui <- renderUI({
    req(input$data_cols)
    
    # Use isolate to prevent reactive loop
    df <- isolate(data())
    
    brewer_pals <- get_brewer_palettes()
    
    # Build palette choices
    palette_choices <- list()
    palette_choices[["--- Sequential ---"]] <- ""
    for(pal in brewer_pals$Sequential) {
      palette_choices[[pal]] <- pal
    }
    palette_choices[["--- Qualitative ---"]] <- ""
    for(pal in brewer_pals$Qualitative) {
      palette_choices[[pal]] <- pal
    }
    palette_choices[["--- Diverging ---"]] <- ""
    for(pal in brewer_pals$Diverging) {
      palette_choices[[pal]] <- pal
    }

    lapply(input$data_cols, function(col) {
      # Get unique values for this column
      col_values <- unique(sapply(as.character(df[[col]]), standardize_value))
      
      # Use isolate to get current settings without creating reactive dependencies
      current_color_mode <- isolate(input[[paste0("color_mode_", col)]])
      current_brewer_pal <- isolate(input[[paste0("brewer_palette_", col)]])
      current_symbol_mode <- isolate(input[[paste0("symbol_mode_", col)]])
      current_auto_symbol <- isolate(input[[paste0("auto_symbol_", col)]])
      
      if(is.null(current_color_mode)) current_color_mode <- "Auto (Hue)"
      if(is.null(current_brewer_pal)) current_brewer_pal <- "Set1"
      if(is.null(current_symbol_mode)) current_symbol_mode <- "Auto"
      if(is.null(current_auto_symbol)) current_auto_symbol <- 1
      
      tags$div(
        style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
        tags$h4(col),
        
        # Color mode for this column
        radioButtons(
          paste0("color_mode_", col),
          "Color mode",
          choices = c("Auto (Hue)", "ColorBrewer", "Manual"),
          selected = current_color_mode,
          inline = TRUE
        ),
        
        # ColorBrewer palette selector
        conditionalPanel(
          condition = sprintf("input['color_mode_%s'] == 'ColorBrewer'", col),
          selectInput(
            paste0("brewer_palette_", col),
            "ColorBrewer palette",
            choices = palette_choices,
            selected = current_brewer_pal
          )
        ),
        
        # Symbol mode for this column
        radioButtons(
          paste0("symbol_mode_", col),
          "Symbol mode",
          choices = c("Auto", "Manual"),
          selected = current_symbol_mode,
          inline = TRUE
        ),
        
        # Auto symbol selector (applies to all values)
        conditionalPanel(
          condition = sprintf("input['symbol_mode_%s'] == 'Auto'", col),
          selectInput(
            paste0("auto_symbol_", col),
            "Symbol for all values",
            choices = symbol_names,
            selected = current_auto_symbol,
            width = "200px"
          )
        ),
        
        # Value-specific controls (Manual mode for both colors and symbols)
        conditionalPanel(
          condition = sprintf("input['symbol_mode_%s'] == 'Manual' || input['color_mode_%s'] == 'Manual'", col, col),
          tags$div(
            style = "margin-top: 10px;",
            lapply(col_values, function(val) {
              id <- paste0(col, "_", safe_id(val))
              
              # Get current values - preserve them
              color_input_id <- paste0("color_", id)
              symbol_input_id <- paste0("symbol_", id)
              
              current_col <- isolate(input[[color_input_id]])
              if(is.null(current_col)) {
                current_col <- "#808080"
              }
              
              current_sym <- isolate(input[[symbol_input_id]])
              if(is.null(current_sym)) current_sym <- 1
              
              tags$div(
                style = "margin-bottom: 8px; padding: 5px;",
                tags$span(
                  style = paste0("display:inline-block;width:15px;height:15px;background:", current_col, ";margin-right:5px;border:1px solid black;")
                ),
                tags$b(val),
                
                # Color picker - shown only in Manual color mode
                conditionalPanel(
                  condition = sprintf("input['color_mode_%s'] == 'Manual'", col),
                  colourInput(
                    inputId = color_input_id,
                    label = NULL,
                    value = current_col,
                    showColour = "both"
                  )
                ),
                
                # Symbol picker - shown only in Manual symbol mode
                conditionalPanel(
                  condition = sprintf("input['symbol_mode_%s'] == 'Manual'", col),
                  selectInput(
                    inputId = symbol_input_id,
                    label = NULL,
                    choices = symbol_names,
                    selected = current_sym,
                    width = "150px"
                  )
                )
              )
            })
          )
        )
      )
    })
  })

    # ---- Mapping per column ----
  mapping_for_column <- function(col) {
    req(data())
    df <- data()
    
    col_values <- unique(sapply(as.character(df[[col]]), standardize_value))
    
    color_mode <- input[[paste0("color_mode_", col)]]
    symbol_mode <- input[[paste0("symbol_mode_", col)]]
    
    if(is.null(color_mode)) color_mode <- "Auto (Hue)"
    if(is.null(symbol_mode)) symbol_mode <- "Auto"
    
    # Colors
    if(color_mode == "Auto (Hue)") {
      pal <- hue_pal()(length(col_values))
      names(pal) <- col_values
    } else if(color_mode == "ColorBrewer") {
      brewer_pal_name <- input[[paste0("brewer_palette_", col)]]
      if(is.null(brewer_pal_name) || brewer_pal_name == "") brewer_pal_name <- "Set1"
      
      # Get max colors available for this palette
      max_colors <- brewer.pal.info[brewer_pal_name, "maxcolors"]
      n_colors <- min(length(col_values), max_colors)
      
      if(length(col_values) <= max_colors) {
        pal <- brewer.pal(max(3, length(col_values)), brewer_pal_name)[1:length(col_values)]
      } else {
        # If more values than colors, use colorRampPalette to interpolate
        pal <- colorRampPalette(brewer.pal(max_colors, brewer_pal_name))(length(col_values))
      }
      names(pal) <- col_values
    }
    
    colors <- sapply(col_values, function(val) {
      if(color_mode %in% c("Auto (Hue)", "ColorBrewer")) {
        pal[val]
      } else {
        id <- paste0(col, "_", safe_id(val))
        col_val <- input[[paste0("color_", id)]]
        if(is.null(col_val)) "#808080" else col_val
      }
    })
    
    # Symbols
    symbols <- sapply(col_values, function(val) {
      if(symbol_mode == "Auto") {
        # Use the selected auto symbol for all values
        auto_symbol <- input[[paste0("auto_symbol_", col)]]
        if(is.null(auto_symbol)) 1 else as.numeric(auto_symbol)
      } else {
        id <- paste0(col, "_", safe_id(val))
        sym <- input[[paste0("symbol_", id)]]
        if(is.null(sym)) 1 else as.numeric(sym)
      }
    })
    
    data.frame(
      value = col_values,
      color = colors,
      symbol = symbols,
      stringsAsFactors = FALSE
    )
  }

  # ---- Generate output content per column ----
  output_content_list <- reactive({
    df <- data()
  
    if(input$output_type == "CHANGE_LABEL") {
      req(input$old_label_col, input$new_label_col)
    
      lines <- c(
        "LABELS",
        "#use this template to change the leaf labels, or define/change the internal node names",
        "",
        "SEPARATOR TAB",
        "",
        "DATA"
      )
    
      for(i in seq_len(nrow(df))) {
        old_label <- as.character(df[[input$old_label_col]][i])
        new_label <- as.character(df[[input$new_label_col]][i])
        lines <- c(lines, paste(old_label, new_label, sep = "\t"))
      }
    
      list(LABELS = lines)
    
    } else if(input$output_type == "METADATA") {
      # Single METADATA file
      req(input$data_cols, input$id_col)
      field_labels <- paste(input$data_cols, collapse = ",")
      
      lines <- c(
        "METADATA",
        "#use this template to set or update the metadata associated with tree nodes. Metadata values can be numeric or textual",
        "",
        "SEPARATOR COMMA",
        "",
        paste0("FIELD_LABELS,", field_labels),
        "",
        "DATA"
      )
      
      for(i in seq_len(nrow(df))) {
        row_data <- c(
          as.character(df[[input$id_col]][i]),
          sapply(input$data_cols, function(col) standardize_value(df[[col]][i]))
        )
        lines <- c(lines, paste(row_data, collapse = ","))
      }
      
      list(METADATA = lines)
      
    } else {
      # Separate SYMBOL file per column
      result <- list()
      
      for(col_i in seq_along(input$data_cols)) {
        col <- input$data_cols[col_i]
        map <- mapping_for_column(col)
        lines <- c()
        
        position <- -1
        
        for(i in seq_len(nrow(df))) {
          val <- standardize_value(df[[col]][i])
          idx <- match(val, map$value)
          
          sym <- map$symbol[idx]
          colr <- map$color[idx]
          
          lines <- c(lines,
            paste(
              df[[input$id_col]][i],
              sym,
              input$max_size,
              colr,
              1,
              position,
              val,
              sep = ","
            )
          )
        }
        
        # Header
        header <- c(
          "DATASET_SYMBOL",
          "",
          "SEPARATOR COMMA",
          "",
          paste0("DATASET_LABEL,", input$dataset_label, " - ", col),
          "",
          "COLOR,#000000",
          "",
          paste0("LEGEND_TITLE,", col),
          paste0("LEGEND_SHAPES,", paste(map$symbol, collapse = ",")),
          paste0("LEGEND_COLORS,", paste(map$color, collapse = ",")),
          paste0("LEGEND_LABELS,", paste(map$value, collapse = ",")),
          "",
          paste0("MAXIMUM_SIZE,", input$max_size),
          "",
          "DATA",
          "ID,symbol,size,color,fill,position,label"
        )
        
        result[[col]] <- c(header, lines)
      }
      
      result
    }
  })

  # ---- Preview UI ----
  output$preview_ui <- renderUI({
    req(output_content_list())
    
    content_list <- output_content_list()
    
    tab_list <- lapply(names(content_list), function(name) {
      tabPanel(
        name,
        verbatimTextOutput(paste0("preview_", safe_id(name)))
      )
    })
    
    do.call(tabsetPanel, tab_list)
  })
  
  # ---- Preview outputs (dynamic) ----
  observe({
    req(output_content_list())
    content_list <- output_content_list()
    
    lapply(names(content_list), function(name) {
      output_id <- paste0("preview_", safe_id(name))
      
      output[[output_id]] <- renderPrint({
        cat(paste(content_list[[name]], collapse = "\n"))
      })
    })
  })

  # ---- Download UI ----
  output$download_ui <- renderUI({
    req(output_content_list())
    content_list <- output_content_list()
    
    if(length(content_list) == 1) {
      downloadButton("download_single", "Download File")
    } else {
      tagList(
        tags$h5("Download Files:"),
        lapply(names(content_list), function(name) {
          tags$div(
            style = "margin-bottom: 5px;",
            downloadButton(
              paste0("download_", safe_id(name)),
              label = if(input$output_type == "METADATA") {
                "Download metadata.txt"
              } else if(input$output_type == "CHANGE_LABEL") {
                "Download labels.txt"
              } else {
                paste0("Download ", name, ".txt")
              },
              style = "width: 100%;"
            )
          )
        })
      )
    }
  })
  
  # ---- Individual download handlers ----
  observe({
    req(output_content_list())
    content_list <- output_content_list()
    
    lapply(names(content_list), function(name) {
      output[[paste0("download_", safe_id(name))]] <- downloadHandler(
        filename = function() {
          if(input$output_type == "METADATA") {
            "metadata.txt"
          } else if(input$output_type == "CHANGE_LABEL") {
            "labels.txt"
          } else {
            paste0("symbol_", name, ".txt")
          }
        },
        content = function(file) {
          writeLines(content_list[[name]], file)
        }
      )
    })
    
    output$download_single <- downloadHandler(
      filename = function() {
        name <- names(content_list)[1]
        if(input$output_type == "METADATA") {
          "metadata.txt"
        } else if(input$output_type == "CHANGE_LABEL") {
          "labels.txt"
        } else {
          paste0("symbol_", name, ".txt")
        }
      },
      content = function(file) {
        writeLines(content_list[[names(content_list)[1]]], file)
      }
    )
  })
}

# ---------- Run app ----------
shinyApp(ui, server)