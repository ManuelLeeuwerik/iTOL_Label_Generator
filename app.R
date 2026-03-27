# app.R
library(shiny)
library(readr)
library(readxl)
library(DT)
library(dplyr)
library(scales)
library(digest)
library(colourpicker)

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
  if (x_char == "NA" || x_char == "" || grepl("^\\s+$", x_char)) {
    return("Unknown")
  }
  return(x_char)
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
                   choices = c("SYMBOL", "METADATA"), 
                   selected = "SYMBOL"),

      # SYMBOL-specific controls
      conditionalPanel(
        condition = "input.output_type == 'SYMBOL'",
        numericInput("max_size","Symbol size",10, min=1)
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
                 )
        ),
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
      selectInput("id_col","ID column", choices=cols),
      selectizeInput("data_cols","Columns to visualize",
                     choices=cols, multiple=TRUE)
    )
  })

  # ---- Per-column settings UI ----
  output$column_settings_ui <- renderUI({
    req(input$data_cols)
    df <- data()

    lapply(input$data_cols, function(col) {
      # Get unique values for this column
      col_values <- unique(sapply(as.character(df[[col]]), standardize_value))
      
      tags$div(
        style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
        tags$h4(col),
        
        # Color mode for this column
        radioButtons(
          paste0("color_mode_", col),
          "Color mode",
          choices = c("Auto", "Manual"),
          selected = "Auto",
          inline = TRUE
        ),
        
        # Symbol mode for this column
        radioButtons(
          paste0("symbol_mode_", col),
          "Symbol mode",
          choices = c("Auto", "Manual"),
          selected = "Auto",
          inline = TRUE
        ),
        
        # Value-specific controls
        tags$div(
          style = "margin-top: 10px;",
          lapply(col_values, function(val) {
            id <- paste0(col, "_", safe_id(val))
            
            # Get current values
            color_input_id <- paste0("color_", id)
            symbol_input_id <- paste0("symbol_", id)
            
            current_col <- isolate(input[[color_input_id]])
            if(val == "Unknown") {
              current_col <- "#808080"
            } else if(is.null(current_col)) {
              current_col <- "#808080"
            }
            
            current_sym <- isolate(input[[symbol_input_id]])
            if(is.null(current_sym)) current_sym <- 2
            
            # Color picker
            color_input <- conditionalPanel(
              condition = sprintf("input['color_mode_%s'] == 'Manual'", col),
              if(val == "Unknown") {
                tags$span(style="color:#666;font-size:0.9em;margin-left:10px;", "(Fixed: Grey)")
              } else {
                colourInput(
                  inputId = color_input_id,
                  label = NULL,
                  value = current_col,
                  showColour = "both"
                )
              }
            )
            
            # Symbol picker
            symbol_input <- conditionalPanel(
              condition = sprintf("input['symbol_mode_%s'] == 'Manual'", col),
              selectInput(
                inputId = symbol_input_id,
                label = NULL,
                choices = symbol_names,
                selected = current_sym,
                width = "150px"
              )
            )
            
            tags$div(
              style = "margin-bottom: 8px; padding: 5px;",
              tags$span(
                style = paste0("display:inline-block;width:15px;height:15px;background:", current_col, ";margin-right:5px;border:1px solid black;")
              ),
              tags$b(val),
              color_input,
              symbol_input
            )
          })
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
    
    if(is.null(color_mode)) color_mode <- "Auto"
    if(is.null(symbol_mode)) symbol_mode <- "Auto"
    
    # Colors
    if(color_mode == "Auto") {
      pal <- hue_pal()(length(col_values))
      names(pal) <- col_values
    }
    
    colors <- sapply(col_values, function(val) {
      if(val == "Unknown") {
        return("#808080")
      }
      
      if(color_mode == "Auto") {
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
        2
      } else {
        id <- paste0(col, "_", safe_id(val))
        sym <- input[[paste0("symbol_", id)]]
        if(is.null(sym)) 2 else as.numeric(sym)
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
    req(input$data_cols, input$id_col)
    
    df <- data()
    
    if(input$output_type == "METADATA") {
      # Single METADATA file
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
        
        position <- -1  # Each file has its own position
        
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
    
    # Create tabs for each file
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

  # ---- Download UI (replaces single download button in sidebar) ----
  output$download_ui <- renderUI({
    req(output_content_list())
    content_list <- output_content_list()
    
    if(length(content_list) == 1) {
      # Single file - one download button
      downloadButton("download_single", "Download File")
    } else {
      # Multiple files - button for each
      tagList(
        tags$h5("Download Files:"),
        lapply(names(content_list), function(name) {
          tags$div(
            style = "margin-bottom: 5px;",
            downloadButton(
              paste0("download_", safe_id(name)),
              label = if(input$output_type == "METADATA") {
                "Download metadata.txt"
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
    
    # Create handler for each file
    lapply(names(content_list), function(name) {
      output[[paste0("download_", safe_id(name))]] <- downloadHandler(
        filename = function() {
          if(input$output_type == "METADATA") {
            "metadata.txt"
          } else {
            paste0("symbol_", name, ".txt")
          }
        },
        content = function(file) {
          writeLines(content_list[[name]], file)
        }
      )
    })
    
    # Single file handler
    output$download_single <- downloadHandler(
      filename = function() {
        name <- names(content_list)[1]
        if(input$output_type == "METADATA") {
          "metadata.txt"
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

