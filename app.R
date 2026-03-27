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
  # Check for NA, empty string, or whitespace-only string
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
        radioButtons("color_mode","Color mode",
                     choices=c("Auto","Manual"), selected="Auto"),

        radioButtons("symbol_mode","Symbol mode",
                     choices=c("Auto","Manual"), selected="Auto"),

        numericInput("max_size","Max size",10, min=1)
      ),

      downloadButton("download","Download iTOL")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", 
                 DTOutput("table")),
        tabPanel("Symbol Settings",
                 conditionalPanel(
                   condition = "input.output_type == 'SYMBOL'",
                   uiOutput("value_ui")
                 ),
                 conditionalPanel(
                   condition = "input.output_type == 'METADATA'",
                   p("METADATA output will use raw values from selected columns")
                 )
        ),
        tabPanel("Output Preview",
                 verbatimTextOutput("preview"))
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

  # ---- Value UI (manual controls) ----
  output$value_ui <- renderUI({
    req(input$data_cols)
    df <- data()

    # Standardize all values
    values <- unique(sapply(as.character(unlist(df[input$data_cols])), standardize_value))

    tagList(lapply(values, function(val){

      id <- safe_id(val)

      # Preserve color, but force grey for Unknown
      current_col <- isolate(input[[paste0("color_", id)]])
      if(val == "Unknown") {
        current_col <- "#808080"
      } else if(is.null(current_col)) {
        current_col <- "#808080"
      }

      # Preserve symbol
      current_sym <- isolate(input[[paste0("symbol_", id)]])
      if(is.null(current_sym)) current_sym <- 2

      # Disable color picker for Unknown
      color_input <- if(input$color_mode=="Manual"){
        if(val == "Unknown") {
          tags$span(style="color:#666;font-size:0.9em;", "(Fixed: Grey)")
        } else {
          colourInput(
            inputId = paste0("color_", id),
            label = NULL,
            value = current_col,
            showColour = "both"
          )
        }
      }

      symbol_input <- if(input$symbol_mode=="Manual"){
        selectInput(
          inputId = paste0("symbol_", id),
          label = NULL,
          choices = symbol_names,
          selected = current_sym,
          width = "150px"
        )
      }

      tags$div(
        style="margin-bottom:8px;",
        tags$span(
          style = paste0("display:inline-block;width:15px;height:15px;background:", current_col, ";margin-right:5px;border:1px solid black;")
        ),
        tags$b(val),
        tags$br(),
        color_input,
        symbol_input
      )
    }))
  })

  # ---- Mapping ----
  mapping <- reactive({
    req(input$data_cols)
    df <- data()

    # Standardize all values
    values <- unique(sapply(as.character(unlist(df[input$data_cols])), standardize_value))

    # Stable palette
    if(input$color_mode=="Auto"){
      pal <- hue_pal()(length(values))
      names(pal) <- values
    }

    colors <- sapply(values, function(val){
      id <- safe_id(val)

      # Always use grey for Unknown
      if(val == "Unknown") {
        return("#808080")
      }

      if(input$color_mode=="Auto"){
        pal[val]
      } else {
        col <- input[[paste0("color_", id)]]
        if(is.null(col)) "#808080" else col
      }
    })

    symbols <- sapply(values, function(val){
      id <- safe_id(val)

      if(input$symbol_mode=="Auto"){
        2
      } else {
        sym <- input[[paste0("symbol_", id)]]
        if(is.null(sym)) 2 else as.numeric(sym)
      }
    })

    data.frame(
      value = values,
      color = colors,
      symbol = symbols,
      stringsAsFactors = FALSE
    )
  })

  # ---- Generate output content ----
  output_content <- reactive({
    req(input$data_cols, input$id_col)
    
    df <- data()
    
    if(input$output_type == "METADATA") {
      # METADATA format with all mandatory headers
      field_labels <- paste(input$data_cols, collapse = ",")
      
      lines <- c(
        "METADATA",
        "#use this template to set or update the metadata associated with tree nodes. Metadata values can be numeric or textual",
        "",
        "#lines starting with a hash are comments and ignored during parsing",
        "",
        "#=================================================================#",
        "#                    MANDATORY SETTINGS                           #",
        "#=================================================================#",
        "#select the separator which is used to delimit the data below (TAB,SPACE or COMMA).This separator must be used throughout this file (except in the SEPARATOR line, which uses space).",
        "",
        "#SEPARATOR TAB",
        "#SEPARATOR SPACE",
        "SEPARATOR COMMA",
        "",
        "#define the metadata field names. Field names can be any text string. If 'bootstrap' is specified, the original bootstrap values in the tree will be overwritten (and must be present)",
        "",
        paste0("FIELD_LABELS,", field_labels),
        "",
        "#Internal tree nodes can be specified by using IDs directly, or through the 'last common ancestor' method described in iTOL help pages",
        "#=================================================================#",
        "#       Actual data follows after the \"DATA\" keyword              #",
        "#=================================================================#",
        "DATA",
        "#NODE_ID,FIELD1_METADATA_VALUE,FIELD2_METADATA_VALUE...."
      )
      
      # Add data rows with standardized values
      for(i in seq_len(nrow(df))) {
        row_data <- c(
          as.character(df[[input$id_col]][i]),
          sapply(input$data_cols, function(col) standardize_value(df[[col]][i]))
        )
        lines <- c(lines, paste(row_data, collapse = ","))
      }
      
      return(lines)
      
    } else {
      # SYMBOL format
      map <- mapping()
      lines <- c()

      for(col_i in seq_along(input$data_cols)){
        col <- input$data_cols[col_i]
        position <- -col_i

        for(i in seq_len(nrow(df))){
          # Standardize the value
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
              sep=","
            )
          )
        }
      }

      # Header
      header <- c(
        "DATASET_SYMBOL",
        "",
        "SEPARATOR COMMA",
        "",
        paste0("DATASET_LABEL,", input$dataset_label),
        "",
        "COLOR,#000000",
        "",
        "LEGEND_TITLE,Legend",
        paste0("LEGEND_SHAPES,", paste(map$symbol, collapse=",")),
        paste0("LEGEND_COLORS,", paste(map$color, collapse=",")),
        paste0("LEGEND_LABELS,", paste(map$value, collapse=",")),
        "",
        paste0("MAXIMUM_SIZE,", input$max_size),
        "",
        "DATA",
        "ID,symbol,size,color,fill,position,label"
      )

      return(c(header, lines))
    }
  })

  # ---- Preview ----
  output$preview <- renderText({
    req(output_content())
    paste(output_content(), collapse = "\n")
  })

  # ---- Download ----
  output$download <- downloadHandler(
    filename = function(){
      type <- if(input$output_type == "METADATA") "METADATA" else "SYMBOL"
      paste0("itol_", type, "_", Sys.Date(), ".txt")
    },
    content = function(file){
      writeLines(output_content(), file)
    }
  )
}

# ---------- RUN ----------
shinyApp(ui, server)