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
  "checkmark"=6,
  "solid_line"=7,
  "dashed_line"=8,
  "dotted_line"=9,
  "arrow_left"=10,
  "arrow_right"=11,
  "bidirectional_arrow"=12
)

# ---------- UI ----------
ui <- fluidPage(
  titlePanel("iTOL SYMBOL Generator"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload metadata",
                accept = c(".tsv",".csv",".txt",".xlsx")),

      uiOutput("column_ui"),

      radioButtons("color_mode","Color mode",
                   choices=c("Auto","Manual"), selected="Auto"),

      radioButtons("symbol_mode","Symbol mode",
                   choices=c("Auto","Manual"), selected="Auto"),

      numericInput("max_size","Max size",10, min=1),

      downloadButton("download","Download iTOL")
    ),

    mainPanel(
      DTOutput("table"),
      hr(),
      uiOutput("value_ui")
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

    values <- unique(as.character(unlist(df[input$data_cols])))

    tagList(lapply(values, function(val){

      id <- safe_id(val)

      # Preserve color
      current_col <- isolate(input[[paste0("color_", id)]])
      if(is.null(current_col)) current_col <- "#808080"

      # Preserve symbol
      current_sym <- isolate(input[[paste0("symbol_", id)]])
      if(is.null(current_sym)) current_sym <- 2

      color_input <- if(input$color_mode=="Manual"){
        colourInput(
          inputId = paste0("color_", id),
          label = NULL,
          value = current_col,
          showColour = "both"
        )
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

    values <- unique(as.character(unlist(df[input$data_cols])))

    # Stable palette
    if(input$color_mode=="Auto"){
      pal <- hue_pal()(length(values))
      names(pal) <- values
    }

    colors <- sapply(values, function(val){
      id <- safe_id(val)

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

  # ---- Download ----
  output$download <- downloadHandler(
    filename = function(){
      paste0("itol_SYMBOL_", Sys.Date(), ".txt")
    },
    content = function(file){

      df <- data()
      map <- mapping()

      lines <- c()

      for(col_i in seq_along(input$data_cols)){
        col <- input$data_cols[col_i]
        position <- -col_i

        for(i in seq_len(nrow(df))){
          val <- as.character(df[[col]][i])
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

      # ---- Header (STRICT iTOL FORMAT) ----
      header <- c(
        "DATASET_SYMBOL",
        "",
        "SEPARATOR COMMA",
        "",
        "DATASET_LABEL,Generated",
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

      writeLines(c(header, lines), file)
    }
  )
}

# ---------- RUN ----------
shinyApp(ui, server)