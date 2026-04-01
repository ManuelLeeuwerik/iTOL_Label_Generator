# iTOL Label Generator
# A professional tool for generating iTOL annotation files from metadata

library(shiny)
library(bslib)
library(readr)
library(readxl)
library(DT)
library(dplyr)
library(scales)
library(digest)
library(colourpicker)
library(RColorBrewer)
library(shinyWidgets)
library(zip)

# ---------- Helper Functions ----------

#' Generate safe HTML IDs from column names
safe_id <- function(x) {
  paste0("id_", digest(x))
}

#' Symbol mapping for iTOL
symbol_names <- c(
  "Square" = 1,
  "Circle" = 2,
  "Star" = 3,
  "Triangle Right" = 4,
  "Triangle Left" = 5,
  "Checkmark" = 6
)

#' Standardize NA-like values to NA
standardize_value <- function(x) {
  if (is.na(x) || is.null(x)) return(NA_character_)
  x_char <- as.character(x)
  # Only convert truly empty strings to NA, keep "NA" as literal "NA"
  if (x_char == "" || grepl("^\\s+$", x_char)) {
    return(NA_character_)
  }
  return(x_char)
}

#' Get ColorBrewer palettes organized by type
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

ui <- page_sidebar(
  
  # Theme configuration - using only system fonts
  theme = bs_theme(
    version = 5,
    preset = "flatly",
    primary = "#2C5F8D",
    secondary = "#5A7A9B",
    success = "#2C5F8D",
    info = "#5DADE2",
    warning = "#F39C12",
    danger = "#E74C3C",
    base_font = "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif",
    heading_font = "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif",
    font_scale = 0.95
  ),
  
  # Custom CSS for enhanced academic styling
  tags$head(
    tags$style(HTML("
      body {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
        color: #2c3e50;
        background-color: #e8f1f7;
      }
      
      .bslib-sidebar-layout {
        background-color: #f7fbfd;
      }
      
      .logo-container {
        text-align: center;
        padding: 1.25rem 0 1rem;
        margin-bottom: 1rem;
        border-bottom: 1px solid #dee2e6;
        background-color: #ffffff;
      }
      
      .logo-container img {
        max-width: 200px;
        height: auto;
        margin-bottom: 0.75rem;
      }
      
      .logo-container h4 {
        margin: 0;
        font-weight: 600;
        color: #2C3E50;
        font-size: 1.1rem;
        letter-spacing: 0.5px;
      }
      
      .card {
        border: 1px solid #b8d4e8;
        box-shadow: 0 1px 3px rgba(44, 95, 141, 0.08);
        margin-bottom: 1rem;
        border-radius: 0.375rem;
      }
      
      .card-header {
        background-color: #e8f1f7;
        border-bottom: 1px solid #b8d4e8;
        font-weight: 600;
        font-size: 0.95rem;
        color: #2C5F8D;
        padding: 0.75rem 1rem;
      }
      
      .card-body {
        padding: 1rem;
        background-color: #ffffff;
      }
      
      .info-box {
        background-color: #d6ebf5;
        border-left: 3px solid #2C5F8D;
        padding: 0.875rem 1rem;
        margin: 0 0 1rem 0;
        border-radius: 0.25rem;
        font-size: 0.9rem;
      }
      
      .info-box p {
        margin: 0;
        color: #1e4d73;
        line-height: 1.5;
      }
      
      .value-config {
        display: flex;
        align-items: center;
        gap: 0.75rem;
        padding: 0.625rem;
        margin-bottom: 0.5rem;
        background-color: #f0f7fb;
        border-radius: 0.25rem;
        border-left: 3px solid #5DADE2;
      }
      
      .value-label {
        font-weight: 500;
        min-width: 120px;
        color: #495057;
        font-size: 0.9rem;
      }
      
      .help-text {
        font-size: 0.825rem;
        color: #6c757d;
        margin-top: 0.375rem;
        line-height: 1.4;
        font-style: italic;
      }
      
      .form-label {
        font-weight: 500;
        color: #2c3e50;
        margin-bottom: 0.375rem;
        font-size: 0.9rem;
      }
      
      .form-control, .form-select {
        font-size: 0.9rem;
        border-color: #dee2e6;
      }
      
      .form-control:focus, .form-select:focus {
        border-color: #5DADE2;
        box-shadow: 0 0 0 0.2rem rgba(93, 173, 226, 0.25);
      }
      
      .btn {
        font-weight: 500;
        font-size: 0.9rem;
        padding: 0.5rem 1rem;
        border-radius: 0.25rem;
        transition: all 0.2s ease;
      }
      
      .btn-success {
        background-color: #538edbff;
        border-color: #538edbff;
      }
      
      .btn-success:hover {
        background-color: #4278c4;
        border-color: #4278c4;
      }
      
      .btn-primary {
        background-color: #3498DB;
        border-color: #3498DB;
      }
      
      .btn-primary:hover {
        background-color: #2980b9;
        border-color: #2980b9;
      }

      .nav-link {
        color: #7B8A8B;
        font-weight: 500;
        font-size: 0.9rem;
        padding: 0.75rem 1rem;
      }
      
      .nav-link.active {
        color: #2C3E50;
        background-color: #ffffff;
        border-color: #dee2e6 #dee2e6 #fff;
      }
      
      .nav-link:hover {
        color: #2C3E50;
      }

      .radioGroupButtons .btn-group {
        display: inline-flex;
        gap: 0.25rem;
      }
      
      .radioGroupButtons .btn-xs {
        padding: 0.25rem 0.6rem;
        font-size: 0.8rem;
        line-height: 1.3;
      }
      
      .accordion-button {
        background-color: #f8f9fa;
        color: #2C3E50;
        font-weight: 500;
        padding: 0.75rem 1rem;
        font-size: 0.9rem;
        border: none;
      }
      
      .accordion-button:not(.collapsed) {
        background-color: #e9ecef;
        color: #2C3E50;
        box-shadow: none;
      }
      
      .accordion-button:focus {
        box-shadow: none;
        border-color: rgba(0,0,0,.125);
      }
      
      .accordion-body {
        padding: 1.25rem;
        background-color: #ffffff;
        border-top: 1px solid #dee2e6;
      }

      hr {
        margin: 1rem 0;
        border-top: 1px solid #dee2e6;
      }
      
      pre {
        font-family: 'Courier New', Courier, monospace;
        font-size: 0.85rem;
        line-height: 1.5;
      }
      
      .dataTables_wrapper {
        font-size: 0.9rem;
      }
      
      h6 {
        font-weight: 600;
        color: #2C3E50;
        font-size: 0.9rem;
      }
    "))
  ),
  
  # Application title
  title = "iTOL Label Generator",
  
  # Sidebar
  sidebar = sidebar(
    width = 450,
    
    # Header with logo
    div(class = "logo-container",
        tags$img(src = "https://wi.knaw.nl/images/westerdijk-logo.png", 
                 alt = "Westerdijk Institute Logo"),
        tags$h4("iTOL Label Generator")
    ),
    
    # File upload section
    card(
      card_header("Upload Data"),
      card_body(
        fileInput(
          "file", 
          NULL,
          accept = c(".tsv", ".csv", ".xlsx"),
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        ),
        div(class = "help-text",
            "Supported formats: TSV, CSV, XLSX")
      )
    ),
    
    # Column selection (shown after file upload)
    uiOutput("column_selection_card"),
    
    # Dataset label
    uiOutput("dataset_label_card")
  ),
  
      # Main panel with tabs
    navset_card_tab(
      id = "main_tabs",
      
      # Data preview tab
      nav_panel(
        "Data Preview",
        icon = icon("table"),
        card_body(
          div(class = "info-box",
              p(icon("info-circle"), "Preview your uploaded data. Verify that all columns are correctly loaded.")
          ),
          DTOutput("table")
        )
      ),
      
      # Symbol Annotations tab
      nav_panel(
        "Symbol Annotations",
        icon = icon("shapes"),
        card_body(
          div(class = "info-box",
              p(icon("info-circle"), "Configure colors and symbols for each metadata column.")
          ),
          
          
          # Column-specific settings
          uiOutput("symbol_column_settings_ui"),
          
          # ColorBrewer reference (collapsible)
          tags$details(
            tags$summary(
              style = "cursor: pointer; font-weight: 600; color: #2C5F8D; margin: 1rem 0 0.5rem 0;",
              "View ColorBrewer Palette Reference"
            ),
            plotOutput("brewer_plot_symbol", height = "600px")
          ),
          
          tags$hr(),
          
          # Download section for symbols
          uiOutput("symbol_download_card")
        )
      ),

      # Binary Set tab
      nav_panel(
        "Binary Set",
        icon = icon("chart-simple"),
        card_body(
          div(class = "info-box",
              p(icon("info-circle"), "Generate DATASET_BINARY annotations. Configure binary presence/absence patterns for each metadata column.")
          ),
          
          # Binary configuration
          uiOutput("binary_column_settings_ui"),
          
          tags$hr(),
          
          # Download section for binary
          uiOutput("binary_download_card")
        )
      ),
      
      # Simple Bar Chart tab
      nav_panel(
        "Simple Bar Chart",
        icon = icon("chart-bar"),
        card_body(
          div(class = "info-box",
              p(icon("info-circle"), "Generate DATASET_SIMPLEBAR annotations. Display numeric values as bars outside the tree.")
          ),
          
          # Bar chart configuration
          uiOutput("bar_column_settings_ui"),
          
          tags$hr(),
          
          # Download section for bar charts
          uiOutput("bar_download_card")
        )
      ),
      
      # Metadata tab
      nav_panel(
        "Metadata",
        icon = icon("database"),
        card_body(
          div(class = "info-box",
              p(icon("info-circle"), " Generate METADATA annotations. Selected columns will be included as multibar fields in iTOL.")
          ),
          
          # Metadata preview
          uiOutput("metadata_preview_ui"),
          
          tags$hr(),
          
          # Download section for metadata
          uiOutput("metadata_download_card")
        )
      ),
      
      # Change Labels tab
      nav_panel(
        "Change Labels",
        icon = icon("tags"),
        card_body(
          div(class = "info-box",
              p(icon("info-circle"), " Replace tree labels with new values from your data.")
          ),
          
          # Column selection for labels
          card(
            card_header("Label Configuration"),
            card_body(
              uiOutput("label_column_selection")
            )
          ),
          
          # Labels preview
          uiOutput("labels_preview_ui"),
          
          tags$hr(),
          
          # Download section for labels
          uiOutput("labels_download_card")
        )
      )
    )
  )


# ---------- Server ----------

server <- function(input, output, session) {
  
  # ---- Data reactive with sheet selection ----
  data <- reactive({
    req(input$file)
    file <- input$file$datapath
    ext <- tools::file_ext(input$file$name)
    
    tryCatch({
      if (ext == "tsv") {
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
    
    # Build palette choices
    palette_choices <- list()
    palette_choices[["--- Sequential ---"]] <- ""
    for(pal in brewer_pals$Sequential) palette_choices[[pal]] <- pal
    palette_choices[["--- Qualitative ---"]] <- ""
    for(pal in brewer_pals$Qualitative) palette_choices[[pal]] <- pal
    palette_choices[["--- Diverging ---"]] <- ""
    for(pal in brewer_pals$Diverging) palette_choices[[pal]] <- pal
    
    # Create accordion for each column
    accordion_items <- lapply(seq_along(input$data_cols), function(idx) {
      col <- input$data_cols[idx]
      col_values <- unique(sapply(as.character(df[[col]]), standardize_value))
      
      # Get current settings
      current_color_mode <- isolate(input[[paste0("color_mode_", col)]])
      current_brewer_pal <- isolate(input[[paste0("brewer_palette_", col)]])
      current_symbol_mode <- isolate(input[[paste0("symbol_mode_", col)]])
      current_auto_symbol <- isolate(input[[paste0("auto_symbol_", col)]])
      
      if(is.null(current_color_mode)) current_color_mode <- "Auto (Hue)"
      if(is.null(current_brewer_pal)) current_brewer_pal <- "Set1"
      if(is.null(current_symbol_mode)) current_symbol_mode <- "Auto"
      if(is.null(current_auto_symbol)) current_auto_symbol <- 1
      
      # Create accordion item
      accordion_panel(
        title = col,
        value = paste0("panel_", idx),
        
        # Color mode selection
        radioGroupButtons(
          paste0("color_mode_", col),
          "Color Mode",
          choices = c("Auto (Hue)" = "Auto (Hue)", 
                      "ColorBrewer" = "ColorBrewer", 
                      "Manual" = "Manual"),
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
            "Select Palette",
            choices = palette_choices,
            selected = current_brewer_pal
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
            val_id <- safe_id(paste(col, val, sep = "_"))
            
            current_color <- isolate(input[[paste0("color_", val_id)]])
            current_symbol <- isolate(input[[paste0("symbol_", val_id)]])
            
            if(is.null(current_color)) current_color <- "#3498DB"
            if(is.null(current_symbol)) current_symbol <- 2
            
            div(
              class = "value-config",
              div(class = "value-label", val),
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
    par(mar = c(2, 8, 2, 2))
    display.brewer.all()
  }, res = 96)
  
  # ---- Generate symbol outputs ----
  symbol_outputs <- reactive({
    req(data(), input$id_col, input$data_cols)
    
    df <- data()
    output_list <- list()
    
    for(col in input$data_cols) {
      col_values <- unique(sapply(as.character(df[[col]]), standardize_value))
      
      # Get color and symbol settings
      color_mode <- input[[paste0("color_mode_", col)]]
      symbol_mode <- input[[paste0("symbol_mode_", col)]]
      symbol_filled <- input[[paste0("symbol_filled_", col)]]
      
      if(is.null(color_mode)) color_mode <- "Auto (Hue)"
      if(is.null(symbol_mode)) symbol_mode <- "Auto"
      if(is.null(symbol_filled)) symbol_filled <- TRUE
      
      # Generate colors
      if(color_mode == "ColorBrewer") {
        brewer_pal <- input[[paste0("brewer_palette_", col)]]
        if(is.null(brewer_pal)) brewer_pal <- "Set1"
        n_colors <- max(3, min(length(col_values), 12))
        colors <- brewer.pal(n_colors, brewer_pal)
        if(length(col_values) > length(colors)) {
          colors <- colorRampPalette(colors)(length(col_values))
        }
        color_map <- setNames(colors[1:length(col_values)], col_values)
      } else if(color_mode == "Manual") {
        color_map <- setNames(
          sapply(col_values, function(val) {
            val_id <- safe_id(paste(col, val, sep = "_"))
            color <- input[[paste0("color_", val_id)]]
            if(is.null(color)) "#3498DB" else color
          }),
          col_values
        )
      } else {  # Auto (Hue)
        colors <- hue_pal()(length(col_values))
        color_map <- setNames(colors, col_values)
      }
      
      # Generate symbols
      if(symbol_mode == "Auto") {
        auto_symbol <- input[[paste0("auto_symbol_", col)]]
        if(is.null(auto_symbol)) auto_symbol <- 1
        symbol_map <- setNames(rep(auto_symbol, length(col_values)), col_values)
      } else {  # Manual
        symbol_map <- setNames(
          sapply(col_values, function(val) {
            val_id <- safe_id(paste(col, val, sep = "_"))
            symbol <- input[[paste0("symbol_", val_id)]]
            if(is.null(symbol)) 2 else symbol
          }),
          col_values
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
      
      # Set maximum symbol size
      content <- c(content, "MAXIMUM_SIZE\t5")
      content <- c(content, "")

      # Label settings to prevent size/position shifting
      content <- c(content, "SHOW_LABELS\t1")
      content <- c(content, "LABEL_SIZE_FACTOR\t1")
      content <- c(content, "LABEL_ROTATION\t0")
      content <- c(content, "LABEL_SHIFT\t0")
      content <- c(content, "")
      content <- c(content, "DATA")
      
      # Data format: ID, symbol, size, color, fill, position, label
      # Use uniform size of 10 for all symbols (iTOL will scale based on MAXIMUM_SIZE)
      for(i in 1:nrow(df)) {
        id <- as.character(df[[input$id_col]][i])
        val <- standardize_value(df[[col]][i])
        symbol <- symbol_map[val]
        color <- color_map[val]
        fill_value <- if(symbol_filled) "1" else "0"
        content <- c(content, paste(id, symbol, "1", color, fill_value, "-1", val, sep = "\t"))
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
          downloadButton(
            "download_symbol_single", 
            "Download Symbol File",
            class = "btn-success w-100",
            icon = icon("download")
          )
        } else {
          tagList(
            downloadButton(
              "download_symbols_zip",
              "Download All Symbol Files (ZIP)",
              class = "btn-success w-100",
              icon = icon("file-zipper")
            ),
            tags$br(),
            tags$br(),
            div(class = "help-text",
                "Or download each annotation file separately:"),
            tags$br(),
            lapply(names(content_list), function(name) {
              tags$div(
                style = "margin-bottom: 0.5rem;",
                downloadButton(
                  paste0("download_symbol_", safe_id(name)),
                  label = paste0(name, ".txt"),
                  class = "btn-primary w-100 btn-sm",
                  icon = icon("download")
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
      
      # Get current settings
      current_binary_shape <- isolate(input[[paste0("binary_shape_", col)]])
      current_binary_color <- isolate(input[[paste0("binary_color_", col)]])
      current_binary_filled <- isolate(input[[paste0("binary_filled_", col)]])
      
      if(is.null(current_binary_shape)) current_binary_shape <- 2
      if(is.null(current_binary_color)) current_binary_color <- "#3498DB"
      if(is.null(current_binary_filled)) current_binary_filled <- FALSE
      
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
        checkboxInput(
          paste0("binary_filled_", col),
          "Show only filled shapes (hide empty shapes)",
          value = current_binary_filled
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
    req(data(), input$id_col, input$data_cols)
    
    df <- data()
    output_list <- list()
    
    for(col in input$data_cols) {
      col_values <- unique(sapply(as.character(df[[col]]), standardize_value))
      
      # Get settings
      binary_mode <- input[[paste0("binary_mode_", col)]]
      binary_shape <- input[[paste0("binary_shape_", col)]]
      binary_color <- input[[paste0("binary_color_", col)]]
      binary_filled <- input[[paste0("binary_filled_", col)]]
      selected_values <- input[[paste0("binary_values_", col)]]
      
      if(is.null(binary_mode)) binary_mode <- "all"
      if(is.null(binary_shape)) binary_shape <- 2
      if(is.null(binary_color)) binary_color <- "#3498DB"
      if(is.null(binary_filled)) binary_filled <- FALSE
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
        
        # Create binary vector
        binary_vec <- sapply(fields, function(field) {
          if(val == field) {
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
          downloadButton(
            "download_binary_single", 
            "Download Binary File",
            class = "btn-success w-100",
            icon = icon("download")
          )
        } else {
          tagList(
            downloadButton(
              "download_binary_zip",
              "Download All Binary Files (ZIP)",
              class = "btn-success w-100",
              icon = icon("file-zipper")
            ),
            tags$br(),
            tags$br(),
            div(class = "help-text",
                "Or download each annotation file separately:"),
            tags$br(),
            lapply(names(content_list), function(name) {
              tags$div(
                style = "margin-bottom: 0.5rem;",
                downloadButton(
                  paste0("download_binary_", safe_id(name)),
                  label = paste0(name, "_binary.txt"),
                  class = "btn-primary w-100 btn-sm",
                  icon = icon("download")
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
        checkboxInput(
          paste0("bar_show_value_", col),
          "Show values on bars",
          value = isolate(input[[paste0("bar_show_value_", col)]]) %||% TRUE
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
            selected = isolate(input[[paste0("bar_value_position_", col)]]) %||% "outside-right"
          ),
          
          checkboxInput(
            paste0("bar_auto_color_", col),
            "Automatic label color (white/black based on bar darkness)",
            value = isolate(input[[paste0("bar_auto_color_", col)]]) %||% TRUE
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
      content <- c(content, "")
      
      # Add scale lines if specified (convert comma-separated to tab-separated)
      if(bar_scale != "" && !is.na(bar_scale)) {
        scale_values <- trimws(unlist(strsplit(bar_scale, ",")))
        scale_line <- paste(scale_values, collapse = "\t")
        content <- c(content, paste("DATASET_SCALE", scale_line, sep = "\t"))
      }
      
      content <- c(content, "")
      content <- c(content, "SHOW_LABELS\t1")
      
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
      
      # Log filtered values
      if(na_count > 0) {
        message(sprintf("Column '%s': %d NA/non-numeric value(s) filtered from bar chart output", col, na_count))
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
          downloadButton(
            "download_bar_single", 
            "Download Bar Chart File",
            class = "btn-success w-100",
            icon = icon("download")
          )
        } else {
          tagList(
            downloadButton(
              "download_bar_zip",
              "Download All Bar Chart Files (ZIP)",
              class = "btn-success w-100",
              icon = icon("file-zipper")
            ),
            tags$br(),
            tags$br(),
            div(class = "help-text",
                "Or download each annotation file separately:"),
            tags$br(),
            lapply(names(content_list), function(name) {
              tags$div(
                style = "margin-bottom: 0.5rem;",
                downloadButton(
                  paste0("download_bar_", safe_id(name)),
                  label = paste0(name, "_bar.txt"),
                  class = "btn-primary w-100 btn-sm",
                  icon = icon("download")
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
        downloadButton(
          "download_metadata",
          "Download metadata.txt",
          class = "btn-success w-100",
          icon = icon("download")
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
        downloadButton(
          "download_labels",
          "Download labels.txt",
          class = "btn-success w-100",
          icon = icon("download")
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
    req(symbol_outputs())
    content_list <- symbol_outputs()
    
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
    req(binary_outputs())
    content_list <- binary_outputs()
    
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
    req(bar_outputs())
    content_list <- bar_outputs()
    
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
  })
      
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

# Run the application
shinyApp(ui = ui, server = server)