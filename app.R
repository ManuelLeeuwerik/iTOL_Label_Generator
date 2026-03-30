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

#' Standardize NA-like values to "Unknown"
standardize_value <- function(x) {
  if (is.na(x) || is.null(x)) return("Unknown")
  x_char <- as.character(x)
  if (x_char == "NA" || x_char == "N/A" || x_char == "unknown" || 
      x_char == "" || grepl("^\\s+$", x_char)) {
    return("Unknown")
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
    primary = "#2C5F8D",      # Darker blue
    secondary = "#5A7A9B",    # Blue-gray
    success = "#2C5F8D",
    info = "#5DADE2",         # Lighter blue
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
        background-color: #e8f1f7;  /* Light blue background */
      }
      
      .bslib-sidebar-layout {
        background-color: #f7fbfd;  /* Very light blue */
      }
      
      .card {
        border: 1px solid #b8d4e8;  /* Blue-tinted border */
        box-shadow: 0 1px 3px rgba(44, 95, 141, 0.08);  /* Blue shadow */
        margin-bottom: 1rem;
        border-radius: 0.375rem;
      }
      
      .card-header {
        background-color: #e8f1f7;  /* Light blue header */
        border-bottom: 1px solid #b8d4e8;
        font-weight: 600;
        font-size: 0.95rem;
        color: #2C5F8D;  /* Dark blue text */
        padding: 0.75rem 1rem;
      }
      
      .info-box {
        background-color: #d6ebf5;  /* Blue info box */
        border-left: 3px solid #2C5F8D;
        padding: 0.875rem 1rem;
        margin: 0 0 1rem 0;
        border-radius: 0.25rem;
        font-size: 0.9rem;
      }
      
      .info-box p {
        margin: 0;
        color: #1e4d73;  /* Dark blue text */
        line-height: 1.5;
      }
      
      .value-config {
        display: flex;
        align-items: center;
        gap: 0.75rem;
        padding: 0.625rem;
        margin-bottom: 0.5rem;
        background-color: #f0f7fb;  /* Light blue */
        border-radius: 0.25rem;
        border-left: 3px solid #5DADE2;  /* Bright blue */
      }
      
      .btn-primary {
        background-color: #2C5F8D;  /* Dark blue */
        border-color: #2C5F8D;
      }
      
      .btn-primary:hover {
        background-color: #234a6d;
        border-color: #234a6d;
      }
      
      .form-control:focus, .form-select:focus {
        border-color: #5DADE2;  /* Blue focus */
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
        background-color: #538edbff;
        border-color: #538edbff;
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
    width = 500,
    
    # Header with logo
    div(class = "logo-container",
        tags$img(src = "https://wi.knaw.nl/images/westerdijk-logo.png", 
                 alt = "Westerdijk Institute Logo"),
        tags$h4("iTOL Label Generator")
    ),
    
    # File upload section
    card(
      card_header("1. Upload Data"),
      card_body(
        fileInput(
          "file", 
          NULL,
          accept = c(".tsv", ".csv", ".txt", ".xlsx"),
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        ),
        div(class = "help-text",
            "Supported formats: TSV, CSV, TXT, XLSX")
      )
    ),
    
        # Column selection (shown after file upload)
    uiOutput("column_selection_card"),
    
    # Output configuration
    card(
      card_header("3. Output Configuration"),
      card_body(
        textInput(
          "dataset_label", 
          "Dataset Label",
          value = "My Dataset",
          placeholder = "Enter a descriptive name"
        ),
        div(class = "help-text",
            "This label will appear in iTOL"),
        
        tags$hr(),
        
        radioGroupButtons(
          "output_type",
          "Output Type",
          choices = c(
            "Symbol Annotations" = "SYMBOL",
            "Metadata" = "METADATA",
            "Change Labels" = "CHANGE_LABEL"
          ),
          selected = "SYMBOL",
          justified = TRUE,
          status = "primary",
          size = "sm"
        ),
        
        # SYMBOL-specific controls
        conditionalPanel(
          condition = "input.output_type == 'SYMBOL'",
          tags$hr(),
          numericInput(
            "max_size",
            "Symbol Size",
            value = 5,
            min = 1,
            max = 50,
            step = 1
          ),
          div(class = "help-text",
              "Maximum size for symbols in iTOL")
        )
      )
    ),
    
    # Download section
    uiOutput("download_card")
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
            p("Preview your uploaded data. Verify that all columns are correctly loaded.")
        ),
        DTOutput("table")
      )
    ),
    
    # Configuration tab
    nav_panel(
      "Symbol Configuration",
      icon = icon("palette"),
      card_body(
        conditionalPanel(
          condition = "input.output_type == 'SYMBOL'",
          div(class = "info-box",
              p("Configure colors and symbols for each metadata column. Use ColorBrewer palettes for publication-quality figures.")
          ),
          uiOutput("column_settings_ui")
        ),
        conditionalPanel(
          condition = "input.output_type == 'METADATA'",
          div(class = "info-box",
              p(icon("info-circle"), " METADATA output uses raw values from selected columns without symbol mapping.")
          )
        ),
        conditionalPanel(
          condition = "input.output_type == 'CHANGE_LABEL'",
          div(class = "info-box",
              p(icon("info-circle"), " CHANGE_LABEL replaces current tree labels with new labels from your data.")
          )
        )
      )
    ),
    
    # ColorBrewer reference
    nav_panel(
      "ColorBrewer Palettes",
      icon = icon("swatchbook"),
      card_body(
        div(class = "info-box",
            p("ColorBrewer provides carefully designed color schemes for scientific visualization. Choose palettes appropriate for your data type.")
        ),
        plotOutput("brewer_plot", height = "800px")
      )
    ),
    
    # Output preview
    nav_panel(
      "Output Preview",
      icon = icon("file-code"),
      card_body(
        div(class = "info-box",
            p("Preview the generated iTOL annotation files before downloading.")
        ),
        uiOutput("preview_ui")
      )
    )
  )
)

# ---------- Server ----------

server <- function(input, output, session) {
  
  # ---- Load data ----
  data <- reactive({
    req(input$file)
    file <- input$file$datapath
    ext <- tools::file_ext(input$file$name)
    
    tryCatch({
      if (ext %in% c("tsv", "txt")) {
        read_tsv(file, show_col_types = FALSE)
      } else if (ext == "csv") {
        read_csv(file, show_col_types = FALSE)
      } else if (ext == "xlsx") {
        read_excel(file)
      } else {
        stop("Unsupported file format")
      }
    }, error = function(e) {
      showNotification(
        paste("Error loading file:", e$message),
        type = "error",
        duration = 5
      )
      NULL
    })
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
  
  # ---- Column selection card (shown after file upload) ----
  output$column_selection_card <- renderUI({
    req(data())
    cols <- names(data())
    
    card(
      card_header("2. Column Selection"),
      card_body(
        # Different inputs based on output type
        conditionalPanel(
          condition = "input.output_type != 'CHANGE_LABEL'",
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
            "Columns to Visualize",
            choices = cols,
            multiple = TRUE,
            options = list(
              placeholder = 'Select one or more columns',
              plugins = list('remove_button')
            )
          ),
          div(class = "help-text",
              "Select metadata columns to include in the output")
        ),
        
        conditionalPanel(
          condition = "input.output_type == 'CHANGE_LABEL'",
          selectInput(
            "old_label_col",
            "Current Label Column",
            choices = cols
          ),
          div(class = "help-text",
              "Column containing existing tree labels"),
          
          tags$br(),
          
          selectInput(
            "new_label_col",
            "New Label Column",
            choices = cols
          ),
          div(class = "help-text",
              "Column containing new labels to use")
        )
      )
    )
  })
  
  # ---- Download card (shown when output is ready) ----
  output$download_card <- renderUI({
    req(output_content_list())
    content_list <- output_content_list()
    
    card(
      card_header("4. Download"),
      card_body(
        if(length(content_list) == 1) {
          # Single file download
          downloadButton(
            "download_single", 
            "Download File",
            class = "btn-success w-100",
            icon = icon("download")
          )
        } else {
          # Multiple files - offer both individual and bulk download
          tagList(
            # Download all as ZIP
            downloadButton(
              "download_all_zip",
              "Download All Files (ZIP)",
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
                  paste0("download_", safe_id(name)),
                  label = if(input$output_type == "METADATA") {
                    "Download metadata.txt"
                  } else if(input$output_type == "CHANGE_LABEL") {
                    "Download labels.txt"
                  } else {
                    paste0(name, ".txt")
                  },
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
  
  # ---- Display ColorBrewer palettes ----
  output$brewer_plot <- renderPlot({
    par(mar = c(2, 8, 2, 2))
    display.brewer.all()
  }, res = 96)
  
  # ---- Column settings UI (symbol configuration) ----
  output$column_settings_ui <- renderUI({
    req(input$data_cols)
    
    df <- isolate(data())
    brewer_pals <- get_brewer_palettes()
    
    # Build palette choices with separators
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
      
      # Get current settings (preserving state)
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
          width = "60%",
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
        
        # Manual configuration for each value
        conditionalPanel(
          condition = sprintf("input['color_mode_%s'] == 'Manual' || input['symbol_mode_%s'] == 'Manual'", col, col),
          tags$h6("Configure Individual Values"),
          lapply(col_values, function(val) {
            val_id <- safe_id(paste(col, val, sep = "_"))
            
            # Get current manual settings
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
  
  # ---- Generate output content ----
  output_content_list <- reactive({
    req(data())
    
    if(input$output_type == "CHANGE_LABEL") {
      req(input$old_label_col, input$new_label_col)
      df <- data()
      
      content <- c("LABELS")
      content <- c(content, "SEPARATOR TAB")
      content <- c(content, "DATA")
      
      for(i in 1:nrow(df)) {
        old_label <- as.character(df[[input$old_label_col]][i])
        new_label <- as.character(df[[input$new_label_col]][i])
        content <- c(content, paste(old_label, new_label, sep = "\t"))
      }
      
      return(list("labels" = paste(content, collapse = "\n")))
      
    } else if(input$output_type == "METADATA") {
      req(input$id_col, input$data_cols)
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
      
      return(list("metadata" = paste(content, collapse = "\n")))
      
        } else {  # SYMBOL output
      req(input$id_col, input$data_cols)
      df <- data()
      
      output_list <- list()
      
      for(col in input$data_cols) {
        col_values <- unique(sapply(as.character(df[[col]]), standardize_value))
        
        # Get color and symbol settings
        color_mode <- input[[paste0("color_mode_", col)]]
        symbol_mode <- input[[paste0("symbol_mode_", col)]]
        
        if(is.null(color_mode)) color_mode <- "Auto (Hue)"
        if(is.null(symbol_mode)) symbol_mode <- "Auto"
        
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
        content <- c(content, paste("COLOR", "#2755ecff", sep = "\t"))
        content <- c(content, "")
        content <- c(content, paste("LEGEND_TITLE", col, sep = "\t"))
        content <- c(content, paste("LEGEND_SHAPES", paste(symbol_map, collapse = "\t"), sep = "\t"))
        content <- c(content, paste("LEGEND_COLORS", paste(color_map, collapse = "\t"), sep = "\t"))
        content <- c(content, paste("LEGEND_LABELS", paste(names(color_map), collapse = "\t"), sep = "\t"))
        content <- c(content, "")
        content <- c(content, paste("MAXIMUM_SIZE", input$max_size, sep = "\t"))
        content <- c(content, "")
        content <- c(content, "DATA")
        
        # Data format: ID, symbol, size, color, fill, position
        for(i in 1:nrow(df)) {
          id <- as.character(df[[input$id_col]][i])
          val <- standardize_value(df[[col]][i])
          symbol <- symbol_map[val]
          color <- color_map[val]
          # ID, symbol, size, color, fill (1=filled), position (-1=external)
          content <- c(content, paste(id, symbol, input$max_size, color, "1", "-1", sep = "\t"))
        }
        
        output_list[[col]] <- paste(content, collapse = "\n")
      }
      
      return(output_list)
    }
  })
  
  # ---- Preview output ----
  output$preview_ui <- renderUI({
    req(output_content_list())
    content_list <- output_content_list()
    
    if(length(content_list) == 1) {
      card(
        card_header(if(input$output_type == "METADATA") "metadata.txt" 
                   else if(input$output_type == "CHANGE_LABEL") "labels.txt" 
                   else names(content_list)[1]),
        card_body(
          tags$pre(
            style = "max-height: 500px; overflow-y: auto; background-color: #f8f9fa; padding: 1rem; border-radius: 0.25rem; border: 1px solid #dee2e6;",
            content_list[[1]]
          )
        )
      )
    } else {
      lapply(names(content_list), function(name) {
        card(
          card_header(paste0(name, ".txt")),
          card_body(
            tags$pre(
              style = "max-height: 400px; overflow-y: auto; background-color: #f8f9fa; padding: 1rem; border-radius: 0.25rem; border: 1px solid #dee2e6;",
              content_list[[name]]
            )
          )
        )
      })
    }
  })
  
  # ---- Download handlers ----
  output$download_single <- downloadHandler(
    filename = function() {
      if(input$output_type == "METADATA") {
        "metadata.txt"
      } else if(input$output_type == "CHANGE_LABEL") {
        "labels.txt"
      } else {
        paste0(input$dataset_label, ".txt")
      }
    },
    content = function(file) {
      content_list <- output_content_list()
      writeLines(content_list[[1]], file)
    }
  )
      
  # Download all files as ZIP
  output$download_all_zip <- downloadHandler(
    filename = function() {
      paste0(input$dataset_label, "_annotations_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      content_list <- output_content_list()
      
      # Create temporary directory
      temp_dir <- tempdir()
      temp_files <- c()
      
      # Write each file to temp directory
      for(name in names(content_list)) {
        temp_file <- file.path(temp_dir, paste0(name, ".txt"))
        writeLines(content_list[[name]], temp_file)
        temp_files <- c(temp_files, temp_file)
      }
      
      # Create ZIP file
      zip::zip(
        zipfile = file,
        files = basename(temp_files),
        root = temp_dir,
        mode = "cherry-pick"
      )
      
      # Clean up temp files
      unlink(temp_files)
    }
  )
      
  # Dynamic download handlers for multiple files
  observe({
    req(output_content_list())
    content_list <- output_content_list()
    
    if(length(content_list) > 1) {
      lapply(names(content_list), function(name) {
        local({
          my_name <- name
          output[[paste0("download_", safe_id(my_name))]] <- downloadHandler(
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
}

# Run the application
shinyApp(ui = ui, server = server)