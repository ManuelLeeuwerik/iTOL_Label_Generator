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
    req(input$data_cols)
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
  # Metadata download
  output$download_metadata <- downloadHandler(
    filename = function() {
      "metadata.txt"
    },
    content = function(file) {
      writeLines(metadata_output(), file)
    }
  )