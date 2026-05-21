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

  # Labels download
  output$download_labels <- downloadHandler(
    filename = function() {
      "labels.txt"
    },
    content = function(file) {
      writeLines(labels_output(), file)
    }
  )