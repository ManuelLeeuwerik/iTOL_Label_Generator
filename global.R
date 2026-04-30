# global.R
# This file is sourced before ui.R and server.R

# Libraries
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

# Helper function for creating centered download buttons
centered_download_button <- function(id, label, class = "btn-success", icon_name = "download", width = "750px") {
  div(
    style = "display: flex; justify-content: center;",
    div(
      style = paste0("max-width: ", width, "; width: 100%;"),
      downloadButton(
        id,
        label,
        class = class,
        style = "width: 100%;",
        icon = icon(icon_name)
      )
    )
  )
}

# Standardize column names
sanitize_colname <- function(x) {
  # Replace spaces and special characters with underscores
  x <- gsub("[^A-Za-z0-9_]", "_", x)
  # Remove consecutive underscores
  x <- gsub("_{2,}", "_", x)
  # Remove leading/trailing underscores
  x <- gsub("^_|_$", "", x)
  # Ensure it doesn't start with a number
  if(grepl("^[0-9]", x)) {
    x <- paste0("col_", x)
  }
  return(x)
}