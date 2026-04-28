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