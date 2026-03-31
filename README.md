# Goal
Automate label generation for iTOL, instead of writing or changing scripts (see example below).


Activate the conda environment and run the app.

**Conda:**

```bash
conda env create -f rshiny.yaml
conda activate rshiny 
```

To **run** Rshiny app:

```bash
Rscript run_app.R 
```

Example previous script:

```R
library(readr)
library(dplyr)
library(tidyverse)

coalesce_joined <- function(df) {
  # find base column names that appear as both .x and .y
  base_cols <- intersect(
    sub("\\.x$", "", grep("\\.x$", names(df), value = TRUE)),
    sub("\\.y$", "", grep("\\.y$", names(df), value = TRUE))
  )
  
  for (col in base_cols) {
    df[[col]] <- dplyr::coalesce(df[[paste0(col, ".x")]], df[[paste0(col, ".y")]])
  }
  
  # drop the .x and .y columns
  df <- df %>%
    dplyr::select(-dplyr::ends_with(".x"), -dplyr::ends_with(".y"))
  
  return(df)
}

# === Load and trim isolate data ===
metadata_df <- read_tsv("/metadata") 

# === Generate iTOL SYMBOL DATASET for LATASPID ===
# Assign colors to unique countries
LATASPID_colors <- metadata_df %>%
  distinct(LATASPID) %>%
  mutate(color = case_when(
    is.na(LATASPID) ~ "#808080",                # Gray for Unknown
    TRUE ~ scales::hue_pal()(n())[row_number()] # Unique colors for each LATASPID
  ))

metadata_df <- metadata_df %>%
  left_join(LATASPID_colors, by = "LATASPID") %>%
  rename(LATASPID_color = color)

LATASPID_symbol_df <- metadata_df %>%
  distinct(library_ID, LATASPID_color) %>%
  rowwise() %>%
  mutate(
    LATASPID_symbol = paste(library_ID, 3, 3, LATASPID_color, 1, -1, "Country", sep = ",")
  ) %>%
  ungroup()

# Write LATASPID SYMBOL dataset
LATASPID_symbol_header <- c(
  "DATASET_SYMBOL",
  "",
  "SEPARATOR COMMA",
  "",
  "DATASET_LABEL,Country",
  "",
  "COLOR,#000000",
  "",
  "LEGEND_TITLE,Country",
  "LEGEND_SCALE,1",
  "LEGEND_POSITION_X,100",
  "LEGEND_POSITION_Y,100",
  "LEGEND_VERTICAL,1",
  "LEGEND_VISIBLE,1",
  paste0("LEGEND_SHAPES,", paste(rep(3, nrow(LATASPID_colors)), collapse = ",")),
  paste0("LEGEND_COLORS,", paste(LATASPID_colors$color, collapse = ",")),
  paste0("LEGEND_LABELS,", paste(LATASPID_colors$LATASPID, collapse = ",")),
  "",
  "MAXIMUM_SIZE,10",
  "",
  "DATA",
  "ID,symbol,size,color,fill,position,label"
)

LATASPID_symbol_lines <- LATASPID_symbol_df %>%
  pull(LATASPID_symbol)

write_lines(LATASPID_symbol_header, file.path("itol_LATASPID_symbols.txt"))
write_lines(LATASPID_symbol_lines, file.path("itol_LATASPID_symbols.txt"), append = TRUE)

message("iTOL SYMBOL dataset for CYP51A Mutation written to ", file.path("itol_cyp51a_mutation_symbols.txt"))
message("iTOL SYMBOL dataset for Country written to ", file.path("itol_LATASPID_symbols.txt"))
message("iTOL SYMBOL dataset written to ", file.path("itol_resistance_source_symbols.txt"))
message("iTOL LABELS dataset written to ", file.path("itol_labels.txt"))
```