# Goal
Automate label generation for iTOL. 

Currently all seperate scripts to generate files for iTOL. 

So make an UI with Rshiny

Input:
- tsv
- csv
- txt
- excel (xlsx)

User functions:
- Select wanted colors and symbols for each column and unqiue value or all values (So X yellow Y red or column B yellow etc)
- Appointing ID column coresponding with tree tips
- Be able to switch ID labels on tree tips with another column
- Option to define ranges etc.
- Present abscense possibilities.

Ouput:
- Folder with text files which users can drag and drop in iTOL
- Each outer track should be a different text file, iTOL handles this properly


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
metadata_df <- read_tsv("/data/users/m.leeuwerik/2024-latasp/analysis/redcap_analysis/dup_isolates_to_delete.tsv") 

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

Conda env needed

conda activate rshiny 

Create installation and dependency step with YAML.

Open port on local machine:
```bash
mleeuwerik@DSK-MEYE-003:~$ ssh -L 8000:localhost:8000 m.leeuwerik@srv-lnx-meye1
```

To run Rshiny app Rscript run_app.R 

Make a readme in the app and how to use the app. Fix previews.
Change all option, so column X i want instead of standard square symbol them all circle 
Download all button!
Utilizing more iTOL functions.
Look into Rshiny live (https://posit-dev.github.io/r-shinylive/)

