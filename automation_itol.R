library(optparse)
library(yaml)
library(readr)
library(dplyr)
library(scales)
library(viridisLite)
library(RColorBrewer)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# iTOL symbol mapping (filled shapes)
symbol_map <- c(
  square = 1,
  circle = 2,
  star = 3,
  triangle = 4,
  left_triangle = 5,
  empty_circle = 6,
  empty_square = 7,
  checkmark = 6,
  solid_line = 7,
  dashed_line = 8,
  dotted_line = 9,
  arrow_left = 10,
  arrow_right = 11,
  bidirectional = 12,
  diamond = 5
)

# Helper function to generate palette
get_palette <- function(values, palette_name = "hue") {
  n <- length(unique(values))
  if (palette_name == "viridis") {
    pal <- viridis(n)
  } else if (palette_name == "brewer") {
    pal <- brewer.pal(min(n, 8), "Set2")
    if (n > 8) pal <- colorRampPalette(pal)(n)
  } else { # default hue
    pal <- hue_pal()(n)
  }
  names(pal) <- unique(values)
  return(pal)
}

make_symbol_dataset <- function(df, id_col, column, config, outdir) {
  
  # Handle "all columns"
  cols_to_use <- if(column == "all") setdiff(names(df), id_col) else column
  
  for(col in cols_to_use) {
    values <- df[[col]]
    
    # Colors
    if (!is.null(config$colors)) {
      colors <- config$colors
      default_color <- config$default_color %||% "#808080"
      value_colors <- sapply(values, function(x) if(x %in% names(colors)) colors[[x]] else default_color)
    } else {
      palette_name <- config$palette %||% "hue"
      pal <- get_palette(values, palette_name)
      value_colors <- pal[as.character(values)]
      colors <- pal
    }
    
    # Symbol
    symbol <- symbol_map[[config$symbol]] %||% 2
    fill <- config$fill %||% 1
    size <- config$size %||% 5
    position <- config$position %||% -1
    
    # Data lines
    lines <- paste(
      df[[id_col]],
      symbol,
      size,
      value_colors,
      fill,
      position,
      values,
      sep = ","
    )
    
    # Legend
    legend_shapes <- paste(rep(symbol, length(colors)), collapse = ",")
    legend_colors <- paste(colors, collapse = ",")
    legend_labels <- paste(names(colors), collapse = ",")
    
    header <- c(
      "DATASET_SYMBOL",
      "",
      "SEPARATOR COMMA",
      "",
      paste0("DATASET_LABEL,", config$label %||% col),
      "",
      "COLOR,#000000",
      "",
      paste0("LEGEND_TITLE,", config$label %||% col),
      "LEGEND_SCALE,1",
      "LEGEND_POSITION_X,100",
      "LEGEND_POSITION_Y,100",
      "LEGEND_VERTICAL,1",
      "LEGEND_VISIBLE,1",
      paste0("LEGEND_SHAPES,", legend_shapes),
      paste0("LEGEND_COLORS,", legend_colors),
      paste0("LEGEND_LABELS,", legend_labels),
      "",
      "MAXIMUM_SIZE,", size,
      "",
      "DATA",
      "ID,symbol,size,color,fill,position,label"
    )
    
    outfile <- file.path(outdir, paste0("itol_", col, ".txt"))
    readr::write_lines(header, outfile)
    readr::write_lines(lines, outfile, append = TRUE)
    
    message("Written iTOL SYMBOL dataset: ", outfile)
  }
}

# Command-line options
option_list <- list(
  make_option("--metadata", type="character"),
  make_option("--config", type="character"),
  make_option("--outdir", type="character")
)

opt <- parse_args(OptionParser(option_list=option_list))

metadata <- read_tsv(opt$metadata)
config <- yaml::read_yaml(opt$config)

dir.create(opt$outdir, showWarnings = FALSE)

for(ds in config$datasets) {
  make_symbol_dataset(
    df = metadata,
    id_col = config$id_column,
    column = ds$column,
    config = ds,
    outdir = opt$outdir
  )
}