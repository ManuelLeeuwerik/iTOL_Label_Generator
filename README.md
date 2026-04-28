# iTOL Label Generator

A Shiny application for generating iTOL (Interactive Tree of Life) annotation files from tabular metadata.

# Installation
Create **conda** environment and activate it:

```bash
conda env create -f rshiny.yaml  
conda activate rshiny  
```
Run the App:

```bash
Rscript run_app.R  
```

# Usage Workflow
## 1. Upload Data
Click `Browse` to upload a file:
- TSV
- CSV
- XLSX
  - Select sheet (if Excel file has multiple sheets)

## 2. Configure Columns
* **ID Column:** Choose column matching your tree tip labels
* **Columns to Visualize:** Select metadata columns
* **Dataset Label:** Enter a descriptive name

## 3. Generate Annotations
Navigate through tabs to create different annotation types:

- `Data Preview`
  - Verify uploaded data structure
- `Symbol Annotations` (DATASET_SYMBOL)
  - Categorical/numeric data as colored symbols
  - Choose ColorBrewer palettes or custom colors
  - Select symbol shapes: square, circle, star, triangle, checkmark
- `Binary Set` (DATASET_BINARY)
  - Presence/absence patterns
  - Include/exclude specific values or show all as separate fields
- `Simple Bar Chart` (DATASET_SIMPLEBAR)
  - Single numeric values as horizontal bars
  - Customize colors, scale lines, and value labels
- `Multi-Value Bar Chart` (DATASET_MULTIBAR)
  - Multiple numeric columns as stacked/aligned bars
  - Compare variables side-by-side
- `Metadata` (METADATA)
  - Export all selected columns in iTOL metadata format
 - `Change Labels` (LABELS)
  - Replace tree tip labels with alternative values

## 4. Download
Individual files or ZIP archive for multiple annotations. Files are ready to upload directly to iTOL.

## Important Notes
- NA values: Automatically filtered from visualizations. Use "Unknown" if needed
- Symbol sizes: May differ between circular vs rectangular tree layouts in iTOL
- Matching labels: ID column must exactly match tree tip labels (case-sensitive)
- Numeric detection: Columns auto-detected if convertible to numbers

## Troubleshooting
- "No numeric columns selected" → Ensure numeric or convertible values
- "Excel sheet selection doesn't appear" → File has only one sheet
- "Annotations don't appear in iTOL" → Check ID column matches tree labels
- "Download button not appearing" → Ensure valid column selection

> Compatible with: iTOL v7.51 (other versions not tested)