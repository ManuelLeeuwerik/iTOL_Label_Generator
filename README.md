# iTOL Label Generator

A Shiny application for generating [iTOL](https://itol.embl.de/) (Interactive Tree of Life) annotation files from tabular metadata, enabling rapid visualization of phylogenetic data.

# Installation
Clone the repository and go to it:

```bash
git clone https://github.com/ManuelLeeuwerik/iTOL_Label_Generator.git

cd iTOL_Label_Generator/
```

Create [**conda**](https://docs.conda.io/projects/conda/en/stable/user-guide/install/index.html) environment and activate it:

```bash
conda env create -f rshiny.yaml
conda activate rshiny
```
Run the App:

```bash
Rscript run_app.R
```

## Example Data

An example dataset is provided in the `example_data/` folder. You can use this dataset to quickly test the application without preparing your own data.

- `tree.nwk` – phylogenetic tree
- `metadata.csv / .tsv / .xlsx` – tabular metadata

This dataset is derived from a published open-access study and has been used for demonstration purposes.

> Rhodes, J., Abdolrasouli, A., Dunne, K. et al. Population genomics confirms acquisition of drug-resistant Aspergillus fumigatus infection by humans from the environment. Nat Microbiol 7, 663–674 (2022). https://doi.org/10.1038/s41564-022-01091-2


# Usage Workflow
## 1. Upload Data
Click `Browse` to upload a file:
- TSV (tab-separated values)
- CSV (comma-separated values)
- XLSX (Excel file)
  - If the file has multiple sheets, a dialog will appear to select which sheet to import

## 2. Configure Columns
- **ID Column:** Select the column that contains unique identifiers matching your phylogenetic tree tip labels
- **Columns to Visualize:** Choose one or more metadata columns to generate annotations from
- **Dataset Label:** Enter a descriptive name for your annotation set (used in filenames and iTOL legends)

> **Validate Tree Matching (optional)**
> To ensure your annotations will work correctly in iTOL, you can upload your phylogenetic tree file (.nwk, .newick, .tree, .tre) in the sidebar. The app will:
> - Display the number of tip labels and internal nodes in your tree
> - Compare your selected ID column against the tree tip labels
> - Show match percentage and identify mismatches:
>   - IDs in data NOT in tree: Values that won't appear in your iTOL visualization
>   - Tree tips NOT in data: Tree labels that won't receive annotations

## 3. Generate Annotations
Navigate through tabs to create different annotation types:

### `Data Preview`
- Verify uploaded data structure before generating annotations
### `Symbol Annotations`
- Display categorical or numeric data as colored symbols next to tree tips
- **Color Options**:
  - ColorBrewer palettes: Choose from sequential (numeric data) or qualitative (categorical data) color schemes
  - Manual colors: Select custom colors for each unique value
  - Hue Scale: Automatically generate colors using a hue-based palette
- **Symbol Options**:
  - Auto mode: Apply the same symbol shape to all values
  - Manual mode: Assign different shapes (square, circle, star, triangle, checkmark) to each value
- Fill Options: Toggle between filled symbols or outline-only shapes
- View ColorBrewer palette reference within the tab for guidance
### `Binary Set`
- Show presence/absence patterns for categorical data
- Configure symbol shape, color, and fill behavior
- **Value Selection Modes**:
  - Include specific values: Show presence only for selected values
  - Exclude specific values: Show presence for all except selected values
- All values as separate fields: Create individual binary fields for each unique value
- Ideal for displaying trait presence, gene presence/absence, or classification membership
### `Simple Bar Chart`
- Display single numeric values as horizontal bars outside the tree
- Customize bar color, scale lines, and value labels
- **Value Label Options**:
  - Choose label position (outside, left, center, right)
  - Enable automatic label color contrast or set manual colors
- Useful for showing single measurements like genome size, abundance, or scores
### `Multi-Value Bar Chart`
- Display multiple numeric columns simultaneously
- **Display Modes**:
  - Stacked bars: Values stacked on top of each other
  - Aligned bars: Values displayed side-by-side for direct comparison
  - Side stacked: Hybrid approach with fields next to each other
- Configure individual colors for each field
### `Label Styles`
- Customize the appearance of tree tip labels based on metadata values
- **Configuration Options**:
  - Label Color: Set custom colors for labels
  - Font Style: Choose between normal, bold, italic, or bold-italic
  - Background Color: Optional colored background behind labels
  - Size Factor: Adjust label size relative to the global font size (0.1-5.0)
- Each unique value in a selected column can have its own style configuration
- Note: Only affects label styling; does not modify branch or node properties
### `Metadata`
- Export all selected columns in iTOL metadata format
- Generates a comprehensive metadata file that can be used for tree annotation and data exploration in iTOL
- All selected columns are included as-is without filtering
### `Change Labels`
- Replace tree tip labels with alternative values from your data
- Configuration:
  - ID Column: Select the column containing current/original tree labels
  - New Tip Label Column: Select the column with replacement labels
- Useful for switching between different identifier systems (e.g., accession numbers to species names)

## 4. Download
- Single files: Download individual annotation files for each column
- ZIP archives: Download all annotations of the same type at once
- Files are ready to upload directly to iTOL by dragging and dropping onto your tree

## Notes
- **Symbol sizes:** May appear different between circular and rectangular tree layouts in iTOL. The app is optimized for rectangular layouts, this can be changed in iTOL as well
- **Color palettes:** Sequential palettes (Blues, Greens, etc.) work best for numeric data; qualitative palettes (Set1, Paired, etc.) work best for categorical data
- **Bar scaling:** Scale lines help provide reference points for numeric values; specify them as comma-separated values (e.g., "10,50,100")
- **NA values:** Automatically filtered from visualizations. If you need to represent unknown data, use explicit text like "Unknown" instead
- **Matching labels:** The ID column must exactly match tree tip labels (case-sensitive, whitespace-sensitive)
- **Numeric detection:** Columns are automatically detected as numeric if they contain values convertible to numbers

## Troubleshooting
| Issue | Solution | 
| ----- | --------- |
| "No numeric columns selected" | Ensure selected columns contain numeric or numeric-convertible values. Check for non-numeric characters or inconsistent formatting |
| "Annotations don't appear in iTOL" | Verify the ID column exactly matches tree tip labels. iTOL will give the error: "Couldn't find ID.. in the tree" |
| "Download button not appearing" | Ensure valid columns are selected and all required settings are configured. Check for error messages in the UI |
| "Excel sheet selection doesn't appear" | The file contains only one sheet and will be loaded automatically. No selection needed |
| "More values in the iTOL legend than visible on the tree" | The input table contains samples that are not present in the tree. Their values are included in the dataset, resulting in legend entries for categories that don't appear on the tree. Remove samples not present in the tree from your input table to resolve this |

> Compatible with: iTOL v7.51 (other versions not tested). This is an unofficial tool for generating annotation files compatible with iTOL and is not affiliated with or endorsed by iTOL.