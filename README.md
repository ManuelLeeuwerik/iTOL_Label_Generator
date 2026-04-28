# iTOL Label Generator
This Shiny app converts tabular metadata into annotation files for iTOL.
If you're new to iTOL, see the quick guide below.

## Running the app
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

**Note:**
- If the input table contains samples that are not present in the tree, their values may still be included when generating the dataset. This can result in legend entries for categories that are not visible on the tree.
- Symbol sizes in DATASET_SYMBOL datasets appear different when switching between circular and rectangular tree layouts. This is due to how iTOL scales symbols. As a result, symbol sizes may need to be adjusted manually within the iTOL interface depending on the selected layout.
- NAs filtered from set so if you want to note missing data points you can add Unknown or Not done for instead.