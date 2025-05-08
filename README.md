# AUTNES Data Dashboard

This dashboard provides interactive visualizations of data from the AUTNES Online Panel Study. It allows users to explore various political variables over time, analyze mean values, and examine grouped mean values by demographic subgroups such as gender, age, and education level.

This is the R code package for executing the dashboard currently provided at http://131.130.71.40:3838/autnes/

Current Wave implemented: #24

Feedback or comments to: philipp.thannen@tuwien.ac.at or pthannen@ethz.ch.

## Directory Structure

Your project directory should be organized as follows:
```
- README.md (this file)
- flex_dash.Rmd (the dashboard to execute)
- data/
    - dat_percentage.rds
    - dat_means.rds
    - dat_grouped.rds
    - dat_nodes.rds
    - dat_links.rds
    - mappings_variable_text.xlsx
    - AUTNES-Logo(darkred).png
    - mobile-warning.html
    - fa6.html
```

If you want to change the existing relative paths for data files and image (not needed for excuting in the current configuration and structure) adapt the setup chunk at the beginning of flex_dash.Rmd:

```
# -----------------------------------------------------------------------------
# Configuration - Adapt paths here! Adapt election dates here!
# -----------------------------------------------------------------------------
# AUTNES logo file
path_logo <- "data/AUTNES-Logo(darkred).png"

# rds files
path_percentage <- "data/dat_percentage.rds"
path_means <- "data/dat_means.rds"
path_grouped <- "data/dat_grouped.rds"
path_sankey_nodes <- "data/dat_nodes.rds"
path_sankey_links <- "data/dat_links.rds"
```

## Prerequisites

Before running the Dashboard locally, ensure you have the following:

- R (version 4.0 or higher)
- RStudio (optional but recommended locally)
- The following R packages installed:
```r
install.packages(c(
  "shiny", "ggplot2", "dplyr", "plotly", "base64enc", 
  "flexdashboard", "RColorBrewer", "networkD3", "readxl"
))
```

## Running the Dashboard locally
You can view the dashboard online or run the dashboard using RStudio or from the R console.

### Option 1: RStudio

1. Open flex_dash.Rmd in RStudio.
2. Click the Run Document button at the top of the editor.
3. The dashboard will compile and launch in your R or default web browser.

### Option 2: R Console

1. Open your terminal or command prompt.
2. Navigate to the directory containing flex_dash.Rmd.
3. Start R by typing R and pressing Enter.
4. Run the following command:
```r
rmarkdown::run("flex_dash.Rmd")
```
