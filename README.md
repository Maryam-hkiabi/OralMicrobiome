
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OralMicrobiomeSubstanceUse

<!-- badges: start -->
<!-- badges: end -->

## Description

The OralMicrobiomeSubstanceUse package provides tools for analyzing and
comparing microbial species in the oral microbiome of individuals who
use substances, including tobacco, cannabis, and other drugs. This
package is designed to support research on microbial diversity and
distribution within substance user groups, helping to identify common or
unique microbial species associated with each type of substance use. By
facilitating comparisons across different user groups, the package fills
a gap in bioinformatics workflows focused on the oral microbiome of
substance users.

This package is particularly beneficial for researchers studying the
impact of substance use on oral health, as it allows for the
visualization of microbial species variations and outputs summary tables
that aid in exploring these relationships. The tools offered improve
workflows by integrating microbial species data analysis, comparison
functions, and visualization capabilities within a single package,
streamlining analyses that would otherwise require separate tools and
significant manual processing.

The package was developed using R version 4.4.1 and on platform
x86_6-apple-danwin20 Mac, running under macOS Sonoma 14.3.

## Installation

You can install the development version of OralMicrobiomeSubstanceUse
from [GitHub](https://github.com/) with:

``` r
# Install devtools if you haven't already
install.packages("devtools")
library("devtools")

# Install OralMicrobiomeSubstanceUse from GitHub
devtools::install_github("Maryam-hkiabi/OralMicrobiomeSubstanceUse", build_vignettes = TRUE)

# Load the package
library("OralMicrobiomeSubstanceUse")

Note: The Shiny app for this package is currently under construction.
```

To run the shinyApp: Under construction

## Overview

``` r
# List of all functions and datasets in the package
ls("package:OralMicrobiomeSubstanceUse")

# List of any included datasets, if available
data(package = "OralMicrobiomeSubstanceUse")

# Access the package vignettes for a tutorial
browseVignettes("OralMicrobiomeSubstanceUse")
```

OralMicrobiomeSubstanceUse contains 9 functions:

1)  run_blast()

Purpose: Runs a BLAST search on microbial sequences to identify species.

2)  parse_megan()

Purpose: Parses MEGAN output files to extract taxonomic and functional
data.

3)  clean_sequences()

Purpose: Cleans and formats raw sequence data to prepare for BLAST
analysis.

4)  visualize_taxonomy()

Purpose: Creates bar plots or heatmaps to visualize taxonomic profiles.

5)  download_homd_data()

Purpose: Retrieves microbial species data from the Human Oral Microbiome
Database (HOMD).

6)  compare_groups()

Purpose: Compares microbial communities between substance users and
non-users.

7)  compare_substances()

Purpose: Analyzes differences in microbial communities between types of
substance users.

8)  species_table()

Purpose: Generates a table listing microbial species for each type of
substance use.

9)  plot_species_substance()

Purpose: Creates visualizations showing the presence and relative
abundance of microbial species by substance use type.

This package includes xxx dataset and yyy dataset

## Contributions

## References

## Acknowledgements

This package was developed as part of an assessment for 2024 BCB410H:
Applied Bioinformatics course at the University of Toronto, Toronto,
CANADA. By student Maryam Hasanzadehkiabi. OralMicrobiomeSubstanceUse
welcomes issues, enhancement requests, and other contributions. To
submit an issue, use the GitHub issues.
