
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

OralMicrobiomeSubstanceUse contains 10 functions:

1)  ***load_data()***

Loads and preprocesses microbiome data from a user-specified file (e.g.,
CSV, Excel, JSON). This function ensures that the data is correctly
formatted for analysis.

2)  ***clean_sequences()***

Cleans and formats raw sequence data to prepare it for BLAST analysis,
removing contaminants and standardizing data.

3)  ***run_blast()***

Performs a BLAST search on the provided microbial sequences, querying
NCBI to retrieve taxonomic information for each sequence.

4)  ***download_homd_data()***

Retrieves microbial species data from the Human Oral Microbiome Database
(HOMD), allowing users to obtain reference data on the human oral
microbiome.

5)  ***parse_megan()***

Parses MEGAN output files, extracting taxonomic and functional data.
This function is useful for users who have run microbiome analyses in
MEGAN and want to integrate the results.

6)  ***compare_groups()***

Compares microbial communities between substance users and non-users,
identifying unique or common species between these groups.

7)  ***compare_substances()***

Compares microbial communities across different types of substance users
(e.g., smokers vs. cannabis users).

8)  ***species_table()***

Generates a summary table listing microbial species presence and
frequency across different substance use types.

9)  ***visualize_taxonomy()***

Creates a bar plot or heatmap to visualize the taxonomic profiles,
showing diversity and abundance of species across different groups.

10) ***plot_species_substance()***

Plots the presence and relative abundance of microbial species across
different substance use types, providing a visual comparison.

**This package can run on 3 types of datasets:**

1)  user-provided: load own data in JSON, EXCEL or CSV format

2)  sample-dataset: run package on provided sample dataset already
    inlcuded in this package

3)  retrieve-data: retrieve data directly from Human Oral Microbiome
    Database (HOMD)

## Example output of plots from sample data

<img src="../preview.png" width="100%" /><img src="../preview2.png" width="100%" />

## Contributions

The author of OralMicrobiomSubstanceUse R package is Maryam
Hasanzadehkiabi. The author wrote the 10 functions in this package.

To help with the process of making this package the following were used:

1)  course material from BCB420 Fall 2024

2)  online textbook R Packages (2e) by Hadley Wickham and Jennifer Bryan

## References

Wickham, H. and J. Bryan (2019). R Packages (2nd edition). Newton,
Massachusetts: O’Reilly Media. <https://r-pkgs.org/>. Accessed 5
November 2024

Silva, Anjali. TestingPackage. GitHub,
<https://github.com/anjalisilva/TestingPackage>. Accessed 5 November
2024.

OpenAI. ChatGPT. Accessed 5 November 2024.

## Acknowledgements

This package was developed as part of an assessment for 2024 BCB410H:
Applied Bioinformatics course at the University of Toronto, Toronto,
CANADA. By student Maryam Hasanzadehkiabi. OralMicrobiomeSubstanceUse
welcomes issues, enhancement requests, and other contributions. To
submit an issue, use the GitHub issues.
