#' Generate a Table of Microbial Species by Substance Use Type
#'
#' This function generates a table showing microbial species counts across different substance use types.
#' Optionally integrates data from HMP16SData for additional context.
#'
#' @param data A data frame containing microbial species and substance use data.
#' Must include columns: "SubstanceUseType", "Species", and "Count".
#' @param include_hmp Logical. If `TRUE`, integrates data from HMP16SData. Default is `FALSE`.
#' @return A data frame with species counts summarized by substance use type.
#' @details The function groups data by `SubstanceUseType` and `Species` to calculate total counts,
#' and optionally merges data from HMP16SData for reference.
#' @import dplyr
#' @import SummarizedExperiment
#' @import ExperimentHub
#' @export
species_table <- function(data, include_hmp = FALSE) {
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  required_columns <- c("SubstanceUseType", "Species", "Count")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("The data is missing the following required column(s):", paste(missing_columns, collapse = ", ")))
  }

  library(dplyr)

  # Summarize species counts by substance use type
  summarized_data <- data %>%
    group_by(SubstanceUseType, Species) %>%
    summarize(TotalCount = sum(Count, na.rm = TRUE), .groups = "drop")

  # Integrate HMP16SData if requested
  if (include_hmp) {
    if (!requireNamespace("HMP16SData", quietly = TRUE)) {
      stop("The HMP16SData package is required but not installed. Please install it using Bioconductor.")
    }

    tryCatch({
      library(SummarizedExperiment)
      library(ExperimentHub)
      hub <- ExperimentHub::ExperimentHub()
      hmp_dataset <- hub[["EH2786"]]  # Example ID for an HMP16SData dataset
      hmp_data <- as.data.frame(SummarizedExperiment::assay(hmp_dataset))
      hmp_data <- hmp_data %>%
        dplyr::mutate(
          SubstanceUseType = "HMP Reference",
          Species = rownames(hmp_data),
          Count = rowSums(hmp_data, na.rm = TRUE)
        ) %>%
        select(SubstanceUseType, Species, Count)

      # Combine HMP data with summarized data
      summarized_data <- dplyr::bind_rows(summarized_data, hmp_data)
    }, error = function(e) {
      warning("Failed to load HMP16SData: ", e$message)
    })
  }

  return(as.data.frame(summarized_data))
}
