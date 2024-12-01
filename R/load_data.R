#' Load and Preprocess Microbiome Data
#'
#' This function loads and preprocesses microbiome data for analysis.
#' It supports CSV, Excel, JSON, and FASTA file formats.
#'
#' @param file_path A string containing the path to the data file.
#' @param file_type A string specifying the file type: "csv", "excel", "json", or "fasta". Default is "csv".
#' @return A preprocessed data frame ready for analysis.
#' @details The function ensures that the input data contains the required columns: "SubstanceUseType", "Species", and "Abundance".
#' Unsupported file types will raise an error.
#' For FASTA files, the function extracts sequence names and their corresponding sequences.
#' @import readr
#' @import readxl
#' @importFrom jsonlite fromJSON
#' @import dplyr
#' @import Biostrings
#' @export

load_data <- function(file_path, file_type = "csv") {
  # Validate file existence
  if (!file.exists(file_path)) {
    stop("The specified file does not exist.")
  }

  # Load data based on file type
  data <- switch(
    file_type,
    "csv" = {
      library(readr)
      read_csv(file_path, show_col_types = FALSE)
    },
    "excel" = {
      library(readxl)
      read_excel(file_path)
    },
    "json" = {
      library(jsonlite)
      fromJSON(file_path, flatten = TRUE)
    },
    "fasta" = {
      library(Biostrings)
      fasta_data <- readDNAStringSet(file_path)
      data.frame(
        SequenceName = names(fasta_data),
        Sequence = as.character(fasta_data)
      )
    },
    stop("Unsupported file type. Use 'csv', 'excel', 'json', or 'fasta'.")
  )

  # If the file type is FASTA, skip column validation
  if (file_type == "fasta") {
    return(data)
  }

  # Validate required columns
  required_columns <- c("SubstanceUseType", "Species", "Abundance")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("The data is missing the following required column(s):", paste(missing_columns, collapse = ", ")))
  }

  # Preprocessing: Ensure data consistency
  data <- data %>%
    dplyr::mutate(
      SubstanceUseType = as.factor(SubstanceUseType),
      Species = as.character(Species),
      Abundance = as.numeric(Abundance)
    )

  return(data)
}
