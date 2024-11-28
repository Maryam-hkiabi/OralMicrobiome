#' Load and Preprocess Microbiome Data
#'
#' This function loads and preprocesses microbiome data for analysis.
#' It supports CSV, Excel, and JSON file formats.
#'
#' @param file_path A string containing the path to the data file.
#' @param file_type A string specifying the file type: "csv", "excel", or "json". Default is "csv".
#' @return A preprocessed data frame ready for analysis.
#' @details The function ensures that the input data contains the required columns: "SubstanceUseType", "Species", and "Abundance".
#' Unsupported file types will raise an error.
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom jsonlite fromJSON
#' @export
load_data <- function(file_path, file_type = "csv") {
  # Validate file existence
  if (!file.exists(file_path)) {
    stop("The specified file does not exist.")
  }

  # Load data based on file type
  if (file_type == "csv") {
    library(readr)
    data <- read_csv(file_path, show_col_types = FALSE)
  } else if (file_type == "excel") {
    library(readxl)
    data <- read_excel(file_path)
  } else if (file_type == "json") {
    library(jsonlite)
    data <- fromJSON(file_path, flatten = TRUE)
  } else {
    stop("Unsupported file type. Use 'csv', 'excel', or 'json'.")
  }

  # Validate required columns
  required_columns <- c("SubstanceUseType", "Species", "Abundance")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("The data is missing the following required column(s):", paste(missing_columns, collapse = ", ")))
  }

  # Optional preprocessing: Ensure data consistency
  data <- data %>%
    dplyr::mutate(
      SubstanceUseType = as.factor(SubstanceUseType),
      Species = as.character(Species),
      Abundance = as.numeric(Abundance)
    )

  return(data)
}

