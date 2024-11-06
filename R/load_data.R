#' Load Oral microbiome data from a file (sample included in package)
#' first function
#'
#' @param file_path Path to the data file (e.g., CSV, Excel, JSON).
#' @return A data frame with loaded oral microbiome data.
load_data <- function(file_path, file_type = "csv") {
  if (file_type == "csv") {
    data <- read.csv(file_path)
  } else if (file_type == "excel") {
    library(readxl)
    data <- read_excel(file_path)
  } else if (file_type == "json") {
    library(jsonlite)
    data <- fromJSON(file_path, flatten = TRUE)
  } else {
    stop("Unsupported file type. Use 'csv', 'excel', or 'json'.")
  }

  if (!"SubstanceUseType" %in% colnames(data)) {
    stop("The data must include a 'SubstanceUseType' column.")
  }

  return(data)
}

