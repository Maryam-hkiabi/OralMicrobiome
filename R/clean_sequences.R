#' Clean and format sequences for BLAST input
#'
#' This function cleans and formats raw sequence data for compatibility with BLAST.
#'
#' @param raw_data A data frame with raw sequence data. Must include a "Sequence" column.
#' @return A data frame with cleaned sequences and assigned class "cleaned_sequences".
#' @details The function performs the following steps:
#' - Ensures the input is a data frame and contains a "Sequence" column.
#' - Removes rows where the "Sequence" column is `NA` or empty.
#' - Trims whitespace from sequences.
#' - Ensures the returned data frame retains the same column structure as the input.
#' If no valid sequences are found after cleaning, the function returns an empty data frame
#' with the same columns as the input and issues a warning.
#' @importFrom dplyr filter mutate
#' @importFrom magrittr %>%
#' @export
clean_sequences <- function(raw_data) {
  # Check if the input is a data frame
  if (!is.data.frame(raw_data)) {
    stop("Input must be a data frame.")
  }

  # Ensure the raw_data has the required column "Sequence"
  if (!"Sequence" %in% colnames(raw_data)) {
    stop("The input data frame must have a column named 'Sequence'.")
  }

  # Example cleaning step: Remove rows with invalid or empty sequences
  cleaned_data <- raw_data %>%
    dplyr::filter(!is.na(Sequence) & Sequence != "") %>%
    dplyr::mutate(Sequence = gsub("\\s+", "", Sequence))  # Remove whitespace

  # Handle edge case: No valid sequences remain after cleaning
  if (nrow(cleaned_data) == 0) {
    warning("No valid sequences found after cleaning.")
  }

  # Assign an S3 class to the output
  class(cleaned_data) <- c("cleaned_sequences", class(cleaned_data))

  return(cleaned_data)
}

# Example: Testing the function
# test_data <- data.frame(
#  ID = c(1, 2, 3, 4),
#  Sequence = c(" ATCG ", " GCTA  ", "", NA),
#  stringsAsFactors = FALSE
#)

# Run the function
#cleaned_data <- clean_sequences(test_data)

# Print the output
#print(cleaned_data)

# Check the class
#class(cleaned_data)

