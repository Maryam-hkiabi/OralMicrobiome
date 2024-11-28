#' Compare microbial communities between different types substance user types
#' need to finish
#' need help with finding the substance use only data
#'
#'
#'
#' @param data Microbiome data with columns for SubstanceUseType and Species.
#' @return A data frame summarizing microbial communities by substance type.
compare_substances <- function(data) {
  # Summarize species counts by SubstanceUseType and Species
  result <- data %>%
    group_by(SubstanceUseType, Species) %>%
    summarize(TotalCount = sum(Count), .groups = "drop")

  return(as.data.frame(result))
}


#' Compare Microbial Communities Between Substance Use Types
#'
#' This function compares microbial communities across different types of substance use.
#' @param data A data frame containing microbial abundance data.
#' @param substance_col The column name indicating substance use type.
#' @return A data frame with comparison results.
#' @importFrom stats aov TukeyHSD
#' @export
compare_substances <- function(data, substance_col) {
  # Function code here
}
