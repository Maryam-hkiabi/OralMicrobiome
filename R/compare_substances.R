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
