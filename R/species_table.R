#' Generates a summary species table by substance use type
#' this need to be finished
#' need to first find data that belongs to substance use only
#'
#'
#'
#' @param data Microbiome data with columns for SubstanceUseType and Species.
#' @return A data frame with species counts by substance use type.
species_table <- function(data) {
  result <- data %>%
    group_by(SubstanceUseType, Species) %>%
    summarize(TotalCount = sum(Count), .groups = "drop")

  return(as.data.frame(result))
}


#' Generate a Table of Microbial Species by Substance Use Type
#'
#' This function generates a table showing microbial species present in each substance use type.
#' @param data A data frame containing microbial species and substance use data.
#' @return A summary table of microbial species.
#' @importFrom dplyr group_by summarize
#' @export
species_table <- function(data) {
  # Function code here
}
