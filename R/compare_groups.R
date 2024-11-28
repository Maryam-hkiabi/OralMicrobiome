#' Comparing oral microbial communities between substance users and non-users
#'doesnt work
#'need to finish
#'
#'
#' @param data Microbiome data with a column for SubstanceUseType.
#' @return A data frame summarizing species differences between groups.
compare_groups <- function(data) {
  result <- data %>%
    group_by(SubstanceUseType, Species) %>%
    summarize(Count = sum(Count), .groups = "drop")

  return(as.data.frame(result))
}


#' Compare Microbial Communities Between Groups
#'
#' This function compares microbial communities between two groups (e.g., substance users vs. non-users).
#' @param data A data frame containing microbial abundance data.
#' @param group_col The column name indicating group membership.
#' @return A data frame with statistical comparison results.
#' @importFrom stats t.test
#' @export
compare_groups <- function(data, group_col) {
  # Function code here
}



