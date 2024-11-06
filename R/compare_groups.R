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




