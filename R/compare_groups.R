#' Compare Oral microbial communities between substance users and non-users
#' need to finish
#' need to somehow demonstrate and find data that belongs to substance users and non-users and differentiate them
#'
#' @param data Oral Microbiome data with a column for SubstanceUseType.
#' @return A data frame summarizing species differences between groups.
compare_groups <- function(data) {
  # Example for sample data code: summarize species by SubstanceUseType
  result <- data %>%
    group_by(SubstanceUseType, Species) %>%
    summarize(Count = sum(Count), .groups = "drop")

  return(as.data.frame(result))
}


