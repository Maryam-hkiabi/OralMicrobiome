#' Compare Microbial Communities Between Groups
#'
#' This function compares microbial communities between two groups (e.g., substance users vs. non-users).
#'
#' @param data A data frame containing microbial abundance data. Must include the columns specified in `group_col` and `Species`.
#' @param group_col The column name indicating group membership (e.g., "SubstanceUseType").
#' @return A data frame with statistical comparison results for each species, including p-values.
#' @details The function performs the following steps:
#' - Groups the data by `Species` and compares abundance between two groups using a t-test.
#' - Returns a summary data frame with columns for species, means for each group, and p-values.
#' If any species lacks data for one of the groups, it is excluded from the results.
#' @importFrom dplyr group_by summarize
#' @importFrom magrittr %>%
#' @importFrom stats t.test
#' @export
compare_groups <- function(data, group_col) {
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame.")
  }
  if (!all(c(group_col, "Species", "Abundance") %in% colnames(data))) {
    stop(paste("Input data must contain the columns:", group_col, "Species, and Abundance."))
  }

  # Perform group-wise comparisons
  result <- data %>%
    group_by(Species) %>%
    summarize(
      Group1_Mean = mean(Abundance[get(group_col) == unique(get(group_col))[1]], na.rm = TRUE),
      Group2_Mean = mean(Abundance[get(group_col) == unique(get(group_col))[2]], na.rm = TRUE),
      p_value = tryCatch(
        t.test(
          Abundance[get(group_col) == unique(get(group_col))[1]],
          Abundance[get(group_col) == unique(get(group_col))[2]]
        )$p.value,
        error = function(e) NA
      ),
      .groups = "drop"
    )

  # Assign class
  class(result) <- c("group_comparison", class(result))

  return(result)
}


# Example: Testing the function
# test_data <- data.frame(
#  Species = c("A", "A", "B", "B", "C", "C", "D", "D"),
#  SubstanceUseType = c("User", "NonUser", "User", "NonUser", "User", "NonUser", "User", "NonUser"),
#  Abundance = c(10, 15, 20, 18, 5, 7, 12, NA)
#)

#comparison_results <- compare_groups(test_data, group_col = "SubstanceUseType")
#print(comparison_results)

