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
#' @import dplyr
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

  # Debugging: Print unique groups and counts
  print("Unique groups in data:")
  print(table(data[[group_col]]))

  # Perform group-wise comparisons
  result <- data %>%
    filter(!is.na(Abundance)) %>%  # Remove rows with NA abundance
    group_by(Species) %>%
    summarize(
      Group1_Mean = mean(Abundance[data[[group_col]] == unique(data[[group_col]])[1]], na.rm = TRUE),
      Group2_Mean = mean(Abundance[data[[group_col]] == unique(data[[group_col]])[2]], na.rm = TRUE),
      p_value = if (length(unique(data[[group_col]])) == 2) {
        tryCatch(
          t.test(
            Abundance[data[[group_col]] == unique(data[[group_col]])[1]],
            Abundance[data[[group_col]] == unique(data[[group_col]])[2]]
          )$p.value,
          error = function(e) NA
        )
      } else {
        NA
      },
      .groups = "drop"
    ) %>%
    filter(!is.na(p_value))  # Remove rows with invalid p-values

  # Debugging: Print intermediate result
  print("Resulting comparison:")
  print(result)

  # Assign class
  class(result) <- c("group_comparison", class(result))

  return(result)
}


# Testing Example:
test_data <- data.frame(
  Species = c("Bacteria1", "Bacteria1", "Bacteria2", "Bacteria2"),
  SubstanceUseType = c("User", "Non-User", "User", "Non-User"),
  Abundance = c(10, 15, 20, 25)
)

# Run the function
result <- compare_groups(test_data, group_col = "SubstanceUseType")

# Print the results
print(result)
