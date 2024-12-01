#' Compare Microbial Communities Between Groups
#'
#' This function compares microbial communities between two groups using PERMANOVA or t-tests.
#'
#' @param data A data frame containing microbial abundance data.
#' @param group_col The name of the column indicating group membership (e.g., "Group").
#' @param method The distance method for community comparison (e.g., "bray", "jaccard"). Defaults to "bray".
#' @param test_type The statistical test to perform: "permanova" or "ttest".
#' @return A data frame with statistical results.
#' @import vegan
#' @importFrom reshape2 acast
#' @import dplyr
#' @importFrom stats t.test
#' @export

compare_groups <- function(data, group_col, method = "bray", test_type = "permanova") {
  # Validate input
  if (!is.data.frame(data)) stop("Input must be a data frame.")
  if (!all(c(group_col, "Species", "Abundance") %in% colnames(data))) {
    stop(paste("Input data must contain the columns:", group_col, "Species, and Abundance."))
  }

  if (test_type == "permanova") {
    # Convert to wide-format matrix for distance calculation
    abundance_matrix <- reshape2::acast(
      data,
      data[[group_col]] ~ Species,
      value.var = "Abundance",
      fill = 0
    )

    # Check if matrix is valid
    if (nrow(abundance_matrix) == 0 || ncol(abundance_matrix) == 0) {
      stop("Abundance matrix is empty. Check input data.")
    }

    # Calculate distance matrix
    distance_matrix <- vegan::vegdist(abundance_matrix, method = method)

    # Prepare grouping variable for PERMANOVA
    group_vector <- rownames(abundance_matrix)

    # Debugging: Print inputs
    print("Debug: Distance matrix")
    print(distance_matrix)
    print("Debug: Group vector")
    print(group_vector)

    # Check if grouping is valid
    if (length(unique(group_vector)) < 2) stop("Grouping variable must have at least two levels.")

    # Perform PERMANOVA using adonis2 with more permutations
    adonis_results <- vegan::adonis2(distance_matrix ~ group_vector, permutations = 999)

    # Debugging: Print adonis2 results
    print("Debug: Adonis2 results")
    print(adonis_results)

    # Summarize results
    r_squared <- adonis_results$R2[1]
    p_value <- adonis_results$`Pr(>F)`[1]

    if (is.na(p_value)) {
      warning("PERMANOVA could not calculate a valid p-value. This may be due to insufficient data variability.")
    }

    # Check for valid p-value and residuals
    if (is.na(adonis_results$`Pr(>F)`[1]) || adonis_results$Df[2] == 0) {
      warning("PERMANOVA could not calculate a valid p-value due to insufficient data variability.")
      return(data.frame(R_squared = adonis_results$R2[1], p_value = NA))
    }


    return(data.frame(
      R_squared = r_squared,
      p_value = ifelse(is.na(p_value), NA, p_value)
    ))

  } else if (test_type == "ttest") {

    # Perform T-Test
    test_results <- data %>%
      dplyr::filter(!is.na(Abundance)) %>%
      dplyr::group_by(Species) %>%
      dplyr::summarise(
        Group1_Mean = mean(Abundance[data[[group_col]] == unique(data[[group_col]])[1]], na.rm = TRUE),
        Group2_Mean = mean(Abundance[data[[group_col]] == unique(data[[group_col]])[2]], na.rm = TRUE),
        p_value = tryCatch(
          stats::t.test(
            Abundance[data[[group_col]] == unique(data[[group_col]])[1]],
            Abundance[data[[group_col]] == unique(data[[group_col]])[2]]
          )$p.value,
          error = function(e) NA
        ),
        .groups = "drop"
      )

    return(as.data.frame(test_results))
  } else {
    stop("Invalid test_type specified. Use 'permanova' or 'ttest'.")
  }
}





# Testing Example:
# test_data <- data.frame(
#  Species = c("A", "A", "B", "B", "C", "C", "A", "B"),
#  Group = c("User", "NonUser", "User", "NonUser", "User", "NonUser", "User", "NonUser"),
#  Abundance = c(10, 15, 20, 18, 5, 7, 12, 22)
# )


# Test PERMANOVA
#result_permanova <- compare_groups(test_data, group_col = "Group", method = "bray", test_type = "permanova")
#print("PERMANOVA Results:")
#print(result_permanova)

# Test T-Test
#result_ttest <- compare_groups(test_data, group_col = "Group", test_type = "ttest")
#print("T-Test Results:")
#print(result_ttest)


