test_that("compare_substances returns correct structure and results", {
  # Test data
  test_data <- data.frame(
    Species = c("A", "A", "A", "B", "B", "C", "C", "C"),
    SubstanceUseType = c("Type1", "Type2", "Type3", "Type1", "Type2", "Type1", "Type2", "Type3"),
    Abundance = c(10, 15, 20, 8, 12, 5, 7, 6)
  )

  # Run the function
  result <- compare_substances(test_data, substance_col = "SubstanceUseType")

  # Check structure
  expect_type(result, "list")
  expect_true(all(c("summarized_data", "anova_results", "visualization") %in% names(result)))

  # Validate summarized_data
  expect_s3_class(result$summarized_data, "data.frame")
  expect_true(all(c("SubstanceUseType", "Species", "TotalCount") %in% colnames(result$summarized_data)))

  # Check that summarized_data includes all combinations
  expected_combinations <- length(unique(test_data$SubstanceUseType)) * length(unique(test_data$Species))
  expect_equal(nrow(result$summarized_data), expected_combinations)

  # Validate ANOVA results
  if (!is.null(result$anova_results)) {
    expect_s3_class(result$anova_results, "data.frame")
    expect_true(all(c("Species", "PairwiseComparisons", "Difference", "p_value") %in% colnames(result$anova_results)))

    # Check that p-values are within a valid range
    expect_true(all(result$anova_results$p_value >= 0 & result$anova_results$p_value <= 1, na.rm = TRUE))
  }

  # Validate visualization
  expect_s3_class(result$visualization, "ggplot")
})

test_that("compare_substances handles invalid input gracefully", {
  # Invalid input: missing required columns
  invalid_data <- data.frame(
    Species = c("A", "B"),
    Abundance = c(10, 15)
  )
  expect_error(compare_substances(invalid_data, substance_col = "SubstanceUseType"),
               "Input data must contain the columns")

  # Invalid input: input is not a data frame
  expect_error(compare_substances(list(Species = c("A")), substance_col = "SubstanceUseType"),
               "Input must be a data frame.")
})
