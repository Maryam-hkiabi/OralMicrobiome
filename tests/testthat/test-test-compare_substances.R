test_that("compare_substances performs comparisons correctly", {
  # Test data
  test_data <- data.frame(
    Species = c("A", "A", "A", "B", "B", "C", "C", "C"),
    SubstanceUseType = c("Type1", "Type2", "Type3", "Type1", "Type2", "Type1", "Type2", "Type3"),
    Abundance = c(10, 15, 20, 8, 12, 5, 7, 6)
  )

  # Run the function
  results <- compare_substances(test_data, substance_col = "SubstanceUseType")

  # Test summarized data
  summarized <- results$summarized_data
  expect_true("Species" %in% colnames(summarized))
  expect_true("TotalCount" %in% colnames(summarized))
  expect_equal(nrow(summarized), 8)  # 8 rows of summarized data

  # Check specific summarized values
  expect_equal(summarized$TotalCount[summarized$Species == "A" & summarized$SubstanceUseType == "Type1"], 10)
  expect_equal(summarized$TotalCount[summarized$Species == "C" & summarized$SubstanceUseType == "Type3"], 6)

  # Test ANOVA results
  anova_results <- results$anova_results
  expect_true("Species" %in% colnames(anova_results))
  expect_true("p_value" %in% colnames(anova_results))
  expect_gt(nrow(anova_results), 0)  # At least one ANOVA result
  expect_true(all(anova_results$p_value > 0 & anova_results$p_value <= 1))  # Valid p-values
})
