test_that("compare_groups performs group comparisons correctly", {
  # Test data
  test_data <- data.frame(
    Species = c("A", "A", "B", "B", "C", "C", "D", "D"),
    SubstanceUseType = c("User", "NonUser", "User", "NonUser", "User", "NonUser", "User", "NonUser"),
    Abundance = c(10, 15, 20, 18, 5, 7, 12, NA)
  )

  # Run the function
  result <- compare_groups(test_data, group_col = "SubstanceUseType")

  # Check class
  expect_s3_class(result, "group_comparison")

  # Check columns
  expect_true(all(c("Species", "Group1_Mean", "Group2_Mean", "p_value") %in% colnames(result)))

  # Check number of rows
  expect_equal(nrow(result), 3)  # Excludes species with invalid comparisons (e.g., NA values)

  # Check p-value calculation
  expect_true(!is.na(result$p_value[1]))
  expect_true(result$p_value[1] < 1)  # Ensure the p-value is reasonable
})
