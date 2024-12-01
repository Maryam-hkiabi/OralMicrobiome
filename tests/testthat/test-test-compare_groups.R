test_that("compare_groups performs group comparisons correctly", {
  # Test data
  test_data <- data.frame(
    Species = c("A", "A", "B", "B", "C", "C", "D", "D"),
    SubstanceUseType = c("User", "NonUser", "User", "NonUser", "User", "NonUser", "User", "NonUser"),
    Abundance = c(10, 15, 20, 18, 5, 7, 12, NA)  # Includes missing value for species D
  )

  # Run the function
  result <- compare_groups(test_data, group_col = "SubstanceUseType")

  # Check class
  expect_s3_class(result, "group_comparison")

  # Check columns
  expect_true(all(c("Species", "Group1_Mean", "Group2_Mean", "p_value") %in% colnames(result)))

  # Check number of rows
  expect_equal(nrow(result), 3)  # Excludes species with missing data (e.g., species D)

  # Check p-value calculation
  expect_true(all(!is.na(result$p_value)))  # No NA p-values
  expect_true(all(result$p_value >= 0 & result$p_value <= 1))  # P-values in [0, 1]
})

test_that("compare_groups handles missing groups gracefully", {
  # Test data with only one group
  single_group_data <- data.frame(
    Species = c("A", "A", "B", "B"),
    SubstanceUseType = c("User", "User", "User", "User"),
    Abundance = c(10, 15, 20, 18)
  )

  # Run the function
  result <- compare_groups(single_group_data, group_col = "SubstanceUseType")

  # Expect an empty result
  expect_equal(nrow(result), 0)
})

test_that("compare_groups handles no valid species", {
  # Test data with no valid rows
  invalid_data <- data.frame(
    Species = c("A", "A"),
    SubstanceUseType = c("User", "NonUser"),
    Abundance = c(NA, NA)
  )

  # Run the function
  result <- compare_groups(invalid_data, group_col = "SubstanceUseType")

  # Expect an empty data frame
  expect_equal(nrow(result), 0)
})

test_that("compare_groups stops on invalid input", {
  # Missing columns
  invalid_data <- data.frame(Species = c("A"), Abundance = c(10))
  expect_error(compare_groups(invalid_data, group_col = "SubstanceUseType"),
               "Input data must contain the columns")

  # Not a data frame
  expect_error(compare_groups(list(Species = c("A")), group_col = "SubstanceUseType"),
               "Input data must be a data frame.")
})
