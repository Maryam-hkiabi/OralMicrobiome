test_that("compare_groups performs PERMANOVA correctly", {
  test_data <- data.frame(
    Species = c("A", "A", "B", "B", "C", "C"),
    Group = c("User", "NonUser", "User", "NonUser", "User", "NonUser"),
    Abundance = c(10, 15, 20, 18, 5, 7)
  )

  result <- compare_groups(test_data, group_col = "Group", method = "bray", test_type = "permanova")

  # Check output structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("R_squared", "p_value") %in% colnames(result)))

  # Check R-squared
  expect_true(result$R_squared > 0 & result$R_squared <= 1)

  # Check p-value
  expect_true(result$p_value >= 0 & result$p_value <= 1)
})

test_that("compare_groups performs t-tests correctly", {
  test_data <- data.frame(
    Species = c("A", "A", "B", "B", "C", "C"),
    Group = c("User", "NonUser", "User", "NonUser", "User", "NonUser"),
    Abundance = c(10, 15, 20, 18, 5, 7)
  )

  result <- compare_groups(test_data, group_col = "Group", test_type = "ttest")

  # Check output structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("Species", "Group1_Mean", "Group2_Mean", "p_value") %in% colnames(result)))

  # Check p-value
  expect_true(all(result$p_value >= 0 & result$p_value <= 1, na.rm = TRUE))
})

test_that("compare_groups handles invalid inputs gracefully", {
  invalid_data <- data.frame(Species = c("A", "B"), Abundance = c(10, 15))

  # Missing group column
  expect_error(compare_groups(invalid_data, group_col = "Group"),
               "Input data must contain the columns")

  # Not a data frame
  expect_error(compare_groups(list(Species = c("A")), group_col = "Group"),
               "Input must be a data frame.")
})

test_that("compare_groups handles insufficient group levels for PERMANOVA", {
  single_group_data <- data.frame(
    Species = c("A", "A", "B", "B", "C", "C"),
    Group = c("User", "User", "User", "User", "User", "User"),
    Abundance = c(10, 15, 20, 18, 5, 7)
  )

  expect_error(compare_groups(single_group_data, group_col = "Group", test_type = "permanova"),
               "Grouping variable must have at least two levels.")
})
