test_that("compare_substances compares substance types correctly", {

  # Sample data
  data <- data.frame(SubstanceUseType = c("Smoker", "Cannabis", "Smoker"), Species = c("Streptococcus", "Prevotella", "Neisseria"))

  # Test comparison function
  result <- compare_substances(data)

  # Expectations
  expect_s3_class(result, "data.frame")
})
