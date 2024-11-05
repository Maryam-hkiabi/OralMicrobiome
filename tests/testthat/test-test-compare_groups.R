test_that("compare_groups compares groups correctly", {

  # Sample data for testing
  data <- data.frame(SubstanceUseType = c("Smoker", "Non-user", "Smoker"), Species = c("Streptococcus", "Fusobacterium", "Neisseria"))

  # Test comparison function
  result <- compare_groups(data)

  # Expectations
  expect_s3_class(result, "data.frame")
})
