test_that("compare_substances compares substance types correctly", {
  # Sample data for testing
  data <- data.frame(
    SubstanceUseType = c("Smoker", "Cannabis", "Smoker", "Cannabis"),
    Species = c("Streptococcus", "Prevotella", "Neisseria", "Veillonella"),
    Count = c(50, 30, 40, 25)
  )


  result <- compare_substances(data)

  #  expectations
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3)
  expect_true(all(c("SubstanceUseType", "Species", "TotalCount") %in% colnames(result)))
  expect_equal(nrow(result), 4)
})
