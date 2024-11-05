test_that("load_data loads data correctly", {
  # Sample data for testing
  test_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(SubstanceUseType = c("Smoker", "Cannabis", "Non-user"), Species = c("Streptococcus", "Prevotella", "Fusobacterium"), Count = c(50, 30, 20)), test_file, row.names = FALSE)

  # Test loading CSV data
  data <- load_data(test_file)

  # Expectations
  expect_s3_class(data, "data.frame")
  expect_equal(ncol(data), 3)  # Check column count
  expect_equal(nrow(data), 3)  # Check row count

  # Check that column names include 'SubstanceUseType'
  expect_true("SubstanceUseType" %in% colnames(data))
})
