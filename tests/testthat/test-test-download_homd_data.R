test_that("download_homd_data retrieves HOMD data correctly", {
  # Test: Fetch all data
  all_data <- download_homd_data()
  expect_true(is.data.frame(all_data))  # Ensure it returns a data frame
  expect_true(nrow(all_data) > 0)  # Ensure the data frame has rows

  # Test: Fetch data for a specific genus
  genus_data <- download_homd_data(genus = "Prevotella")
  expect_true(is.data.frame(genus_data))  # Ensure it returns a data frame
  expect_true(nrow(genus_data) > 0)  # Ensure the data frame has rows
  expect_true(any(grepl("Prevotella", genus_data$genus, ignore.case = TRUE)))  # Ensure genus filtering works
})
