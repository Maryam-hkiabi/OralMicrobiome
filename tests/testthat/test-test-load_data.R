test_that("load_data handles different file types and validates data correctly", {
  # Create temporary test files
  csv_file <- tempfile(fileext = ".csv")
  json_file <- tempfile(fileext = ".json")
  excel_file <- tempfile(fileext = ".xlsx")

  # Example data
  test_data <- data.frame(
    SubstanceUseType = c("User", "NonUser", "User"),
    Species = c("A", "B", "C"),
    Abundance = c(10, 15, 20)
  )

  # Save as CSV
  write.csv(test_data, csv_file, row.names = FALSE)
  # Save as JSON
  library(jsonlite)
  write_json(test_data, json_file)
  # Save as Excel
  library(openxlsx)
  write.xlsx(test_data, excel_file)

  # Test loading CSV
  csv_data <- load_data(csv_file, file_type = "csv")
  expect_true(is.data.frame(csv_data))
  expect_equal(nrow(csv_data), 3)

  # Test loading JSON
  json_data <- load_data(json_file, file_type = "json")
  expect_true(is.data.frame(json_data))
  expect_equal(nrow(json_data), 3)

  # Test loading Excel
  excel_data <- load_data(excel_file, file_type = "excel")
  expect_true(is.data.frame(excel_data))
  expect_equal(nrow(excel_data), 3)

  # Test missing columns
  incomplete_data <- data.frame(
    SubstanceUseType = c("User", "NonUser"),
    Species = c("A", "B")
  )
  write.csv(incomplete_data, csv_file, row.names = FALSE)
  expect_error(load_data(csv_file, file_type = "csv"), "The data is missing the following required column")

  # Test non-existent file
  expect_error(load_data("nonexistent.csv", file_type = "csv"), "The specified file does not exist.")
})
