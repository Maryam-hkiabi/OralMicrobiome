test_that("species_table generates a summary table correctly", {
  # Mock data
  mock_data <- data.frame(
    SubstanceUseType = c("Type1", "Type1", "Type2", "Type2", "Type3"),
    Species = c("Species1", "Species2", "Species1", "Species3", "Species2"),
    Count = c(10, 15, 20, 5, 7)
  )

  # Generate species table
  result <- species_table(mock_data, include_hmp = FALSE)

  # Validate the result
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("SubstanceUseType", "Species", "TotalCount"))
  expect_true(all(result$SubstanceUseType %in% c("Type1", "Type2", "Type3")))
})

test_that("species_table integrates HMP16SData correctly", {
  skip_if_not(requireNamespace("HMP16SData", quietly = TRUE), "HMP16SData package not installed.")

  # Mock data
  mock_data <- data.frame(
    SubstanceUseType = c("Type1", "Type1"),
    Species = c("Species1", "Species2"),
    Count = c(10, 15)
  )

  # Generate species table with HMP integration
  result <- species_table(mock_data, include_hmp = TRUE)

  # Validate combined data
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > nrow(mock_data))
  expect_true("HMP Reference" %in% result$SubstanceUseType)
})

test_that("species_table handles missing columns gracefully", {
  # Mock data with missing columns
  mock_data <- data.frame(
    Species = c("Species1", "Species2"),
    Count = c(10, 15)
  )

  # Expect error
  expect_error(species_table(mock_data), "missing the following required column")
})

test_that("species_table handles invalid input gracefully", {
  # Invalid input (not a data frame)
  expect_error(species_table("invalid_input"), "must be a data frame")
})
