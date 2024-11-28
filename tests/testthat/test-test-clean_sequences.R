test_that("clean_sequences formats sequence data correctly", {
  # Test data
  raw_data <- data.frame(
    ID = c(1, 2, 3, 4),
    Sequence = c("ATCG", "GCTA", "", NA),
    stringsAsFactors = FALSE
  )

  # Run the function
  cleaned_data <- clean_sequences(raw_data)

  # Check that the output is an S3 object with the correct class
  expect_s3_class(cleaned_data, "cleaned_sequences")

  # Check dimensions
  expect_equal(ncol(cleaned_data), ncol(raw_data))  # Columns remain the same
  expect_equal(nrow(cleaned_data), 2)              # Only 2 valid rows remain

  # Check that column names are unchanged
  expect_true(all(colnames(cleaned_data) == colnames(raw_data)))

  # Check the content of the cleaned data
  expect_equal(cleaned_data$Sequence, c("ATCG", "GCTA"))
})

test_that("clean_sequences handles no valid sequences correctly", {
  raw_data <- data.frame(
    ID = c(1, 2),
    Sequence = c("", NA),
    stringsAsFactors = FALSE
  )

  # Run the function
  cleaned_data <- clean_sequences(raw_data)

  # Expect an empty data frame with the same columns
  expect_equal(nrow(cleaned_data), 0)  # No rows remain
  expect_equal(ncol(cleaned_data), ncol(raw_data))  # Columns remain the same
  expect_true(all(colnames(cleaned_data) == colnames(raw_data)))
})

test_that("clean_sequences trims whitespace from sequences", {
  raw_data <- data.frame(
    ID = c(1, 2),
    Sequence = c(" ATCG  ", "  GCTA"),
    stringsAsFactors = FALSE
  )

  # Run the function
  cleaned_data <- clean_sequences(raw_data)

  # Check cleaned content
  expect_equal(cleaned_data$Sequence, c("ATCG", "GCTA"))
})

test_that("clean_sequences stops on invalid input", {
  # Input is not a data frame
  expect_error(clean_sequences(list(Sequence = c("ATCG", "GCTA"))), "Input must be a data frame.")

  # Missing "Sequence" column
  raw_data <- data.frame(ID = c(1, 2))
  expect_error(clean_sequences(raw_data), "The input data frame must have a column named 'Sequence'.")
})
