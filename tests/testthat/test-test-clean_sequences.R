test_that("clean_sequences formats sequence data correctly", {
  # Sample raw data
  raw_data <- data.frame(SequenceID = c("Seq1", "Seq2"), Sequence = c("ATGC", "CGTA"))

  # Test cleaning function
  cleaned_data <- clean_sequences(raw_data)

  # Expectations
  expect_s3_class(cleaned_data, "data.frame")
  expect_equal(ncol(cleaned_data), ncol(raw_data))  # Ensure column count remains the same
  expect_equal(nrow(cleaned_data), nrow(raw_data))  # Ensure row count remains the same
})
