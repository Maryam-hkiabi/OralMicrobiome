test_that("clean_sequences formats sequence data correctly", {
  # testing on sample data
  raw_data <- data.frame(SequenceID = c("Seq1", "Seq2"), Sequence = c("ATGC", "CGTA"))

  cleaned_data <- clean_sequences(raw_data)

  expect_s3_class(cleaned_data, "data.frame")
  expect_equal(ncol(cleaned_data), ncol(raw_data))
  expect_equal(nrow(cleaned_data), nrow(raw_data))
})
