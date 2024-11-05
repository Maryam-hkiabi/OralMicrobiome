test_that("run_blast retrieves BLAST results correctly", {
  # Sample input sequence
  sequences <- c("ATGC", "CGTA")

  result <- run_blast(sequences)

  # Expectations
  expect_s3_class(result, "data.frame")
})
