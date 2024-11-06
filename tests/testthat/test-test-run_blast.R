test_that("run_blast retrieves BLAST results correctly", {
  # from sample dataset
  sequences <- c("ATGC", "CGTA", "GCTA")

  result <- run_blast(sequences)

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), length(sequences))
  expect_true(all(c("SequenceID", "Species", "Identity") %in% colnames(result)))
})
