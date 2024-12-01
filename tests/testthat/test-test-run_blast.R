test_that("run_blast handles missing sequences gracefully", {
  expect_error(run_blast(character(0), database = "mock_db"), "No sequences provided")
})

test_that("run_blast handles invalid database paths gracefully", {
  expect_error(run_blast(c("ATGCGTACGTAG"), database = "invalid_path"), "Failed to connect to BLAST database")
})

test_that("run_blast performs remote BLAST correctly", {
  skip_if_offline()
  mock_sequences <- c("ATGCGTACGTAG", "CGTAGCTAGCTA")
  result <- run_blast(mock_sequences, database = "nt", remote = TRUE)

  # Validate remote BLAST response
  expect_type(result, "character")
  expect_true(grepl("RID", result))  # Placeholder: Check for Request ID in NCBI response
})
