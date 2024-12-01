test_that("run_blast performs local BLAST correctly", {
  skip_if_not(requireNamespace("rBLAST", quietly = TRUE), "rBLAST package not installed.")

  # Mock sequences
  mock_sequences <- c("ATGCGTACGTAG", "CGTAGCTAGCTA", "TGCATGCTAGCT")

  # Path to mock BLAST database
  mock_db <- tempfile()
  dir.create(mock_db)
  # Create a mock database (requires makeblastdb to be available)
  writeLines(c(">MockSeq1", "ATGCGTACGTAG", ">MockSeq2", "CGTAGCTAGCTA"), file.path(mock_db, "mock.fasta"))
  system(paste("makeblastdb -in", file.path(mock_db, "mock.fasta"), "-dbtype nucl -out", file.path(mock_db, "mock_db")))

  # Perform BLAST
  result <- run_blast(mock_sequences, database = file.path(mock_db, "mock_db"), remote = FALSE)

  # Validate result
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  # Cleanup
  unlink(mock_db, recursive = TRUE)
})

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
