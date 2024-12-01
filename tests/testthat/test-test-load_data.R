test_that("load_data handles CSV files correctly", {
  # Mock CSV file
  temp_file <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      SubstanceUseType = c("Type1", "Type2"),
      Species = c("Bacteria1", "Bacteria2"),
      Abundance = c(10, 15)
    ),
    temp_file,
    row.names = FALSE
  )

  # Load data
  result <- load_data(temp_file, file_type = "csv")

  # Validate
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("SubstanceUseType", "Species", "Abundance"))
  expect_true(all(result$SubstanceUseType %in% c("Type1", "Type2")))

  # Cleanup
  unlink(temp_file)
})

test_that("load_data handles Excel files correctly", {
  # Mock Excel file
  temp_file <- tempfile(fileext = ".xlsx")
  library(openxlsx)
  write.xlsx(
    data.frame(
      SubstanceUseType = c("Type1", "Type2"),
      Species = c("Bacteria1", "Bacteria2"),
      Abundance = c(10, 15)
    ),
    temp_file
  )

  # Load data
  result <- load_data(temp_file, file_type = "excel")

  # Validate
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("SubstanceUseType", "Species", "Abundance"))
  expect_true(all(result$SubstanceUseType %in% c("Type1", "Type2")))

  # Cleanup
  unlink(temp_file)
})

test_that("load_data handles JSON files correctly", {
  # Mock JSON file
  temp_file <- tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      SubstanceUseType = c("Type1", "Type2"),
      Species = c("Bacteria1", "Bacteria2"),
      Abundance = c(10, 15)
    ),
    temp_file
  )

  # Load data
  result <- load_data(temp_file, file_type = "json")

  # Validate
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("SubstanceUseType", "Species", "Abundance"))
  expect_true(all(result$SubstanceUseType %in% c("Type1", "Type2")))

  # Cleanup
  unlink(temp_file)
})

test_that("load_data handles FASTA files correctly", {
  # Mock FASTA file
  temp_file <- tempfile(fileext = ".fasta")
  cat(">seq1\nATGCGTACGTAGCTAG\n>seq2\nCGTAGCTAGCTAGTCA\n", file = temp_file)

  # Load FASTA data
  result <- load_data(temp_file, file_type = "fasta")

  # Validate
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("SequenceName", "Sequence"))
  expect_true(all(result$SequenceName %in% c("seq1", "seq2")))

  # Cleanup
  unlink(temp_file)
})

test_that("load_data handles missing columns", {
  # Mock CSV file
  temp_file <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      Species = c("Bacteria1", "Bacteria2"),
      Abundance = c(10, 15)
    ),
    temp_file,
    row.names = FALSE
  )

  # Test for error
  expect_error(load_data(temp_file, file_type = "csv"), "missing the following required column")
  unlink(temp_file)
})

test_that("load_data handles unsupported file types", {
  expect_error(load_data("invalid_file.txt", file_type = "txt"), "Unsupported file type")
})
