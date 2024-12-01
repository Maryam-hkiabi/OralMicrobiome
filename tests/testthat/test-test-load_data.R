test_that("load_data handles CSV files correctly", {
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

  result <- load_data(temp_file, file_type = "csv")

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("SubstanceUseType", "Species", "Abundance"))
  expect_true(all(result$SubstanceUseType %in% c("Type1", "Type2")))

  unlink(temp_file)
})

test_that("load_data handles Excel files correctly", {
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

  result <- load_data(temp_file, file_type = "excel")

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("SubstanceUseType", "Species", "Abundance"))
  expect_true(all(result$SubstanceUseType %in% c("Type1", "Type2")))

  unlink(temp_file)
})

test_that("load_data handles JSON files correctly", {
  temp_file <- tempfile(fileext = ".json")
  jsonlite::write_json(
    data.frame(
      SubstanceUseType = c("Type1", "Type2"),
      Species = c("Bacteria1", "Bacteria2"),
      Abundance = c(10, 15)
    ),
    temp_file
  )

  result <- load_data(temp_file, file_type = "json")

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("SubstanceUseType", "Species", "Abundance"))
  expect_true(all(result$SubstanceUseType %in% c("Type1", "Type2")))

  unlink(temp_file)
})

test_that("load_data handles FASTA files correctly", {
  temp_file <- tempfile(fileext = ".fasta")
  cat(">seq1\nATGCGTACGTAGCTAG\n>seq2\nCGTAGCTAGCTAGTCA\n", file = temp_file)

  result <- load_data(temp_file, file_type = "fasta")

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("SequenceName", "Sequence"))
  expect_true(all(result$SequenceName %in% c("seq1", "seq2")))

  unlink(temp_file)
})

test_that("load_data handles missing columns", {
  temp_file <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      Species = c("Bacteria1", "Bacteria2"),
      Abundance = c(10, 15)
    ),
    temp_file,
    row.names = FALSE
  )

  expect_error(load_data(temp_file, file_type = "csv"), "missing the following required column")
  unlink(temp_file)
})

test_that("load_data handles unsupported file types", {
  temp_file <- tempfile(fileext = ".txt")
  writeLines("Invalid content", temp_file)

  expect_error(load_data(temp_file, file_type = "txt"), "Unsupported file type")
  unlink(temp_file)
})
