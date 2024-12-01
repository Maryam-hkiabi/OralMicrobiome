test_that("parse_megan parses valid MEGAN XML files correctly", {
  # Mock MEGAN XML file
  temp_file <- tempfile(fileext = ".xml")
  cat(
    '<root>
      <taxonomy>Species1</taxonomy><count>10</count>
      <taxonomy>Species2</taxonomy><count>15</count>
      <taxonomy>Species1</taxonomy><count>5</count>
    </root>',
    file = temp_file
  )

  # Parse MEGAN data (data frame)
  result <- parse_megan(temp_file, to_phyloseq = FALSE)

  # Validate the result
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("Taxonomy", "TotalCount"))
  expect_equal(result$Taxonomy, c("Species1", "Species2"))
  expect_equal(result$TotalCount, c(15, 15)) # Summed counts

  # Parse MEGAN data (phyloseq object)
  phyloseq_result <- parse_megan(temp_file, to_phyloseq = TRUE)

  # Validate the phyloseq object
  expect_s4_class(phyloseq_result, "phyloseq")
  expect_true("otu_table" %in% slotNames(phyloseq_result))
  expect_true("tax_table" %in% slotNames(phyloseq_result))
  expect_equal(taxa_names(phyloseq_result), c("Species1", "Species2"))

  # Cleanup
  unlink(temp_file)
})

test_that("parse_megan handles missing or mismatched data gracefully", {
  # Mock MEGAN XML with mismatched entries
  temp_file <- tempfile(fileext = ".xml")
  cat(
    '<root>
      <taxonomy>Species1</taxonomy><count>10</count>
      <taxonomy>Species2</taxonomy>
    </root>',
    file = temp_file
  )

  # Expect an error for mismatched data
  expect_error(parse_megan(temp_file), "Mismatch between taxonomy entries and counts")

  # Cleanup
  unlink(temp_file)
})

test_that("parse_megan handles invalid file paths gracefully", {
  expect_error(parse_megan("nonexistent_file.xml"), "does not exist")
})

test_that("parse_megan handles invalid XML files gracefully", {
  # Mock invalid XML file
  temp_file <- tempfile(fileext = ".xml")
  cat(
    '<root>
      <taxonomy>Species1<taxonomy><count>10</count>
    </root>',
    file = temp_file
  )

  # Expect an error for invalid XML
  expect_error(parse_megan(temp_file), "Error reading the XML file")

  # Cleanup
  unlink(temp_file)
})
