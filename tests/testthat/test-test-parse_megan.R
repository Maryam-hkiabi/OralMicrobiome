test_that("parse_megan parses MEGAN XML files correctly", {
  # Create a temporary MEGAN XML file
  megan_file <- tempfile(fileext = ".xml")
  writeLines(c(
    "<root>",
    "  <taxonomy>Genus A</taxonomy>",
    "  <count>20</count>",
    "  <taxonomy>Genus B</taxonomy>",
    "  <count>30</count>",
    "  <taxonomy>Genus A</taxonomy>",
    "  <count>10</count>",
    "</root>"
  ), megan_file)

  # Run the function
  parsed_data <- parse_megan(megan_file)

  # Check structure
  expect_true(is.data.frame(parsed_data))
  expect_true("Taxonomy" %in% colnames(parsed_data))
  expect_true("TotalCount" %in% colnames(parsed_data))

  # Check values
  expect_equal(nrow(parsed_data), 2)  # Two unique taxa
  expect_equal(parsed_data$TotalCount[parsed_data$Taxonomy == "Genus A"], 30)
  expect_equal(parsed_data$TotalCount[parsed_data$Taxonomy == "Genus B"], 30)

  # Test for a non-existent file
  expect_error(parse_megan("nonexistent.xml"), "The specified MEGAN file does not exist.")
})
