test_that("parse_megan parses MEGAN output", {
  megan_file <- "path/to/sample_megan_output.txt"
  result <- parse_megan(megan_file)

  expect_s3_class(result, "data.frame")
})
