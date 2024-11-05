test_that("download_homd_data retrieves HOMD data", {

  result <- download_homd_data("Streptococcus")

  # Expectations
  expect_s3_class(result, "data.frame")
  expect_true("genus" %in% colnames(result))  # Assuming HOMD data includes a genus column
})
