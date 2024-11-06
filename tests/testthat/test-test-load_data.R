test_that("load_data loads data correctly", {
  test_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(SubstanceUseType = c("Smoker", "Cannabis", "Non-user"), Species = c("Streptococcus", "Prevotella", "Fusobacterium"), Count = c(50, 30, 20)), test_file, row.names = FALSE)

  data <- load_data(test_file)

  expect_s3_class(data, "data.frame")
  expect_equal(ncol(data), 3)
  expect_equal(nrow(data), 3)
  expect_true("SubstanceUseType" %in% colnames(data))
})
