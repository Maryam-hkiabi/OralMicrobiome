test_that("compare_groups compares groups correctly", {
  # testing on sample dataset
  # test doesnt work

  data <- data.frame(SubstanceUseType = c("Smoker", "Non-user", "Smoker"), Species = c("Streptococcus", "Fusobacterium", "Neisseria"))
  result <- compare_groups(data)

  expect_s3_class(result, "data.frame")
})
