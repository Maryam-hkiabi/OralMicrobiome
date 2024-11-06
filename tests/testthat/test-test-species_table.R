test_that("species_table generates species summary table", {
  data <- data.frame(SubstanceUseType = c("Smoker", "Cannabis", "Non-user"), Species = c("Streptococcus", "Prevotella", "Fusobacterium"))
  result <- species_table(data)

  expect_s3_class(result, "data.frame")
})

