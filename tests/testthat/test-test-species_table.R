test_that("species_table generates species summary table", {

  # Sample data

  data <- data.frame(SubstanceUseType = c("Smoker", "Cannabis", "Non-user"), Species = c("Streptococcus", "Prevotella", "Fusobacterium"))

  # Test species table generation
  result <- species_table(data)

  # Expectations
  expect_s3_class(result, "data.frame")
})
