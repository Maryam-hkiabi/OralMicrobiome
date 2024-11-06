test_that("visualize_taxonomy creates taxonomy plot", {
  taxonomy_data <- data.frame(
    SubstanceUseType = c("Smoker", "Cannabis", "Non-user"),
    Species = c("Streptococcus", "Prevotella", "Fusobacterium"),
    Count = c(50, 30, 20)
  )

  plot <- visualize_taxonomy(taxonomy_data)

  expect_s3_class(plot, "ggplot")
})
