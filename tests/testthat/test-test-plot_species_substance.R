test_that("plot_species_substance creates species distribution plot", {
  data <- data.frame(
    SubstanceUseType = c("Smoker", "Cannabis", "Non-user"),
    Species = c("Streptococcus", "Prevotella", "Fusobacterium"),
    Count = c(50, 30, 20)
  )

  plot <- plot_species_substance(data)

  expect_s3_class(plot, "ggplot")
})
