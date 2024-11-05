test_that("plot_species_substance creates species distribution plot", {

  # Sample data

  data <- data.frame(SubstanceUseType = c("Smoker", "Cannabis"), Species = c("Streptococcus", "Prevotella"))

  # Test plotting function
  plot <- plot_species_substance(data)

  # Expectations
  expect_s3_class(plot, "ggplot")
})
