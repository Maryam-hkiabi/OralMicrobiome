test_that("plot_species_substance creates a valid ggplot object", {
  # Test data
  test_data <- data.frame(
    SubstanceUseType = c("Type1", "Type2", "Type1", "Type3", "Type2", "Type3", "Type1"),
    Species = c("A", "A", "B", "B", "C", "C", "A")
  )

  # Run the function
  plot <- plot_species_substance(test_data)

  # Check that the output is a ggplot object
  expect_s3_class(plot, "ggplot")

  # Check for required aesthetics in the plot
  expect_true("SubstanceUseType" %in% ggplot2::ggplot_build(plot)$data[[1]]$x)
  expect_true("Species" %in% names(ggplot2::ggplot_build(plot)$plot$mapping))

  # Test with missing columns
  incomplete_data <- data.frame(
    SubstanceUseType = c("Type1", "Type2")
  )
  expect_error(plot_species_substance(incomplete_data), "The data is missing the following required column")
})
