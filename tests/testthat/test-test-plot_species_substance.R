test_that("plot_species_substance creates a ggplot2 plot", {
  # Mock data
  mock_data <- data.frame(
    SubstanceUseType = c("Type1", "Type1", "Type2", "Type2", "Type3"),
    Species = c("Species1", "Species2", "Species1", "Species3", "Species2")
  )

  # Generate plot
  plot <- plot_species_substance(mock_data, interactive = FALSE)

  # Validate ggplot2 object
  expect_s3_class(plot, "ggplot")
})

test_that("plot_species_substance creates a plotly interactive plot", {
  # Mock data
  mock_data <- data.frame(
    SubstanceUseType = c("Type1", "Type1", "Type2", "Type2", "Type3"),
    Species = c("Species1", "Species2", "Species1", "Species3", "Species2")
  )

  # Generate interactive plot
  plot <- plot_species_substance(mock_data, interactive = TRUE)

  # Validate plotly object
  expect_s3_class(plot, "plotly")
})

test_that("plot_species_substance handles missing columns gracefully", {
  # Mock data with missing columns
  mock_data <- data.frame(
    Species = c("Species1", "Species2")
  )

  # Expect error
  expect_error(plot_species_substance(mock_data), "missing the following required column")
})

test_that("plot_species_substance handles invalid input gracefully", {
  # Invalid input (not a data frame)
  expect_error(plot_species_substance("invalid_input"), "must be a data frame")
})
