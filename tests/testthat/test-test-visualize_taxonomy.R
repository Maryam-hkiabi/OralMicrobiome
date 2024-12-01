test_that("visualize_taxonomy creates a ggplot2 plot", {
  # Mock data
  mock_data <- data.frame(
    SubstanceUseType = c("Type1", "Type1", "Type2", "Type2", "Type3"),
    Species = c("Species1", "Species2", "Species1", "Species3", "Species2"),
    Abundance = c(10, 15, 20, 5, 7)
  )

  # Generate static plot
  plot <- visualize_taxonomy(mock_data, interactive = FALSE)

  # Validate ggplot2 object
  expect_s3_class(plot, "ggplot")
})

test_that("visualize_taxonomy creates a plotly interactive plot", {
  # Mock data
  mock_data <- data.frame(
    SubstanceUseType = c("Type1", "Type1", "Type2", "Type2", "Type3"),
    Species = c("Species1", "Species2", "Species1", "Species3", "Species2"),
    Abundance = c(10, 15, 20, 5, 7)
  )

  # Generate interactive plot
  plot <- visualize_taxonomy(mock_data, interactive = TRUE)

  # Validate plotly object
  expect_s3_class(plot, "plotly")
})

test_that("visualize_taxonomy handles missing columns gracefully", {
  # Mock data with missing columns
  mock_data <- data.frame(
    SubstanceUseType = c("Type1", "Type1"),
    Species = c("Species1", "Species2")
  )

  # Expect error
  expect_error(visualize_taxonomy(mock_data), "missing the following required column")
})

test_that("visualize_taxonomy handles invalid input gracefully", {
  # Invalid input (not a data frame)
  expect_error(visualize_taxonomy("invalid_input"), "must be a data frame")
})
