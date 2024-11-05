test_that("visualize_taxonomy creates taxonomy plot", {

  # Sample taxonomic data

  taxonomy_data <- data.frame(taxonomy_level = c("Genus", "Genus"), substance_type = c("Smoker", "Non-user"))

  # Test visualization function
  plot <- visualize_taxonomy(taxonomy_data)

  # Expectations
  expect_s3_class(plot, "ggplot")
})
