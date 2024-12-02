#' Compare Microbial Communities Between Substance Use Types
#'
#' This function compares microbial communities across different types of substance use.
#'
#' @param data A data frame containing microbial abundance data. Must include columns for substance use type and microbial species.
#' @param substance_col The column name indicating substance use type (e.g., "SubstanceUseType").
#' @return A list with three elements:
#' - `summarized_data`: A data frame summarizing species counts by substance type.
#' - `anova_results`: A data frame with statistical results (ANOVA + Tukey's HSD).
#' - `visualization`: A ggplot object visualizing species abundance by substance type.
#' @importFrom dplyr group_by summarise full_join mutate arrange
#' @importFrom tidyr replace_na
#' @importFrom stats aov TukeyHSD
#' @importFrom ggplot2 ggplot aes_string geom_boxplot theme_minimal labs
#' @export

compare_substances <- function(data, substance_col) {
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  # Debug: Print column names
  print("Debug: Column names in input data:")
  print(colnames(data))

  # Ensure required columns are present
  if (!all(c(substance_col, "Species", "Abundance") %in% colnames(data))) {
    stop(paste("Input data must contain the columns:", substance_col, "Species, and Abundance."))
  }

  # Ensure substance_col is treated as a factor
  data[[substance_col]] <- as.factor(data[[substance_col]])

  # Generate a full grid of all possible combinations
  all_combinations <- expand.grid(
    SubstanceUseType = unique(data[[substance_col]]),
    Species = unique(data$Species)
  )
  colnames(all_combinations)[1] <- substance_col  # Rename to match substance_col

  # Summarize species counts by substance type and species
  summarized_data <- data %>%
    dplyr::group_by(.data[[substance_col]], Species) %>%
    dplyr::summarise(TotalCount = sum(Abundance, na.rm = TRUE), .groups = "drop") %>%
    dplyr::full_join(all_combinations, by = c(substance_col, "Species")) %>%
    dplyr::mutate(TotalCount = tidyr::replace_na(TotalCount, 0)) %>%
    dplyr::arrange(Species, .data[[substance_col]])

  # Perform ANOVA and Tukey's HSD for each species
  anova_results <- lapply(unique(data$Species), function(species) {
    species_data <- data[data$Species == species, ]

    # Check if there are enough groups for ANOVA
    if (length(unique(species_data[[substance_col]])) > 1) {
      aov_model <- stats::aov(Abundance ~ get(substance_col), data = species_data)
      tukey <- stats::TukeyHSD(aov_model)
      return(data.frame(
        Species = species,
        PairwiseComparisons = rownames(tukey[[1]]),
        Difference = tukey[[1]][, "diff"],
        p_value = tukey[[1]][, "p adj"]
      ))
    } else {
      return(NULL)  # Skip species with only one substance type
    }
  })

  # Combine all results into a single data frame
  anova_results <- do.call(rbind, anova_results)

  # Visualization using ggplot2
  visualization <- ggplot2::ggplot(data, ggplot2::aes_string(x = substance_col, y = "Abundance", fill = "Species")) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Species Abundance by Substance Type",
                  x = substance_col,
                  y = "Abundance")

  # Return both summarized data, statistical results, and visualization
  return(list(
    summarized_data = summarized_data,
    anova_results = anova_results,
    visualization = visualization
  ))
}

# Testing example:
# test_data <- data.frame(
#  Species = c("A", "A", "A", "B", "B", "C", "C", "C"),
#  SubstanceUseType = c("Type1", "Type2", "Type3", "Type1", "Type2", "Type1", "Type2", "Type3"),
#  Abundance = c(10, 15, 20, 8, 12, 5, 7, 6)
#)

# Run the function
#comparison_results <- compare_substances(test_data, substance_col = "SubstanceUseType")

# Print summarized data
# print(comparison_results$summarized_data)

# Print ANOVA results
# print(comparison_results$anova_results)

# View visualization
# print(comparison_results$visualization)

