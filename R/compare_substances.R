#' Compare Microbial Communities Between Substance Use Types
#'
#' This function compares microbial communities across different types of substance use.
#'
#' @param data A data frame containing microbial abundance data. Must include columns for substance use type and microbial species.
#' @param substance_col The column name indicating substance use type (e.g., "SubstanceUseType").
#' @return A list with two elements:
#' - `summarized_data`: A data frame summarizing species counts by substance type.
#' - `anova_results`: A data frame with statistical results (ANOVA + Tukey's HSD).
#' @details The function performs the following steps:
#' - Groups data by substance type and species, summarizing the total abundance for each.
#' - Performs ANOVA to compare abundance across substance use types for each species.
#' - Applies Tukey's post hoc test to identify specific pairwise differences.
#' @importFrom dplyr group_by summarise arrange
#' @importFrom stats aov TukeyHSD
#' @export
compare_substances <- function(data, substance_col) {
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame.")
  }
  if (!all(c(substance_col, "Species", "Abundance") %in% colnames(data))) {
    stop(paste("Input data must contain the columns:", substance_col, "Species, and Abundance."))
  }

  # Ensure substance_col is treated as a factor
  data[[substance_col]] <- as.factor(data[[substance_col]])

  # Summarize species counts by substance type and species
  summarized_data <- data %>%
    group_by(!!sym(substance_col), Species) %>%
    summarise(TotalCount = sum(Abundance, na.rm = TRUE), .groups = "drop") %>%
    arrange(Species, !!sym(substance_col))

  # Perform ANOVA and Tukey's HSD for each species
  anova_results <- lapply(unique(data$Species), function(species) {
    species_data <- data[data$Species == species, ]
    if (length(unique(species_data[[substance_col]])) > 1) {
      aov_model <- aov(Abundance ~ .data[[substance_col]], data = species_data)
      tukey <- TukeyHSD(aov_model)
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

  # Return both summarized data and statistical results
  return(list(
    summarized_data = summarized_data,
    anova_results = anova_results
  ))
}


# testing
test_data <- data.frame(
  Species = c("A", "A", "A", "B", "B", "C", "C", "C"),
  SubstanceUseType = c("Type1", "Type2", "Type3", "Type1", "Type2", "Type1", "Type2", "Type3"),
  Abundance = c(10, 15, 20, 8, 12, 5, 7, 6)
)

str(test_data)
colnames(test_data)

comparison_results <- compare_substances(test_data, substance_col = "SubstanceUseType")

# Print summarized data
print(comparison_results$summarized_data)

# Print ANOVA + Tukey results
print(comparison_results$anova_results)


