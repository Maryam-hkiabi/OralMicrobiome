#' Visualize Taxonomy Profile by Substance Use Type
#'
#' This function creates a bar plot or an interactive plot of taxonomic profiles by substance use type.
#'
#' @param taxonomy_data A data frame containing taxonomic data.
#' Must include columns: "SubstanceUseType", "Species", and "Abundance".
#' @param interactive Logical. If `TRUE`, creates an interactive plot using `plotly`. Default is `FALSE`.
#' @return A `ggplot2` object or a `plotly` object.
#' @details This function summarizes taxonomic profiles by substance use type and visualizes them as a bar plot.
#' @importFrom ggplot2 ggplot aes geom_bar labs scale_fill_brewer theme_minimal
#' @importFrom plotly ggplotly
#' @export
visualize_taxonomy <- function(taxonomy_data, interactive = FALSE) {
  # Validate input
  if (!is.data.frame(taxonomy_data)) {
    stop("Input must be a data frame.")
  }
  required_columns <- c("SubstanceUseType", "Species", "Abundance")
  missing_columns <- setdiff(required_columns, colnames(taxonomy_data))
  if (length(missing_columns) > 0) {
    stop(paste("The data is missing the following required column(s):", paste(missing_columns, collapse = ", ")))
  }

  library(ggplot2)

  # Create a ggplot object
  p <- ggplot(taxonomy_data, aes(x = SubstanceUseType, y = Abundance, fill = Species)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(
      title = "Taxonomic Profile by Substance Use Type",
      x = "Substance Use Type",
      y = "Abundance",
      fill = "Species"
    ) +
    scale_fill_brewer(palette = "Set3") +
    theme_minimal()

  # Convert to interactive plot if requested
  if (interactive) {
    library(plotly)
    p <- ggplotly(p)
  }

  return(p)
}
