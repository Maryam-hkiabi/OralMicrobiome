#' Plot Microbial Species Across Substance Use Types
#'
#' This function creates a bar plot showing microbial species distribution across substance user types.
#'
#' @param data A data frame containing microbial species and substance use data.
#' Must include columns: "SubstanceUseType" and "Species".
#' @param interactive Logical. If `TRUE`, returns an interactive Plotly plot. Default is `FALSE`.
#' @return A `ggplot2` object or a `plotly` object representing the distribution of species by substance use type.
#' @details The plot shows the count of each microbial species grouped by substance use type.
#' @importFrom ggplot2 ggplot aes geom_bar labs scale_fill_brewer theme_minimal
#' @importFrom plotly ggplotly
#' @export

plot_species_substance <- function(data, interactive = FALSE) {
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  required_columns <- c("SubstanceUseType", "Species")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("The data is missing the following required column(s):", paste(missing_columns, collapse = ", ")))
  }

  # Generate the ggplot
  library(ggplot2)
  p <- ggplot(data, aes(x = SubstanceUseType, fill = Species)) +
    geom_bar(position = "dodge") +
    labs(
      title = "Species Distribution by Substance Use Type",
      x = "Substance Use Type",
      y = "Count",
      fill = "Species"
    ) +
    scale_fill_brewer(palette = "Set3") +  # Use a colorblind-friendly palette
    theme_minimal()

  # Convert to interactive Plotly plot if requested
  if (interactive) {
    library(plotly)
    p <- ggplotly(p)
  }

  return(p)
}
