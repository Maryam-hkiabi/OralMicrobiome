#' Plot species distribution by substance type
#' need to finish
#' need to find substance use only data first
#'
#'
#' @param data Microbiome data frame.
#' @return A ggplot object showing species distribution by substance type.
plot_species_substance <- function(data) {
  library(ggplot2)

  p <- ggplot(data, aes(x = SubstanceUseType, fill = Species)) +
    geom_bar(position = "dodge") +
    labs(title = "Species Distribution by Substance Type", x = "Substance Use Type", y = "Count") +
    theme_minimal()

  return(p)
}


#' Plot Microbial Species Across Substance Use Types
#'
#' This function creates a plot showing microbial species distribution across substance user types.
#' @param data A data frame containing microbial species and substance use data.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal
#' @export
plot_species_substance <- function(data) {
  # Function code here
}
