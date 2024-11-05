#' Plot species distribution by substance type
#' need to finish
#' need to find substance use only data first
#'
#' @param data Microbiome data frame.
#' @return A ggplot2 object for visualization.
plot_species_substance <- function(data) {
  library(ggplot2)
  ggplot(data, aes(x = SubstanceUseType, fill = Species)) +
    geom_bar() +
    labs(title = "Species Distribution by Substance Type")
}
