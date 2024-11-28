#' Visualize taxonomy profile by substance type
#' need to finish
#' need to find substance use only data
#'
#'
#'
#' @param taxonomy_data Data frame with taxonomic profiles.
#' @return A ggplot object showing taxonomy profile.
visualize_taxonomy <- function(taxonomy_data) {
  library(ggplot2)

  p <- ggplot(taxonomy_data, aes(x = SubstanceUseType, fill = Species)) +
    geom_bar() +
    labs(title = "Taxonomic Profile by Substance Type", x = "Substance Use Type", y = "Count") +
    theme_minimal()

  return(p)
}

#' Visualize Taxonomy
#'
#' This function creates a bar plot of taxonomic profiles.
#' @param tax_data A data frame containing taxonomic data.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot geom_bar aes theme_minimal
#' @export
visualize_taxonomy <- function(tax_data) {
  ggplot(tax_data, aes(x = Taxon, y = Abundance, fill = Group)) +
    geom_bar(stat = "identity") +
    theme_minimal()
}
