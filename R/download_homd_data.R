#' Download microbial species data from the Human Oral Microbiome Database (HOMD)
#' Fourth Function
#' need to finish
#' need to somehow implement that its only for substance use
#'
#' @param genus Optional filter by genus (e.g., "Prevotella").
#' @return Data frame with HOMD species data.
download_homd_data <- function(genus = NULL) {
  library(httr)
  library(jsonlite)

  url <- "https://www.homd.org/api/microbial/species"
  if (!is.null(genus)) {
    url <- paste0(url, "?genus=", genus)
  }

  response <- GET(url)
  if (status_code(response) != 200) {
    stop("Failed to retrieve data from HOMD.")
  }

  data <- fromJSON(content(response, "text"), flatten = TRUE)
  return(data)
}
